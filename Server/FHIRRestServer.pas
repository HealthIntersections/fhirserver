Unit FHIRRestServer;

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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
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
{$IFDEF MSWINDOWS} Windows, ActiveX, ComObj, {$ELSE} OSXUtils, {$ENDIF} SyncObjs,
  SysUtils, Classes, IniFiles, System.Generics.Collections, {JCL JclDebug,}
  EncdDecd, HMAC, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer,
  IdTCPServer, IdContext, IdSSLOpenSSL, IdHTTP, MimeMessage, IdCookie,
  IdZLibCompressorBase, IdCompressorZLib, IdZLib, IdSSLOpenSSLHeaders,
  IdGlobalProtocols, IdWebSocket,

  EncodeSupport, GUIDSupport, DateSupport, BytesSupport, StringSupport,
  ThreadSupport, CertificateSupport, SystemSupport, HashSupport, Logging,

  AdvBuffers, AdvObjectLists, AdvStringMatches, AdvZipParts, AdvZipReaders,
  AdvVCLStreams, AdvMemories, AdvIntegerObjectMatches, AdvExceptions,
  AdvGenerics, AdvFiles, AdvZipWriters,

  kCritSct, ParseMap, TextUtilities, KDBManager, HTMLPublisher, KDBDialects,
  AdvJSON, libeay32, RDFUtilities, JWT,

  MXML, GraphQL, {$IFDEF MSWINDOWS} MsXml, MsXmlParser, {$ENDIF}
  FHIRTypes, FHIRResources, FHIRParser, FHIRConstants,
  FHIRBase, FHIRParserBase, FHIRTags, FHIRSupport, FHIRLang, FHIRStorageService,
  FHIRUtilities, FHIRSecurity, SmartOnFhirUtilities,
  QuestionnaireBuilder, FHIRClient, CDSHooksUtilities, CDSHooksClientManager,
  FHIRXhtml, FHIRGraphQL,

  TerminologyServer, TerminologyServerStore, SnomedServices, SnomedPublisher,
  SnomedExpressions, LoincServices, LoincPublisher,
  TerminologyWebServer, AuthServer, TwilioClient, ReverseClient, CDSHooksServer,
  WebSourceProvider,

  FHIRUserProvider, FHIRServerContext, FHIRServerConstants, SCIMServer,
  ServerUtilities, ClientApplicationVerifier, JWTService, TerminologyServices, ServerJavascriptHost,
  ServerPostHandlers{$IFNDEF FHIR2}, OpenMHealthServer{$ENDIF};

Const
  OWIN_TOKEN_PATH = 'oauth/token';

Type
  ERestfulAuthenticationNeeded = class(ERestfulException)
  private
    FMsg: String;
  public
    Constructor Create(Const sSender, sMethod, sReason, sMsg: String; aStatus: word); Overload; Virtual;
    Property Msg: String read FMsg;
  end;

  TFhirWebServer = class;

  TFhirServerMaintenanceThread = class(TThread)
  private
    FServer: TFhirWebServer;
    FLastSweep: TDateTime;
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  TFhirServerSubscriptionThread = class(TThread)
  private
    FServer: TFhirWebServer;
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  TFhirServerEmailThread = class(TThread)
  private
    FServer: TFhirWebServer;
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  TAsyncTaskThread = class(TThread)
  private
    FKey : integer;
    FServer : TFhirWebServer;
    FRequest : TFHIRRequest;
    FFormat : TFHIRFormat;
    files : TAdvMap<TAdvFile>;
    FBundle : TFHIRBundle;
    procedure SetRequest(const Value: TFHIRRequest);
    procedure SetServer(const Value: TFhirWebServer);

    procedure status(status : TAsyncTaskStatus; message : String);
    procedure details;
    procedure callback(IntParam: Integer; StrParam: String);

    procedure saveOutcome(response : TFHIRResponse);
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TFhirBundleTypeEnum; out builder : TFhirBundleBuilder);
  protected
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    procedure kill;

    property Key : integer read FKey write FKey;
    property Format : TFHIRFormat read FFormat write FFormat;
    Property Server : TFhirWebServer read FServer write SetServer;
    Property Request : TFHIRRequest read FRequest write SetRequest;
  end;

  TFHIRWebServerClientInfo = class(TAdvObject)
  private
    FContext: TIdContext;
    FActivity: String;
    FSession: TFHIRSession;
    FCount: integer;
    FStart: cardinal;
    procedure SetSession(const Value: TFHIRSession);
  public
    Destructor Destroy; Override;
    property Context: TIdContext read FContext write FContext;
    property Session: TFHIRSession read FSession write SetSession;
    property Activity: String read FActivity write FActivity;
    property Count: integer read FCount write FCount;
  end;

{$IFDEF MSWINDOWS}

  TFHIRWebServerPatientViewContext = class(TAdvObject)
  private
    FCards: TAdvList<TCDSHookCard>;
    FErrors: TStringList;
    FManager: TCDSHooksManager;
    procedure SetManager(const Value: TCDSHooksManager);
  public
    constructor Create; Override;
    Destructor Destroy; Override;
    property manager: TCDSHooksManager read FManager write SetManager;
    property Errors: TStringList read FErrors;
    property cards: TAdvList<TCDSHookCard> read FCards;
  end;
{$ENDIF}

  TFhirWebServer = Class(TAdvObject)
  Private
    FIni: TFHIRServerIniFile;
    FLock: TCriticalSection;

    // base web server configuration
    FActive: boolean;
    // can start without actually making the web servers available - for internal use e.g. loading...
    FName: String; // name of this server
    FOwnerName: String; // name of the org that adminsiters the service
    FHomePage: String;
    FAdminEmail: String; // stated administrator
    FFacebookLike: boolean;
    FHostSms: String; // for status update messages
    FSourceProvider: TFHIRWebServerSourceProvider;

    // web configuration
    FHost: String;
    FActualPort: integer;
    FBasePath: String;
    FActualSSLPort: integer;
    FSecurePath: String;
    FCertFile: String;
    FRootCertFile: String;
    FSSLPassword: String;
    FInLog : TLogger;
    FOutLog : TLogger;

    // security admin
    FUseOAuth: boolean;
    FOWinSecurityPlain: boolean;
    FOWinSecuritySecure: boolean;
    FServeMissingCertificate: boolean;
    FServeUnknownCertificate: boolean;
    FCertificateIdList: TStringList;
    FServeMissingJWT: boolean;
    FServeUnverifiedJWT: boolean;
    FJWTAuthorities: TDictionary<String, String>;

    // Reverse proxy support. stated vs actual: to allow for a reverse proxy
    FStatedPort: integer;
    FStatedSSLPort: integer;
    FSecureToken: String;

    // operational fields
    FPlainServer: TIdHTTPServer;
    FSSLServer: TIdHTTPServer;
    FIOHandler: TIdServerIOHandlerSSLOpenSSL;
    FTotalCount: cardinal;
    FRestCount: cardinal;
    FStartTime: cardinal;
    FTotalTime: cardinal;
    FRestTime: cardinal;
    FClients: TAdvList<TFHIRWebServerClientInfo>;
    FServerContext: TFHIRServerContext;
    FTerminologyWebServer: TTerminologyWebServer;
    FMaintenanceThread: TFhirServerMaintenanceThread;
    FSubscriptionThread: TFhirServerSubscriptionThread;
    FEmailThread: TFhirServerEmailThread;
    FAuthServer: TAuth2Server;
    FAdaptors: TAdvMap<TFHIRFormatAdaptor>;
    carry: TAdvZipReader; // for uploading support
    carryName: String;
    FPatientViewServers: TDictionary<String, String>;
{$IFDEF MSWINDOWS}
    FPatientHooks: TAdvMap<TFHIRWebServerPatientViewContext>;
{$ENDIF}
    FReverseProxyList: TAdvList<TReverseProxyInfo>;
    FReverseProxyByVersion : TAdvMap<TReverseProxyInfo>;
    FCDSHooksServer: TCDSHooksServer;
    FIsTerminologyServerOnly: boolean;
    FThreads : TList<TAsyncTaskThread>;

    function readVersion(mt : String) : TFHIRVersion;
    procedure convertFromVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion);
    procedure convertToVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion);
    function OAuthPath(secure: boolean): String;
    procedure PopulateConformanceAuth(rest: TFhirCapabilityStatementRest);
    procedure PopulateConformance(sender: TObject; conf: TFhirCapabilityStatement);
    function WebDump: String;
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TFhirBundleTypeEnum; out builder : TFhirBundleBuilder);

    function hasInternalSSLToken(request: TIdHTTPRequestInfo): boolean;
    procedure cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
{$IFDEF MSWINDOWS}
    procedure OnCDSResponse(manager: TCDSHooksManager; server: TRegisteredFHIRServer; Context: TObject; response: TCDSHookResponse; error: String);
{$ENDIF}
    function GetResource(Session: TFHIRSession; rtype: String; lang, id, ver, op: String): TFhirResource;
    function FindResource(Session: TFHIRSession; rtype: String; lang, params: String): TFhirResource;
    function DoSearch(Session: TFHIRSession; rtype: string; lang, params: String): TFHIRBundle;
    function LookupReference(Context: TFHIRRequest; id: String): TResourceWithReference;
{$IFDEF MSWINDOWS}
    function transform1(resource: TFhirResource; lang, xslt: String; saveOnly: boolean): string;
    function HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
{$ENDIF}
    function HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
{$IFDEF MSWINDOWS}
    procedure startHooks(ctxt: TFHIRWebServerPatientViewContext; patient: TFHIRPatient; url: String);
    function HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
{$ENDIF}
    function patientAppList(base, id : String) : string;
    function HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function getReferencesByType(t : String) : String;

    procedure logRequest(secure : boolean; id : String; request : TIdHTTPRequestInfo);
    procedure logResponse(id : String; resp : TIdHTTPResponseInfo);

    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    // Procedure ReadTags(Headers: TIdHeaderList; Request : TFHIRRequest); overload;
    Procedure ReadTags(header: String; request: TFHIRRequest); overload;
    function CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
    Function BuildFhirHomePage(compList : TAdvList<TFHIRCompartmentId>; logId, lang, host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
    Function BuildFhirAuthenticationPage(lang, host, path, logId, Msg: String; secure: boolean): String;
    Function BuildFhirUploadPage(lang, host, sBaseURL: String; aType: String; Session: TFHIRSession): String;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    Procedure ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: boolean);
    Procedure ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkExit(AContext: TIdContext);
    Procedure ReverseProxy(proxy: TReverseProxyInfo; AContext: TIdContext; request: TIdHTTPRequestInfo; Session: TFHIRSession; response: TIdHTTPResponseInfo; secure: boolean);
    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdX509);
    Procedure HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
    Procedure HandleDiscoveryRedirect(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip: boolean);
    function extractFileData(lang : String; form: TMimeMessage; const name: String; var sContentType: String): TStream;
    Procedure StartServer(active: boolean);
    Procedure StopServer;
    Function ProcessZip(lang: String; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerIniFile; Context: TOperationContext; var cursor: integer): TFHIRBundle;
    procedure SSLPassword(var Password: String);
    procedure SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; lang, message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFhirIssueTypeEnum);
    Procedure ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
    function encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; fmt : TFHIRFormat; transactionTime : TDateTimeEx; names : TStringList) : string;
    Procedure ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
    Procedure ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
    function BuildRequest(lang, sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sContentEncoding,
      sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse; var aFormat: TFHIRFormat; var redirect: boolean; form: TMimeMessage;
      bAuth, secure: boolean; out relativeReferenceAdjustment: integer; var style : TFHIROutputStyle; Session: TFHIRSession; cert: TIdX509): TFHIRRequest;
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    Function WebDesc: String;
    function EndPointDesc(secure: boolean): String;
    procedure GetWebUILink(resource: TFhirResource; base, statedType, id, ver: String; var link, text: String);
    function loadMultipartForm(const request: TStream; const contentType: String; var upload: boolean): TMimeMessage;
    function processProvenanceHeader(header, lang: String): TFhirProvenance;
    function DoVerifyPeer(Certificate: TIdX509; AOk: boolean; ADepth, AError: integer): boolean;
    Procedure ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
    Procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception = nil);
    procedure smsStatus(Msg: String);
    procedure loadConfiguration;
    procedure SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
    procedure StopAsyncTasks;
    function makeTaskRedirect(base, id : String; msg : String; fmt : TFHIRFormat; names : TStringList) : string;
    procedure CheckAsyncTasks;
    function processRegistration(request : TIdHTTPRequestInfo; session : TFhirSession) : String;
    function loadFromRsaDer(cert : string) : TJWKList;

    procedure checkRequestByJs(context : TOperationContext; request : TFHIRRequest);
  Public
    Constructor Create(ini: TFHIRServerIniFile; name: String; TerminologyServer: TTerminologyServer; Context: TFHIRServerContext);
    Destructor Destroy; Override;

    Procedure Start(active: boolean);
    Procedure Stop;
    Procedure Transaction(stream: TStream; init : boolean; name, base: String; ini: TFHIRServerIniFile; callback: TInstallerCallback);
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TDictionary<String, String> = nil); overload;
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TDictionary<String, String> = nil); overload;
    Procedure RunPostHandler(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean);

    Property ServerContext: TFHIRServerContext read FServerContext;
    property AuthServer: TAuth2Server read FAuthServer;
    Property SourceProvider: TFHIRWebServerSourceProvider read FSourceProvider write SetSourceProvider;
    property host: String read FHost;
    property CDSHooksServer: TCDSHooksServer read FCDSHooksServer;

    property UseOAuth: boolean read FUseOAuth write FUseOAuth;
    property OWinSecurityPlain: boolean read FOWinSecurityPlain write FOWinSecurityPlain;
    property OWinSecuritySecure: boolean read FOWinSecuritySecure write FOWinSecuritySecure;
    property ServeMissingCertificate: boolean read FServeMissingCertificate write FServeMissingCertificate;
    property ServeUnknownCertificate: boolean read FServeUnknownCertificate write FServeUnknownCertificate;
    property CertificateIdList: TStringList read FCertificateIdList;
    property ServeMissingJWT: boolean read FServeMissingJWT write FServeMissingJWT;
    property ServeUnverifiedJWT: boolean read FServeUnverifiedJWT write FServeUnverifiedJWT;
    property JWTAuthorities: TDictionary<String, String> read FJWTAuthorities;

    function ClientAddress(secure: boolean): String;
    property IsTerminologyServerOnly: boolean read FIsTerminologyServerOnly write FIsTerminologyServerOnly;
  End;

Function ProcessPath(base, path: String): string;

Implementation

Uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  FHIRLog,
  SystemService,

  FileSupport,
  FacebookSupport;

Function GetMimeTypeForExt(AExt: String): String;
{$IFDEF MSWINDOWS}
Var
  fReg: TRegistry;
{$ENDIF}
Begin
  result := '';
  AExt := lowercase(AExt);
  if (AExt = '.css') Then
    result := 'text/css'
  Else if AExt = '.ico' Then
    result := 'image/x-icon'
  Else if AExt = '.png' Then
    result := 'image/png'
  Else if AExt = '.gif' Then
    result := 'image/gif'
  Else if AExt = '.jpg' Then
    result := 'image/jpeg'
  Else if AExt = '.mpg' Then
    result := 'video/mpeg'
  Else if AExt = '.js' Then
    result := 'text/javascript'
  Else
  Begin
{$IFDEF MSWINDOWS}
    Try
      fReg := TRegistry.Create;
      Try
        fReg.RootKey := HKEY_LOCAL_MACHINE;
        fReg.OpenKeyReadOnly('Software\Classes\' + AExt);
        result := fReg.ReadString('Content Type');
        fReg.CloseKey;
      Finally
        fReg.Free;
      End;
    Except
    End;
{$ENDIF}
  End;
  If result = '' Then
    result := 'application/octet-stream';
end;

{ TFhirWebServer }

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

procedure TFhirWebServer.loadConfiguration;
var
  s: String;
  ts: TStringList;
begin
  logt('Load Configuration');

  // web identity / configuration
  FHomePage := FIni.ReadString(voMaybeVersioned, 'web', 'homepage', 'homepage.html');
  FFacebookLike := FIni.ReadString(voVersioningNotApplicable, 'facebook.com', 'like', '') = '1';
  FHost := FIni.ReadString(voVersioningNotApplicable, 'web', 'host', '');

  // web server configuration
  FActualPort := FIni.ReadInteger(voMaybeVersioned, 'web', 'http', 0);
  FBasePath := FIni.ReadString(voMaybeVersioned, 'web', 'base', '');
  FActualSSLPort := FIni.ReadInteger(voMaybeVersioned, 'web', 'https', 0);
  FSecurePath := FIni.ReadString(voMaybeVersioned, 'web', 'secure', '');
  FCertFile := FIni.ReadString(voMaybeVersioned, 'web', 'certname', '');
  FRootCertFile := FIni.ReadString(voMaybeVersioned, 'web', 'cacertname', '');
  FSSLPassword := FIni.ReadString(voMaybeVersioned, 'web', 'certpword', '');

  FStatedPort := FIni.ReadInteger(voMaybeVersioned, 'web', 'stated-http', FActualPort);
  FStatedSSLPort := FIni.ReadInteger(voMaybeVersioned, 'web', 'stated-https', FActualSSLPort);
  FSecureToken := FIni.ReadString(voVersioningNotApplicable, 'web', 'secure-token', '');
  ts := TStringList.Create;
  try
    FIni.ReadSection(voMaybeVersioned, 'reverse-proxy', ts);
    for s in ts do
      FReverseProxyList.Add(TReverseProxyInfo.Create(s, FIni.ReadString(voMaybeVersioned, 'reverse-proxy', s, '')));
  finally
    ts.Free;
  end;

  if FIni.ReadString(voMaybeVersioned, 'web', 'insecure', '') = 'conformance' then
    FServerContext.ValidatorContext.setNonSecureTypes(['Conformance', 'StructureDefinition', 'ValueSet', 'ConceptMap', 'DataElement', 'OperationDefinition',
      'SearchParameter', 'NamingSystem'])
  else if FIni.ReadString(voMaybeVersioned, 'web', 'insecure', '') = 'conformance+patient' then
    FServerContext.ValidatorContext.setNonSecureTypes(['Conformance', 'StructureDefinition', 'ValueSet', 'ConceptMap', 'DataElement', 'OperationDefinition',
      'SearchParameter', 'NamingSystem', 'Patient'])
  else
    FServerContext.ValidatorContext.setNonSecureTypes([]);

  FUseOAuth := FIni.ReadBool(voMaybeVersioned, 'security', 'oauth', true);
  FOWinSecuritySecure := FIni.ReadBool(voMaybeVersioned, 'security', 'owin', false);
  FOWinSecurityPlain := FIni.ReadBool(voMaybeVersioned, 'security', 'owin-http', false);
  FServeMissingCertificate := FIni.ReadBool(voMaybeVersioned, 'security', 'no-cert', false);
  FServeUnknownCertificate := FIni.ReadBool(voMaybeVersioned, 'security', 'unknown-cert', false);
  FServeMissingJWT := FIni.ReadBool(voMaybeVersioned, 'security', 'no-jwt', false);
  FServeUnverifiedJWT := FIni.ReadBool(voMaybeVersioned, 'security', 'unverified-jwt', false);

  if FIni.SectionExists(voVersioningNotApplicable, 'patient-view') then
  begin
    ts := TStringList.Create;
    try
      FIni.ReadSection(voVersioningNotApplicable, 'patient-view', ts);
      for s in ts do
        FPatientViewServers.Add(s, FIni.ReadString(voVersioningNotApplicable, 'patient-view', s, ''));
    finally
      ts.Free;
    end;
  end;

  FOwnerName := FIni.ReadString(voVersioningNotApplicable, 'admin', 'ownername', '');
  if FOwnerName = '' then
    FOwnerName := 'Health Intersections';
  FAdminEmail := FIni.ReadString(voVersioningNotApplicable, 'admin', 'email', '');
  if FAdminEmail = '' then
    raise exception.Create('Ad admin email is required');

  FHostSms := FIni.ReadString(voVersioningNotApplicable, 'sms', 'owner', '');
end;

function TFhirWebServer.loadFromRsaDer(cert: string): TJWKList;
var
  fn : String;
begin
  fn := Path([SystemTemp, TDateTimeEx.makeUTC.toString('yyyymmmddhhnnss')+'.'+inttostr(HashStringToCode32(cert))+'.cer']);
  StringToFile(cert, fn, TEncoding.UTF8);
  try
    result := TJWKList.create;
    try
      result.Add(TJWTUtils.loadKeyFromRSACert(fn));
      result.Link;
    finally
      result.Free;
    end;
  finally
    DeleteFile(fn);
  end;
end;

function port(actual, default : integer) : String;
begin
  if actual = default then
    result := ''
  else
    result := ':' + inttostr(actual);
end;

Constructor TFhirWebServer.Create(ini: TFHIRServerIniFile; name: String; TerminologyServer: TTerminologyServer; Context: TFHIRServerContext);
var
  txu: String;
Begin
  Inherited Create;
  FLock := TCriticalSection.Create('fhir-rest');
  FThreads := TList<TAsyncTaskThread>.create;
  FCertificateIdList := TStringList.Create;
  FName := Name;
  FIni := ini;

  if (FolderExists('c:\temp')) then
    FInLog := TLogger.Create('c:\temp\fhirserver-http-in.log')
  else
    FInLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+'fhirserver-http-in.log');
  FInLog.Policy.FullPolicy := lfpChop;
  FInLog.Policy.MaximumSize := 100*1024*1024;
  FInLog.Policy.AllowExceptions := false;

  if (FolderExists('c:\temp')) then
    FOutLog := TLogger.Create('c:\temp\fhirserver-http-out.log')
  else
    FOutLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+'fhirserver-http-out.log');
  FOutLog.Policy.FullPolicy := lfpChop;
  FOutLog.Policy.MaximumSize := 100*1024*1024;
  FOutLog.Policy.AllowExceptions := false;

  FClients := TAdvList<TFHIRWebServerClientInfo>.Create;
  FPatientViewServers := TDictionary<String, String>.Create;
{$IFDEF MSWINDOWS}
  FPatientHooks := TAdvMap<TFHIRWebServerPatientViewContext>.Create;
{$ENDIF}
  FReverseProxyList := TAdvList<TReverseProxyInfo>.Create;
  FReverseProxyByVersion := TAdvMap<TReverseProxyInfo>.Create;
  FServerContext := Context;

  loadConfiguration;

  FServerContext.FormalURLPlain := 'http://' + FIni.ReadString(voMaybeVersioned, 'web', 'host', '') + port(FStatedPort, 80);
  FServerContext.FormalURLSecure := 'https://' + FIni.ReadString(voMaybeVersioned, 'web', 'host', '') + port(FStatedSSLPort, 443);
  FServerContext.FormalURLPlainOpen := 'http://' + FIni.ReadString(voMaybeVersioned, 'web', 'host', '') + port(FStatedPort, 80) + FBasePath;
  FServerContext.FormalURLSecureOpen := 'https://' + FIni.ReadString(voMaybeVersioned, 'web', 'host', '') + port(FStatedSSLPort, 443) + FBasePath;
  FServerContext.FormalURLSecureClosed := 'https://' + FIni.ReadString(voMaybeVersioned, 'web', 'host', '') + port(FStatedSSLPort, 443) + FSecurePath;
  FServerContext.ClientApplicationVerifier := TClientApplicationVerifier.Create;
  ServerContext.JWTServices := TJWTServices.Create;
  ServerContext.JWTServices.host := FHost;
  ServerContext.JWTServices.cert := FCertFile;
  ServerContext.JWTServices.Password := FSSLPassword;
  ServerContext.JWTServices.DatabaseId := ServerContext.SystemId;

  logt('Load & Cache Store: ');

  // Base Web server configuration
  logt(inttostr(FServerContext.Storage.TotalResourceCount) + ' resources');

  if FStatedPort = 80 then
    txu := 'http://' + FHost
  else
    txu := 'http://' + FHost + ':' + inttostr(FStatedPort);
  if TerminologyServer <> nil then
    FTerminologyWebServer := TTerminologyWebServer.Create(TerminologyServer.link, FServerContext.ValidatorContext.link, txu, FBasePath + '/', ReturnProcessedFile);

  if FIni.ReadString(voVersioningNotApplicable, 'web', 'clients', '') = '' then
    raise exception.Create('No Authorization file found');
  FAuthServer := TAuth2Server.Create(FIni, FHost, inttostr(FStatedSSLPort));
  FAuthServer.ServerContext := FServerContext.link;
  FAuthServer.OnProcessFile := ReturnProcessedFile;
  FAuthServer.OnDoSearch := DoSearch;
  FAuthServer.path := FIni.ReadString(voMaybeVersioned, 'web', 'auth-path', '/oauth2');
  FAuthServer.AdminEmail := FAdminEmail;
  FAuthServer.EndPoint := OAuthPath(true);
  FAuthServer.host := host;
  FAuthServer.active := FUseOAuth;

  ServerContext.JWTServices.JWKAddress := FAuthServer.KeyPath;
  ServerContext.ClientApplicationVerifier.server := FIni.ReadString(voMaybeVersioned, 'web', 'cavs', FAuthServer.CavsPath);

  FAdaptors := TAdvMap<TFHIRFormatAdaptor>.Create;
{$IFDEF FHIR3}
  FAdaptors.Add('dataPoints', TOpenMHealthAdaptor.Create);
{$ENDIF}
  logt('Web Server:');
  if (FActualPort = 0) then
    logt('  http: not active')
  else if FStatedPort <> FActualPort then
    logt('  http: listen on ' + inttostr(FActualPort) + ', but claim = ' + inttostr(FStatedPort) + ' (reverse proxy mode')
  else
    logt('  http: listen on ' + inttostr(FActualPort));

  if (FActualSSLPort = 0) then
    logt('  https: not active')
  else if FStatedSSLPort <> FActualSSLPort then
    logt('  https: listen on ' + inttostr(FActualSSLPort) + ', but claim = ' + inttostr(FStatedSSLPort) + ' (reverse proxy mode')
  else
    logt('  https: listen on ' + inttostr(FActualSSLPort));

  if (FBasePath <> '') and (FSecurePath <> '') then
    logt(' ...paths: open = ' + FBasePath + ', secure = ' + FSecurePath)
  else if (FActualPort <> 0) then
    logt(' ...paths: open = ' + FBasePath)
  else if (FActualSSLPort <> 0) then
    logt(' ...paths: secure = ' + FSecurePath)
  else
    logt(' ...paths: <none>');
  FCDSHooksServer := TCDSHooksServer.Create(FServerContext);

  // FAuthRequired := FIni.ReadString('fhir', 'oauth-secure', '') = '1';
  // FAppSecrets := FIni.ReadString('fhir', 'oauth-secrets', '');
End;

Destructor TFhirWebServer.Destroy;
Begin
  StopAsyncTasks;
  FCDSHooksServer.Free;
  carry.Free;
  FAdaptors.Free;
  FTerminologyWebServer.Free;
  FIni.Free;
  FAuthServer.Free;
  FPatientViewServers.Free;
  FClients.Free;
{$IFDEF MSWINDOWS}
  FPatientHooks.Free;
{$ENDIF}
  FReverseProxyList.Free;
  FReverseProxyByVersion.Free;
  FServerContext.Free;
  FThreads.Free;
  FLock.Free;
  FCertificateIdList.Free;
  FSourceProvider.Free;
  FInLog.Free;
  Inherited;
End;

procedure TFhirWebServer.DoConnect(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
{$IFDEF MSWINDOWS}
  CoInitialize(nil);
{$ENDIF}
  GJsHost := TJsHost.Create;
  GJsHost.registry := ServerContext.EventScriptRegistry.Link;
  AContext.Connection.IOHandler.MaxLineLength := 100 * 1024;
  FLock.Lock;
  try
    ci := TFHIRWebServerClientInfo.Create;
    FClients.Add(ci);
    AContext.Data := ci;
    ci.Context := AContext;
  finally
    FLock.Unlock;
  end;
end;

procedure TFhirWebServer.DoDisconnect(AContext: TIdContext);
begin
  FLock.Lock;
  try
    FClients.Remove(TFHIRWebServerClientInfo(AContext.Data));
    AContext.Data := nil;
  finally
    FLock.Unlock;
  end;
  GJsHost.Free;
  GJshost := nil;
  {$IFNDEF FHIR2}
  if ServerContext.JavaServices <> nil then
    ServerContext.JavaServices.detach;
  {$ENDIF}
{$IFDEF MSWINDOWS}
  CoUninitialize;
{$ENDIF}
end;

procedure TFhirWebServer.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TFhirBundleTypeEnum; out builder: TFhirBundleBuilder);
begin
  if context.Format = ffNDJson then
    raise EFHIRException.CreateLang('NDJSON-ASYNC', request.Lang);
  builder := TFHIRBundleBuilderSimple.Create(TFHIRBundle.create(aType));
end;

function TFhirWebServer.DoSearch(Session: TFHIRSession; rtype: string; lang, params: String): TFHIRBundle;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
begin
  request := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.link);
  Context := TOperationContext.Create;
  try
    response := TFHIRResponse.Create;
    try
      response.OnCreateBuilder := doGetBundleBuilder;
      request.Session := Session.link;
      request.ResourceName := rtype;
      request.lang := lang;
      request.LoadParams(params);
      request.CommandType := fcmdSearch;
      checkRequestByJs(context, request);
      ProcessRequest(Context, request, response);
      result := response.bundle.link;
    finally
      response.Free;
      request.Free;
    end;
  finally
    Context.Free;
  end;
end;

function TFhirWebServer.DoVerifyPeer(Certificate: TIdX509; AOk: boolean; ADepth, AError: integer): boolean;
var
  i: integer;
begin
  result := ServeUnknownCertificate or FCertificateIdList.Find(Certificate.FingerprintAsString, i);
end;

function TFhirWebServer.encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; fmt : TFHIRFormat; transactionTime: TDateTimeEx; names: TStringList): string;
var
  j, o : TJsonObject;
  a : TJsonArray;
  s : String;
begin
  j := TJsonObject.Create;
  try
    j.str['request'] := reqUrl;
    j.str['transaction_time'] := transactionTime.toXML;
    j.bool['secure'] := true;
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

function TFhirWebServer.EndPointDesc(secure: boolean): String;
begin
  result := '';
  if (secure) then
  begin
    if FBasePath <> '' then
      result := result + ' <li><a href="http://' + FHost + port(FStatedPort, 80) + FBasePath + '">Unsecured access at ' + FBasePath +
        '</a> - direct access with no security considerations</li>'#13#10;
    if FSecurePath <> '' then
      result := result + ' <li><a href="' + FSecurePath + '">Secured access at ' + FSecurePath +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end
  else
  begin
    if FBasePath <> '' then
      result := result + ' <li><a href="' + FBasePath + '">Unsecured access at ' + FBasePath +
        '</a> - direct access with no security considerations</li>'#13#10;
    if FSecurePath <> '' then
      result := result + ' <li><a href="https://' + FHost + port(FStatedSSLPort, 443) + FSecurePath + '">Secured access at ' + FSecurePath +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end;
end;

Procedure TFhirWebServer.Start(active: boolean);
Begin
  FActive := active;
  FStartTime := GetTickCount;
  StartServer(active);
  {$IFNDEF FHIR2}
  if active and (ServerContext.JavaServices <> nil) then
    ServerContext.JavaServices.txConnect(Fini.ReadString(voMaybeVersioned, 'Web', 'java-tx', 'http://localhost:'+Fini.ReadString(voMaybeVersioned, 'Web', 'http', '80')+Fini.ReadString(voMaybeVersioned, 'Web', 'base', '/')));
  {$ENDIF}

  if (active) and (ServerContext.SubscriptionManager <> nil) then
  begin
    FMaintenanceThread := TFhirServerMaintenanceThread.Create(self);
    FSubscriptionThread := TFhirServerSubscriptionThread.Create(self);
    FEmailThread := TFhirServerEmailThread.Create(self);
    smsStatus('The server ' + ServerContext.FormalURLPlain + ' for ' + ServerContext.OwnerName + ' has started');
  end;
End;

procedure TFhirWebServer.smsStatus(Msg: String);
var
  client: TTwilioClient;
begin
  try
    client := TTwilioClient.Create;
    try
      client.Account := ServerContext.SubscriptionManager.SMSAccount;
      if (client.Account <> '') and (FHostSms <> '') then
      begin
        client.Token := ServerContext.SubscriptionManager.SMSToken;
        client.From := ServerContext.SubscriptionManager.SMSFrom;
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

{$IFDEF MSWINDOWS}

procedure TFhirWebServer.startHooks(ctxt: TFHIRWebServerPatientViewContext; patient: TFHIRPatient; url: String);
var
  server: TRegisteredFHIRServer;
  req: TCDSHookRequest;
  s, u, i : String;
  be: TFHIRBundleEntry;
begin
  for s in FPatientViewServers.Keys do
  begin
    server := TRegisteredFHIRServer.Create;
    try
      server.name := s;
      StringSplit(FPatientViewServers[s], '|', u, i);
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
    req.hookInstance := FServerContext.FormalURLPlain; // arbitrary global
    req.patient := patient.id;
    be := TFHIRBundleEntry.Create;
    req.preFetch.Add('patient', be);
    be.resource := patient.link;
    ctxt.manager.makeRequest(req, OnCDSResponse, ctxt);
  finally
    req.Free;
  end;
end;
{$ENDIF}

Procedure TFhirWebServer.Stop;
Begin
  if ServerContext.SubscriptionManager <> nil then
    smsStatus('The server ' + ServerContext.FormalURLPlain + ' for ' + ServerContext.OwnerName + ' is stopping');
  if FSubscriptionThread <> nil then
    FSubscriptionThread.Terminate;
  if FMaintenanceThread <> nil then
    FMaintenanceThread.Terminate;
  if FEmailThread <> nil then
    FEmailThread.Terminate;
  StopServer;
End;

procedure TFhirWebServer.StopAsyncTasks;
var
  task : TAsyncTaskThread;
  done : boolean;
  i : integer;
begin
  done := false;
  FLock.Lock;
  try
    for task in FThreads do
    begin
      task.Terminate;
      done := true;
    end;
  finally
    FLock.Unlock;
  end;
  if done then
  begin
    i := 0;
    repeat
      sleep(100);
      inc(i);
      done := true;
      FLock.Lock;
      try
        for task in FThreads do
          done := false;
      finally
        FLock.Unlock;
      end;
    until done or (i = 10);
    if not done then
    begin
      FLock.Lock;
      try
        for task in FThreads do
        begin
          task.kill;
          ServerContext.Storage.updateAsyncTaskStatus(task.Key, atsTerminated, 'Terminated due to system shut down');
        end;
      finally
        FLock.Unlock;
      end;
    end;
  end;
end;

Procedure TFhirWebServer.StartServer(active: boolean);
Begin
  if FActualPort > 0 then
  begin
    FPlainServer := TIdHTTPServer.Create(Nil);
    FPlainServer.ServerSoftware := 'Health Intersections FHIR Server';
    FPlainServer.ParseParams := false;
    FPlainServer.DefaultPort := FActualPort;
    FPlainServer.KeepAlive := false;
    FPlainServer.OnCreatePostStream := CreatePostStream;
    FPlainServer.OnCommandGet := PlainRequest;
    FPlainServer.OnCommandOther := PlainRequest;
    FPlainServer.OnConnect := DoConnect;
    FPlainServer.OnDisconnect := DoDisconnect;
    FPlainServer.OnParseAuthentication := ParseAuthenticationHeader;
    FPlainServer.active := active;
  end;
  if FActualSSLPort > 0 then
  begin
    If Not FileExists(FCertFile) Then
      Raise exception.Create('SSL Certificate "' + FCertFile + ' could not be found');
    If Not FileExists(ChangeFileExt(FCertFile, '.key')) Then
      Raise exception.Create('SSL Certificate Private Key "' + ChangeFileExt(FCertFile, '.key') + ' could not be found');
    If (FRootCertFile <> '') and (Not FileExists(FRootCertFile)) Then
      Raise exception.Create('SSL Certificate "' + FRootCertFile + ' could not be found');
    FSSLServer := TIdHTTPServer.Create(Nil);
    FSSLServer.ServerSoftware := 'Health Intersections FHIR Server';
    FSSLServer.ParseParams := false;
    FSSLServer.DefaultPort := FActualSSLPort;
    FSSLServer.KeepAlive := false;
    FSSLServer.OnCreatePostStream := CreatePostStream;
    FIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Nil);
    FSSLServer.IOHandler := FIOHandler;
    FIOHandler.SSLOptions.Method := sslvSSLv23;
    FIOHandler.SSLOptions.Mode := sslmServer;
    // SSL v3 / TLS 1 required for older versions of DotNet
    FIOHandler.SSLOptions.SSLVersions := [sslvSSLv3, {$IFNDEF NCTS}sslvTLSv1, {$ENDIF} sslvTLSv1_2];
    FIOHandler.SSLOptions.CipherList := {$IFDEF NCTS}'ALL:!SSLv2:!DES:!RC4:!MD5:!SHA-1'{$ELSE}'ALL:!SSLv2:!DES'{$ENDIF};
    FIOHandler.SSLOptions.CertFile := FCertFile;
    FIOHandler.SSLOptions.KeyFile := ChangeFileExt(FCertFile, '.key');
    FIOHandler.SSLOptions.RootCertFile := FRootCertFile;
    if not FServeMissingCertificate then
      FIOHandler.SSLOptions.VerifyMode := [sslvrfPeer, sslvrfFailIfNoPeerCert, sslvrfClientOnce]
    else
      FIOHandler.SSLOptions.VerifyMode := [sslvrfPeer, sslvrfClientOnce];
    FIOHandler.SSLOptions.VerifyDepth := 2;
    FIOHandler.OnVerifyPeer := DoVerifyPeer;
    // FIOHandler.SSLOptions.
    FIOHandler.OnGetPassword := SSLPassword;
    FSSLServer.OnCommandGet := SecureRequest;
    FSSLServer.OnCommandOther := SecureRequest;
    FSSLServer.OnConnect := DoConnect;
    FSSLServer.OnDisconnect := DoDisconnect;
    FSSLServer.OnParseAuthentication := ParseAuthenticationHeader;
    FSSLServer.active := active;
    LoadEAYExtensions;
  end;
end;

Procedure TFhirWebServer.StopServer;
Begin
  if FSSLServer <> nil then
  begin
    FSSLServer.active := false;
    FreeAndNil(FSSLServer);
    FreeAndNil(FIOHandler);
    UnloadEAYExtensions;
  end;
  if FPlainServer <> nil then
  begin
    FPlainServer.active := false;
    FreeAndNil(FPlainServer);
  end;
End;

procedure TFhirWebServer.Transaction(stream: TStream; init : boolean; name, base: String; ini: TFHIRServerIniFile; callback: TInstallerCallback);
var
  req: TFHIRRequest;
  resp: TFHIRResponse;
  // op : TFHIRNativeOperationEngine;
  cursor: integer;
  Context: TOperationContext;
begin
  // if init then
  // op := FServerContext.Storage.createOperationContext('en');
  Context := TOperationContext.Create(true, callback, 'Load from ' + name);
  try
    req := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roUpload, FServerContext.Indexes.Compartments.link);
    try
      req.CommandType := fcmdTransaction;
      if ExtractFileExt(name) = '.xml' then
        req.resource := TFHIRXmlParser.ParseFile(FServerContext.ValidatorContext.link, 'en', name)
      else if ExtractFileExt(name) = '.json' then
        req.resource := TFHIRJsonParser.ParseFile(FServerContext.ValidatorContext.link, 'en', name)
      else
        req.resource := ProcessZip('en', stream, name, base, init, ini, Context, cursor);
      if not (req.Resource is TFHIRBundle) then
        req.resource := TFHIRBundle.wrap(BundleTypeTransaction, req.resource.Link);

      req.resource.tags['duplicates'] := 'ignore';
      req.Session := FServerContext.SessionManager.CreateImplicitSession('n/a', ServerContext.OwnerName, 'Service Manager', systemInternal, true, false);
      req.Session.allowAll;
      req.LoadParams('');
      req.baseUrl := FServerContext.Bases[0];
      Context.message := 'Process ' + name;
      GJSHost.registry := ServerContext.EventScriptRegistry.link;
      resp := TFHIRResponse.Create;
      try
        resp.OnCreateBuilder := doGetBundleBuilder;
        checkRequestByJs(context, req);
        ProcessRequest(Context, req, resp);
      finally
        resp.Free;
      end;
      if (ini <> nil) then
        if (cursor = -1) then
          ini.DeleteKey('process', 'start')
        else
          ini.WriteInteger('process', 'start', cursor);
    finally
      req.Free;
    end;
  finally
    Context.Free;
  end;
end;

function TFhirWebServer.WebDesc: String;
begin
  if (FActualPort = 0) then
    result := 'HTTPS is supported on Port ' + inttostr(FStatedSSLPort) + '.'
  else if FActualSSLPort = 0 then
    result := 'HTTP is supported on Port ' + inttostr(FStatedPort) + '.'
  else
    result := 'HTTPS is supported on Port ' + inttostr(FStatedSSLPort) + '. HTTP is supported on Port ' + inttostr(FStatedPort) + '.'
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
    FLock.Lock;
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
        b.Append(ci.FActivity);
        b.Append('</td><td>');
        if ci.FStart > 0 then
          b.Append(inttostr(GetTickCount - ci.FStart));
        b.Append('</td></tr>'#13#10);
      end;
    finally
      FLock.Unlock;
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

function TFhirWebServer.patientAppList(base, id : String): string;
var
  b : TStringBuilder;
  apps : TAdvList<TRegisteredClientInformation>;
  app : TRegisteredClientInformation;
begin
  b := TStringBuilder.Create;
  try
    apps := TAdvList<TRegisteredClientInformation>.create;
    try
      FServerContext.Storage.fetchClients(apps);
      for app in apps do
      begin
        if app.patientContext then
        begin
          b.Append('  <li><a href="');
          b.Append(app.url);
          b.Append('?iss=https://');
          b.Append(base);
          b.Append('&launch=');
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

Procedure TFhirWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  Session: TFHIRSession;
  c: integer;
  check, handled: boolean;
  rp: TReverseProxyInfo;
  version : TFHIRVersion;
  id : string;
begin
  Session := nil;
  MarkEntry(AContext, request, response);
  try
    id := ServerContext.nextRequestId;
    logRequest(false, id, request);

    response.CustomHeaders.Add('X-Request-Id: '+id);
    if (request.AuthUsername = INTERNAL_SECRET) then
      FServerContext.SessionManager.GetSession(request.AuthPassword, Session, check);
    if (Session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        FServerContext.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1), Session, check);
    end;

    if OWinSecurityPlain and (((Session = nil) and (request.Document <> FBasePath + OWIN_TOKEN_PATH)) or not ServerContext.UserProvider.allowInsecure) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + FBasePath + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
    else if (request.CommandType = hcOption) then
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
    else if OWinSecurityPlain and ServerContext.UserProvider.allowInsecure and (request.Document = FBasePath + OWIN_TOKEN_PATH) then
      HandleOWinToken(AContext, false, request, response)
    else if FSourceProvider.exists(FSourceProvider.AltFile(request.Document, FBasePath)) then
      ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(request.Document, FBasePath), false)
    else if request.Document.EndsWith('.hts') and FSourceProvider.exists(ChangeFileExt(FSourceProvider.AltFile(request.Document, FBasePath), '.html')) then
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(FSourceProvider.AltFile(request.Document, FBasePath), '.html'), false)
    else if request.Document.EndsWith('.phs') and FSourceProvider.exists(ChangeFileExt(FSourceProvider.AltFile(request.Document, FBasePath), '.html')) then
      runPostHandler(request, response, Session, request.Document, ChangeFileExt(FSourceProvider.AltFile(request.Document, FBasePath), '.html'), false)
      // else if FSourceProvider.FileExists(FSourcePath+ExtractFileName(request.Document.replace('/', '\'))) then
      // ReturnSpecFile(response, request.Document, FSourcePath+ExtractFileName(request.Document.replace('/', '\')))
      // else if FSourceProvider.FileExists(FSpecPath+ExtractFileName(request.Document.replace('/', '\'))) then
      // ReturnSpecFile(response, request.Document, FSpecPath+ExtractFileName(request.Document.replace('/', '\')))
    else if request.Document = FBasePath + '/.well-known/openid-configuration' then
      HandleDiscoveryRedirect(AContext, request, response)
    else if request.Document = '/.well-known/openid-configuration' then
      FAuthServer.HandleDiscovery(AContext, request, response)
    else if request.Document.StartsWith(FBasePath + '/cds-services') and FCDSHooksServer.active then
      FCDSHooksServer.HandleRequest(false, FBasePath, Session, AContext, request, response)
    else if request.Document.StartsWith(AppendForwardSlash(FBasePath) + 'websockets', false) then
      HandleWebSockets(AContext, request, response, false, false, FBasePath)
    else if request.Document.StartsWith(FBasePath, false) then
      HandleRequest(AContext, request, response, false, false, FBasePath, id, Session, nil)
    else if request.Document.StartsWith(AppendForwardSlash(FBasePath) + 'FSecurePath', false) then
      HandleWebSockets(AContext, request, response, false, false, FSecurePath)
    else if (FSecurePath <> '') and (request.Document.StartsWith(FSecurePath, false) and hasInternalSSLToken(request)) then
      HandleRequest(AContext, request, response, true, true, FSecurePath, id, Session, nil)
    else if request.Document = '/diagnostics' then
      ReturnDiagnostics(AContext, request, response, false, false, FSecurePath)
    else if request.Document = '/' then
      ReturnProcessedFile(request, response, Session, '/' + FHomePage, FSourceProvider.AltFile('/' + FHomePage, FBasePath), false)
    else if (FTerminologyWebServer <> nil) and FTerminologyWebServer.handlesRequest(request.Document) then
      FTerminologyWebServer.Process(AContext, request, Session, response, false)
    else
    begin
      handled := false;
      for rp in FReverseProxyList do
        if request.Document.StartsWith(rp.path) then
        begin
          ReverseProxy(rp, AContext, request, Session, response, false);
          handled := true;
          break;
        end;
      if not handled then
      begin
        response.ResponseNo := 404;
        response.ContentText := 'Document ' + request.Document + ' not found';
        logt('miss: ' + request.Document);
      end;
    end;
    logResponse(id, response);
  finally
    MarkExit(AContext);
  end;
end;

procedure TFhirWebServer.PopulateConformanceAuth(rest: TFhirCapabilityStatementRest);
var
  c: TFHIRCoding;
  ext: TFhirExtension;
begin
  if rest.security = nil then
    rest.security := TFhirCapabilityStatementRestSecurity.Create;
  rest.security.cors := true;
  if FAuthServer <> nil then
  begin
    c := rest.security.serviceList.Append.codingList.Append;
    c.System := 'http://hl7.org/fhir/restful-security-service';
    c.code := 'SMART-on-FHIR';
    c.display := 'SMART-on-FHIR';
    rest.security.description := 'This server implements OAuth2 for login using the Smart App Launch profile';

    ext := rest.security.extensionList.Append;
    ext.url := 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris';
    // ext.addExtension('dscovery', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure)+FAuthServer.AuthPath+'/discovery'));
    ext.addExtension('register', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure) + FAuthServer.RegisterPath));
    ext.addExtension('authorize', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure) + FAuthServer.AuthPath));
    ext.addExtension('token', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure) + FAuthServer.TokenPath));
  end;
end;

procedure TFhirWebServer.PopulateConformance(sender: TObject; conf: TFhirCapabilityStatement);
var
  i: integer;
begin
  for i := 0 to conf.restList.Count - 1 do
    PopulateConformanceAuth(conf.restList[i]);
end;

Procedure TFhirWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  Session: TFHIRSession;
  check, handled: boolean;
  c: integer;
  rp: TReverseProxyInfo;
  cert: TIdX509;
  id : String;
  JWT: TJWT;
begin
  check := false;
  cert := (AContext.Connection.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLSocket.PeerCert;

  Session := nil;
  MarkEntry(AContext, request, response);
  try
    id := ServerContext.nextRequestId;
    logRequest(true, id, request);
    response.CustomHeaders.Add('X-Request-Id: '+id);
    if (request.AuthUsername = INTERNAL_SECRET) then
      if request.AuthPassword.StartsWith('urn:') then
        FServerContext.SessionManager.GetSession(request.AuthPassword, Session, check)
      else
      begin
        JWT := TJWTUtils.unpack(request.AuthPassword, false, nil);
        // todo: change this to true, and validate the JWT, under the right conditions
        try
          if cert = nil then
            Session := FServerContext.SessionManager.getSessionFromJWT(request.RemoteIP, 'Unknown', systemUnknown, JWT)
          else
            Session := FServerContext.SessionManager.getSessionFromJWT(request.RemoteIP, cert.CanonicalName, systemFromCertificate, JWT);
        finally
          JWT.Free;
        end;
      end;

    if (Session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        FServerContext.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1), Session, check);
      // actually, in this place, we ignore check.  we just established the session
    end;

    if request.Document.StartsWith(FAuthServer.path) then
      FAuthServer.HandleRequest(AContext, request, Session, response)
    else if (request.CommandType = hcOption) then
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
    else if OWinSecuritySecure and (request.Document = URLPath([FSecurePath, OWIN_TOKEN_PATH])) then
      HandleOWinToken(AContext, true, request, response)
    else if request.Document = FBasePath + '/.well-known/openid-configuration' then
      HandleDiscoveryRedirect(AContext, request, response)
    else if FSourceProvider.exists(FSourceProvider.AltFile(request.Document, FSecurePath)) then
      ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(request.Document, FSecurePath), true)
    else if request.Document.EndsWith('.hts') and FSourceProvider.exists(ChangeFileExt(FSourceProvider.AltFile(request.Document, FSecurePath), '.html')) then
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(FSourceProvider.AltFile(request.Document, FSecurePath), '.html'), true)
    else if request.Document.StartsWith(FSecurePath, false) then
      HandleRequest(AContext, request, response, true, true, FSecurePath, id, Session, cert)
    else if OWinSecuritySecure and ((Session = nil) and (request.Document <> URLPath([FSecurePath, OWIN_TOKEN_PATH]))) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + FBasePath + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
      // else if FSourceProvider.FileExists(IncludeTrailingPathDelimiter(FSourcePath)+request.Document) then
      // ReturnSpecFile(response, request.Document, IncludeTrailingPathDelimiter(FSourcePath)+request.Document)
      // else if FSourceProvider.FileExists(FSourceProvider.AltFile(ExtractFileName(request.Document))) then
      // ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(ExtractFileName(request.Document)))
    else if request.Document.StartsWith('/scim') then
      ProcessScimRequest(AContext, request, response)
    else if request.Document = '/.well-known/openid-configuration' then
      FAuthServer.HandleDiscovery(AContext, request, response)
      // else if request.Document.StartsWith(FBasePath, false) then
      // HandleRequest(AContext, request, response, true, false, FBasePath)
    else if (FTerminologyWebServer <> nil) and FTerminologyWebServer.handlesRequest(request.Document) then
      FTerminologyWebServer.Process(AContext, request, Session, response, true)
    else if request.Document.StartsWith(FSecurePath + '/cds-services') and FCDSHooksServer.active then
      FCDSHooksServer.HandleRequest(true, FSecurePath, Session, AContext, request, response)
    else if request.Document = '/' then
      ReturnProcessedFile(request, response, Session, '/hompage.html', FSourceProvider.AltFile('/homepage.html', FSecurePath), true)
    else
    begin
      handled := false;
      for rp in FReverseProxyList do
        if request.Document.StartsWith(rp.path) then
        begin
          ReverseProxy(rp, AContext, request, Session, response, true);
          handled := true;
          break;
        end;
      if not handled then
      begin
        response.ResponseNo := 404;
        response.ContentText := 'Document ' + request.Document + ' not found';
        logt('miss: ' + request.Document);
      end;
    end;
    logResponse(id, response);
  finally
    MarkExit(AContext);
    Session.Free;
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

procedure TFhirWebServer.HandleDiscoveryRedirect(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  if FAuthServer = nil then
  begin
    response.ResponseNo := 404;
    response.ResponseText := 'Not Found';
    response.ContentText := 'This server does not support authentication';
  end
  else
  begin
    response.ResponseNo := 301;
    response.ResponseText := 'Moved Permanently';
    response.Location := FAuthServer.BasePath + '/discovery';
  end;
end;

procedure TFhirWebServer.HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  pm: TParseMap;
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
        pm := TParseMap.Create(StreamToString(request.PostStream, TEncoding.UTF8))
      else
        pm := TParseMap.Create(request.UnparsedParams);
      try
        if pm.GetVar('grant_type') <> 'password' then
          response.ContentText := 'Unknown content type - must be ''password'''
        else if not ServerContext.UserProvider.CheckLogin(pm.GetVar('username'), pm.GetVar('password'), userkey) then
          response.ContentText := 'Unknown usernmame/password'
        else
        begin
          Session := FServerContext.SessionManager.CreateImplicitSession(request.RemoteIP, pm.GetVar('username'), 'Anonymous', systemFromOWin, false, true);
          try
            Session.ExternalUserKey := userkey;
            json := TJsonObject.Create;
            try
              json.str['access_token'] := Session.Cookie;
              json.num['expires_in'] := inttostr(trunc((Session.Expires - TDateTimeEx.makeUTC.DateTime) / DATETIME_SECOND_ONE));
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

Procedure TFhirWebServer.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdX509);
var
  sHost: string;
  oRequest: TFHIRRequest;
  oResponse: TFHIRResponse;
  sCookie: string;
  sContentType: String;
  oStream: TStream;
  sDoc: String;
  s: String;
  aFormat: TFHIRFormat;
  lang: String;
  sPath: String;
  redirect: boolean;
  form: TMimeMessage;
  relativeReferenceAdjustment: integer;
  style : TFHIROutputStyle;
  c: integer;
  domain: String;
  sBearer: String;
  noErrCode, upload: boolean;
  Context: TOperationContext;
  Session: TFHIRSession;
Begin
  noErrCode := false;
  upload := false;

  Session := nil;
  try
    if ssl then
      sHost := 'https://' + request.host
    else
      sHost := 'http://' + request.host;
    domain := request.host;
    if domain.Contains(':') then
      domain := domain.Substring(0, domain.IndexOf(':'));

    lang := request.AcceptLanguage;
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
        form := loadMultipartForm(request.PostStream, request.contentType, upload);
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
          oResponse := TFHIRResponse.Create;
          try
            oResponse.OnCreateBuilder := doGetBundleBuilder;
            response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
            // response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE');
            response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
            // response.CustomHeaders.add('Access-Control-Expose-Headers: *');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
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

                noErrCode := StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters.GetVar('nohttperr')) or
                  StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters.GetVar('_nohttperr'));
                ReadTags(request.RawHeaders.Values['Category'], oRequest);
                Session := oRequest.Session.link;

                // allow scripting to change anything about the request
                GJsHost.previewRequest(Session, oRequest);

                if redirect then
                begin
                  if oRequest.Session <> nil then
                  begin
                    FAuthServer.setCookie(response, FHIR_COOKIE_NAME, oRequest.Session.Cookie, domain, '', oRequest.Session.Expires, false);
                    cacheResponse(response, cacheNotAtAll);
                    response.redirect(oRequest.Session.OriginalUrl);
                  end
                  else
                    response.redirect(oRequest.baseUrl);
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
                else if (oRequest.CommandType = fcmdConformanceStmt) and (oRequest.ResourceName <> '') then
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
                      Context.upload := upload;
                      checkRequestByJs(context, oRequest);
                      if oRequest.CommandType = fcmdWebUI then
                        HandleWebUIRequest(oRequest, oResponse, secure)
                      else if oRequest.commandType in [fcmdTask, fcmdDeleteTask] then
                        ProcessTaskRequest(Context, oRequest, oResponse)
                      else if (request.RawHeaders.Values['Prefer'] = 'respond-async') or (oRequest.Parameters.GetVar('_async') = 'true') then
                        ProcessAsyncRequest(Context, oRequest, oResponse)
                      else
                        ProcessRequest(Context, oRequest, oResponse);
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
                  RecordExchange(oRequest, oResponse);
                  ProcessOutput(oRequest, oResponse, request, response, relativeReferenceAdjustment, style, request.AcceptEncoding.Contains('gzip'));
                  // no - just use *              if request.RawHeaders.Values['Origin'] <> '' then
                  // response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
                  if oResponse.versionId <> '' then
                    response.ETag := 'W/"' + oResponse.versionId + '"';
                  response.LastModified := oResponse.lastModifiedDate;
                  // todo: timezone
                  if oResponse.tags.Count > 0 then
                    response.CustomHeaders.Add('Category: ' + oResponse.tags.AsHeader);
                  if oResponse.link_List.Count > 0 then
                    response.CustomHeaders.Add('Link: ' + oResponse.link_List.AsHeader);
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
                RecordExchange(oRequest, oResponse);
              except
                on e: exception do
                begin
                  RecordExchange(oRequest, oResponse, e);
                  raise;
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
          response.ContentStream := StringToUTF8Stream(BuildFhirAuthenticationPage(lang, sHost, sPath + sDoc, logId, e.Msg, ssl));
        end
        else
          SendError(response, logId, e.status, aFormat, lang, e.message, sPath, e, Session, true, sPath + sDoc, relativeReferenceAdjustment, IssueTypeLogin);
      end;
      on e: ETerminologyError do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            IssueTypeNotSupported);
      end;
      on e: ETerminologySetup do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            IssueTypeNotSupported);
      end;
      on e: ETooCostly do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, IssueTypeTooCostly)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            IssueTypeTooCostly);
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
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, IssueTypeNull)
        else
          SendError(response, logId, HTTP_ERR_INTERNAL, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, IssueTypeNull);
      end;
    end;
  finally
    Session.Free;
  end;
end;

{$IFDEF MSWINDOWS}

function TFhirWebServer.HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  profile: TFhirStructureDefinition;
  builder: TQuestionnaireBuilder;
  questionnaire: TFHIRQuestionnaire;
  s, id, fid: String;
begin
  // get the right questionnaire
  if request.Parameters.GetVar('profile').StartsWith('Profile/') then
  begin
    id := request.Parameters.GetVar('profile').Substring(8);
    profile := GetResource(request.Session, 'StructureDefinition', request.lang, id, '', '') as TFhirStructureDefinition;
  end
  else
    profile := FindResource(request.Session, 'StructureDefinition', request.lang, 'url=' + request.Parameters.GetVar('profile')) as TFhirStructureDefinition;
  try
    id := profile.id;
    fid := request.baseUrl + 'StructureDefinition/' + id + '/$questionnaire';
    s := FServerContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
    if s = '' then
    begin
      builder := TQuestionnaireBuilder.Create(request.lang);
      try
        questionnaire := FServerContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
        try
          if questionnaire = nil then
          begin
            builder.profile := profile.link;
            builder.OnExpand := FServerContext.Storage.ExpandVS;
            builder.onLookupCode := FServerContext.Storage.LookupCode;
            builder.QuestionnaireId := fid;
            builder.onLookupReference := LookupReference;
            builder.Context := request.link;

            builder.build;
            questionnaire := builder.questionnaire.link;
            FServerContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
          end;
          // convert to xhtml
          s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', true);
          FServerContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
        finally
          questionnaire.Free;
        end;
        // insert page headers:
        s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
        s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION) +
          '<p><a href="' + builder.QuestionnaireId + '">Questionnaire for this form</a>.' + ' The QuestionnaireAnswers should be submitted as a POST to <i>' +
          request.baseUrl + '$qa-post</i> with a questionnaire reference of <a href="' + builder.QuestionnaireId + '">' + builder.QuestionnaireId +
          '</a></p>'#13#10);
        s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
        s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '$qa-post";');
      finally
        builder.Free;
      end;
    end;

    response.Body := s;
    result := Now; // don't want anyone caching anything
    response.contentType := 'text/html; charset=UTF-8';
  finally
    profile.Free;
  end;
end;
{$ENDIF}

function TFhirWebServer.HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  typ, id, ver: String;
  r: TFhirResource;
  s: String;
  comp: TFHIRComposer;
begin
  result := 0;

  // get the right questionnaire
  StringSplit(request.id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  if not(StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.ValidatorContext.hasCustomResource(typ)) then
    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [typ]);

  r := GetResource(request.Session, typ, request.lang, id, '', '');
  try
    if r is TFhirOperationOutcome then
    begin
      response.resource := r.link;
      response.HTTPCode := 500;
      response.message := 'Internal Error';
    end
    else
    begin

      if request.Parameters.GetVar('srcformat') = 'json' then
        comp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.link, OutputStylePretty, request.lang)
      else
        comp := TFHIRXMLComposer.Create(FServerContext.Validator.Context.link, OutputStylePretty, request.lang);
      try
        comp.LogId := request.internalRequestId;
        s := comp.Compose(r, nil);
      finally
        comp.Free;
      end;

      s := '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<!DOCTYPE HTML "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + #13#10 + '' +
        #13#10 + '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">' + #13#10 + '<head>' + #13#10 +
        '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>Direct Edit for /' + typ + '/' + id + '</title>' +
        #13#10 + TFHIRXhtmlComposer.PageLinks + FHIR_JS + '</head>' + #13#10 + '' + #13#10 + '<body>' + #13#10 + '' + #13#10 +
        TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION) + '<h2>Direct Edit for ' + request.id + '</h2>' + #13#10 +
        '<form action="' + request.baseUrl + '_web/' + typ + '/' + id + '/$post" method="POST">'#13#10 + '  <input type="hidden" name="srcformat" value="' +
        request.Parameters.GetVar('srcformat') + '"/>'#13#10 + '  <textarea cols="80" rows="24" name="source" style="white-space:pre">' +
        FormatXMLForTextArea(s) + #13#10'</textarea><br/>'#13#10 + '  <input type="submit" value="Save"/>'#13#10 + '</form>'#13#10 +
        TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, true);

      response.Body := s;
      result := Now; // don't want anyone caching anything
      response.contentType := 'text/html; charset=UTF-8';
    end;
  finally
    r.Free;
  end;
end;

function TFhirWebServer.HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  s, typ, id, ver: String;
  p: TParseMap;
  prsr: TFHIRParser;
  Context: TOperationContext;
begin
  StringSplit(request.id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  request.id := id;
  if not(StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.ValidatorContext.hasCustomResource(typ)) then
    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.lang, [typ]);
  request.ResourceName := typ;
  request.CommandType := fcmdUpdate;

  Context := TOperationContext.Create;
  try
    p := TParseMap.Create(TEncoding.UTF8.GetString(request.Source.AsBytes), true);
    try
      if p.GetVar('srcformat') = 'json' then
        prsr := FServerContext.Factory.newJsonParser(request.lang)
      else
        prsr := FServerContext.Factory.newXmlParser(request.lang);
      try
        s := p.GetVar('source');
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

{$IFDEF MSWINDOWS}

function TFhirWebServer.HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  id, ver, fullid: String;
  profile: TFhirStructureDefinition;
  builder: TQuestionnaireBuilder;
  questionnaire: TFHIRQuestionnaire;
  s: String;
begin
  // get the right questionnaire
  StringSplit(request.id.Substring(8), '/', id, ver);
  profile := GetResource(request.Session, 'StructureDefinition', request.lang, id, ver, '') as TFhirStructureDefinition;
  try
    fullid := request.baseUrl + 'StructureDefinition/' + id + '/$questionnaire';
    s := FServerContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
    if s = '' then
    begin
      builder := TQuestionnaireBuilder.Create(request.lang);
      try
        questionnaire := FServerContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
        try
          if questionnaire = nil then
          begin
            builder.profile := profile.link;
            builder.OnExpand := FServerContext.Storage.ExpandVS;
            builder.onLookupCode := FServerContext.Storage.LookupCode;
            builder.onLookupReference := LookupReference;
            builder.Context := request.link;
            builder.QuestionnaireId := fullid;
            builder.build;
            questionnaire := builder.questionnaire.link;
            FServerContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
          end;
          // convert to xhtml
          s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', true);
          FServerContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
        finally
          questionnaire.Free;
        end;
      finally
        builder.Free;
      end;
    end;
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
    s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '$qa-post";');
    response.Body := s;
    result := Now; // don't want anyone caching anything
    response.contentType := 'text/html; charset=UTF-8';
  finally
    profile.Free;
  end;
end;
{$ENDIF}

function TFhirWebServer.HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id, ver: String;
  s, xhtml: String;
  patient: TFHIRPatient;
  hookid: String;
{$IFDEF MSWINDOWS}
  hooks: TFHIRWebServerPatientViewContext;
{$ENDIF}
begin
  result := 0;
  StringSplit(request.id.Substring(8), '/', id, ver);
  hookid := NewGuidId;
{$IFDEF MSWINDOWS}
  hooks := TFHIRWebServerPatientViewContext.Create;
  hooks.manager := TCDSHooksManager.Create;
  FLock.Lock;
  try
    FPatientHooks.Add(hookid, hooks);
  finally
    FLock.Unlock;
  end;
{$ENDIF}
  patient := GetResource(request.Session, 'Patient', request.lang, id, ver, '') as TFHIRPatient;
  try
    xhtml := patient.text.div_.AsPlainText;
{$IFDEF MSWINDOWS}
    startHooks(hooks, patient, request.baseUrl);
{$ENDIF}
  finally
    patient.Free;
  end;

  s := FSourceProvider.getSource('patient.html');
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%hookid%]', hookid, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%patient-details%]', xhtml, [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%patient-app-list%]', patientAppList(FHost + FSecurePath, id), [rfReplaceAll])
  else
    s := s.Replace('[%patient-app-list%]', patientAppList(FHost + ':' + inttostr(FStatedSSLPort) + FSecurePath, id), [rfReplaceAll]);
  s := s.Replace('[%patient-id%]', id, [rfReplaceAll]);
  s := s.Replace('[%admin%]', FAdminEmail, [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%host%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', FHost + ':' + inttostr(FStatedPort), [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%securehost%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', FHost + ':' + inttostr(FStatedSSLPort), [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%baseOpen%]', FHost + FBasePath, [rfReplaceAll])
  else
    s := s.Replace('[%baseOpen%]', FHost + ':' + inttostr(FStatedPort) + FBasePath, [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%baseSecure%]', FHost + FSecurePath, [rfReplaceAll])
  else
    s := s.Replace('[%baseSecure%]', FHost + ':' + inttostr(FStatedSSLPort) + FSecurePath, [rfReplaceAll]);
  if request.secure then
    s := s.Replace('[%root%]', FSecurePath, [rfReplaceAll])
  else
    s := s.Replace('[%root%]', FBasePath, [rfReplaceAll]);

  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);

  response.Body := s;
  response.contentType := 'text/html; charset=UTF-8';
end;

{$IFDEF MSWINDOWS}

function TFhirWebServer.HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id: String;
  // s, xhtml : String;
  // patient : TFHIRPatient;
  // hookid : String;
  hooks: TFHIRWebServerPatientViewContext;

begin
  result := 0;

  id := request.id.Substring(13);
  FLock.Lock;
  try
    if FPatientHooks.TryGetValue(id, hooks) then
      hooks.link
    else
      hooks := nil;
  finally
    FLock.Unlock;
  end;

  if hooks <> nil then
  begin
    try
      while hooks.manager.waiting do
        sleep(1000);
      FLock.Lock;
      try
        response.Body := presentAsHtml(hooks.cards, nil, hooks.Errors);
        FPatientHooks.Remove(id);
      finally
        FLock.Unlock;
      end;
      response.HTTPCode := 200;
      response.contentType := 'text/html; charset=UTF-8';
    finally
      hooks.Free;
    end;
  end;
end;

function TFhirWebServer.HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  id, ver: String;
  questionnaire: TFHIRQuestionnaire;
  s: String;
begin
  // get the right questionnaire
  StringSplit(request.id.Substring(14), '/', id, ver);
  questionnaire := GetResource(request.Session, 'Questionnaire', request.lang, id, ver, '') as TFHIRQuestionnaire;
  try
    // convert to xhtml
    s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', false);
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
    s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '/QuestionnaireAnswers";');
    response.Body := s;
    result := Now; // don't want anyone caching anything
    response.contentType := 'text/html; charset=UTF-8';
  finally
    questionnaire.Free;
  end;
end;

function TFhirWebServer.HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  typ, id, ver: String;
  r: TFhirResource;
  qa: TFhirQuestionnaireResponse;
  q: TFHIRQuestionnaire;
  s, j: String;
  json: TFHIRJsonComposer;
begin
  result := 0;

  // get the right questionnaire
  StringSplit(request.id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  if not(StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.ValidatorContext.hasCustomResource(typ)) then
    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.lang, [typ]);

  r := GetResource(request.Session, typ, request.lang, id, ver, 'qa-edit');
  try
    if r is TFhirOperationOutcome then
    begin
      response.resource := r.link;
      response.HTTPCode := 500;
      response.message := 'Internal Error';
    end
    else
    begin
      qa := r as TFhirQuestionnaireResponse;
      q := (FindContainedResource(qa, qa.questionnaire) as TFHIRQuestionnaire).link;
      if q = nil then
        raise EFHIRException.CreateLang('CANNOT_FIND', request.lang, ['Questionnaire', qa.questionnaire.reference.Substring(1)]);

      // convert to xhtml
      s := transform1(q, request.lang, 'QuestionnaireToHTML.xslt', true);

      // make clean qa
      qa.questionnaire.reference := 'Questionnaire/' + qa.questionnaire.reference.Substring(1);
      qa.containedList.Clear;
      json := TFHIRJsonComposer.Create(request.Context.link, OutputStyleNormal, request.lang);
      try
        j := json.Compose(qa, nil);
      finally
        json.Free;
      end;

      // insert page headers:
      s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
      s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION));
      s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
      // insert the answer:
      s := s.Replace('var QuestionnaireResponse=null;', 'var QuestionnaireResponse=' + j + ';');
      response.Body := s;
      result := Now; // don't want anyone caching anything
      response.contentType := 'text/html; charset=UTF-8';
    end;
  finally
    r.Free;
  end;
end;
{$ENDIF}

procedure TFhirWebServer.HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
var
  ws: TIdWebSocket;
begin
  ws := TIdWebSocket.Create(nil);
  try
    if ws.open(AContext, request, response) then
      ServerContext.SubscriptionManager.HandleWebSocket(ws);
  finally
    ws.Free;
  end;
end;

function TFhirWebServer.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
begin
  if request.id.EndsWith('$edit') then
    result := HandleWebEdit(request, response)
  else if request.id.EndsWith('$post') then
    result := HandleWebPost(request, response)
{$IFDEF MSWINDOWS}
  else if request.id.EndsWith('$qa-edit') then
    result := HandleWebQuestionnaireInstance(request, response)
  else if request.id.StartsWith('Questionnaire/') then
    result := HandleWebQuestionnaire(request, response)
  else if request.id.StartsWith('StructureDefinition/') then
    result := HandleWebProfile(request, response)
  else if request.id.StartsWith('PatientHooks/') then
    result := HandleWebPatientHooks(request, response, secure)
  else if request.id = 'Create' then
    result := HandleWebCreate(request, response)
{$ENDIF}
  else if request.id.StartsWith('Patient/') then
    result := HandleWebPatient(request, response, secure)
  else
    raise EFHIRException.CreateLang('MSG_UNKNOWN_CONTENT', request.lang, [request.id, 'web UI']);
end;

function TFhirWebServer.hasInternalSSLToken(request: TIdHTTPRequestInfo): boolean;
begin
  result := request.RawHeaders.Values[SECURE_TOKEN_HEADER] = FSecureToken;
end;

procedure TFhirWebServer.SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; lang, message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFhirIssueTypeEnum);
var
  issue: TFhirOperationOutcome;
  report: TFhirOperationOutcomeIssue;
  oComp: TFHIRComposer;
  ext: TFhirExtension;
  d: String;
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
    issue := TFhirOperationOutcome.Create;
    try
      issue.text := TFhirNarrative.Create;
      issue.text.status := NarrativeStatusGenerated;
      issue.text.div_ := TFHIRXhtmlParser.Parse(lang, xppReject, [], '<div><p>' + FormatTextToXML(message, xmlText) + '</p></div>');
      if addLogins then
      begin
        if FAuthServer.HL7Appid <> '' then
        begin
          ext := issue.extensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.Value := TFhirString.Create('http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey=' + FAuthServer.HL7Appid + '&returnURL=' + EncodeMime(path) +
            '/state/' + FAuthServer.MakeLoginToken(path, apHL7));
        end;
        if FAuthServer.FacebookAppid <> '' then
        begin
          ext := issue.extensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.Value := TFhirString.Create('https://www.facebook.com/dialog/oauth?client_id=' + FAuthServer.FacebookAppid + '&redirect_uri=' + path + '&state=' +
            FAuthServer.MakeLoginToken(path, apFacebook));
        end;
        if FAuthServer.GoogleAppid <> '' then
        begin
          ext := issue.extensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.Value := TFhirString.Create('https://accounts.google.com/o/oauth2/auth?client_id=' + FAuthServer.GoogleAppid +
            '&response_type=code&scope=openid%20email&redirect_uri=' + path + '&state=' + FAuthServer.MakeLoginToken(path, apGoogle));
        end;
      end;
      report := issue.issueList.Append;
      report.severity := IssueSeverityError;
      report.details := TFhirCodeableConcept.Create;
      report.details.text := message;
      d := ExceptionStack(e);
      if d <> '' then
      begin (*
          issue.text.div_.AddTag('hr');
          issue.text.div_.AddTag('p').AddTag('b').AddText('Stack Dump:');
          p := issue.text.div_.AddTag('p');
          lines := TStringList.Create;
          try
          lines.Text := d;
          d := '';
          for s in lines do
          begin
          l := s.subString(s.indexof('}')+1);
          p.AddText(l);
          p.AddTag('br');
          d := d + l+#13#10;
          end;
          finally
          lines.Free;
          end; *)
        report.diagnostics := d;
      end;
      report.code := code;
      response.ContentStream := TMemoryStream.Create;
      oComp := nil;
      case format of
        ffXml:
          oComp := TFHIRXMLComposer.Create(FServerContext.Validator.Context.link, OutputStyleNormal, lang);
        ffXhtml:
          begin
            oComp := TFHIRXhtmlComposer.Create(FServerContext.Validator.Context.link, OutputStyleNormal, lang);
            TFHIRXhtmlComposer(oComp).baseUrl := AppendForwardSlash(url);
            TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
            TFHIRXhtmlComposer(oComp).Session := Session.link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          end;
        ffJson, ffNDJson:
          oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.link, OutputStyleNormal, lang);
        ffText:
          oComp := TFHIRTextComposer.Create(FServerContext.Validator.Context.link, OutputStyleNormal, lang);
      end;
      try
        response.contentType := oComp.MimeType;
        oComp.LogId := logId;
        oComp.Compose(response.ContentStream, issue, nil);
        response.ContentStream.Position := 0;
      finally
        oComp.Free;
      end;
    finally
      issue.Free;
    end;
  end;
end;

procedure TFhirWebServer.SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
begin
  FSourceProvider.Free;
  FSourceProvider := Value;
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

Function TFhirWebServer.BuildRequest(lang, sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept,
  sContentEncoding, sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse; var aFormat: TFHIRFormat; var redirect: boolean;
  form: TMimeMessage; bAuth, secure: boolean; out relativeReferenceAdjustment: integer; var style : TFHIROutputStyle; Session: TFHIRSession; cert: TIdX509)
  : TFHIRRequest;
Var
  sURL, Msg: String;
  oRequest: TFHIRRequest;
  parser: TFHIRParser;
  check: boolean;
  comp: TIdCompressorZLib;
  mem: TMemoryStream;
  cursor: integer;
  bundle: TFHIRBundle;
  b : TBytes;
  inVer : TFHIRVersion;
Begin

  relativeReferenceAdjustment := 0;
  result := nil;
  oRequest := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.link);
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
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', 'images not served', HTTP_ERR_NOTFOUND, IssueTypeNotFound)
      else
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_NO_MODULE', lang), [sResource]), HTTP_ERR_NOTFOUND,
          IssueTypeNotFound);
    end;

    sURL := copy(sResource, Length(sBaseURL) + 1, $FFF);
    sURL := oRequest.preanalyse(sURL);

    if (sCommand <> 'GET') then
    begin
      oRequest.Version := readVersion(sContentType);
      if StringStartsWithInsensitive(sContentType, 'application/x-ndjson') or StringStartsWithInsensitive(sContentType, 'application/fhir+ndjson') then
        oRequest.PostFormat := ffNDJson
      else if StringStartsWithInsensitive(sContentType, 'application/json') or StringStartsWithInsensitive(sContentType, 'application/fhir+json') or
        StringStartsWithInsensitive(sContentType, 'application/json+fhir') or StringStartsWithInsensitive(sContentType, 'json') or
        StringStartsWithInsensitive(sContentType, 'text/json') Then
        oRequest.PostFormat := ffJson
      else if StringStartsWithInsensitive(sContentType, 'text/html') or StringStartsWithInsensitive(sContentType, 'html') or
        StringStartsWithInsensitive(sContentType, 'application/x-zip-compressed') or StringStartsWithInsensitive(sContentType, 'application/zip') Then
        oRequest.PostFormat := ffXhtml
      else if StringStartsWithInsensitive(sContentType, 'text/fhir') Then
        oRequest.PostFormat := ffText
      else if StringStartsWithInsensitive(sContentType, 'text/turtle') Then
        oRequest.PostFormat := ffTurtle
      else if StringStartsWithInsensitive(sContentType, 'text/xml') or StringStartsWithInsensitive(sContentType, 'application/xml') or
        StringStartsWithInsensitive(sContentType, 'application/fhir+xml') or StringStartsWithInsensitive(sContentType, 'application/xml+fhir') or
        StringStartsWithInsensitive(sContentType, 'xml') Then
        oRequest.PostFormat := ffXml;
      if (sContentType <> 'application/x-www-form-urlencoded') and oRequest.Parameters.VarExists('_format') and (form = nil) and (oRequest.Parameters.GetVar('_format') <> '') then
        sContentType := oRequest.Parameters.GetVar('_format');
    end;

    oResponse.Version := readVersion(sContentAccept);
    if oRequest.Parameters.VarExists('_format') and (oRequest.Parameters.GetVar('_format') <> '') then
      sContentAccept := oRequest.Parameters.GetVar('_format');
    if StringStartsWithInsensitive(sContentAccept, 'application/x-ndjson') or StringStartsWithInsensitive(sContentAccept, 'application/fhir+ndjson') then
      oResponse.format := ffNDJson
    else if StringExistsSensitive(sContentAccept, 'application/json') or StringExistsSensitive(sContentAccept, 'application/fhir+json') or
      StringExistsInsensitive(sContentAccept, 'json') Then
      oResponse.format := ffJson
    else if StringExistsSensitive(sContentAccept, 'text/xml') Then
      oResponse.format := ffXml
    else if StringExistsSensitive(sContentAccept, 'text/html') Then
      oResponse.format := ffXhtml
    else if StringExistsSensitive(sContentAccept, 'text/fhir') Then
      oResponse.format := ffText
    else if StringExistsSensitive(sContentAccept, 'text/turtle') Then
      oResponse.format := ffTurtle
    else if StringExistsSensitive(sContentAccept, 'application/xml') Then
      oResponse.format := ffXml
    else if StringExistsSensitive(sContentAccept, 'application/fhir+xml') Then
      oResponse.format := ffXml
    else if StringExistsSensitive(sContentAccept, '*/*') Then
      // specially for stupid IE...
      oResponse.format := ffXhtml
    else if StringExistsInsensitive(sContentAccept, 'html') Then
      oResponse.format := ffXhtml
    else if StringExistsInsensitive(sContentAccept, 'xml') Then
      oResponse.format := ffXml
    else if StringExistsInsensitive(sContentAccept, 'text') Then
      oResponse.format := ffText
    else if StringExistsInsensitive(sContentAccept, 'rdf') Then
      oResponse.format := ffTurtle
    else if oRequest.PostFormat <> ffUnspecified then
      oResponse.format := oRequest.PostFormat;

    if oRequest.Parameters.VarExists('_pretty') and (oRequest.Parameters.GetVar('_pretty') = 'true') then
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
        FServerContext.SessionManager.EndSession(sCookie, sClient);
        oRequest.Session := nil;
        redirect := true;
      end
      else if (sURL = 'internal') then
        redirect := true
      else if (Session <> nil) and FServerContext.SessionManager.isOkSession(Session) then
        oRequest.Session := Session.link
      else if (sURL <> 'auth-login') and FServerContext.SessionManager.GetSession(sCookie, Session, check) then
      begin
        if check and not CheckSessionOK(Session, sClient) then
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), Msg, HTTP_ERR_UNAUTHORIZED);
        oRequest.Session := Session
      end
      else if (secure and FServerContext.SessionManager.isOkBearer(sBearer, sClient, Session)) then
        oRequest.Session := Session
      else
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), Msg, HTTP_ERR_UNAUTHORIZED);
    end
    else if cert <> nil then
      oRequest.Session := FServerContext.SessionManager.CreateImplicitSession(sClient, cert.CanonicalName, 'Anonymous', systemFromCertificate, false, false)
    else
      oRequest.Session := FServerContext.SessionManager.CreateImplicitSession(sClient, 'Unknown', 'Anonymous', systemUnknown, false, false);

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
            oRequest.resource := ProcessZip(lang, oPostStream, NewGuidURN, 'http://hl7.org/fhir', false, nil, nil, cursor)
          else
          begin
            oRequest.Source := TAdvBuffer.Create;
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
            if (oRequest.ResourceEnum = frtBinary) and (oRequest.PostFormat = ffUnspecified) then
            begin
              oRequest.resource := TFhirBinary.Create;
              SetLength(b, oPostStream.Size - oPostStream.Position);
              if oPostStream.Size - oPostStream.Position > 0 then
                oPostStream.Read(b[0], oPostStream.Size - oPostStream.Position);
              TFhirBinary(oRequest.Resource).Content := b;
              TFhirBinary(oRequest.Resource).ContentType := sContentType;
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
              oRequest.resource := parseParamsFromForm(oPostStream);
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
                if oRequest.Version <> COMPILED_FHIR_VERSION then
                begin
                  convertFromVersion(oPostStream, oRequest.PostFormat, oRequest.Version);
                  oRequest.CopyPost(oPostStream);
                end;

                parser := MakeParser(FServerContext.Validator.Context, lang, oRequest.PostFormat, oPostStream, xppReject);
                try
                  oRequest.resource := parser.resource.link;
                  if oRequest.PostFormat = ffUnspecified then
                    oRequest.PostFormat := parser.format;

                  if (oRequest.CommandType = fcmdTransaction) and not(oRequest.resource is TFHIRBundle) then
                  begin
                    bundle := TFHIRBundle.Create(BundleTypeTransactionResponse);
                    // oRequest.Feed.base := oRequest.baseUrl;
                    bundle.entryList.Add(TFHIRBundleEntry.Create);
                    bundle.entryList[0].resource := oRequest.resource.link;
                    bundle.entryList[0].resource.id := FhirGUIDToString(CreateGUID);
                    oRequest.resource := bundle;
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

    result := oRequest.link;
  Finally
    oRequest.Free;
  End;
End;

procedure TFhirWebServer.cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
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

Function TFhirWebServer.ProcessZip(lang: String; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerIniFile; Context: TOperationContext;
  var cursor: integer): TFHIRBundle;
var
  rdr: TAdvZipReader;
  p: TFHIRParser;
  i, k: integer;
  s: TAdvVCLStream;
  e: TFHIRBundleEntry;
  bnd: TFHIRBundle;
  inc: TStringList;
  istart, iend: integer;
  function ok(res: TFhirResource): boolean;
  begin
    result := (inc.Count = 0) or (inc.IndexOf(CODES_TFhirResourceType[res.ResourceType]) > -1);
  end;

begin
  inc := TStringList.Create;
  result := TFHIRBundle.Create(BundleTypeTransaction);
  try
    if init and (ini <> nil) then
      inc.CommaText := ini.ReadString(voVersioningNotApplicable, 'control', 'include', '');
    result.id := NewGuidURN;
    // result.base := base;
    rdr := carry.link as TAdvZipReader;
    try
      if (rdr = nil) or (name <> carryName) then
      begin
        rdr.Free;
        carry.Free;
        rdr := TAdvZipReader.Create;
        s := TAdvVCLStream.Create;
        s.stream := oStream;
        rdr.stream := s;
        rdr.Parts := TAdvZipPartList.Create;
        rdr.ReadZip;
        carry := rdr.link as TAdvZipReader;
        carryName := name;
      end;

      if (init) or (ini = nil) then
      begin
        istart := 0;
        iend := rdr.Parts.Count - 1;
      end
      else
      begin
        istart := ini.ReadInteger(voVersioningNotApplicable, 'process', 'start', 0);
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
          writeln('Parse ' + rdr.Parts[i].name);
          if rdr.Parts[i].name.EndsWith('.json') then
            p := FServerContext.Factory.newJsonParser(lang)
          else if rdr.Parts[i].name.EndsWith('.map') then
            p := TFHIRTextParser.Create(FServerContext.Validator.Context.link, lang)
          else
            p := FServerContext.Factory.newXmlParser(lang);
          try
            p.Source := TBytesStream.Create(rdr.Parts[i].AsBytes);
            p.AllowUnknownContent := true;
            p.Parse;
            if p.resource is TFHIRBundle then
            begin
              bnd := TFHIRBundle(p.resource);
              case bnd.type_ of
                BundleTypeDocument, BundleTypeMessage, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection:
                  for k := 0 to bnd.entryList.Count - 1 do
                    if ok(bnd.entryList[k].resource) then
                      result.entryList.Add(bnd.entryList[k].link);
                BundleTypeTransaction, BundleTypeTransactionResponse:
                  ; // we ignore these for now
              end;
            end
            else if not(p.resource is TFhirParameters) and ok(p.resource) then
            begin
              e := TFHIRBundleEntry.Create;
              try
                e.resource := p.resource.link;
                result.entryList.Add(e.link);
              finally
                e.Free;
              end;
            end;
          finally
            p.Source.Free;
            p.Free;
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

procedure TFhirWebServer.SSLPassword(var Password: String);
begin
  Password := FSSLPassword;
end;

Procedure TFhirWebServer.ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;
  relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip: boolean);
var
  oComp: TFHIRComposer;
  b: TBytes;
  stream: TStream;
  ownsStream: boolean;
  comp: TIdCompressorZLib;
  Body: boolean;
  res: TFhirResource;
begin
  ownsStream := false;
  gzip := false;
  response.ResponseNo := oResponse.HTTPCode;
  response.contentType := oResponse.contentType;
  res := oResponse.resource;
  if (res = nil) then
    res := oResponse.outcome;
  Body := (request.Command = 'GET') or (request.RawHeaders.Values['Prefer'] <> 'return=minimal') or (oResponse.format = ffXhtml);
  if Body and (request.RawHeaders.Values['Prefer'] = 'return=OperationOutcome') and (oResponse.outcome <> nil) then
    res := oResponse.outcome;

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
        if res is TFhirBinary then
        begin
          if (Length(TFhirBinary(res).content) > 0) and (Body) then
            stream.Write(TFhirBinary(res).content[0], Length(TFhirBinary(res).content));
          stream.Position := 0;
          response.contentType := TFhirBinary(res).contentType;
          if StrToBoolDef(oRequest.Parameters.GetVar('no-attachment'), false) then
            response.ContentDisposition := 'attachment;';
          response.Expires := Now + 0.25;
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
            oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang)
          else if oResponse.format = ffXhtml then
          begin
            oComp := TFHIRXhtmlComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang);
            TFHIRXhtmlComposer(oComp).baseUrl := AppendForwardSlash(oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
            TFHIRXhtmlComposer(oComp).Session := oRequest.Session.link;
            TFHIRXhtmlComposer(oComp).tags := oResponse.tags.link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
            TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
            TFHIRXhtmlComposer(oComp).OperationName := oRequest.OperationName;
            // response.Expires := 0;
            response.Pragma := '';
          end
          else if oResponse.format = ffNDJson then
            oComp := TFHIRNDJsonComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang)
          else if oResponse.format = ffXml then
            oComp := TFHIRXMLComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang)
          else if oResponse.format = ffText then
            oComp := TFHIRTextComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang)
{$IFNDEF FHIR2}
          else if (oResponse.format = ffTurtle) or (res._source_format = ffTurtle) then
          begin
            oComp := TFHIRTurtleComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang);
            if (res <> nil) and (res.id <> '') then
              TFHIRTurtleComposer(oComp).url := AppendForwardSlash(oRequest.baseUrl) + CODES_TFhirResourceType[res.ResourceType] + '/' + res.id;
          end
{$ENDIF}
          else if res._source_format = ffJson then
            oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang)
          else
            oComp := TFHIRXMLComposer.Create(FServerContext.Validator.Context.link, style, oRequest.lang);
          try
            response.contentType := oComp.MimeType;
            oComp.SummaryOption := oRequest.Summary;
            oComp.ElementToCompose.Assign(oRequest.Elements);
            oComp.LogId := oRequest.internalRequestId;
            oComp.Compose(stream, res, oResponse.link_List);
          finally
            oComp.Free;
          end;
          if oResponse.Version <> COMPILED_FHIR_VERSION then
            convertToVersion(stream, oResponse.Format, oResponse.Version);
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
  finally
    if ownsStream then
      stream.Free;
  end;
end;

function TFhirWebServer.processProvenanceHeader(header, lang: String): TFhirProvenance;
var
  json: TFHIRJsonParser;
  ss: TStringStream;
begin
  if header = '' then
    result := nil
  else
  begin
    ss := TStringStream.Create(header, TEncoding.UTF8);
    try
      json := FServerContext.Factory.newJsonParser(lang);
      try
        json.Source := ss;
        json.Parse;
        result := (json.resource as TFhirProvenance).link;
      finally
        json.Free;
      end;
    finally
      ss.Free;
    end;
  end;
end;

function TFhirWebServer.processRegistration(request: TIdHTTPRequestInfo; session : TFhirSession): String;
var
  pm : TParseMap;
  client : TRegisteredClientInformation;
  s : String;
  jwks : TJWKList;
  json : TJsonObject;
begin
  if session = nil then
    raise EFHIRException.Createlang('MSG_AUTH_REQUIRED', request.AcceptLanguage);

  pm := TParseMap.create(request.UnparsedParams);
  try
    client := TRegisteredClientInformation.Create;
    try
      client.name := pm.GetVar('client_name').Trim;
      if client.name = '' then
        raise EFHIRException.Createlang('INFO_MISSING', request.AcceptLanguage, ['client_name']);
      client.url := pm.GetVar('client_uri').Trim;
      client.logo := pm.GetVar('logo_uri').Trim;
      client.softwareId := pm.GetVar('software_id').Trim;
      client.softwareVersion := pm.GetVar('software_version').Trim;
      client.PatientContext := pm.getVar('ctxt-patient') <> '';
      case StrToIntDef(pm.GetVar('mode'), 0) of
        1: begin
           client.mode := rcmOAuthClient;
           client.secret := NewGuidId;
           client.redirects.Text := pm.GetVar('redirect_uris');
           end;
        2: begin
           client.mode := rcmOAuthClient;
           client.redirects.Text := pm.GetVar('redirect_uris');
           end;
        3: begin
           client.mode := rcmBackendServices;
           client.issuer := pm.GetVar('issuer').Trim;
           if (client.issuer = '') then
            raise EFHIRException.Createlang('INFO_MISSING', request.AcceptLanguage, ['issuer']);
           s := pm.GetVar('public_key').Trim;
           if s = '' then
             raise EFHIRException.Createlang('INFO_MISSING', request.AcceptLanguage, ['A public key is required']);
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
        raise EFHIRException.Createlang('MSG_UNKNOWN_CONTENT', request.AcceptLanguage, ['Mode', 'Processing Registration']);
      end;

      if client.secret <> ''  then
        result := '<p><b>Success</b><br/>Your client has been Registered and assigned a client_id of "'+ServerContext.Storage.storeClient(client, session.Key)+'". Use "'+client.secret+'" as your client secret</p>'
      else
        result := '<p><b>Success</b><br/>Your client has been Registered and assigned a client_id of "'+ServerContext.Storage.storeClient(client, session.Key)+'"</p>'
    finally
      client.Free;
    end;
  finally
    pm.free;
  end;
end;

function sNow: String;
begin
  result := FormatDateTime('c', Now);
end;

function parseMimeType(s : String) : TFHIRFormat;
begin
  if StringStartsWithInsensitive(s, 'application/x-ndjson') or StringStartsWithInsensitive(s, 'application/fhir+ndjson') then
    result := ffNDJson
  else if StringStartsWithInsensitive(s, 'application/json') or StringStartsWithInsensitive(s, 'application/fhir+json') or
    StringStartsWithInsensitive(s, 'application/json+fhir') or StringStartsWithInsensitive(s, 'json') or
    StringStartsWithInsensitive(s, 'text/json') Then
    result := ffJson
  else if StringStartsWithInsensitive(s, 'text/html') or StringStartsWithInsensitive(s, 'html') or
    StringStartsWithInsensitive(s, 'application/x-zip-compressed') or StringStartsWithInsensitive(s, 'application/zip') Then
    result := ffXhtml
  else if StringStartsWithInsensitive(s, 'text/fhir') Then
    result := ffText
  else if StringStartsWithInsensitive(s, 'text/turtle') Then
    result := ffTurtle
  else if StringStartsWithInsensitive(s, 'text/xml') or StringStartsWithInsensitive(s, 'application/xml') or
    StringStartsWithInsensitive(s, 'application/fhir+xml') or StringStartsWithInsensitive(s, 'application/xml+fhir') or
    StringStartsWithInsensitive(s, 'xml') Then
    result := ffXml
  else
    result := ffUnspecified;
end;

procedure TFhirWebServer.ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  thread : TAsyncTaskThread;
  id : String;
begin
  if not (request.CommandType in [fcmdSearch, fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem, fcmdTransaction, fcmdBatch, fcmdUpload, fcmdOperation]) then
    raise EFHIRException.CreateLang('NO_ASYNC', request.Lang);
  thread := TAsyncTaskThread.create;
  FLock.Lock;
  try
    FThreads.add(thread);
  finally
    FLock.Unlock;
  end;
  id := NewGuidId;
  thread.key := ServerContext.Storage.createAsyncTask(request.url, id, response.Format);
  thread.server := self.link as TFhirWebServer;
  thread.request := request.Link;
  if request.Parameters.VarExists('output-format') then
    thread.Format := parseMimeType(request.Parameters.GetVar('output-format'))
  else
    thread.Format := response.Format;
  thread.Start;
  response.HTTPCode := 202;
  response.Message := 'Accepted';
  response.ContentLocation := request.baseUrl+'task/'+id;
  if response.format = ffXhtml then
  begin
    response.ContentType := 'text/html';
    response.Body := makeTaskRedirect(request.baseUrl, id, 'Preparing', response.Format, nil);
  end;
end;


procedure TFhirWebServer.ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  op: TFHIROperationEngine;
  t: cardinal;
  us, cs: String;
begin
  FLock.Lock;
  try
    inc(FRestCount);
  finally
    FLock.Unlock;
  end;
  if request.internalRequestId = '' then
    request.internalRequestId := ServerContext.nextRequestId;

  t := GetTickCount;
  op := FServerContext.Storage.createOperationContext(request.lang);
  try
    op.OnPopulateConformance := PopulateConformance;
    op.Execute(Context, request, response);
    FServerContext.Storage.yield(op, nil);
  except
    on e: exception do
    begin
      FServerContext.Storage.yield(op, e);
      raise;
    end;
  end;
  FLock.Lock;
  try
    inc(FRestCount);
  finally
    FLock.Unlock;
  end;
  t := GetTickCount - t;
  try
    inc(FRestTime, t);
  finally
    FLock.Unlock;
  end;
  if request.Session = nil then // during OAuth only
    us := 'user=(in-oauth)'
  else
    us := 'user=' + request.Session.UserName;
  if request.CommandType = fcmdOperation then
    cs := '$' + request.OperationName
  else
    cs := 'cmd=' + CODES_TFHIRCommandType[request.CommandType];
  logt('Request: ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source + '. rt = ' +
    inttostr(t)+'  ('+GService.MemoryStatus+')');
end;

procedure TFhirWebServer.ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  sCookie: String;
  c: integer;
  Session: TFHIRSession;
  check: boolean;
begin
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
    FServerContext.SessionManager.EndSession(sCookie, request.RemoteIP);
    response.redirect('/closed');
  end
  else if (FServerContext.SessionManager.GetSession(sCookie, Session, check)) then
  begin
    try
      if check and not CheckSessionOK(Session, request.RemoteIP) then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage),
          'Session Expired', HTTP_ERR_UNAUTHORIZED);
      if not Session.canAdministerUsers then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage),
          'This Session is not authorised to manage users', HTTP_ERR_UNAUTHORIZED);
      ServerContext.UserProvider.ProcessRequest(AContext, request, response, Session);
    finally
      Session.Free;
    end;
  end
  else
    Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage),
      'Authentication required', HTTP_ERR_UNAUTHORIZED);
end;

procedure TFhirWebServer.ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  status : TAsyncTaskStatus;
  message, s, originalRequest : String;
  transactionTime, expires : TDateTimeEx;
  names : TStringList;
  outcome : TBytes;
  fmt : TFHIRFormat;
  key, i : integer;
  l : TFhirBundleLink;
  n, f : string;
  zip : TAdvZipWriter;
  m : TAdvMemoryStream;
begin
  names := TStringList.Create;
  try
    if FServerContext.Storage.fetchTaskDetails(request.Id, key, status, fmt, message, originalRequest, transactionTime, expires, names, outcome) then
    begin
      if request.CommandType = fcmdDeleteTask then
      begin
        ServerContext.Storage.MarkTaskDeleted(key);
        for n in names do
        begin
          f := Path([ServerContext.TaskFolder, 'task-'+inttostr(key)+'-'+n+EXT_WEB_TFHIRFormat[fmt]]);
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
          m := TAdvMemoryStream.Create;
          try
            zip := TAdvZipWriter.Create;
            try
              zip.Stream := m.Link;
              for n in names do
              begin
                f := Path([ServerContext.TaskFolder, 'task-'+inttostr(key)+'-'+n+EXT_WEB_TFHIRFormat[fmt]]);
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
            FServerContext.Storage.recordDownload(key, request.subId);
          finally
            m.Free;
          end;
        end
        else
        begin
          f := Path([ServerContext.TaskFolder, 'task-'+inttostr(key)+'-'+request.SubId]);
          if not FileExists(f) then
          begin
            response.HTTPCode := 500;
            response.Message := 'Server Error';
            response.resource := BuildOperationOutcome(request.Lang, 'The source for file '+ExtractFileName(f)+' could not be found');
          end
          else
          begin
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Stream := TAdvFile.create(f, fmOpenRead + fmShareDenyWrite);
            response.ContentType := MIMETYPES_TFHIRFormat[response.format];
            FServerContext.Storage.recordDownload(key, request.subId);
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
            begin
              l := TFhirBundleLink.Create;
              try
                l.url := request.baseUrl+'task/'+request.id+'/'+s+EXT_WEB_TFHIRFormat[fmt];
                l.relation := 'item';
                response.link_list.add(l.Link);
              finally
                l.Free;
              end;
            end;
            l := TFhirBundleLink.Create;
            try
              l.url := request.baseUrl+'task/'+request.id+'.zip';
              l.relation := 'collection';
              response.link_list.add(l.Link);
            finally
              l.Free;
            end;
            if response.format = ffXhtml then
            begin
              response.ContentType := 'text/html';
              response.Body := makeTaskRedirect(request.baseUrl, request.id, '', fmt, names);
            end
            else
            begin
              response.ContentType := 'application/json';
              response.Body := encodeAsyncResponseAsJson(request, originalRequest, fmt, transactionTime, names);
            end;
            end;
          atsTerminated, atsError :
            begin
            response.HTTPCode := 500;
            response.Message := 'Error';
            fmt := ffJson;
            if length(outcome) > 0 then
              response.resource := bytesToResource(outcome, fmt)
            else
              response.resource := BuildOperationOutcome(request.Lang, message);
            end;
          atsAborted:
            begin
            response.HTTPCode := 400;
            response.Message := 'Error';
            response.resource := BuildOperationOutcome(request.Lang, 'This task has been cancelled');
            end;
          atsDeleted:
            begin
            response.HTTPCode := 404;
            response.Message := 'Not found';
            response.Resource := BuildOperationOutcome('en', 'Task has been deleted', IssueTypeUnknown);
            end;
        end;
      end
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not found';
      response.Resource := BuildOperationOutcome('en', 'Unknown task', IssueTypeUnknown);
    end;
  finally
    names.Free;
  end;
end;

function TFhirWebServer.BuildFhirAuthenticationPage(lang, host, path, logId, Msg: String; secure: boolean): String;
var
  authurl: string;
begin
  authurl := OAuthPath(secure);

  result := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' + FHIR_GENERATED_VERSION
    + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + #13#10 + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 + ''#13#10 +
    TFHIRXhtmlComposer.header(nil, FBasePath, lang, SERVER_VERSION) + '<h2>' + FOwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + '</h2>'#13#10;

  result := result + '<p>'#13#10 + GetFhirMessage('MSG_AUTH_REQUIRED', lang) + '</p>'#13#10;
  if Msg <> '' then
    result := result + '<p><b>' + FormatTextToHTML(Msg) + '</b></p>'#13#10;

  result := result + '<p><a href="' + FAuthServer.BasePath + '/auth?client_id=c.1&response_type=code&scope=openid%20profile%20user/*.*%20' + SCIM_ADMINISTRATOR
    + '&redirect_uri=' + authurl + '/internal&aud=' + authurl + '&state=' + FAuthServer.MakeLoginToken(path, apGoogle) + '">Login using OAuth</a></p>' + #13#10;

  if FSecurePath <> '' then
    result := result + '<p>Or use the <a href="http://' + FHost + port(FStatedPort, 80) + FBasePath + '">unsecured API</a>.</p>'#13#10;

  result := result + '<p>&nbsp;</p>'#13#10 +
    '<p>This server uses <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">Smart App Launch</a> for OAuth logins</p>'#13#10;
  result := result + TFHIRXhtmlComposer.Footer(lang, lang, logid);
end;

function TFhirWebServer.BuildFhirHomePage(compList : TAdvList<TFHIRCompartmentId>; logId, lang, host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
var
  counts: TStringList;
  a: String;
  s: String;
  names: TStringList;
  profiles: TAdvStringMatch;
  i, j, ix: integer;
  b: TStringBuilder;
  pol: String;
begin
  logt('home page: ' + Session.scopes);
  counts := TStringList.Create;
  try
    for a in FServerContext.ValidatorContext.allResourceNames do
    begin
      ix := counts.Add(a);
      if (compList.Empty) or FServerContext.Indexes.Compartments.existsInCompartment(frtPatient, a) then
        counts.Objects[ix] := TObject(0)
      else
        counts.Objects[ix] := TObject(-1);
    end;

    pol := FServerContext.Storage.ProfilesAsOptionList;
    profiles := TAdvStringMatch.Create;
    try
      profiles.forced := true;
      counts := FServerContext.Storage.FetchResourceCounts(compList);

      s := host + sBaseURL;
      b := TStringBuilder.Create;
      try
        b.Append('<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
          '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
          '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
          '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' +
          FHIR_GENERATED_VERSION + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 +
          TFHIRXhtmlComposer.header(Session, sBaseURL, lang, SERVER_VERSION));

        b.Append('<h2>' + FOwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + '</h2>'#13#10);

        if Session <> nil then
          if secure then
          begin
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10);
          end
          else if FSecurePath = '' then
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10)
          else
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + ' (or use <a href="https://' + FHost + port(FStatedSSLPort, 443) + FSecurePath +
              '">Secure API</a>)</p>'#13#10);

        b.Append('<p>'#13#10 + StringFormat(GetFhirMessage('MSG_HOME_PAGE_1', lang), ['<a href="http://hl7.org/fhir">http://hl7.org/fhir</a>']) + #13#10 +
          StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', lang), [s]) +
          ' This server defines some <a href="local.hts">extensions to the API</a>, and also offers <a href="/tx">Terminology Services</a>' + #13#10);
        if Session.canGetUser and (Session.User <> nil) and not Session.isAnonymous then
        begin
          b.Append('. You can also <a href="registerclient.html">Register a client</a>.'+#13#10);
          b.Append(' or <a href="token.hts">get your bearer token</a> (use this to get access to the secure API without needing OAuth login).</p>');
        end
        else
        begin
          b.Append('. If you login through OAuth, you can also Register a client'+#13#10);
          b.Append(' or get your bearer token (use this to get access to the secure API without needing OAuth login in the application).</p>');
        end;


        b.Append(
          '</p>'#13#10 + '<hr/>'#13#10 + ''#13#10 + '<p>' + GetFhirMessage('SYSTEM_OPERATIONS', lang) + ':</p><ul><li> <a href="' + sBaseURL + '/metadata">' +
          GetFhirMessage('CONF_PROFILE', lang) + '</a> ' + '(' + GetFhirMessage('OR', lang) + ' <a href="' + sBaseURL +
          '/metadata?_format=text/xml">as xml</a> (' + GetFhirMessage('OR', lang) + ' <a href="' + sBaseURL +
          '/metadata?_format=application/json">JSON</a>)</li>' + #13#10);
        if not FIsTerminologyServerOnly then
          b.Append('<li><a class="tag" href="' + sBaseURL + '/$meta">' + GetFhirMessage('SYSTEM_TAGS', lang) + '</a></li>');
        b.Append('<li><a href="' + sBaseURL + '/_search">' + GetFhirMessage('GENERAL_SEARCH', lang) + '</a></li>');
        if not FIsTerminologyServerOnly then
          b.Append('<li><a href="' + sBaseURL + '/_history">' + StringFormat(GetFhirMessage('MSG_HISTORY', lang), [GetFhirMessage('NAME_SYSTEM', lang)]) +
            '</a> (History of all resources)</li>' + #13#10);
        if not FIsTerminologyServerOnly then
          b.Append('<li><a href="#upload">' + GetFhirMessage('NAME_UPLOAD_SERVICES', lang) + '</a></li>' + #13#10);

        if not FIsTerminologyServerOnly then
          b.Append('<li>Create/Edit a new resource based on the profile: <form action="' + sBaseURL + '/_web/Create" method="GET"><select name="profile">' + pol
            + '</select> <input type="submit" value="GO"></form></li>' + #13#10);

        if (Session.canAdministerUsers) then
          b.Append('<li><a href="/scim/web">Manage Users</a></li>' + #13#10);

        b.Append('</ul>' + #13#10 + ''#13#10 + '<hr/>'#13#10 + '<p>' + GetFhirMessage('MSG_HOME_PAGE_3', lang) + '</p>' + #13#10);

        b.Append('<table class="lines">'#13#10 +

          '<tr><th>' + GetFhirMessage('NAME_TYPE', lang) + '</th>' + '<th>' + GetFhirMessage('NAME_STORED', lang) + '</th>' + '<th colspan="4">' +
          GetFhirMessage('NAME_OPERATIONS', lang) + '</th><td style="border-left: 1px solid grey"/><th>' + GetFhirMessage('NAME_TYPE', lang) + '</th>' + '<th>'
          + GetFhirMessage('NAME_STORED', lang) + '</th>' + '<th colspan="4">' + GetFhirMessage('NAME_OPERATIONS', lang) + '</th></tr>'#13#10);

        names := TStringList.Create;
        Try
          for a in FServerContext.ValidatorContext.allResourceNames do
          begin
            ix := counts.IndexOf(a);
            if (ix >= 0) and (integer(counts.Objects[ix]) > -1) and (FServerContext.ResConfig[a].Supported) then
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
        if not FIsTerminologyServerOnly then
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
        b.Append(TFHIRXhtmlComposer.Footer(sBaseURL, lang, logId));
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

function TFhirWebServer.BuildFhirUploadPage(lang, host, sBaseURL: String; aType: String; Session: TFHIRSession): String;
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
    ''#13#10 + '  &copy; HL7.org 2011-2013'#13#10 + '  &nbsp;'#13#10 + '  ' + FOwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + ''#13#10 +
    '  &nbsp;'#13#10 + '  FHIR ' + GetFhirMessage('NAME_VERSION', lang) + ' ' + FHIR_GENERATED_VERSION + ''#13#10;

  if Session <> nil then
    result := result + '&nbsp;&nbsp;' + FormatTextToXML(Session.SessionName, xmlText);

  result := result + '  &nbsp;<a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', lang) + '</a>'#13#10 + '</div>'#13#10 + ''#13#10 +
    '<div id="div-cnt" class="content">'#13#10 + '<h2>' + StringFormat(GetFhirMessage('UPLOAD', lang), [aType]) + '</h2>'#13#10 + '<form action="' + s +
    lowercase(aType) + '/upload" enctype="multipart/form-data" method="POST">' + #13#10 + '<input type="hidden" name="format" size="text/html"/><br/>' + #13#10
    + '' + GetFhirMessage('MSG_CONTENT_UPLOAD', lang) + ': <input type="file" name="file" size="60"/><br/>' + #13#10 +
    '<input type="submit" value="Upload"/>'#13#10 + '</form>'#13#10 + ''#13#10 + '<p><br/><a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', lang) +
    '</a></p>' + '</div>'#13#10 + '</body>'#13#10 + '</html>'#13#10 + ''#13#10
end;

function TFhirWebServer.loadMultipartForm(const request: TStream; const contentType: String; var upload: boolean): TMimeMessage;
var
  m: TMimeMessage;
  mp: TMimePart;
begin
  m := TMimeMessage.Create;
  Try
    m.ReadFromStream(request, contentType);
    result := m;
    upload := false;
    for mp in m.Parts do
      if SameText(mp.FileName, 'cda.zip') then
        upload := true;
  Except
    on e: exception do
    begin
      m.Free;
      recordStack(e);
      raise;
    end;
  End;
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
  package : TAdvBytesBuilder;
  b : TBytes;
begin
  package := TAdvBytesBuilder.Create;
  try
    package.addUtf8('-----------------------------------------------------------------'#13#10);
    package.addUtf8(id);
    package.addUtf8(' @ ');
    package.addUtf8(TDateTimeEx.makeUTC.toXML);
    if secure then
      package.addUtf8(' (https)');
    package.addUtf8(#13#10);
    package.addUtf8(request.RawHTTPCommand);
    package.addUtf8(#13#10);
    package.addUtf8(request.RawHeaders.Text);
    if request.PostStream <> nil then
    begin
      package.addUtf8(#13#10);
      SetLength(b, request.PostStream.Size);
      request.PostStream.Read(b[0], length(b));
      request.PostStream.Position := 0;
      if isText(request.ContentType) and (request.ContentEncoding = '') then
        package.Append(b)
      else
        package.addBase64(b);
      package.addUtf8(#13#10);
    end
    else if request.ContentType = 'application/x-www-form-urlencoded' then
    begin
      package.addUtf8(#13#10);
      package.addUtf8(request.UnparsedParams);
      package.addUtf8(#13#10);
    end;

    FInLog.WriteToLog(package.AsBytes);
  finally
    package.Free;
  end;
end;

procedure TFhirWebServer.logResponse(id: String; resp: TIdHTTPResponseInfo);
  procedure log(msg : String);
  var
    package : TAdvBytesBuilder;
    b : TBytes;
  begin
    package := TAdvBytesBuilder.Create;
    try
      package.addUtf8('-----------------------------------------------------------------'#13#10);
      package.addUtf8(id);
      package.addUtf8(' @ ');
      package.addUtf8(TDateTimeEx.makeUTC.toXML);
      if (msg <> '') then
        package.addUtf8(' '+msg);
      package.addUtf8(#13#10);
      package.addUtf8(inttostr(resp.ResponseNo)+' '+resp.ResponseText);
      package.addUtf8(#13#10);
      package.addUtf8(resp.RawHeaders.Text);
      if resp.ContentStream <> nil then
      begin
        package.addUtf8(#13#10);
        SetLength(b, resp.ContentStream.Size);
        resp.ContentStream.Read(b[0], length(b));
        resp.ContentStream.Position := 0;
        if isText(resp.ContentType) and (resp.ContentEncoding = '') then
          package.Append(b)
        else
          package.addBase64(b);
        package.addUtf8(#13#10);
      end
      else if resp.ContentText <> '' then
      begin
        package.addUtf8(#13#10);
        package.addUtf8(resp.ContentText);
        package.addUtf8(#13#10);
      end;

      FOutLog.WriteToLog(package.AsBytes);
    finally
      package.Free;
    end;
  end;
begin
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

function TFhirWebServer.LookupReference(Context: TFHIRRequest; id: String): TResourceWithReference;
var
  store: TFHIROperationEngine;
begin
  store := FServerContext.Storage.createOperationContext(TFHIRRequest(Context).lang);
  try
    result := store.LookupReference(Context, id);
    FServerContext.Storage.yield(store, nil);
  except
    on e: exception do
    begin
      FServerContext.Storage.yield(store, e);
      raise;
    end;
  end;
end;

function TFhirWebServer.makeTaskRedirect(base, id: String; msg : String; fmt : TFHIRFormat; names: TStringList): string;
var
  s, n, body, r : String;
begin
  s := FSourceProvider.getSource('task-redirect.html');
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

procedure TFhirWebServer.MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  FLock.Lock;
  try
    ci.Activity := request.Command + ' ' + request.Document + '?' + request.UnparsedParams;
    ci.Count := ci.Count + 1;
    inc(FTotalCount);
    ci.FStart := GetTickCount;
  finally
    FLock.Unlock;
  end;
end;

procedure TFhirWebServer.MarkExit(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  FLock.Lock;
  try
    ci.Activity := '';
    inc(FTotalTime, GetTickCount - ci.FStart);
    ci.FStart := 0;
  finally
    FLock.Unlock;
  end;
end;

function TFhirWebServer.extractFileData(lang : String; form: TMimeMessage; const name: String; var sContentType: String): TStream;
var
  sLeft, sRight: String;
  iLoop: integer;
  oPart: TMimePart;
  sHeader: String;
  sName: String;
  sFilename: String;
  sContent: String;
begin
  result := nil;
  For iLoop := 0 To form.Parts.Count - 1 Do
  Begin
    oPart := form.Parts[iLoop];
    sHeader := oPart.ContentDisposition;
    StringSplit(sHeader, ';', sLeft, sHeader);
    If trim(sLeft) = 'form-data' Then
    Begin
      StringSplit(trim(sHeader), ';', sLeft, sHeader);
      StringSplit(trim(sLeft), '=', sLeft, sRight);
      If trim(sLeft) = 'name' Then
        sName := RemoveQuotes(trim(sRight));
      StringSplit(trim(sHeader), '=', sLeft, sRight);
      If trim(sLeft) = 'filename' Then
        sFilename := RemoveQuotes(trim(sRight));
      If (result = nil) and (sName <> '') And (sFilename <> '') And (oPart.content.Size > 0) Then
      begin
        result := TBytesStream.Create(oPart.content.AsBytes);
        sContentType := oPart.Mediatype;
      end
      else if (result = nil) and (sName = 'src') then
      begin
        sContent := BytesAsString(oPart.content.AsBytes);
        result := TStringStream.Create(oPart.content.AsBytes); // trim
        if StringStartsWith(sContent, '<', false) then
          sContentType := 'application/fhir+xml'
        else if StringStartsWith(sContent, '{', false) then
          sContentType := 'application/fhir+json'
        else
          raise EFHIRException.CreateLang('FORMAT_UNRECOGNIZED', lang, [sContent]);
      end;
    End
  End;
end;

function TFhirWebServer.GetResource(Session: TFHIRSession; rtype: string; lang, id, ver, op: String): TFhirResource;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
begin
  result := nil;
  request := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.link);
  response := TFHIRResponse.Create;
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

function TFhirWebServer.FindResource(Session: TFHIRSession; rtype: string; lang, params: String): TFhirResource;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
begin
  result := nil;
  request := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.link);
  response := TFHIRResponse.Create;
  try
    response.OnCreateBuilder := doGetBundleBuilder;
    request.Session := Session.link;
    request.ResourceName := rtype;
    request.lang := lang;
    request.LoadParams(params);
    request.CommandType := fcmdSearch;
    Context := TOperationContext.Create;
    try
      checkRequestByJs(context, request);
      ProcessRequest(Context, request, response);
    finally
      Context.Free;
    end;
    if (response.bundle <> nil) and (response.bundle.entryList.Count = 1) then
      result := response.bundle.entryList[0].resource.link
    else
      raise EFHIRException.CreateLang('MSG_NO_MATCH', lang, [rtype + '?' + params]);
  finally
    response.Free;
    request.Free;
  end;
end;

function TFhirWebServer.getReferencesByType(t: String): String;
var
  bundle : TFhirBundle;
  entry : TFhirBundleEntry;
  b : TStringBuilder;
  s : String;
begin
  b := TStringBuilder.create;
  try
    for s in t.trim.Split(['|']) do
    begin
      bundle := DoSearch(nil, s, 'en', '_summary=true&__wantObject=true&_sort=name&_count=50');
      for entry in bundle.entryList do
      begin
        b.Append('<option value="');
        b.Append(entry.resource.id);
        b.Append('">');
        if entry.resource is TFhirPatient then
          b.Append(HumanNamesAsText(TFhirPatient(entry.resource).nameList))
        else if entry.resource is TFhirRelatedPerson then
        {$IFDEF FHIR2}
          b.Append(HumanNameAsText(TFhirRelatedPerson(entry.resource).name))
        {$ELSE}
          b.Append(HumanNamesAsText(TFhirRelatedPerson(entry.resource).nameList))
        {$ENDIF}
        else if entry.resource is TFhirOrganization then
          b.Append(TFhirOrganization(entry.resource).name)
        else
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

procedure TFhirWebServer.GetWebUILink(resource: TFhirResource; base, statedType, id, ver: String; var link, text: String);
var
  tail: String;
begin
  link := '';
  if (resource <> nil) and (id <> '') then
  begin
    tail := id;
    if ver <> '' then
      tail := tail + '/' + ver;
    if resource.ResourceType = frtQuestionnaire then
    begin
      text := 'Try out the Questionnaire as a web form';
      if statedType = 'Profile' then
        link := FBasePath + '/_web/StructureDefinition/' + tail
      else
        link := FBasePath + '/_web/Questionnaire/' + tail;
    end;
    if resource.ResourceType = frtStructureDefinition then
    begin
      link := FBasePath + '/_web/StructureDefinition/' + tail;
      text := 'Try out the Profile as a questionnaire based web form';
    end;
    if resource.ResourceType = frtValueSet then
    begin
      link := FBasePath + '/ValueSet/' + id + '/$expand?filter=';
      text := 'Expand this value set';
    end;
    if resource.ResourceType = frtPatient then
    begin
      link := FBasePath + '/_web/Patient/' + id;
      text := 'Patient Record Page';
    end;
  end;
end;

function TFhirWebServer.OAuthPath(secure: boolean): String;
begin
  if secure then
  begin
    if FStatedSSLPort = 443 then
      result := 'https://' + FHost + FSecurePath
    else
      result := 'https://' + FHost + ':' + inttostr(FStatedSSLPort) + FSecurePath;
  end
  else
  begin
    if FStatedPort = 80 then
      result := 'http://' + FHost + FSecurePath
    else
      result := 'http://' + FHost + ':' + inttostr(FStatedPort) + FSecurePath;
  end;
end;

{$IFDEF MSWINDOWS}

procedure TFhirWebServer.OnCDSResponse(manager: TCDSHooksManager; server: TRegisteredFHIRServer; Context: TObject; response: TCDSHookResponse; error: String);
var
  ctxt: TFHIRWebServerPatientViewContext;
begin
  ctxt := TFHIRWebServerPatientViewContext(Context);

  FLock.Lock;
  try
    if error <> '' then
      ctxt.Errors.Add(error + ' (from ' + server.name + ')')
    else if response = nil then
      ctxt.Errors.Add('Unknown Error (from ' + server.name + ')')
    else
      ctxt.cards.AddAll(response.cards);
  finally
    FLock.Unlock;
  end;
end;
{$ENDIF}

constructor ERestfulAuthenticationNeeded.Create(const sSender, sMethod, sReason, sMsg: String; aStatus: word);
begin
  Create(sSender, sMethod, sReason, aStatus, IssueTypeLogin);
  FMsg := sMsg;
end;

procedure TFhirWebServer.CheckAsyncTasks;
var
  tasks : TAdvList<TAsyncTaskInformation>;
  task : TAsyncTaskInformation;
  n, fn : string;
begin
  tasks := TAdvList<TAsyncTaskInformation>.create;
  try
    ServerContext.Storage.fetchExpiredTasks(tasks);
    for task in tasks do
    begin
      ServerContext.Storage.MarkTaskDeleted(task.key);
      for n in task.names do
      begin
        fn := Path([ServerContext.TaskFolder, 'task-'+inttostr(task.key)+'-'+n+EXT_WEB_TFHIRFormat[task.format]]);
        if FileExists(fn) then
          DeleteFile(fn);
      end;
    end;
  finally
    tasks.free;
  end;
end;

procedure TFhirWebServer.checkRequestByJs(context: TOperationContext; request: TFHIRRequest);
begin
  // js-todo - figure out which scripts to run, and then run them
end;

function TFhirWebServer.CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
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
    FServerContext.SessionManager.MarkSessionChecked(Session.Cookie)
  else
    FServerContext.SessionManager.EndSession(Session.Cookie, ip);
end;

function TFhirWebServer.ClientAddress(secure: boolean): String;
begin
  if secure then
    result := 'https://localhost:' + inttostr(FActualSSLPort) + FSecurePath
  else
    result := 'http://localhost:' + inttostr(FActualPort) + FBasePath;
end;

procedure TFhirWebServer.convertFromVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion);
var
  b : TBytes;
begin
  {$IFDEF FHIR2}
  raise Exception.Create('Version Conversion Services are not available in R2');
  {$ELSE}
  if ServerContext.JavaServices = nil then
    raise Exception.Create('Version Conversion Services are not available');

  b := StreamToBytes(stream);
  b := ServerContext.JavaServices.convertResource(b, format, version);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.position := 0;
  {$ENDIF}
end;

procedure TFhirWebServer.convertToVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion);
var
  b : TBytes;
begin
  {$IFDEF FHIR2}
  raise Exception.Create('Version Conversion Services are not available in R2');
  {$ELSE}
  if ServerContext.JavaServices = nil then
    raise Exception.Create('Version Conversion Services are not available');

  b := StreamToBytes(stream);
  b := ServerContext.JavaServices.uNConvertResource(b, format, version);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.position := 0;
  {$ENDIF}
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
Procedure TFhirWebServer.ReadTags(header: String; request: TFHIRRequest);
// var
// s, s1, l, r, n, v : string;
// cat : TFHIRAtomCategory;
begin
  // raise Exception.Create('todo');
end;


function TFhirWebServer.readVersion(mt : String): TFHIRVersion;
var
  i, s, p, pi,l,r : string;
begin
  result := COMPILED_FHIR_VERSION;

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
        else if r = '3.0.1' then
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


procedure TFhirWebServer.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
var
  op: TFhirTestScriptSetupActionOperation;
begin
  if req.Session = nil then
    exit;
  if req.Session.TestScript = nil then
    exit;
  op := TFhirTestScriptSetupActionOperation.Create;
  req.Session.TestScript.testList.Append.actionList.Append.operation := op;
  if req.CommandType = fcmdOperation then
    op.type_ := TFHIRCoding.Create('http://hl7.org/fhir/testscript-operation-codes', req.OperationName)
  else
    op.type_ := TFHIRCoding.Create('http://hl7.org/fhir/testscript-operation-codes', CODES_TFHIRCommandType[req.CommandType].ToLower);
  op.resourceElement := TFhirCode.Create(req.ResourceName);
  if resp.format = ffJson then
    op.Accept := ContentTypeJson
  else
    op.Accept := ContentTypeXml;
  op.params := req.Parameters.Source;
  op.requestHeaderList.Add('Host', req.baseUrl);
  op.requestHeaderList.Add('Content-Type', MIMETYPES_TFHIRFormat[req.PostFormat]);
  if (req.lastModifiedDate <> 0) then
    op.requestHeaderList.Add('Last-Modified', DateTimeToXMLDateTimeTimeZoneString(req.lastModifiedDate, TimeZoneBias));
  op.requestHeaderList.Add('Language', req.lang);
  op.requestHeaderList.Add('if-match', req.IfMatch);
  op.requestHeaderList.Add('if-none-match', req.IfNoneMatch);
  if (req.IfModifiedSince <> 0) then
    op.requestHeaderList.Add('if-modified-since', DateTimeToXMLDateTimeTimeZoneString(req.IfModifiedSince, TimeZoneBias));
  op.requestHeaderList.Add('if-none-exist', req.IfNoneExist);
  if req.provenance <> nil then
    op.requestHeaderList.Add('x-provenance', ComposeJson(FServerContext.ValidatorContext, req.provenance));
  op.url := req.url;
end;

procedure TFhirWebServer.ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean;
  path: String);
var
  vars: TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.Create;
  try
    vars.Add('status.db', FormatTextToHTML(KDBManagers.dump));
    vars.Add('status.locks', FormatTextToHTML(DumpLocks));
    vars.Add('status.thread.maintenance', ServerContext.TerminologyServer.MaintenanceThreadStatus);
    vars.Add('status.thread.subscriptions', ServerContext.TerminologyServer.SubscriptionThreadStatus);
    vars.Add('status.thread.email', ServerContext.TerminologyServer.EmailThreadStatus);
    vars.Add('status.sessions', ServerContext.SessionManager.DumpSessions);
    vars.Add('status.web', WebDump);
    vars.Add('status.tx', ServerContext.TerminologyServer.Summary);
    vars.Add('status.web-total-count', inttostr(FTotalCount));
    vars.Add('status.web-rest-count', inttostr(FRestCount));
    vars.Add('status.web-total-time', inttostr(FTotalTime));
    vars.Add('status.web-rest-time', inttostr(FRestTime));
{$IFDEF MSWINDOWS}
    vars.Add('status.cds.client', inttostr(FPatientHooks.Count));
{$ENDIF}
    vars.Add('status.run-time', DescribePeriod((GetTickCount - FStartTime) * DATETIME_MILLISECOND_ONE));
    vars.Add('status.run-time.ms', inttostr(GetTickCount - FStartTime));
    ReturnProcessedFile(request, response, nil, 'Diagnostics', FSourceProvider.AltFile('/diagnostics.html', FBasePath), false, vars);
  finally
    vars.Free;
  end;

end;

procedure TFhirWebServer.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TDictionary<String, String> = nil);
begin
  ReturnProcessedFile(request, response, Session, path, path, secure, variables);
end;

procedure TFhirWebServer.RunPostHandler(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean);
var
  handler : TFHIRServerPostHandler;
  params : TParseMap;
  variables: TDictionary<String, String>;
  s : string;
begin
  params := TParseMap.create(request.UnparsedParams);
  try
    s := params.GetVar('handler');
  {$IFNDEF FHIR2}
    if s = 'coverage' then
      handler := TFHIRServerCoveragePostHandler.Create
    else {$ENDIF}
      raise Exception.Create('Unknown Handler');
    try
      handler.secure := secure;
      handler.params := params.Link;
      handler.context := ServerContext.Link;
      handler.session := Session.Link;
      variables := handler.execute;
      try
        ReturnProcessedFile(request, response, session, claimed, actual, secure, variables);
      finally
        variables.Free;
      end;
    finally
      handler.Free;
    end;
  finally
    params.Free;
  end;
end;

procedure TFhirWebServer.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TDictionary<String, String> = nil);
var
  s, n, p, v, t: String;
begin
  logt('script: ' + claimed);

  s := FSourceProvider.getSource(actual);
  // actions....
  if s.Contains('<!--[%clientregistration%]-->') then
  begin
    s := s.Replace('<!--[%clientregistration%]-->', processRegistration(request, session), [rfReplaceAll]);
  end;
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%specurl%]', FHIR_SPEC_URL, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION, [rfReplaceAll]);
  s := s.Replace('[%path%]', FBasePath, [rfReplaceAll]);
  s := s.Replace('[%spath%]', FSecurePath, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%admin%]', FAdminEmail, [rfReplaceAll]);
  if (Session = nil) then
    s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll])
  else
    s := s.Replace('[%logout%]', '|&nbsp;User: ' + Session.SessionName + '&nbsp; <a href="/closed/logout" title="Log Out"><img src="/logout.png"></a>  &nbsp;',
      [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%host%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', FHost + ':' + inttostr(FStatedPort), [rfReplaceAll]);
  if (Session <> nil) and Session.canGetUser and (Session.User <> nil) then
    s := s.Replace('[%jwt%]', Session.JWTPacked, [rfReplaceAll])
  else
    s := s.Replace('[%jwt%]', 'JWT not available', [rfReplaceAll]);

  if FStatedSSLPort = 443 then
    s := s.Replace('[%securehost%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', FHost + ':' + inttostr(FStatedSSLPort), [rfReplaceAll]);
//  if s.Contains('[%fitbit-redirect%]') then
//    s := s.Replace('[%fitbit-redirect%]', FitBitInitiate(FAuthServer.ini.ReadString(voVersioningNotApplicable, 'fitbit', 'secret', ''), // secret,
//      FAuthServer.ini.ReadString(voVersioningNotApplicable, 'fitbit', 'key', ''), // key
//      NewGuidId, // nonce
//      'https://local.healthintersections.com.au:961/closed/_web/fitbit.html')
//      // callback
//      , [rfReplaceAll]);

  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%' + n + '%]', variables[n], [rfReplaceAll]);

  while s.contains('[%options-reference') do
  begin
    StringSplit(s, '[%options-reference', p, v);
    StringSplit(v, '%]', v, t);
    v := getReferencesByType(v);
    s := p+v+t;
  end;

  response.Expires := Now + 1;
  response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
  response.FreeContentStream := true;
  response.contentType := 'text/html; charset=UTF-8';
end;

procedure TFhirWebServer.ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
var
  src : String;
begin
  logt('file: ' + stated);
  if not secure and path.EndsWith('.html') then
  begin
    src := FSourceProvider.getSource(path);
    if src.Contains('<!--[%requires-secure=true%]-->') then
    begin
      response.Expires := Now + 1;
      response.Redirect(ServerContext.FormalURLSecure+stated);
      exit;
    end;
  end;

  response.Expires := Now + 1;
  response.ContentStream := FSourceProvider.asStream(path);
  response.FreeContentStream := true;
  response.contentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

procedure TFhirWebServer.ReverseProxy(proxy: TReverseProxyInfo; AContext: TIdContext; request: TIdHTTPRequestInfo; Session: TFHIRSession;
  response: TIdHTTPResponseInfo; secure: boolean);
var
  client: TReverseClient;
begin
  client := TReverseClient.Create;
  try
    client.proxy := proxy.link;
    client.Context := AContext;
    client.request := request;
    client.response := response;
    if secure then
      client.SecureToken := FSecureToken;
    client.Execute;
  finally
    client.Free;
  end;
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

{$IFDEF MSWINDOWS}

function TFhirWebServer.transform1(resource: TFhirResource; lang, xslt: String; saveOnly: boolean): string;
var
  xml: TFHIRXMLComposer;
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
    xml := TFHIRXMLComposer.Create(FServerContext.ValidatorContext.link, OutputStyleNormal, lang);
    try
      xml.Compose(b, resource, nil);
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
  logt(doc.documentElement.namespaceURI + ', ' + doc.documentElement.nodeName);

  v := CreateOLEObject('MSXML2.FreeThreadedDOMDocument.6.0');
  src := IUnknown(TVarData(v).VDispatch) as IXMLDOMDocument2;
  src.async := false;
  src.resolveExternals := false;
  src.validateOnParse := false;
  src.setProperty('AllowDocumentFunction', true);
  if not src.loadXML(FSourceProvider.getSource(xslt)) then
    raise exception.Create('unable to parse XSLT: ' + src.parseError.reason);

  v := CreateOLEObject('MSXML2.XSLTemplate.6.0');
  xform := IUnknown(TVarData(v).VDispatch) as IXSLTemplate;
  xform.stylesheet := src;

  proc := xform.createProcessor;
  proc.Input := doc;
  proc.addParameter('useMicrosoft', 'true', '');

  if FStatedPort <> 0 then
    url := 'http://' + FHost + ':' + inttostr(FStatedPort)
  else
    url := 'https://' + FHost + ':' + inttostr(FStatedSSLPort);

  if saveOnly then
    proc.addParameter('saveOnly', 'true', '');

  proc.addParameter('expansionServer', url + FBasePath, '');
  proc.addParameter('iconPath', url, '');
  proc.addParameter('jQueryPath', url + '/js', '');

  proc.Transform;
  result := proc.Output;
end;
{$ENDIF}
// function TFhirWebServer.transform2(resource: TFhirResource; lang, xslt: String): string;
// var
// xslt2: AltovaXMLLib_TLB.XSLT2;
// xml : TFHIRXmlComposer;
// s : String;
// AltovaXml : AltovaXMLLib_TLB.Application;
// begin
// xml := TFHIRXmlComposer.Create(lang);
// try
// s := xml.Compose('', '', '', resource, false, nil);
// finally
// xml.Free;
// end;
//
// AltovaXml := AltovaXMLLib_TLB.CoApplication.Create;
// xslt2 := AltovaXml.XSLT2;
// xslt2.InputXMLFromText := s;
// xslt2.XSLFileName := xslt;
// result := xslt2.ExecuteAndGetResultAsString;
// xslt2 := nil;
// AltovaXml := nil;
// end;

{ TFHIRWebServerClientInfo }

destructor TFHIRWebServerClientInfo.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TFHIRWebServerClientInfo.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

{$IFDEF MSWINDOWS}
{ TFHIRWebServerPatientViewContext }

constructor TFHIRWebServerPatientViewContext.Create;
begin
  inherited;
  FCards := TAdvList<TCDSHookCard>.Create;
  FErrors := TStringList.Create;
end;

destructor TFHIRWebServerPatientViewContext.Destroy;
begin
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
{$ENDIF}
{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  FLastSweep := Now;
  inherited Create;
end;

procedure TFhirServerMaintenanceThread.Execute;
begin

  logt('Starting TFhirServerMaintenanceThread');
  try
    FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'starting';
{$IFDEF MSWINDOWS}
    CoInitialize(nil);
{$ENDIF}
    GJsHost := TJsHost.Create;
    GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;

    repeat
      FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'sleeping';
      sleep(1000);
      if not terminated and (FLastSweep < Now - (DATETIME_SECOND_ONE * 5)) then
      begin
        try
          FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Sweeping Sessions';
          FServer.FServerContext.Storage.Sweep;
        except
        end;
        FLastSweep := Now;
      end;
      if not FServer.ServerContext.ForLoad then
      begin
        if (not terminated) then
          try
            FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Building Indexes';
            FServer.FServerContext.TerminologyServer.BuildIndexes(false);
          except
          end;
      end;
        if (not terminated) then
          try
            FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Processing Observations';
             FServer.FServerContext.Storage.ProcessObservations;
          except
          end;
        if (not terminated) then
          try
            FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Checking Async Tasks';
            FServer.CheckAsyncTasks;
          except
          end;
    until terminated;
    try
      FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'dead';
    except
    end;
    try
      FServer.FMaintenanceThread := nil;
    except
    end;
    GJsHost.Free;
    GJsHost := nil;
    {$IFNDEF FHIR2}
    if FServer.ServerContext.JavaServices <> nil then
      FServer.ServerContext.JavaServices.detach;
    {$ENDIF}


{$IFDEF MSWINDOWS}
    CoUninitialize;
{$ENDIF}
    logt('Ending TFhirServerMaintenanceThread');
  except
    logt('Failing TFhirServerMaintenanceThread');
  end;
end;

{ TFhirServerSubscriptionThread }

constructor TFhirServerSubscriptionThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create;
end;

procedure TFhirServerSubscriptionThread.Execute;
begin
  GJsHost := TJsHost.Create;
  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  logt('Starting TFhirServerSubscriptionThread');
  try
    FServer.ServerContext.TerminologyServer.SubscriptionThreadStatus := 'starting';
    repeat
      FServer.ServerContext.TerminologyServer.SubscriptionThreadStatus := 'sleeping';
      sleep(1000);
      if FServer.FActive then
      begin
        FServer.ServerContext.TerminologyServer.SubscriptionThreadStatus := 'processing subscriptions';
        FServer.FServerContext.Storage.ProcessSubscriptions;
      end;
    until terminated;
    try
      FServer.ServerContext.TerminologyServer.SubscriptionThreadStatus := 'dead';
    except
    end;
    try
      FServer.FSubscriptionThread := nil;
    except
    end;
    logt('Ending TFhirServerSubscriptionThread');
  except
    logt('Failing TFhirServerSubscriptionThread');
  end;
  GJsHost.Free;
  GJsHost := nil;
end;

{ TFhirServerEmailThread }

constructor TFhirServerEmailThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create;
end;

procedure TFhirServerEmailThread.Execute;
var
  i: integer;
begin
  GJsHost := TJsHost.Create;
  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  logt('Starting TFhirServerEmailThread');
  try
    FServer.ServerContext.TerminologyServer.EmailThreadStatus := 'starting';
    repeat
      FServer.ServerContext.TerminologyServer.EmailThreadStatus := 'sleeping';
      i := 0;
      while not terminated and (i < 60) do
      begin
        sleep(1000);
        inc(i);
      end;
      if FServer.FActive and not terminated then
      begin
        FServer.ServerContext.TerminologyServer.EmailThreadStatus := 'processing Emails';
        FServer.FServerContext.Storage.ProcessEmails;
      end;
    until terminated;
    try
      FServer.ServerContext.TerminologyServer.EmailThreadStatus := 'dead';
    except
    end;
    try
      FServer.FEmailThread := nil;
    except
    end;
    logt('Ending TFhirServerEmailThread');
  except
    logt('Failing TFhirServerEmailThread');
  end;
  GJsHost.Free;
  GJsHost := nil;

end;

{ TAsyncTaskThread }

procedure TAsyncTaskThread.callback(IntParam: Integer; StrParam: String);
begin
  status(atsProcessing, StrParam);
end;

constructor TAsyncTaskThread.Create;
begin
  inherited Create(true); // suspended
end;

destructor TAsyncTaskThread.Destroy;
begin
  Files.free;
  FRequest.Free;
  FServer.Free;
  FBundle.Free;
  inherited;
end;

procedure TAsyncTaskThread.details;
begin
  if FBundle <> nil then
    FServer.serverContext.Storage.setAsyncTaskDetails(key, {$IFDEF FHIR4}Fbundle.timestamp{$ELSE}TDateTimeEx.makeUTC{$ENDIF}, Fbundle.Links['self']);
end;

procedure TAsyncTaskThread.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TFhirBundleTypeEnum; out builder: TFhirBundleBuilder);
begin
  FBundle := TFHIRBundle.create(aType);
  if context.Format = ffNDJson then
  begin
    files := TAdvMap<TAdvFile>.create;
    builder := TFHIRBundleBuilderNDJson.Create(FBundle.link, IncludeTrailingPathDelimiter(FServer.ServerContext.TaskFolder)+'task-'+inttostr(FKey), files.link)
  end
  else
    builder := TFHIRBundleBuilderSimple.Create(FBundle.link);
end;

procedure TAsyncTaskThread.Execute;
var
  response : TFHIRResponse;
  op: TFHIROperationEngine;
  t: cardinal;
  us, cs: String;
  ctxt : TOperationContext;
begin
  GJsHost := TJsHost.Create;
  try
    GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
    status(atsWaiting, 'Waiting to start');
    sleep(100);
    response := TFHIRResponse.Create;
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
      logt('Start Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source);
      op := FServer.ServerContext.Storage.createOperationContext(request.lang);
      try
        op.OnPopulateConformance := FServer.PopulateConformance;
        ctxt := TOperationContext.create(false, callback, 'starting');
        try
          op.Execute(ctxt, request, response);
        finally
          ctxt.Free;
        end;
        FServer.ServerContext.Storage.yield(op, nil);
      except
        on e: exception do
        begin
          FServer.ServerContext.Storage.yield(op, e);
          raise;
        end;
      end;
      details;
      saveOutcome(response);
      status(atsComplete, 'Complete');
      t := GetTickCount - t;
      logt('Finish Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source + '. rt = ' + inttostr(t));
    finally
      response.Free;
    end;
  except
    on e : exception do
    begin
      logt('Error Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source + '. rt = ' + inttostr(t)+': '+e.Message);
      status(atsError, e.Message);
    end;
  end;
  FServer.FLock.Lock;
  try
    FServer.FThreads.Remove(self);
  finally
    FServer.FLock.Unlock;
  end;
  FreeOnTerminate := true;
  GJsHost.Free;
  GJsHost := nil;
end;

procedure TAsyncTaskThread.kill;
begin
  TerminateThread(ThreadHandle, 1);
end;

procedure TAsyncTaskThread.saveOutcome;
var
  names : TStringList;
  f : TFileStream;
  n : String;
begin
  names := TStringList.Create;
  try
    if files = nil then
    begin
      names.Add('content');
      f := TFileStream.Create(Path([FServer.ServerContext.TaskFolder, 'task-'+inttostr(key)+'-content'+EXT_WEB_TFHIRFormat[format]]), fmCreate);
      try
        if fFormat = ffNDJson then
          resourceToStream(response.Resource, f, ffJson, OutputStyleNormal)
        else
          resourceToStream(response.Resource, f, FFormat, OutputStyleNormal);
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
    FServer.ServerContext.Storage.MarkTaskForDownload(key, names);
  finally
    names.Free;
  end;
end;

procedure TAsyncTaskThread.SetRequest(const Value: TFHIRRequest);
begin
  FRequest.Free;
  FRequest := Value;
end;

procedure TAsyncTaskThread.SetServer(const Value: TFhirWebServer);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TAsyncTaskThread.status(status: TAsyncTaskStatus; message: String);
begin
  FServer.serverContext.Storage.updateAsyncTaskStatus(key, status, message);
end;

Initialization
  IdSSLOpenSSLHeaders.Load;
End.
