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
  Windows, SysUtils, Classes, IniFiles, ActiveX, System.Generics.Collections, ComObj, {JCL JclDebug, }EncdDecd,  HMAC,  {$IFNDEF VER260} System.NetEncoding, {$ENDIF}

  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer,
  IdTCPServer, IdContext, IdSSLOpenSSL, IdHTTP, MimeMessage, IdCookie,
  IdZLibCompressorBase, IdCompressorZLib, IdZLib, IdSSLOpenSSLHeaders, IdGlobalProtocols, IdWebSocket,

  EncodeSupport, GUIDSupport, DateSupport, BytesSupport, StringSupport, ThreadSupport, CertificateSupport,

  AdvBuffers, AdvObjectLists, AdvStringMatches, AdvZipParts, AdvZipReaders, AdvVCLStreams, AdvMemories, AdvIntegerObjectMatches, AdvExceptions, AdvGenerics,

  kCritSct, ParseMap, TextUtilities, KDBManager, HTMLPublisher, KDBDialects,
  AdvJSON, libeay32, RDFUtilities, JWT,

  MXML, GraphQL, MsXml, MsXmlParser,

  FHIRTypes, FHIRResources, FHIRParser, FHIRConstants,
  FHIRBase, FHIRParserBase, FHIRTags, FHIRSupport, FHIRLang, FHIRStorageService, FHIRUtilities, FHIRSecurity, SmartOnFhirUtilities,
  QuestionnaireBuilder, FHIRClient, CDSHooksUtilities, CDSHooksClientManager, FHIRXhtml, FHIRGraphQL,

  TerminologyServer, TerminologyServerStore, SnomedServices, SnomedPublisher, SnomedExpressions, LoincServices, LoincPublisher,
  TerminologyWebServer, AuthServer, TwilioClient, ReverseClient, CDSHooksServer, WebSourceProvider,

  FHIRUserProvider, FHIRServerContext, FHIRServerConstants, SCIMServer, ServerUtilities, ClientApplicationVerifier, JWTService, TerminologyServices {$IFNDEF FHIR2}, OpenMHealthServer{$ENDIF};

Const
  OWIN_TOKEN_PATH = 'oauth/token';

Type
  ERestfulAuthenticationNeeded = class (ERestfulException)
  private
    FMsg : String;
  public
    Constructor Create(Const sSender, sMethod, sReason, sMsg : String; aStatus : word); Overload; Virtual;
    Property Msg : String read FMsg;
  end;

  TFhirWebServer = class;

  TFhirServerMaintenanceThread = class (TThread)
  private
    FServer : TFhirWebServer;
    FLastSweep : TDateTime;
  protected
    procedure Execute; override;
  public
    constructor create(server : TFHIRWebServer);
  end;

  TFhirServerSubscriptionThread = class (TThread)
  private
    FServer : TFhirWebServer;
  protected
    procedure Execute; override;
  public
    constructor create(server : TFHIRWebServer);
  end;

  TFhirServerEmailThread = class (TThread)
  private
    FServer : TFhirWebServer;
  protected
    procedure Execute; override;
  public
    constructor create(server : TFHIRWebServer);
  end;

  TFHIRWebServerClientInfo = class (TAdvObject)
  private
    FContext: TIdContext;
    FActivity: String;
    FSession: TFHIRSession;
    FCount: integer;
    FStart : cardinal;
    procedure SetSession(const Value: TFHIRSession);
  public
    Destructor Destroy; Override;
    property Context : TIdContext read FContext write FContext;
    property Session : TFHIRSession read FSession write SetSession;
    property Activity : String read FActivity write FActivity;
    property Count : integer read FCount write FCount;
  end;

  TFHIRWebServerPatientViewContext = class (TAdvObject)
  private
    FCards: TAdvList<TCDSHookCard>;
    FManager: TCDSHooksManager;
    FErrors : TStringList;
    procedure SetManager(const Value: TCDSHooksManager);
  public
    constructor Create; Override;
    Destructor Destroy; Override;
    property manager : TCDSHooksManager read FManager write SetManager;
    property Errors : TStringList read FErrors;
    property cards : TAdvList<TCDSHookCard> read FCards;
  end;


  TFhirWebServer = Class(TAdvObject)
  Private
    FIni : TFHIRServerIniFile;
    FLock : TCriticalSection;

    // base web server configuration
    FActive : boolean; // can start without actually making the web servers available - for internal use e.g. loading...
    FName : String; // name of this server
    FOwnerName : String; // name of the org that adminsiters the service
    FHomePage : String;
    FAdminEmail : String;  // stated administrator
    FFacebookLike : boolean;
    FHostSms : String;  // for status update messages
    FSourceProvider : TFHIRWebServerSourceProvider;

    // web configuration
    FHost : String;
    FActualPort : Integer;
    FBasePath : String;
    FActualSSLPort : Integer;
    FSecurePath : String;
    FCertFile : String;
    FRootCertFile : String;
    FSSLPassword : String;

    // security admin
    FUseOAuth : boolean;
    FOWinSecurityPlain : boolean;
    FOWinSecuritySecure : boolean;
    FServeMissingCertificate: boolean;
    FServeUnknownCertificate : boolean;
    FCertificateIdList : TStringList;
    FServeMissingJWT : boolean;
    FServeUnverifiedJWT : boolean;
    FJWTAuthorities : TDictionary<String, String>;

    // Reverse proxy support. stated vs actual: to allow for a reverse proxy
    FStatedPort : Integer;
    FStatedSSLPort : Integer;
    FSecureToken : String;

    // operational fields
    FPlainServer : TIdHTTPServer;
    FSSLServer : TIdHTTPServer;
    FIOHandler: TIdServerIOHandlerSSLOpenSSL;
    FTotalCount : cardinal;
    FRestCount : cardinal;
    FStartTime : cardinal;
    FTotalTime : cardinal;
    FRestTime : cardinal;
    FClients : TAdvList<TFHIRWebServerClientInfo>;
    FServerContext : TFHIRServerContext;
    FTerminologyWebServer : TTerminologyWebServer;
    FMaintenanceThread : TFhirServerMaintenanceThread;
    FSubscriptionThread : TFhirServerSubscriptionThread;
    FEmailThread : TFhirServerEmailThread;
    FAuthServer : TAuth2Server;
    FAdaptors : TAdvMap<TFHIRFormatAdaptor>;
    carry : TAdvZipReader; // for uploading support
    carryName : String;
    FPatientViewServers : TDictionary<String, String>;
    FPatientHooks : TAdvMap<TFHIRWebServerPatientViewContext>;
    FReverseProxyList : TAdvList<TReverseProxyInfo>;
    FCDSHooksServer : TCDSHooksServer;
    FIsTerminologyServerOnly : boolean;

    function OAuthPath(secure : boolean):String;
    procedure PopulateConformanceAuth(rest: TFhirCapabilityStatementRest);
    procedure PopulateConformance(sender : TObject; conf : TFhirCapabilityStatement);
    function WebDump : String;

    function hasInternalSSLToken(request : TIdHTTPRequestInfo) : boolean;
    procedure cacheResponse(response : TIdHTTPResponseInfo; caching : TFHIRCacheControl);
    procedure OnCDSResponse(manager : TCDSHooksManager; server : TRegisteredFHIRServer; context : TObject; response : TCDSHookResponse; error : String);
    function GetResource(session : TFhirSession; rtype : String; lang, id, ver, op : String) : TFhirResource;
    function FindResource(session : TFhirSession; rtype : String; lang, params : String) : TFhirResource;
    function DoSearch(session : TFhirSession; rtype : string; lang, params : String) : TFHIRBundle;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference;
    function transform1(resource : TFhirResource; lang, xslt : String; saveOnly : boolean) : string;
    function HandleWebUIRequest(request : TFHIRRequest; response : TFHIRResponse; secure : boolean) : TDateTime;
    function HandleWebQuestionnaire(request : TFHIRRequest; response : TFHIRResponse) : TDateTime;
    function HandleWebQuestionnaireInstance(request : TFHIRRequest; response : TFHIRResponse) : TDateTime;
    function HandleWebEdit(request : TFHIRRequest; response : TFHIRResponse) : TDateTime;
    function HandleWebPost(request : TFHIRRequest; response : TFHIRResponse) : TDateTime;
    function HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
    procedure startHooks(ctxt: TFHIRWebServerPatientViewContext; patient : TFHIRPatient; url : String);

    function HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure : boolean) : TDateTime;
    function HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure : boolean) : TDateTime;
    function HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;

    Procedure ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path : String);
//    Procedure ReadTags(Headers: TIdHeaderList; Request : TFHIRRequest); overload;
    Procedure ReadTags(header : String; Request : TFHIRRequest); overload;
    function CheckSessionOK(session : TFhirSession; ip : string) : Boolean;
    Function BuildFhirHomePage(comps, lang, host, sBaseURL : String; session : TFhirSession; secure : boolean): String;
    Function BuildFhirAuthenticationPage(lang, host, path, msg : String; secure : boolean) : String;
    Function BuildFhirUploadPage(lang, host, sBaseURL : String; aType : String; session : TFhirSession) : String;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    Procedure ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    Procedure ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkExit(AContext: TIdContext);
    Procedure ReverseProxy(proxy : TReverseProxyInfo; AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String; esession : TFhirSession; cert : TIdX509);
    Procedure HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
    Procedure HandleDiscoveryRedirect(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleOWinToken(AContext: TIdContext; secure : boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; relativeReferenceAdjustment : integer; pretty, gzip : boolean);
    function extractFileData(form : TMimeMessage; const name: String; var sContentType : String): TStream;
    Procedure StartServer(active : boolean);
    Procedure StopServer;
    Function ProcessZip(lang : String; oStream : TStream; name, base : String; init : boolean; ini : TFHIRServerIniFile; context : TOperationContext; var cursor : integer) : TFHIRBundle;
    procedure SSLPassword(var Password: String);
    procedure SendError(response: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; e : exception; session : TFhirSession; addLogins : boolean; path : String; relativeReferenceAdjustment : integer; code : TFhirIssueTypeEnum);
    Procedure ProcessRequest(context : TOperationContext; request : TFHIRRequest; response : TFHIRResponse);
    function BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sContentEncoding, sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse;     var aFormat: TFHIRFormat; var redirect: boolean; form: TMimeMessage; bAuth, secure : Boolean; out relativeReferenceAdjustment : integer; var pretty : boolean; session : TFhirSession; cert : TIdX509): TFHIRRequest;
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    Function WebDesc : String;
    function EndPointDesc(secure : boolean) : String;
    procedure GetWebUILink(resource : TFhirResource; base, statedType, id, ver : String; var link, text : String);
    function loadMultipartForm(const request: TStream; const contentType : String; var upload : boolean): TMimeMessage;
    function processProvenanceHeader(header, lang : String) : TFhirProvenance;
    function DoVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
    Procedure ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
    Procedure RecordExchange(req : TFHIRRequest; resp : TFHIRResponse; e : Exception = nil);
    procedure smsStatus(msg : String);
    procedure loadConfiguration;
    procedure SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
  Public
    Constructor Create(ini : TFHIRServerIniFile; Name : String; terminologyServer : TTerminologyServer; context : TFHIRServerContext);
    Destructor Destroy; Override;

    Procedure Start(active : boolean);
    Procedure Stop;
    Procedure Transaction(stream : TStream; init: boolean; name, base : String; ini : TFHIRServerIniFile; callback : TInstallerCallback);
    Procedure ReturnProcessedFile(response : TIdHTTPResponseInfo; session : TFhirSession; path : String; secure : boolean; variables: TDictionary<String, String> = nil); overload;
    Procedure ReturnProcessedFile(response : TIdHTTPResponseInfo; session : TFhirSession; claimed, actual : String; secure : boolean; variables: TDictionary<String, String> = nil); overload;

    Property ServerContext : TFHIRServerContext read FServerContext;
    property AuthServer : TAuth2Server read FAuthServer;
    Property SourceProvider : TFHIRWebServerSourceProvider read FSourceProvider write SetSourceProvider;
    property Host : String read FHost;
    property CDSHooksServer : TCDSHooksServer read FCDSHooksServer;

    property UseOAuth : boolean read FUseOAuth write FUseOAuth;
    property OWinSecurityPlain : boolean read FOWinSecurityPlain write FOWinSecurityPlain;
    property OWinSecuritySecure : boolean read FOWinSecuritySecure write FOWinSecuritySecure;
    property ServeMissingCertificate: boolean read FServeMissingCertificate write FServeMissingCertificate;
    property ServeUnknownCertificate : boolean read FServeUnknownCertificate write FServeUnknownCertificate;
    property CertificateIdList : TStringList read FCertificateIdList;
    property ServeMissingJWT : boolean read FServeMissingJWT write FServeMissingJWT;
    property ServeUnverifiedJWT : boolean read FServeUnverifiedJWT write FServeUnverifiedJWT;
    property JWTAuthorities : TDictionary<String, String> read FJWTAuthorities;

    function ClientAddress(secure : boolean) : String;
    property IsTerminologyServerOnly : boolean read FIsTerminologyServerOnly write FIsTerminologyServerOnly;
  End;

Function ProcessPath(base, path : String): string;

Implementation


Uses
  Registry,

  FHIRLog, SystemService,

  FileSupport,
  FacebookSupport;

Function GetMimeTypeForExt(AExt: String): String;
Var
  fReg: TRegistry;
Begin
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
    Try
      fReg := TRegistry.Create;
      Try
        fReg.RootKey := HKEY_LOCAL_MACHINE;
        fReg.OpenKeyReadOnly('Software\Classes\' + AExt);
        Result := freg.ReadString('Content Type');
        fReg.CloseKey;
      Finally
        freg.Free;
      End;
    Except
    End;
  End;
  If Result = '' Then
    Result := 'application/octet-stream';
End;

{ TFhirWebServer }

Function ProcessPath(base, path : String): string;
var
  s : String;
begin
  base := base.Substring(0, base.Length-1);
  if path.StartsWith('..\') then
  begin
    s := base;
    while path.StartsWith('..\') do
    begin
      path := path.Substring(3);
      s := ExtractFilePath(s);
      s := s.Substring(0, s.Length-1);
    end;
    result := IncludeTrailingPathDelimiter(s)+IncludeTrailingPathDelimiter(path);
  end
  else
    result := IncludeTrailingPathDelimiter(path);
end;

procedure TFhirWebServer.loadConfiguration;
var
  s : String;
  ts : TStringList;
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
      FReverseProxyList.Add(TReverseProxyInfo.create(s, FIni.ReadString(voMaybeVersioned, 'reverse-proxy', s, '')));
  finally
    ts.Free;
  end;

  if FIni.ReadString(voMaybeVersioned, 'web', 'insecure', '') = 'conformance' then
    FServerContext.ValidatorContext.setNonSecureTypes(['Conformance', 'StructureDefinition', 'ValueSet', 'ConceptMap', 'DataElement', 'OperationDefinition', 'SearchParameter', 'NamingSystem'])
  else if FIni.ReadString(voMaybeVersioned, 'web', 'insecure', '') = 'conformance+patient' then
    FServerContext.ValidatorContext.setNonSecureTypes(['Conformance', 'StructureDefinition', 'ValueSet', 'ConceptMap', 'DataElement', 'OperationDefinition', 'SearchParameter', 'NamingSystem', 'Patient'])
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
    ts := TStringList.create;
    try
      FIni.ReadSection(voVersioningNotApplicable, 'patient-view', ts);
      for s in ts do
        FPatientViewServers.Add(s, FIni.ReadString(voVersioningNotApplicable, 'patient-view', s, ''));
    finally
      ts.free;
    end;
  end;

  FOwnerName := Fini.readString(voVersioningNotApplicable, 'admin', 'ownername', '');
  if FOwnerName = '' then
    FOwnerName := 'Health Intersections';
  FAdminEmail := Fini.readString(voVersioningNotApplicable, 'admin', 'email', '');
  if FAdminEmail = '' then
    raise Exception.Create('Ad admin email is required');

  FHostSms := FIni.ReadString(voVersioningNotApplicable, 'sms', 'owner', '');
end;

Constructor TFhirWebServer.Create(ini : TFHIRServerIniFile; Name : String; terminologyServer : TTerminologyServer; context : TFHIRServerContext);
var
  txu : String;
Begin
  Inherited Create;
  FLock := TCriticalSection.Create('fhir-rest');
  FCertificateIdList := TStringList.create;
  FName := Name;
  FIni := ini;

  FClients := TAdvList<TFHIRWebServerClientInfo>.create;
  FPatientViewServers := TDictionary<String, String>.create;
  FPatientHooks := TAdvMap<TFHIRWebServerPatientViewContext>.create;
  FReverseProxyList := TAdvList<TReverseProxyInfo>.create;
  FServerContext := context;

  loadConfiguration;

  FServerContext.FormalURLPlain := 'http://'+FIni.ReadString(voMaybeVersioned, 'web', 'host', '')+':'+inttostr(FStatedPort);
  FServerContext.FormalURLSecure := 'https://'+FIni.ReadString(voMaybeVersioned, 'web', 'host', '')+':'+inttostr(FStatedSSLPort);
  FServerContext.FormalURLPlainOpen := 'http://'+FIni.ReadString(voMaybeVersioned, 'web', 'host', '')+':'+inttostr(FStatedPort)+ FBasePath;
  FServerContext.FormalURLSecureOpen := 'https://'+FIni.ReadString(voMaybeVersioned, 'web', 'host', '')+':'+inttostr(FStatedSSLPort) + FBasePath;
  FServerContext.FormalURLSecureClosed := 'https://'+FIni.ReadString(voMaybeVersioned, 'web', 'host', '')+':'+inttostr(FStatedSSLPort) + FSecurePath;
  FServerContext.ClientApplicationVerifier := TClientApplicationVerifier.create;
  ServerContext.JWTServices := TJWTServices.Create;
  ServerContext.JWTServices.Host := FHost;
  ServerContext.JWTServices.Cert := FCertFile;
  ServerContext.JWTServices.Password := FSSLPassword;
  ServerContext.JWTServices.DatabaseId := ServerContext.SystemId;

  logt('Load & Cache Store: ');

  // Base Web server configuration
  logt(inttostr(FServerContext.Storage.TotalResourceCount)+' resources');

  if FStatedPort = 80 then
    txu := 'http://'+FHost
  else
    txu := 'http://'+FHost+':'+inttostr(FStatedPort);
  if terminologyServer <> nil then
    FTerminologyWebServer := TTerminologyWebServer.create(terminologyServer.Link, FServerContext.ValidatorContext.Link, txu, FBasePath+'/', ReturnProcessedFile);

  if FIni.readString(voVersioningNotApplicable, 'web', 'clients', '') = '' then
    raise Exception.Create('No Authorization file found');
  FAuthServer := TAuth2Server.Create(FIni.readString(voVersioningNotApplicable, 'web', 'clients', ''), FHost, inttostr(FStatedSSLPort));
  FAuthServer.ServerContext := FServerContext.Link;
  FAuthServer.OnProcessFile := ReturnProcessedFile;
  FAuthServer.OnDoSearch := DoSearch;
  FAuthServer.Path := FIni.ReadString(voMaybeVersioned, 'web', 'auth-path', '/oauth2');
  FAuthServer.AdminEmail := FAdminEmail;
  FAuthServer.EndPoint := OAuthPath(true);
  FAuthServer.host := host;
  FAuthServer.Active := FUseOAuth;

  ServerContext.JWTServices.JWKAddress := FAuthServer.KeyPath;
  ServerContext.ClientApplicationVerifier.Server := FIni.ReadString(voMaybeVersioned, 'web', 'cavs', FAuthServer.CavsPath);


  FAdaptors := TAdvMap<TFHIRFormatAdaptor>.create;
  {$IFDEF FHIR3}
  FAdaptors.Add('dataPoints', TOpenMHealthAdaptor.Create);
  {$ENDIF}

  logt('Web Server:');
  if (FActualPort = 0) then
    logt('  http: not active')
  else if FStatedPort <> FActualPort then
    logt('  http: listen on '+inttostr(FActualPort)+', but claim = '+inttostr(FStatedPort)+' (reverse proxy mode')
  else
    logt('  http: listen on '+inttostr(FActualPort));

  if (FActualSSLPort = 0) then
    logt('  https: not active')
  else if FStatedSSLPort <> FActualSSLPort then
    logt('  https: listen on '+inttostr(FActualSSLPort)+', but claim = '+inttostr(FStatedSSLPort)+' (reverse proxy mode')
  else
    logt('  https: listen on '+inttostr(FActualSSLPort));

  if (FBasePath <> '') and (FSecurePath <> '') then
    logt(' ...paths: open = '+FBasePath+', secure = '+FSecurePath)
  else if (FActualPort <> 0) then
    logt(' ...paths: open = '+FBasePath)
  else if (FActualSSLPort <> 0) then
    logt(' ...paths: secure = '+FSecurePath)
  else
    logt(' ...paths: <none>');
  FCDSHooksServer := TCDSHooksServer.create(FServerContext);

//  FAuthRequired := FIni.ReadString('fhir', 'oauth-secure', '') = '1';
//  FAppSecrets := FIni.ReadString('fhir', 'oauth-secrets', '');
End;

Destructor TFhirWebServer.Destroy;
Begin
  FCDSHooksServer.Free;
  carry.Free;
  FAdaptors.Free;
  FTerminologyWebServer.free;
  FIni.Free;
  FAuthServer.Free;
  FPatientViewServers.Free;
  FClients.Free;
  FPatientHooks.Free;
  FReverseProxyList.Free;
  FServerContext.free;
  FLock.Free;
  FCertificateIdList.Free;
  FSourceProvider.Free;
  Inherited;
End;

procedure TFhirWebServer.DoConnect(AContext: TIdContext);
var
  ci : TFHIRWebServerClientInfo;
begin
  CoInitialize(nil);
  FLock.Lock;
  try
    ci := TFHIRWebServerClientInfo.create;
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
  CoUninitialize;
end;

function TFhirWebServer.DoSearch(session: TFhirSession; rtype: string; lang, params: String): TFHIRBundle;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  context : TOperationContext;
begin
  request := TFHIRRequest.create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.Link);
  context := TOperationContext.Create;
  try
    response := TFHIRResponse.Create;
    try
      request.Session := session.link;
      request.ResourceName := rType;
      request.Lang := lang;
      request.LoadParams(params);
      request.CommandType := fcmdSearch;
      ProcessRequest(context, request, response);
      result := response.bundle.Link;
    finally
      response.free;
      request.Free;
    end;
  finally
    context.Free;
  end;
end;

function TFhirWebServer.DoVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
var
  i : integer;
begin
  result := ServeUnknownCertificate or FCertificateIdList.Find(Certificate.FingerprintAsString, i);
end;

function port(port, default : integer): String;
begin
  if (port = default) then
    result := ''
  else
    result := ':'+inttostr(port);
end;

function TFhirWebServer.EndPointDesc(secure : boolean): String;
begin
  result := '';
  if (secure) then
  begin
    if FBasePath <> '' then
      result := result + ' <li><a href="http://'+FHost+port(FStatedPort, 80)+FBasePath+'">Unsecured access at '+FBasePath+'</a> - direct access with no security considerations</li>'#13#10;
    if FSecurePath <> '' then
      result := result + ' <li><a href="'+FSecurePath+'">Secured access at '+FSecurePath+'</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end
  else
  begin
    if FBasePath <> '' then
      result := result + ' <li><a href="'+FBasePath+'">Unsecured access at '+FBasePath+'</a> - direct access with no security considerations</li>'#13#10;
    if FSecurePath <> '' then
      result := result + ' <li><a href="https://'+FHost+port(FStatedSSLPort, 443)+FSecurePath+'">Secured access at '+FSecurePath+'</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end;
end;

Procedure TFhirWebServer.Start(active : boolean);
Begin
  FActive := active;
  FStartTime := GetTickCount;
  StartServer(active);
  if (active) and (ServerContext.SubscriptionManager <> nil) then
  begin
    FMaintenanceThread := TFhirServerMaintenanceThread.create(self);
    FSubscriptionThread := TFhirServerSubscriptionThread.create(self);
    FEmailThread := TFhirServerEmailThread.create(self);
    smsStatus('The server '+ServerContext.FormalURLPlain+' for '+ServerContext.OwnerName+' has started');
  end;
End;

procedure TFhirWebServer.smsStatus(msg : String);
var
  client : TTwilioClient;
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
        client.Body := msg;
        client.send;
      end;
    finally
      client.Free;
    end;
  except
  end;
end;


procedure TFhirWebServer.startHooks(ctxt: TFHIRWebServerPatientViewContext; patient: TFHIRPatient; url : String);
var
  server : TRegisteredFHIRServer;
  req : TCDSHookRequest;
  s : String;
  be : TFHIRBundleEntry;
begin
  for s in FPatientViewServers.Keys do
  begin
    server := TRegisteredFHIRServer.Create;
    try
      server.name := s;
      server.fhirEndpoint := FPatientViewServers[s];
      server.addCdsHook('patient-view', TCDSHooks.patientView);
      ctxt.manager.registerServer(server);
    finally
      server.Free;
    end;
  end;

  req := TCDSHookRequest.Create;
  try
    req.hook := TCDSHooks.patientView;
    req.hookInstance := FServerContext.FormalURLPlain;  // arbitrary global
    req.patient := patient.id;
    be := TFhirBundleEntry.create;
    req.preFetch.Add('patient',  be);
    be.resource := patient.Link;
    ctxt.manager.makeRequest(req, OnCDSResponse, ctxt);
  finally
    req.Free;
  end;
end;

Procedure TFhirWebServer.Stop;
Begin
  if ServerContext.SubscriptionManager <> nil then
    smsStatus('The server '+ServerContext.FormalURLPlain+' for '+ServerContext.OwnerName+' is stopping');
  if FSubscriptionThread <> nil then
    FSubscriptionThread.Terminate;
  if FMaintenanceThread <> nil then
    FMaintenanceThread.Terminate;
  if FEmailThread <> nil then
    FEmailThread.Terminate;
  StopServer;
End;

Procedure TFhirWebServer.StartServer(active : boolean);
Begin
  if FActualPort > 0 then
  begin
    FPlainServer := TIdHTTPServer.Create(Nil);
    FPlainServer.ServerSoftware := 'Health Intersections FHIR Server';
    FPlainServer.ParseParams := False;
    FPlainServer.DefaultPort := FActualPort;
    FPlainServer.KeepAlive := False;
    FPlainServer.OnCreatePostStream := CreatePostStream;
    FPlainServer.OnCommandGet := PlainRequest;
    FPlainServer.OnCommandOther := PlainRequest;
    FPlainServer.OnConnect := DoConnect;
    FPlainServer.OnDisconnect := DoDisconnect;
    FPlainServer.OnParseAuthentication := ParseAuthenticationHeader;
    FPlainServer.Active := active;
  end;
  if FActualSSLPort > 0 then
  begin
    If Not FileExists(FCertFile) Then
      Raise Exception.Create('SSL Certificate "'+FCertFile+' could not be found');
    If Not FileExists(ChangeFileExt(FCertFile, '.key')) Then
      Raise Exception.Create('SSL Certificate Private Key "'+ChangeFileExt(FCertFile, '.key')+' could not be found');
    If (FRootCertFile <> '') and (Not FileExists(FRootCertFile)) Then
      Raise Exception.Create('SSL Certificate "'+FRootCertFile+' could not be found');
    FSSLServer := TIdHTTPServer.Create(Nil);
    FSSLServer.ServerSoftware := 'Health Intersections FHIR Server';
    FSSLServer.ParseParams := False;
    FSSLServer.DefaultPort := FActualSSLPort;
    FSSLServer.KeepAlive := False;
    FSSLServer.OnCreatePostStream := CreatePostStream;
    FIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Nil);
    FSSLServer.IOHandler := FIOHandler;
    FIOHandler.SSLOptions.Method := sslvSSLv23;
    FIOHandler.SSLOptions.Mode := sslmServer;
    // SSL v3 / TLS 1 required for older versions of DotNet
    FIOHandler.SSLOptions.SSLVersions := [sslvSSLv3, {$IFNDEF NCTS}sslvTLSv1, {$endif} sslvTLSv1_2];
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
//    FIOHandler.SSLOptions.
    FIOHandler.OnGetPassword := SSLPassword;
    FSSLServer.OnCommandGet := SecureRequest;
    FSSLServer.OnCommandOther := SecureRequest;
    FSSLServer.OnConnect := DoConnect;
    FSSLServer.OnDisconnect := DoDisconnect;
    FSSLServer.OnParseAuthentication := ParseAuthenticationHeader;
    FSSLServer.Active := active;
    LoadEAYExtensions;
  end;
end;

Procedure TFhirWebServer.StopServer;
Begin
  if FSSLServer <> nil then
  begin
    FSSLServer.Active := False;
    FreeAndNil(FSSLServer);
    FreeAndNil(FIOHandler);
    UnloadEAYExtensions;
  end;
  if FPlainServer <> nil then
  begin
    FPlainServer.Active := False;
    FreeAndNil(FPlainServer);
  end;
End;

procedure TFhirWebServer.Transaction(stream: TStream; init: boolean; name, base : String; ini : TFHIRServerIniFile; callback : TInstallerCallback);
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
//  op : TFHIRNativeOperationEngine;
  cursor : integer;
  context : TOperationContext;
begin
//  if init then
//    op := FServerContext.Storage.createOperationContext('en');
  context := TOperationContext.Create(true, callback, 'Load from '+name);
  try
    req := TFHIRRequest.Create(FServerContext.ValidatorContext.Link, roUpload, FServerContext.Indexes.Compartments.Link);
    try
      req.CommandType := fcmdTransaction;
      if ExtractFileExt(name) = '.xml'  then
        req.resource := TFHIRBundle.wrap(BundleTypeTransaction, TFHIRXmlParser.ParseFile(FServerContext.ValidatorContext.Link, 'en', name))
      else if ExtractFileExt(name) = '.json'  then
        req.resource := TFHIRBundle.wrap(BundleTypeTransaction, TFHIRJsonParser.ParseFile(FServerContext.ValidatorContext.Link, 'en', name))
      else
        req.resource := ProcessZip('en', stream, name, base, init, ini, context, cursor);
      req.resource.tags['duplicates'] := 'ignore';
      req.session := FServerContext.SessionManager.CreateImplicitSession('n/a', ServerContext.OwnerName, 'Service Manager', systemInternal, true, false);
      req.session.allowAll;
      req.LoadParams('');
      req.baseUrl := FServerContext.Bases[0];
      context.message := 'Process '+name;
      resp := TFHIRResponse.Create;
      try
        ProcessRequest(context, req, resp);
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
    context.free;
  end;
end;


function TFhirWebServer.WebDesc: String;
begin
  if (FActualPort = 0) then
    result := 'HTTPS is supported on Port '+inttostr(FStatedSSLPort)+'.'
  else if FActualSSLPort = 0 then
    result := 'HTTP is supported on Port '+inttostr(FStatedPort)+'.'
  else
    result := 'HTTPS is supported on Port '+inttostr(FStatedSSLPort)+'. HTTP is supported on Port '+inttostr(FStatedPort)+'.'
end;

function TFhirWebServer.WebDump: String;
var
  b : TStringBuilder;
  ci : TFHIRWebServerClientInfo;
begin
  b := TStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.append('<tr><td>IP address</td><td>Count</td><td>Session</td><td>Activity</td><td>Length</td></tr>'#13#10);
    FLock.Lock;
    try
      for ci in FClients do
      begin
        b.append('<tr><td>');
        b.append(ci.Context.Binding.PeerIP);
        b.append('</td><td>');
        b.append(inttostr(ci.Count));
        b.append('</td><td>');
        if (ci.Session <> nil) then
          b.append(inttostr(ci.Session.Key));
        b.append('</td><td>');
        b.append(ci.FActivity);
        b.append('</td><td>');
        if ci.FStart > 0 then
          b.append(inttostr(GetTickCount - ci.FStart));
        b.append('</td></tr>'#13#10);
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

procedure TFhirWebServer.ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := SameText(AAuthType, 'Bearer');
  VUserName := INTERNAL_SECRET;
  VPassword := AAuthData;
end;

Procedure TFhirWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  session : TFHIRSession;
  c : integer;
  check, handled : boolean;
  rp : TReverseProxyInfo;
begin
  session := nil;
  MarkEntry(AContext, request, response);
  try
    if (request.AuthUsername = INTERNAL_SECRET) then
      FServerContext.SessionManager.GetSession(request.AuthPassword, session, check);
    if (session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        FServerContext.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1), session, check);
    end;

    if OWinSecurityPlain and (((session = nil) and (request.Document <> FBasePath+OWIN_TOKEN_PATH)) or not ServerContext.UserProvider.allowInsecure) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at '+FBasePath+OWIN_TOKEN_PATH+')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
    else if (request.CommandType = hcOption) then
    begin
      response.ResponseNo := 200;
      response.ContentText := 'ok';
      response.CustomHeaders.add('Access-Control-Allow-Credentials: true');
      response.CustomHeaders.add('Access-Control-Allow-Origin: *');
      response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
      response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
      if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
        response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
    end
    else if OWinSecurityPlain and ServerContext.UserProvider.AllowInsecure and (request.Document = FBasePath+OWIN_TOKEN_PATH) then
      HandleOWinToken(AContext, false, request, response)
    else if FSourceProvider.exists(FSourceProvider.AltFile(request.Document)) then
      ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(request.Document))
    else if request.Document.EndsWith('.hts') and FSourceProvider.exists(ChangeFileExt(FSourceProvider.AltFile(request.Document), '.html')) then
      ReturnProcessedFile(response, session, request.Document, ChangeFileExt(FSourceProvider.AltFile(request.Document), '.html'), false)
  //  else if FSourceProvider.FileExists(FSourcePath+ExtractFileName(request.Document.replace('/', '\'))) then
  //    ReturnSpecFile(response, request.Document, FSourcePath+ExtractFileName(request.Document.replace('/', '\')))
  //  else if FSourceProvider.FileExists(FSpecPath+ExtractFileName(request.Document.replace('/', '\'))) then
  //    ReturnSpecFile(response, request.Document, FSpecPath+ExtractFileName(request.Document.replace('/', '\')))
    else if request.document = FBasePath+'/.well-known/openid-configuration' then
      HandleDiscoveryRedirect(AContext, request, response)
    else if request.document = '/.well-known/openid-configuration' then
      FAuthServer.HandleDiscovery(AContext, request, response)
    else if request.Document.StartsWith(FBasePath+'/cds-services') and FCDSHooksServer.active then
      FCDSHooksServer.HandleRequest(false, FBasePath, session, AContext, request, response)
    else if request.Document.StartsWith(AppendForwardSlash(FBasePath)+'websockets', false) then
      HandleWebSockets(AContext, request, response, false, false, FBasePath)
    else if request.Document.StartsWith(FBasePath, false) then
      HandleRequest(AContext, request, response, false, false, FBasePath, session, nil)
    else if request.Document.StartsWith(AppendForwardSlash(FBasePath)+'FSecurePath', false) then
      HandleWebSockets(AContext, request, response, false, false, FSecurePath)
    else if request.Document.StartsWith(FSecurePath, false) and hasInternalSSLToken(request) then
      HandleRequest(AContext, request, response, true, true, FSecurePath, session, nil)
    else if request.Document = '/diagnostics' then
      ReturnDiagnostics(AContext, request, response, false, false, FSecurePath)
    else if request.Document = '/' then
      ReturnProcessedFile(response, session, '/'+FHomepage, FSourceProvider.AltFile('/'+FHomepage), false)
    else if (FTerminologyWebServer <> nil) and FTerminologyWebServer.handlesRequest(request.Document) then
      FTerminologyWebServer.Process(AContext, request, session, response, false)
    else
    begin
      handled := false;
      for rp in FReverseProxyList do
        if request.Document.StartsWith(rp.path) then
        begin
          ReverseProxy(rp, AContext, request, session, response, false);
          handled := true;
          break;
        end;
      if not handled then
      begin
        response.ResponseNo := 404;
        response.ContentText := 'Document '+request.Document+' not found';
        logt('miss: '+request.Document);
      end;
    end;
  finally
    MarkExit(AContext);
  end;
end;

procedure TFhirWebServer.PopulateConformanceAuth(rest: TFhirCapabilityStatementRest);
var
  c : TFHIRCoding;
  ext : TFhirExtension;
begin
  if rest.security = nil then
    rest.security := TFhirCapabilityStatementRestSecurity.Create;
  rest.security.cors := true;
  if FAuthServer <> nil then
  begin
    c := rest.security.serviceList.Append.codingList.Append;
    c.system := 'http://hl7.org/fhir/restful-security-service';
    c.code := 'SMART-on-FHIR';
    c.display := 'SMART-on-FHIR';
    rest.security.description := 'This server implements OAuth2 for login';

    ext := rest.security.extensionList.Append;
    ext.url := 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris';
//     ext.addExtension('dscovery', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure)+FAuthServer.AuthPath+'/discovery'));
    ext.addExtension('register', TFhirUri.Create('mailto:'+FAdminEmail));
    ext.addExtension('authorize',TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure)+FAuthServer.AuthPath));
    ext.addExtension('token', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure)+FAuthServer.TokenPath));
  end;
end;

procedure TFhirWebServer.PopulateConformance(sender: TObject; conf: TFhirCapabilityStatement);
var
  i : integer;
begin
  for i := 0 to conf.restList.Count - 1 do
    PopulateConformanceAuth(conf.restList[i]);
end;


Procedure TFhirWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  session : TFHIRSession;
  check, handled : boolean;
  c : integer;
  rp : TReverseProxyInfo;
  cert : TIdX509;
  jwt : TJWT;
begin
  check := false;
  cert := (AContext.Connection.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLSocket.PeerCert;

  session := nil;
  MarkEntry(AContext, request, response);
  try
    if (request.AuthUsername = INTERNAL_SECRET) then
      if request.AuthPassword.StartsWith('urn:') then
        FServerContext.SessionManager.GetSession(request.AuthPassword, session, check)
      else
      begin
        jwt := TJWTUtils.unpack(request.AuthPassword, false, nil); // todo: change this to true, and validate the JWT, under the right conditions
        try
          if cert = nil then
            session := FServerContext.SessionManager.getSessionFromJWT(request.RemoteIP, 'Unknown', systemUnknown, jwt)
          else
            session := FServerContext.SessionManager.getSessionFromJWT(request.RemoteIP, cert.CanonicalName, systemFromCertificate, jwt);
        finally
          jwt.free;
        end;
      end;

    if (session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        FServerContext.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1), session, check); // actually, in this place, we ignore check.  we just established the session
    end;

    if request.Document.StartsWith(FAuthServer.Path) then
      FAuthServer.HandleRequest(AContext, request, session, response)
    else if (request.CommandType = hcOption) then
    begin
      response.ResponseNo := 200;
      response.ContentText := 'ok';
      response.CustomHeaders.add('Access-Control-Allow-Credentials: true');
      response.CustomHeaders.add('Access-Control-Allow-Origin: *');
      response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
      response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
      if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
        response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
    end
    else if OWinSecuritySecure and (request.Document = URLPath([FSecurePath, OWIN_TOKEN_PATH])) then
      HandleOWinToken(AContext, true, request, response)
    else if request.document = FBasePath+'/.well-known/openid-configuration' then
      HandleDiscoveryRedirect(AContext, request, response)
    else if request.Document.StartsWith(FSecurePath, false) then
      HandleRequest(AContext, request, response, true, true, FSecurePath, session, cert)
    else if OWinSecuritySecure and ((session = nil) and (request.Document <> URLPath([FSecurePath, OWIN_TOKEN_PATH]))) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at '+FBasePath+OWIN_TOKEN_PATH+')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
    else if FSourceProvider.exists(FSourceProvider.AltFile(request.Document)) then
      ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(request.Document))
    else if request.Document.EndsWith('.hts') and FSourceProvider.exists(ChangeFileExt(FSourceProvider.AltFile(request.Document), '.html')) then
      ReturnProcessedFile(response, session, request.Document, ChangeFileExt(FSourceProvider.AltFile(request.Document), '.html'), true)
  //  else if FSourceProvider.FileExists(IncludeTrailingPathDelimiter(FSourcePath)+request.Document) then
  //    ReturnSpecFile(response, request.Document, IncludeTrailingPathDelimiter(FSourcePath)+request.Document)
  //  else if FSourceProvider.FileExists(FSourceProvider.AltFile(ExtractFileName(request.Document))) then
  //    ReturnSpecFile(response, request.Document, FSourceProvider.AltFile(ExtractFileName(request.Document)))
    else if request.Document.StartsWith('/scim') then
      processSCIMRequest(AContext, request, response)
    else if request.document = '/.well-known/openid-configuration' then
      FAuthServer.HandleDiscovery(AContext, request, response)
  //  else if request.Document.StartsWith(FBasePath, false) then
  //    HandleRequest(AContext, request, response, true, false, FBasePath)
    else if (FTerminologyWebServer <> nil) and FTerminologyWebServer.handlesRequest(request.Document) then
      FTerminologyWebServer.Process(AContext, request, session, response, true)
    else if request.Document.StartsWith(FSecurePath+'/cds-services') and FCDSHooksServer.active then
      FCDSHooksServer.HandleRequest(true, FSecurePath, session, AContext, request, response)
    else if request.Document = '/' then
      ReturnProcessedFile(response, session, '/hompage.html', FSourceProvider.AltFile('/homepage.html'), true)
    else
    begin
      handled := false;
      for rp in FReverseProxyList do
        if request.Document.StartsWith(rp.path) then
        begin
          ReverseProxy(rp, AContext, request, session, response, true);
          handled := true;
          break;
        end;
      if not handled then
      begin
        response.ResponseNo := 404;
        response.ContentText := 'Document '+request.Document+' not found';
        logt('miss: '+request.Document);
      end;
    end;
  finally
    markExit(AContext);
    session.free;
  end;
end;

function processIfModifiedSince(value : String) : TDateTime;
begin
  if value <> '' then
    result := GMTToLocalDateTime(value)
  else
    result := 0;
end;

function processIfMatch(value : string) : String;
begin
  if value.StartsWith('W/') then
    value := value.Substring(2);
  if value.StartsWith('"') and value.EndsWith('"') then
    result := copy(value, 2, length(value)-2)
  else
    result := value;

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
    response.Location := FAuthServer.BasePath+'/discovery';
  end;
end;

procedure TFhirWebServer.HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  pm : TParseMap;
  json : TJsonObject;
  userkey : integer;
  session : TFHIRSession;
begin
  response.ResponseNo := 400;
  response.ResponseText := 'Request Error';
  if request.ContentType <> 'application/x-www-form-urlencoded' then
    response.ContentText := 'Unknown content type - must be application/x-www-form-urlencoded'
  else
  begin
    try
      if request.PostStream <> Nil then
        pm := TParseMap.create(StreamToString(request.PostStream, TEncoding.UTF8))
      else
        pm := TParseMap.create(request.UnparsedParams);
      try
        if pm.GetVar('grant_type') <> 'password' then
          response.ContentText := 'Unknown content type - must be ''password'''
        else if not ServerContext.UserProvider.CheckLogin(pm.GetVar('username'), pm.GetVar('password'), userkey) then
          response.ContentText := 'Unknown usernmame/password'
        else
        begin
          session := FServerContext.SessionManager.CreateImplicitSession(request.RemoteIP, pm.GetVar('username'), 'Anonymous', systemFromOWin, false, true);
          try
            session.ExternalUserKey := userkey;
            json := TJsonObject.Create;
            try
              json.str['access_token'] := session.Cookie;
              json.num['expires_in'] := inttostr(trunc((session.Expires - TDateTimeEx.makeUTC.DateTime) / DATETIME_SECOND_ONE));
              json.str['token_type'] := 'bearer';
              response.ResponseNo := 200;
              response.ResponseText := 'OK';
              response.ContentType := 'application/json';
              response.ContentText := TJSONWriter.writeObjectStr(json, true);
            finally
              json.Free;
            end;
          finally
            session.Free;
          end;
        end;
      finally
        pm.Free;
      end;
    except
      on e:Exception do
        response.ContentText := e.Message;
    end;
  end;
end;

Procedure TFhirWebServer.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String; esession : TFhirSession; cert : TIdX509);
var
  sHost : string;
  oRequest : TFHIRRequest;
  oResponse : TFHIRResponse;
  sCookie : string;
  sContentType : String;
  oStream : TStream;
  sDoc : String;
  s : String;
  aFormat : TFHIRFormat;
  lang : String;
  sPath : String;
  redirect : Boolean;
  form : TMimeMessage;
  relativeReferenceAdjustment : integer;
  pretty : boolean;
  c : integer;
  domain : String;
  sBearer : String;
  noErrCode, upload : boolean;
  context : TOperationContext;
  session : TFhirSession;
Begin
  noErrCode := false;
  upload := false;

  session := nil;
  try
    if ssl then
      sHost := 'https://'+request.Host
    else
      sHost := 'http://'+request.Host;
    domain := request.Host;
    if domain.Contains(':') then
      domain := domain.Substring(0, domain.IndexOf(':'));

    lang := request.AcceptLanguage;
    s := request.ContentType;
    if pos(';', s) > 0 then
      s := copy(s, 1, pos(';', s) - 1); //only read up to the first ';'
    if SameText(s, 'application/x-www-form-urlencoded') or (request.UnparsedParams <> '') then
      sDoc := request.Document +'?'+ request.UnparsedParams
    else
      sDoc := request.Document;
    try
      sContentType := request.ContentType;

    if s.StartsWith('multipart/form-data', true) then
    begin
      form := loadMultipartForm(request.PostStream, request.ContentType, upload);
    end
    else
      form := nil;
    try
      if s.StartsWith('multipart/form-data', true) then
      begin
        oStream := extractFileData(form, 'file', sContentType); // though this might not return the data if we have an operation request
      end
      else if request.PostStream <> nil then
      begin
        oStream := TMemoryStream.create;
        oStream.CopyFrom(request.PostStream, request.PostStream.Size);
        oStream.Position := 0;
      end
      else
        oStream := TStringStream.Create(request.UnparsedParams);

        try
          oResponse := TFHIRResponse.Create;
          try
            response.CustomHeaders.add('Access-Control-Allow-Origin: *');
  //          response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE');
            response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
  //          response.CustomHeaders.add('Access-Control-Expose-Headers: *');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
            if request.RawHeaders.Values['X-Request-Id'] <> '' then
              response.CustomHeaders.add('X-Request-Id: '+request.RawHeaders.Values['X-Request-Id']);
            oRequest := nil;
            Try
              if request.AuthUsername = INTERNAL_SECRET then
                sCookie := request.AuthPassword
              else
              begin
                c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
                if c > -1 then
                  sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1);
              end;

              sBearer := sCookie;
              oRequest := BuildRequest(lang, path, sHost, request.CustomHeaders.Values['Origin'], request.RemoteIP, request.CustomHeaders.Values['content-location'],
                 request.Command, sDoc, sContentType, request.Accept, request.ContentEncoding, sCookie, request.RawHeaders.Values['Provenance'], sBearer,
                 oStream, oResponse, aFormat, redirect, form, secure, ssl, relativeReferenceAdjustment, pretty, esession, cert);
              try
                oRequest.requestId := request.RawHeaders.Values['X-Request-Id'];
                if TFHIRWebServerClientInfo(AContext.Data).Session = nil then
                  TFHIRWebServerClientInfo(AContext.Data).Session := oRequest.Session.Link;

                oRequest.IfMatch := processIfMatch(request.RawHeaders.Values['If-Match']);
                oRequest.IfNoneMatch := processIfMatch(request.RawHeaders.Values['If-None-Match']);
                oRequest.IfNoneExist := request.RawHeaders.Values['If-None-Exist'];
                oRequest.IfModifiedSince := processIfModifiedSince(request.RawHeaders.Values['If-Modified-Since']);
                oRequest.strictSearch := request.RawHeaders.Values['Prefer'] = 'handling=strict';

                noErrCode := StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters.GetVar('nohttperr')) or StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters.GetVar('_nohttperr'));
                ReadTags(request.RawHeaders.Values['Category'], oRequest);
                session := oRequest.Session.Link;
                if redirect then
                begin
                  if oRequest.Session <> nil then
                  begin
                    FAuthServer.setCookie(response, FHIR_COOKIE_NAME, oRequest.Session.Cookie, domain, '', oRequest.Session.Expires, false);
                    cacheResponse(response, cacheNotAtAll);
                    response.Redirect(oRequest.Session.OriginalUrl);
                  end
                  else
                    response.Redirect(oRequest.baseUrl);
                end
                else if oRequest.CommandType = fcmdNull then
                begin
                  response.CustomHeaders.add('Access-Control-Request-Method: GET, POST, PUT, PATCH, DELETE');
                  cacheResponse(response, cacheNormal);
                end
                else if oRequest.CommandType = fcmdUnknown then
                begin
                  cacheResponse(response, oResponse.CacheControl);
                  if oResponse.Format = ffXhtml then
                  begin
                    response.ResponseNo := 200;
                    response.ContentType := 'text/html; charset=UTF-8';
                    response.FreeContentStream := true;
                    response.ContentStream := StringToUTF8Stream(BuildFhirHomePage(oRequest.compartments, lang, sHost, path, oRequest.Session, secure));
                  end
                  else
                  begin
                    response.ResponseNo := 404;
                    response.ContentText := 'Document '+request.Document+' not found';
                  end;
                end
                else if (oRequest.CommandType = fcmdUpload) and (oRequest.Resource = nil) Then
                begin
                  cacheResponse(response, oResponse.CacheControl);
                  response.ResponseNo := 200;
                  response.ContentType := 'text/html; charset=UTF-8';
                  response.FreeContentStream := true;
                  response.ContentStream := StringToUTF8Stream(BuildFhirUploadPage(lang, sHost, '', oRequest.ResourceName, oRequest.Session));
                end
                else if (oRequest.CommandType = fcmdConformanceStmt) and (oRequest.ResourceName <> '') then
                begin
                  cacheResponse(response, oResponse.CacheControl);
                  response.ResponseNo := 200;
                  response.ContentType := 'text/html; charset=UTF-8';
    // no - just use *              response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
                  response.CustomHeaders.add('Access-Control-Request-Method: GET, POST, PUT, PATCH, DELETE');
                  response.FreeContentStream := true;
                  response.ContentStream := StringToUTF8Stream('OK');
                end
                else
                begin
                  try
                    context := TOperationContext.Create;
                    try
                      context.upload := upload;
                      if oRequest.CommandType = fcmdWebUI then
                        HandleWebUIRequest(oRequest, oResponse, secure)
                      else
                        ProcessRequest(context, oRequest, oResponse);
                    finally
                      context.free;
                    end;
                  except
                    on e : EAbort do
                    begin
                      if oResponse.HTTPCode < 300 then
                      begin
                        recordStack(e);
                        raise;
                      end;
                    end;
                    on e : Exception do
                    begin
                      recordStack(e);
                      raise;
                    end;
                  end;
                  cacheResponse(response, oResponse.CacheControl);
                  RecordExchange(oRequest, oResponse);
                  ProcessOutput(oRequest, oResponse, request, response, relativeReferenceAdjustment, pretty, request.AcceptEncoding.Contains('gzip'));
    // no - just use *              if request.RawHeaders.Values['Origin'] <> '' then
    //                 response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
                  if oResponse.versionId <> '' then
                    response.ETag := 'W/"'+oResponse.versionId+'"';
                  response.LastModified := oResponse.lastModifiedDate; // todo: timezone
                  if oResponse.Tags.count > 0 then
                    response.CustomHeaders.add('Category: '+ oResponse.Tags.AsHeader);
                  if oResponse.link_List.count > 0 then
                    response.CustomHeaders.add('Link: '+ oResponse.link_List.AsHeader);
                  if oResponse.originalId <> '' then
                    response.CustomHeaders.add('X-Original-Location: '+oResponse.originalId);
                  if oResponse.ContentLocation <> '' then
                    response.CustomHeaders.add('Content-Location: '+oResponse.ContentLocation);
                  if oResponse.Location <> '' then
                    response.Location := oResponse.Location;
                end;
                if noErrCode then
                  response.ResponseNo := 200;
                response.WriteContent;
                RecordExchange(oRequest, oResponse);
              except
                on e : Exception do
                begin
                  RecordExchange(oRequest, oResponse, e);
                  raise;
                end;
              end;
            finally
              oRequest.Free;
            end;
          Finally
           oResponse.free;
          End;
        finally
          oStream.free;
        end;
      finally
        form.Free;
      end;
    except
      on e : ERestfulAuthenticationNeeded do
      begin
        if aFormat = ffXhtml then
        begin
          response.ResponseNo := 200;
          response.ContentType := 'text/html; charset=UTF-8';
          response.FreeContentStream := true;
          response.ContentStream := StringToUTF8Stream(BuildFhirAuthenticationPage(lang, sHost, sPath + sDoc, e.Msg, ssl));
        end
        else
          SendError(response, e.Status, aFormat, lang, e.Message, sPath, e, session, true, sPath + sDoc, relativeReferenceAdjustment, IssueTypeLogin);
      end;
      on e : ETerminologyError do
      begin
        if noErrCode then
          SendError(response, 200, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported)
        else
          SendError(response, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported);
      end;
      on e : ETerminologySetup do
      begin
        if noErrCode then
          SendError(response, 200, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported)
        else
          SendError(response, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNotSupported);
      end;
      on e : ETooCostly do
      begin
        if noErrCode then
          SendError(response, 200, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeTooCostly)
        else
          SendError(response, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeTooCostly);
      end;
      on e: ERestfulException do
      begin
        if noErrCode then
          SendError(response, 200, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, e.Code)
        else
          SendError(response, e.Status, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, e.Code);
      end;
      on e: Exception do
      begin
        if noErrCode then
          SendError(response, 200, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNull)
        else
          SendError(response, HTTP_ERR_INTERNAL, aFormat, lang, e.Message, sPath, e, session, false, path, relativeReferenceAdjustment, IssueTypeNull);
      end;
    end;
  finally
    session.free;
  end;
end;

function TFhirWebServer.HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  profile : TFhirStructureDefinition;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
  s, id, fid : String;
begin
   // get the right questionnaire
  if request.Parameters.GetVar('profile').StartsWith('Profile/') then
  begin
    id := request.Parameters.GetVar('profile').Substring(8);
    profile := GetResource(request.Session, 'StructureDefinition', request.Lang, id, '','') as TFHirStructureDefinition;
  end
  else
    profile := FindResource(request.Session, 'StructureDefinition', request.Lang, 'url='+request.Parameters.GetVar('profile')) as TFHirStructureDefinition;
  try
    id := profile.id;
    fid := request.baseUrl+'StructureDefinition/'+id+'/$questionnaire';
    s := FServerContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
    if s = '' then
    begin
      builder := TQuestionnaireBuilder.Create(request.Lang);
      try
        questionnaire := FServerContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
        try
          if questionnaire = nil then
          begin
            builder.Profile := profile.Link;
            builder.OnExpand := FServerContext.Storage.ExpandVS;
            builder.onLookupCode := FServerContext.Storage.LookupCode;
            builder.QuestionnaireId := fid;
            builder.onLookupReference := LookupReference;
            builder.Context := request.Link;

            builder.build;
            questionnaire := builder.Questionnaire.Link;
            FServerContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
          end;
          // convert to xhtml
          s := transform1(questionnaire, request.Lang, 'QuestionnaireToHTML.xslt', true);
          FServerContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
        finally
          questionnaire.Free;
        end;
        // insert page headers:
        s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
        s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang, SERVER_VERSION)+
          '<p><a href="'+builder.QuestionnaireId+'">Questionnaire for this form</a>.'+
          ' The QuestionnaireAnswers should be submitted as a POST to <i>'+request.baseUrl+'$qa-post</i> with a questionnaire reference of <a href="'+builder.QuestionnaireId+'">'+builder.QuestionnaireId+'</a></p>'#13#10);
        s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, false));
        s := s.replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "'+request.baseUrl+'$qa-post";');
      finally
        builder.free;
      end;
    end;

    response.body := s;
    result := Now; //don't want anyone caching anything
    response.ContentType := 'text/html; charset=UTF-8';
  finally
    profile.free;
  end;
end;

function TFhirWebServer.HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  typ, id, ver : String;
  r : TFHIRResource;
  s : String;
  comp : TFHIRComposer;
begin
  result := 0;

   // get the right questionnaire
  StringSplit(request.Id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  if not (StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.ValidatorContext.hasCustomResource(typ)) then
    raise Exception.Create('Unknown resource type '+typ);

  r := GetResource(request.Session, typ, request.Lang, id, '', '');
  try
    if r is TFhirOperationOutcome then
    begin
      response.Resource := r.Link;
      response.HTTPCode := 500;
      response.Message := 'Internal Error';
    end
    else
    begin

      if request.Parameters.GetVar('srcformat') = 'json' then
        comp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.Link, request.Lang)
      else
        comp := TFHIRXMLComposer.Create(FServerContext.Validator.Context.Link, request.Lang);
      try
        s := comp.Compose(r, true, nil);
      finally
        comp.Free;
      end;

      s :=
        '<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
        '<!DOCTYPE HTML "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
        ''+#13#10+
        '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
        '<head>'+#13#10+
        '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
        '    <title>Direct Edit for /'+typ+'/'+id+'</title>'+#13#10+
        TFHIRXhtmlComposer.PageLinks+
        FHIR_JS+
        '</head>'+#13#10+
        ''+#13#10+
        '<body>'+#13#10+
        ''+#13#10+
        TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang, SERVER_VERSION)+
        '<h2>Direct Edit for '+request.Id+'</h2>'+#13#10+
        '<form action="'+request.baseUrl+'_web/'+typ+'/'+id+'/$post" method="POST">'#13#10+
        '  <input type="hidden" name="srcformat" value="'+request.Parameters.GetVar('srcformat')+'"/>'#13#10+
        '  <textarea cols="80" rows="24" name="source" style="white-space:pre">'+FormatXMLForTextArea(s)+#13#10'</textarea><br/>'#13#10+
        '  <input type="submit" value="Save"/>'#13#10+
        '</form>'#13#10+
        TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, true);

      response.body := s;
      result := Now; //don't want anyone caching anything
      response.ContentType := 'text/html; charset=UTF-8';
    end;
  finally
    r.free;
  end;
end;

function TFhirWebServer.HandleWebPost(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  s, typ, id, ver : String;
  p : TParseMap;
  prsr : TFHIRParser;
  context : TOperationContext;
begin
  StringSplit(request.Id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  request.Id := Id;
  if not (StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.validatorContext.hasCustomResource(typ)) then
    raise Exception.Create('Unknown resource type '+typ);
  request.ResourceName := typ;
  request.CommandType := fcmdUpdate;

  context := TOperationContext.Create;
  try
    p := TParseMap.create(TEncoding.UTF8.GetString(request.Source.AsBytes), true);
    try
      if p.GetVar('srcformat') = 'json' then
        prsr := TFHIRJsonParser.Create(request.context.link, request.Lang)
      else
        prsr := TFHIRXMLParser.Create(request.context.link, request.Lang);
      try
        s := p.GetVar('source');
        prsr.source := TStringStream.Create(s, TEncoding.UTF8);
        try
          prsr.Parse;
          request.Resource := prsr.resource.Link;
          ProcessRequest(context, request, response);
          if response.HTTPCode < 300 then
          begin
            response.HTTPCode := 303;
            response.Location := request.baseUrl+typ+'/'+id;
          end;
        finally
          prsr.source.Free;
        end;
      finally
        prsr.Free;
      end;
    finally
      p.Free;
    end;
    result := 0;
  finally
    context.Free;
  end;
end;

function TFhirWebServer.HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  id, ver, fullid : String;
  profile : TFHirStructureDefinition;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
  s : String;
begin
   // get the right questionnaire
  StringSplit(request.Id.Substring(8), '/', id, ver);
  profile := GetResource(request.Session, 'StructureDefinition', request.Lang, id, ver, '') as TFHirStructureDefinition;
  try
    fullid := request.baseUrl+'StructureDefinition/'+id+'/$questionnaire';
    s := FServerContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
    if s = '' then
    begin
      builder := TQuestionnaireBuilder.Create(request.Lang);
      try
        questionnaire := FServerContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
        try
          if questionnaire = nil then
          begin
            builder.Profile := profile.Link;
            builder.OnExpand := FServerContext.Storage.ExpandVS;
            builder.onLookupCode := FServerContext.Storage.LookupCode;
            builder.onLookupReference := LookupReference;
            builder.Context := request.Link;
            builder.QuestionnaireId := fullid;
            builder.build;
            questionnaire := builder.Questionnaire.Link;
            FServerContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
          end;
          // convert to xhtml
          s := transform1(questionnaire, request.Lang, 'QuestionnaireToHTML.xslt', true);
          FServerContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
        finally
          questionnaire.Free;
        end;
      finally
        builder.free;
      end;
    end;
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang, SERVER_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, false));
    s := s.replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "'+request.baseUrl+'$qa-post";');
    response.body := s;
    result := Now; //don't want anyone caching anything
    response.ContentType := 'text/html; charset=UTF-8';
  finally
    profile.free;
  end;
end;

function TFhirWebServer.HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure : boolean) : TDateTime;
var
  id, ver : String;
  s, xhtml : String;
  patient : TFHIRPatient;
  hookid : String;
  hooks : TFHIRWebServerPatientViewContext;
begin
  result := 0;
  StringSplit(request.Id.Substring(8), '/', id, ver);
  hookid := NewGuidId;
  hooks := TFHIRWebServerPatientViewContext.Create;
  hooks.manager := TCDSHooksManager.Create;
  FLock.Lock;
  try
    FPatientHooks.Add(hookid, hooks);
  finally
    FLock.Unlock;
  end;

  patient := GetResource(request.Session, 'Patient', request.Lang, id, ver, '') as TFHIRPatient;
  try
    xhtml := patient.text.div_.AsPlainText;
    startHooks(hooks, patient, request.baseUrl);
  finally
    patient.Free;
  end;


  s := FSourceProvider.getSource('patient.html');
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%hookid%]', hookid, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%patient-details%]', xhtml, [rfReplaceAll]);
  s := s.Replace('[%patient-id%]', id, [rfReplaceAll]);
  s := s.Replace('[%admin%]', FAdminEmail, [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%host%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', FHost+':'+inttostr(FStatedPort), [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%securehost%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', FHost+':'+inttostr(FStatedSSLPort), [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%baseOpen%]', FHost+FBasePath, [rfReplaceAll])
  else
    s := s.Replace('[%baseOpen%]', FHost+':'+inttostr(FStatedPort)+FBasePath, [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%baseSecure%]', FHost+FSecurePath, [rfReplaceAll])
  else
    s := s.Replace('[%baseSecure%]', FHost+':'+inttostr(FStatedSSLPort)+FSecurePath, [rfReplaceAll]);
  if request.secure then
    s := s.Replace('[%root%]', FSecurePath, [rfReplaceAll])
  else
    s := s.Replace('[%root%]', FBasePath, [rfReplaceAll]);

  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);

  response.Body := s;
  response.ContentType := 'text/html; charset=UTF-8';
end;

function TFhirWebServer.HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id : String;
//  s, xhtml : String;
//  patient : TFHIRPatient;
//  hookid : String;
  hooks : TFHIRWebServerPatientViewContext;
begin
  result := 0;

  id := request.Id.Substring(13);
  FLock.Lock;
  try
    if FPatientHooks.TryGetValue(id, hooks) then
      hooks.Link
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
        response.Body := presentAsHtml(hooks.Cards, nil, hooks.Errors);
        FPatientHooks.Remove(id);
      finally
        FLock.Unlock;
      end;
      response.HTTPCode := 200;
      response.ContentType := 'text/html; charset=UTF-8';
    finally
      hooks.Free;
    end;
  end;
end;

function TFhirWebServer.HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  id, ver : String;
  questionnaire : TFHIRQuestionnaire;
  s : String;
begin
   // get the right questionnaire
  StringSplit(request.Id.Substring(14), '/', id, ver);
  questionnaire := GetResource(request.Session, 'Questionnaire', request.Lang, id, ver, '') as TFHirQuestionnaire;
  try
    // convert to xhtml
    s := transform1(questionnaire, request.Lang, 'QuestionnaireToHTML.xslt', false);
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang, SERVER_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, false));
    s := s.replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "'+request.baseUrl+'/QuestionnaireAnswers";');
    response.body := s;
    result := Now; //don't want anyone caching anything
    response.ContentType := 'text/html; charset=UTF-8';
  finally
    questionnaire.free;
  end;
end;

function TFhirWebServer.HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse) : TDateTime;
var
  typ, id, ver : String;
  r : TFHIRResource;
  qa : TFhirQuestionnaireResponse;
  q : TFhirQuestionnaire;
  s, j : String;
  json : TFHIRJsonComposer;
begin
  result := 0;

   // get the right questionnaire
  StringSplit(request.Id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  if not (StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FServerContext.validatorContext.hasCustomResource(typ))  then
    raise Exception.Create('Unknown resource type '+typ);

  r := GetResource(request.Session, typ, request.Lang, id, ver, 'qa-edit');
  try
    if r is TFhirOperationOutcome then
    begin
      response.Resource := r.Link;
      response.HTTPCode := 500;
      response.Message := 'Internal Error';
    end
    else
    begin
      qa := r as TFhirQuestionnaireResponse;
      q := (FindContainedResource(qa, qa.questionnaire) as TFhirQuestionnaire).link;
      if q = nil then
        raise Exception.Create('Unable to fetch Questionnaire "'+qa.questionnaire.reference.Substring(1)+'"');

      // convert to xhtml
      s := transform1(q, request.Lang, 'QuestionnaireToHTML.xslt', true);

      // make clean qa
      qa.questionnaire.reference := 'Questionnaire/'+qa.questionnaire.reference.Substring(1);
      qa.containedList.Clear;
      json := TFHIRJsonComposer.Create(request.context.link, request.Lang);
      try
        j := json.Compose(qa, false, nil);
      finally
        json.Free;
      end;

      // insert page headers:
      s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
      s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang, SERVER_VERSION));
      s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, false));
      // insert the answer:
      s := s.Replace('var QuestionnaireResponse=null;', 'var QuestionnaireResponse='+j+';');
      response.body := s;
      result := Now; //don't want anyone caching anything
      response.ContentType := 'text/html; charset=UTF-8';
    end;
  finally
    r.free;
  end;
end;

procedure TFhirWebServer.HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: Boolean; path: String);
var
  ws : TIdWebSocket;
begin
  ws := TIdWebSocket.Create(nil);
  try
    if ws.open(aContext, request, response) then
      ServerContext.SubscriptionManager.HandleWebSocket(ws);
  finally
    ws.Free;
  end;
end;

function TFhirWebServer.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure : boolean) : TDateTime;
begin
  if request.Id.EndsWith('$qa-edit') then
    result := HandleWebQuestionnaireInstance(request, response)
  else if request.Id.EndsWith('$edit') then
    result := HandleWebEdit(request, response)
  else if request.Id.EndsWith('$post') then
    result := HandleWebPost(request, response)
  else if request.Id.StartsWith('Questionnaire/') then
    result := HandleWebQuestionnaire(request, response)
  else if request.Id.StartsWith('StructureDefinition/') then
    result := HandleWebProfile(request, response)
  else if request.Id.StartsWith('Patient/') then
    result := HandleWebPatient(request, response, secure)
  else if request.Id.StartsWith('PatientHooks/') then
    result := HandleWebPatientHooks(request, response, secure)
  else if request.Id ='Create' then
    result := HandleWebCreate(request, response)
  else
    raise Exception.Create('Unknown request: '+request.Id);
end;


function TFhirWebServer.hasInternalSSLToken(request: TIdHTTPRequestInfo): boolean;
begin
  result := request.RawHeaders.Values[SECURE_TOKEN_HEADER] = FSecureToken;
end;

procedure TFhirWebServer.SendError(response: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; e : Exception; session : TFhirSession; addLogins : boolean; path : String; relativeReferenceAdjustment : integer; code : TFhirIssueTypeEnum);
var
  issue : TFhirOperationOutcome;
  report :  TFhirOperationOutcomeIssue;
  oComp : TFHIRComposer;
  ext : TFhirExtension;
  d: String;
begin
  response.ResponseNo := status;
  response.FreeContentStream := true;
  if format = ffUnspecified then
  begin
    response.ContentType := 'text/plain';
    response.ContentStream := StringToUTF8Stream(message);
  end
  else
  begin
    issue := TFhirOperationOutcome.create;
    try
      issue.text := TFhirNarrative.create;
      issue.text.status := NarrativeStatusGenerated;
      issue.text.div_ := TFHIRXhtmlParser.parse(lang, xppReject, [], '<div><p>'+FormatTextToXML(message)+'</p></div>');
      if addLogins then
      begin
        if FAuthServer.HL7Appid <> '' then
        begin
          ext := issue.ExtensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.value := TFhirString.create('http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FAuthServer.HL7Appid+'&returnURL='+EncodeMime(path)+'/state/'+FAuthServer.MakeLoginToken(path, apHL7));
        end;
        if FAuthServer.FacebookAppid <> '' then
        begin
          ext := issue.ExtensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.value := TFhirString.create('https://www.facebook.com/dialog/oauth?client_id='+FAuthServer.FacebookAppid+'&redirect_uri='+path+'&state='+FAuthServer.MakeLoginToken(path, apFacebook));
        end;
        if FAuthServer.GoogleAppid <> '' then
        begin
          ext := issue.ExtensionList.Append;
          ext.url := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          ext.value := TFhirString.create('https://accounts.google.com/o/oauth2/auth?client_id='+FAuthServer.GoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+path+'&state='+FAuthServer.MakeLoginToken(path, apGoogle));
        end;
      end;
      report := issue.issueList.Append;
      report.severity := IssueSeverityError;
      report.details := TFhirCodeableConcept.Create;
      report.details.text := message;
      d := ExceptionStack(e);
      if d <> '' then
      begin  (*
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
        end;*)
        report.diagnostics := d;
      end;
      report.code := code;
      response.ContentStream := TMemoryStream.Create;
      oComp := nil;
      case format of
        ffXml: oComp := TFHIRXmlComposer.Create(FServerContext.Validator.Context.Link, lang);
        ffXhtml:
          begin
          oComp := TFHIRXhtmlComposer.Create(FServerContext.Validator.Context.Link, lang);
          TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(url);
          TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
          TFHIRXhtmlComposer(oComp).Session := Session.Link;
          TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          end;
        ffJson: oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.Link, lang);
        ffText: oComp := TFHIRTextComposer.Create(FServerContext.Validator.Context.Link, lang);
      end;
      try
        response.ContentType := oComp.MimeType;
        oComp.Compose(response.ContentStream, issue, false, nil);
        response.ContentStream.Position := 0;
      finally
        oComp.free;
      end;
    finally
      issue.free;
    end;
  end;
  response.WriteContent;
end;

procedure TFhirWebServer.SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
begin
  FSourceProvider.Free;
  FSourceProvider := Value;
end;

function extractProp(contentType, name : String) : string;
begin
  if contentType.Contains(name+'=') then
  begin
    result := contentType.Substring(contentType.IndexOf(name+'=')+name.Length+1);
    if result.Contains(';') then
      result := result.Substring(0, result.indexOf(';'));
  end
  else
    result := '';
end;


Function TFhirWebServer.BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sContentEncoding, sCookie, provenance, sBearer : String; oPostStream : TStream; oResponse : TFHIRResponse; var aFormat : TFHIRFormat;
   var redirect : boolean; form : TMimeMessage; bAuth, secure : Boolean; out relativeReferenceAdjustment : integer; var pretty : boolean; session : TFhirSession; cert : TIdX509) : TFHIRRequest;
Var
  sURL, msg : String;
  oRequest : TFHIRRequest;
  parser : TFHIRParser;
  check : boolean;
  comp : TIdCompressorZLib;
  mem : TMemoryStream;
  cursor : integer;
  bundle : TFHIRBundle;
Begin
  relativeReferenceAdjustment := 0;
  Result := nil;
  oRequest := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roRest, FServerContext.Indexes.Compartments.Link);
  try
    oRequest.Lang := lang;
    oResponse.origin := sOrigin;
    oRequest.PostFormat := ffUnspecified;
    oResponse.Format := ffUnspecified;
    oRequest.secure := secure;
    aFormat := ffUnspecified;
    oRequest.baseUrl := sHost + AppendForwardSlash(sBaseURL);
    oRequest.url := sHost + sResource;
    oRequest.lastModifiedDate := 0; // Xml
//    oRequest.contentLocation := sContentLocation; // for version aware updates
    oRequest.form := form.link;
    oRequest.Provenance := processProvenanceHeader(provenance, lang);

    If Not StringStartsWithSensitive(sResource, sBaseURL) Then
    begin
      if StringStartsWith(sResource, '/images/', false) then
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', 'images not served', HTTP_ERR_NOTFOUND, IssueTypeNotFound)
      else
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_NO_MODULE', lang), [sResource]), HTTP_ERR_NOTFOUND, IssueTypeNotFound);
    end;

    sURL := Copy(sResource, length(sBaseURL)+1, $FFF);
    sUrl := oRequest.preanalyse(sUrl);

    if (sCommand <> 'GET') then
    begin
      if oRequest.Parameters.VarExists('_format') and (form = nil) and (oRequest.Parameters.GetVar('_format') <> '') then
        sContentType := oRequest.Parameters.GetVar('_format');
      if StringStartsWithInsensitive(sContentType, 'application/json') or StringStartsWithInsensitive(sContentType, 'application/fhir+json') or StringStartsWithInsensitive(sContentType, 'application/json+fhir') or StringStartsWithInsensitive(sContentType, 'json') or StringStartsWithInsensitive(sContentType, 'text/json') Then
        oRequest.PostFormat := ffJson
      else if StringStartsWithInsensitive(sContentType, 'text/html') or StringStartsWithInsensitive(sContentType, 'html') or StringStartsWithInsensitive(sContentType, 'application/x-zip-compressed') or StringStartsWithInsensitive(sContentType, 'application/zip') Then
        oRequest.PostFormat := ffXhtml
      else if StringStartsWithInsensitive(sContentType, 'text/fhir') Then
        oRequest.PostFormat := ffText
      else if StringStartsWithInsensitive(sContentType, 'text/turtle') Then
        oRequest.PostFormat := ffTurtle
      else if StringStartsWithInsensitive(sContentType, 'text/xml') or StringStartsWithInsensitive(sContentType, 'application/xml') or
          StringStartsWithInsensitive(sContentType, 'application/fhir+xml') or StringStartsWithInsensitive(sContentType, 'application/xml+fhir') or StringStartsWithInsensitive(sContentType, 'xml') Then
        oRequest.PostFormat := ffXML;
    end;

    if oRequest.Parameters.VarExists('_format') and (oRequest.Parameters.GetVar('_format') <> '') then
      sContentAccept := oRequest.Parameters.GetVar('_format');
    if StringExistsSensitive(sContentAccept, 'application/json') or StringExistsSensitive(sContentAccept, 'application/fhir+json') or StringExistsInsensitive(sContentAccept, 'json') Then
      oResponse.Format := ffJson
    else if StringExistsSensitive(sContentAccept, 'text/xml') Then
      oResponse.Format := ffXML
    else if StringExistsSensitive(sContentAccept, 'text/html') Then
      oResponse.Format := ffXhtml
    else if StringExistsSensitive(sContentAccept, 'text/fhir') Then
      oResponse.Format := ffText
    else if StringExistsSensitive(sContentAccept, 'text/turtle') Then
      oResponse.Format := ffTurtle
    else if StringExistsSensitive(sContentAccept, 'application/xml') Then
      oResponse.Format := ffXML
    else if StringExistsSensitive(sContentAccept, 'application/fhir+xml') Then
      oResponse.Format := ffXML
    else if StringExistsSensitive(sContentAccept, '*/*') Then // specially for stupid IE...
      oResponse.Format := ffXhtml
    else if StringExistsInsensitive(sContentAccept, 'html') Then
      oResponse.Format := ffXhtml
    else if StringExistsInsensitive(sContentAccept, 'xml') Then
      oResponse.Format := ffXML
    else if StringExistsInsensitive(sContentAccept, 'text') Then
      oResponse.Format := fftext
    else if StringExistsInsensitive(sContentAccept, 'rdf') Then
      oResponse.Format := ffTurtle
    else if oRequest.PostFormat <> ffUnspecified then
      oResponse.Format := oRequest.PostFormat;

    if oRequest.Parameters.VarExists('_pretty') then
      pretty := oRequest.Parameters.GetVar('_pretty') = 'true'
    else if sContentAccept.Contains('pretty=') then
      pretty := extractProp(sContentAccept, 'pretty') = 'true'
    else
      pretty := false;

    aFormat := oResponse.Format;

    // ok, now that we've read the content types, security check
    if bAuth and not (sURL = 'metadata') then
    begin
      if sUrl = 'logout' then
      begin
        FServerContext.SessionManager.EndSession(sCookie, sClient);
        oRequest.session := nil;
        redirect := true;
      end
      else if (session <> nil) and FServerContext.SessionManager.isOkSession(session) then
        oRequest.session := session.Link
      else if (sURL = 'internal') then
        redirect := true
      else if (sUrl <> 'auth-login') and FServerContext.SessionManager.GetSession(sCookie, session, check) then
      begin
        if check and not CheckSessionOK(session, sClient) then
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
        oRequest.session := session
      end
      else if (secure and FServerContext.SessionManager.isOkBearer(sBearer, sClient, Session)) then
        oRequest.session := session
      else
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
    end
    else if cert <> nil then
      oRequest.session := FServerContext.SessionManager.CreateImplicitSession(sClient, cert.CanonicalName, 'Anonymous', systemFromCertificate, false, false)
    else
      oRequest.session := FServerContext.SessionManager.CreateImplicitSession(sClient, 'Unknown', 'Anonymous', systemUnknown, false, false);

    if not redirect then
    begin
      oRequest.analyse(sCommand, sURL, relativeReferenceAdjustment, FAdaptors);

      if (oRequest.CommandType <> fcmdNull)  then
      begin

        if (oRequest.Session <> nil) and (oRequest.Session.User <> nil) and (oRequest.Session.PatientList.Count > 0) then
          oRequest.compartments := oRequest.Session.BuildCompartmentList;

        if (oRequest.CommandType in [fcmdTransaction, fcmdBatch, fcmdUpdate, fcmdPatch, fcmdValidate, fcmdCreate]) or ((oRequest.CommandType in [fcmdUpload, fcmdSearch, fcmdWebUI, fcmdOperation]) and (sCommand = 'POST') and (oPostStream <> nil) and (oPostStream.Size > 0))
          or ((oRequest.CommandType in [fcmdDelete]) and ((sCommand = 'DELETE')) and (oPostStream <> nil) and (oPostStream.size > 0) and (sContentType <> '')) Then
        begin
          oRequest.CopyPost(oPostStream);
          if (sContentType = 'application/x-zip-compressed') or (sContentType = 'application/zip') then
            oRequest.resource := ProcessZip(lang, oPostStream, NewGuidURN, 'http://hl7.org/fhir', false, nil, nil,cursor)
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
                comp.free;
                mem.free;
              end;
            end
            else
              oRequest.Source.LoadFromStream(oPostStream);

            oPostStream.Position := 0;
            if oRequest.ResourceEnum = frtBinary then
            begin
              oRequest.Resource := TFhirBinary.create;
//!              TFhirBinary(oRequest.Resource).Content.loadFromStream(oPostStream);
//!              TFhirBinary(oRequest.Resource).ContentType := sContentType;
            end
            else if (oRequest.Adaptor <> nil) then
              oRequest.Adaptor.Load(oRequest, oPostStream)
            else if (oRequest.CommandType = fcmdPatch) and (sContentType = 'application/json-patch+json') then
            begin
              oRequest.patchJson := TJsonParser.ParseNode(oPostStream) as TJsonArray
            end
            else if (oRequest.CommandType = fcmdPatch) and (sContentType = 'application/xml-patch+xml') then
            begin
              oRequest.patchXml := TMXmlParser.parse(oPostStream, [xpResolveNamespaces]);
            end
            else if (oRequest.CommandType = fcmdOperation) and (sContentType = 'application/x-www-form-urlencoded') then
            begin
              oRequest.Resource := parseParamsFromForm(oPostStream);
            end
            else if (oRequest.CommandType = fcmdOperation) and (sContentType = 'application/graphql') then
            begin
              oRequest.GraphQL := TGraphQLParser.parse(oPostStream);
            end
            else if (oRequest.CommandType = fcmdOperation) and (oRequest.OperationName = 'graphql') and (sContentType = 'application/json') then
            begin
              oRequest.GraphQL := TGraphQLParser.parseJson(oPostStream);
            end
            else if oRequest.CommandType <> fcmdWebUI then
              try
                parser := MakeParser(FServerContext.Validator.Context, lang, oRequest.PostFormat, oPostStream, xppReject);
                try
                  oRequest.Resource := parser.resource.Link;
                  if oRequest.PostFormat = ffUnspecified then
                    oRequest.PostFormat := parser.Format;

                  if (oRequest.CommandType = fcmdTransaction) and not (oRequest.resource is TFHIRBundle) then
                  begin
                    bundle := TFHIRBundle.create(BundleTypeTransactionResponse);
//                    oRequest.Feed.base := oRequest.baseUrl;
                    bundle.entryList.add(TFHIRBundleEntry.create);
                    bundle.entryList[0].resource := oRequest.Resource.link;
                    bundle.entryList[0].resource.id := FhirGUIDToString(CreateGUID);
                    oRequest.resource := bundle;
                  end;
                finally
                  parser.free;
                end;
              except
                on e : Exception do
                  if oRequest.CommandType = fcmdValidate then
                    oResponse.Message := e.Message
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

    result := oRequest.Link;
  Finally
    oRequest.Free;
  End;
End;

procedure TFhirWebServer.cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
begin
  case caching of
    cacheNotAtAll: response.CacheControl := 'no-cache, no-store, must-revalidate';
    cacheAsException: response.CacheControl := 'public, max-age=600, error';
    cacheNormal: response.CacheControl := 'public, max-age=600';
    cacheLong: response.CacheControl := 'public, max-age=31536000';
  end;
end;

Function TFhirWebServer.ProcessZip(lang : String; oStream : TStream; name, base : String; init : boolean; ini : TFHIRServerIniFile; context : TOperationContext; var cursor : integer) : TFHIRBundle;
var
  rdr : TAdvZipReader;
  p : TFHIRParser;
  i, k : integer;
  s : TAdvVCLStream;
  e : TFHIRBundleEntry;
  bnd : TFHIRBundle;
  inc : TStringList;
  istart, iend : integer;
  function ok(res : TFHIRResource) : boolean;
  begin
    result := (inc.Count = 0) or (inc.IndexOf(CODES_TFHIRResourceType[res.ResourceType]) > -1);
  end;
begin
  inc := TStringList.Create;
  result := TFHIRBundle.Create(BundleTypeTransaction);
  try
    if init and (ini <> nil) then
      inc.CommaText := ini.ReadString(voVersioningNotApplicable, 'control', 'include', '');
    result.id := NewGuidURN;
//    result.base := base;
    rdr := carry.link as TAdvZipReader;
    try
      if (rdr = nil) or (name <> carryName) then
      begin
        rdr.Free;
        carry.free;
        rdr := TAdvZipReader.Create;
        s := TAdvVCLStream.Create;
        s.Stream := oStream;
        rdr.Stream := s;
        rdr.Parts := TAdvZipPartList.Create;
        rdr.ReadZip;
        carry := rdr.Link as TAdvZipReader;
        carryName := name;
      end;

      if (init) or (ini = nil) then
      begin
        iStart := 0;
        iEnd := rdr.Parts.Count - 1;
      end
      else
      begin
        iStart := ini.ReadInteger(voVersioningNotApplicable, 'process', 'start', 0);
        iEnd := iStart + 1000;
        if iEnd > rdr.Parts.Count - 1 then
          iEnd := rdr.Parts.Count - 1;
      end;

      for i := iStart to iEnd Do
      begin
        if context <> nil then
          context.progress(trunc(100 * (i - iStart) / (iEnd - iStart)));
        writeln('Parse '+rdr.Parts[i].name);
        if rdr.Parts[i].name.EndsWith('.json') then
          p := TFHIRJsonParser.create(FServerContext.Validator.Context.Link, lang)
        else if rdr.Parts[i].name.EndsWith('.map') then
          p := TFHIRTextParser.create(FServerContext.Validator.Context.Link, lang)
        else
          p := TFHIRXmlParser.create(FServerContext.Validator.Context.Link, lang);
        try
          p.source := TBytesStream.create(rdr.parts[i].AsBytes);
          p.AllowUnknownContent := true;
          p.Parse;
          if  p.resource is TFhirBundle then
          begin
            bnd := TFhirBundle(p.resource);
            case bnd.type_ of
              BundleTypeDocument, BundleTypeMessage, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection :
                 for k := 0 to bnd.entryList.Count - 1 do
                   if ok(bnd.entryList[k].resource) then
                     result.entryList.Add(bnd.entryList[k].link);
              BundleTypeTransaction, BundleTypeTransactionResponse : ; // we ignore these for now
            end;
          end
          else if not (p.resource is TFhirParameters) and ok(p.resource) then
          begin
            e := TFHIRBundleEntry.create;
            try
              e.resource := p.resource.Link;
              result.entryList.add(e.Link);
            finally
              e.free;
            end;
          end;
        finally
          p.source.free;
          p.free;
        end;
      end;
      if iEnd < rdr.Parts.Count - 1 then
        cursor := iEnd + 1
      else
        cursor := -1;
    finally
      rdr.free;
    end;
    result.Link;
  finally
    result.Free;
    inc.Free;
  end;
end;


procedure TFhirWebServer.SSLPassword(var Password: String);
begin
  Password := FSSLPassword;
end;

Procedure TFhirWebServer.ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; relativeReferenceAdjustment : integer; pretty, gzip : boolean);
var
  oComp : TFHIRComposer;
  b : TBytes;
  stream : TMemoryStream;
  ownsStream : boolean;
  comp : TIdCompressorZLib;
  body : boolean;
  res : TFHIRResource;
begin
  ownsStream := false;
  gzip := false;
  response.ResponseNo := oResponse.HTTPCode;
  response.ContentType := oResponse.ContentType;
  res := oResponse.resource;
  if (res = nil) then
    res := oResponse.outcome;
  body := (request.Command = 'GET') or (request.RawHeaders.Values['Prefer'] <> 'return=minimal') or (oResponse.Format = ffXhtml);
  if body and (request.RawHeaders.Values['Prefer'] = 'return=OperationOutcome') and (oResponse.outcome <> nil) then
    res := oResponse.outcome;

  stream := TMemoryStream.create;
  try
    ownsStream := true;
    if res <> nil then
    Begin
      if body then
      begin
        if res is TFhirBinary then
        begin
          if (Length(TFhirBinary(res).content) > 0) and (body) then
            stream.Write(TFhirBinary(res).content[0], Length(TFhirBinary(res).content));
          stream.Position := 0;
          response.ContentType := TFhirBinary(res).ContentType;
          if StrToBoolDef(orequest.Parameters.GetVar('no-attachment'), false) then
            response.ContentDisposition := 'attachment;';
          response.Expires := Now + 0.25;
        end
        else if (oRequest.Adaptor <> nil) then
        begin
          oRequest.Adaptor.compose(oResponse, stream);
          response.ContentType := oRequest.Adaptor.MimeType;
        end
        else
        begin
  //        response.Expires := Now; //don't want anyone caching anything
          response.Pragma := 'no-cache';
          if oResponse.Format = ffJson then
            oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang)
          else if oResponse.Format = ffXhtml then
          begin
            oComp := TFHIRXhtmlComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang);
            TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
            TFHIRXhtmlComposer(oComp).Session := oRequest.Session.Link;
            TFHIRXhtmlComposer(oComp).Tags := oResponse.Tags.Link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
            TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
            TFHIRXhtmlComposer(oComp).OperationName := oRequest.OperationName;
  //          response.Expires := 0;
            response.Pragma := '';
          end
          else if oResponse.Format = ffXml then
            oComp := TFHIRXmlComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang)
          else if oResponse.format = ffText then
            oComp := TFHIRTextComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang)
          {$IFNDEF FHIR2}
          else if (oResponse.Format = ffTurtle) or (res._source_format = ffTurtle) then
          begin
            oComp := TFHIRTurtleComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang);
            if (res <> nil) and (res.id <> '') then
              TFHIRTurtleComposer(oComp).URL := AppendForwardSlash(oRequest.baseUrl) + CODES_TFhirResourceType[res.ResourceType]+'/'+res.id;
          end
          {$ENDIF}
          else if res._source_format = ffJson then
            oComp := TFHIRJsonComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang)
          else
            oComp := TFHIRXmlComposer.Create(FServerContext.Validator.Context.Link, oRequest.lang);
          try
            response.ContentType := oComp.MimeType;
            oComp.SummaryOption := oRequest.Summary;
            oComp.Compose(stream, res, pretty, oresponse.link_List);
          finally
            oComp.Free;
          end;
        end;
      end
    end
    else
    begin
      if response.ContentType = '' then
        response.ContentType := 'text/plain';
      b := TEncoding.UTF8.GetBytes(oResponse.Body);
      stream.write(b, length(b));
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
  json : TFHIRJsonParser;
  ss : TStringStream;
begin
  if header = '' then
    result := nil
  else
  begin
    ss := TStringStream.Create(header, TEncoding.UTF8);
    try
      json := TFHIRJsonParser.Create(FServerContext.Validator.Context.Link, lang);
      try
        json.source := ss;
        json.Parse;
        result := (json.resource as TFhirProvenance).Link;
      finally
        json.Free;
      end;
    finally
      ss.Free;
    end;
  end;
end;

function sNow : String;
begin
  result := FormatDateTime('c', now);
end;

procedure TFhirWebServer.ProcessRequest(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  op : TFHIROperationEngine;
  t : cardinal;
  us, cs : String;
begin
  FLock.Lock;
  try
    inc(FRestCount);
  finally
    FLock.Unlock;
  end;
  t := gettickCount;
  op := FServerContext.Storage.createOperationContext(request.Lang);
  try
    op.OnPopulateConformance := PopulateConformance;
    op.Execute(context, request, response);
    FServerContext.Storage.yield(op, nil);
  except
    on e : Exception do
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
  t := gettickCount - t;
  try
    inc(FRestTime, t);
  finally
    FLock.Unlock;
  end;
  if request.session = nil then // during OAuth only
    us := 'user=(in-oauth)'
  else
    us := 'user='+request.Session.UserName;
  if request.CommandType = fcmdOperation then
    cs := '$'+request.OperationName
  else
    cs := 'cmd='+CODES_TFHIRCommandType[request.CommandType];
  logt('Request: '+cs+', type='+request.ResourceName+', id='+request.Id+', '+us+', params='+request.Parameters.Source+'. rt = '+inttostr(t));
end;

procedure TFhirWebServer.ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  sCookie : String;
  c : integer;
  session : TFhirSession;
  check : boolean;
begin
  if request.AuthUsername = 'Bearer' then
    sCookie := request.AuthPassword
  else
  begin
    c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
    if c > -1 then
      sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1);
  end;

  if (sCookie <> '') and request.Document.StartsWith('/scim/logout') then
  begin
    FServerContext.SessionManager.EndSession(sCookie, request.RemoteIP);
    response.Redirect('/closed');
  end
  else if (FServerContext.SessionManager.GetSession(sCookie, session, check)) then
  begin
    try
      if check and not CheckSessionOK(session, request.RemoteIP) then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage), 'Session Expired', HTTP_ERR_UNAUTHORIZED);
      if not session.canAdministerUsers then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage), 'This Session is not authorised to manage users', HTTP_ERR_UNAUTHORIZED);
      ServerContext.UserProvider.processRequest(acontext, request, response, session);
    finally
      session.Free;
    end;
  end
  else
    Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', request.AcceptLanguage), 'Authentication required', HTTP_ERR_UNAUTHORIZED);
end;

function TFhirWebServer.BuildFhirAuthenticationPage(lang, host, path, msg : String; secure : boolean): String;
var
  authurl : string;
begin
  authurl := OAuthPath(secure);


result :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
'    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'</title>'#13#10+
TFHIRXhtmlComposer.PageLinks+#13#10+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
TFHIRXhtmlComposer.Header(nil, FBasePath, lang, SERVER_VERSION)+
'<h2>'+FOwnerName+' '+GetFhirMessage('NAME_SERVER', lang)+'</h2>'#13#10;



result := result +
'<p>'#13#10+
GetFhirMessage('MSG_AUTH_REQUIRED', lang)+ '</p>'#13#10;
if msg <> '' then
result := result +
  '<p><b>'+ FormatTextToHTML(msg)+'</b></p>'#13#10;

result := result +
'<p><a href="'+FAuthServer.basePath+'/auth?client_id=web&response_type=code&scope=openid%20profile%20user/*.*%20'+SCIM_ADMINISTRATOR+'&redirect_uri='+authurl+'/internal&aud='+authurl+'&state='+FAuthServer.MakeLoginToken(path, apGoogle)+'">Login using OAuth</a></p>'+#13#10;

if FSecurePath <> '' then
result := result +
'<p>Or use the <a href="http://'+FHost+port(FStatedPort, 80)+FBasePath+'">unsecured API</a>.</p>'#13#10;

result := result +
'<p>&nbsp;</p>'#13#10+
'<p>This server uses <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">Smart on FHIR</a> for OAuth logins</p>'#13#10;
result := result +
TFHIRXhtmlComposer.Footer(lang, lang);

end;


function TFhirWebServer.BuildFhirHomePage(comps, lang, host, sBaseURL : String; session : TFhirSession; secure : boolean): String;
var
  counts : TStringList;
  a : String;
  s : String;
  names : TStringList;
  profiles : TAdvStringMatch;
  i, j, ix : integer;
  b : TStringBuilder;
  pol : String;
begin
  logt('home page: '+session.scopes);
  counts := TStringList.create;
  try
    for a in FServerContext.ValidatorContext.allResourceNames do
    begin
      ix := counts.add(a);
      if (comps = '') or FServerContext.Indexes.Compartments.existsInCompartment(frtPatient, a) then
        counts.Objects[ix] := TObject(0)
      else
        counts.Objects[ix] := TObject(-1);
    end;

    pol := FServerContext.Storage.ProfilesAsOptionList;
    profiles := TAdvStringMatch.create;
    try
      profiles.forced := true;
      counts := FServerContext.Storage.FetchResourceCounts(comps);

     s := host+sBaseURL;
     b := TStringBuilder.Create;
     try
      b.Append(
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
    ''#13#10+
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
    '<head>'#13#10+
    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
    '    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'</title>'#13#10+
    TFHIRXhtmlComposer.pagelinks+
    FHIR_JS+
    '</head>'#13#10+
    ''#13#10+
    '<body>'#13#10+
    TFHIRXhtmlComposer.Header(Session, sBaseURL, lang, SERVER_VERSION));

    b.Append(
    '<h2>'+FOwnerName+' '+GetFhirMessage('NAME_SERVER', lang)+'</h2>'#13#10);

      if session <> nil then
        if secure then
        begin
          b.Append('<p>Welcome '+FormatTextToXml(session.SessionName)+'</p>'#13#10);
          if session.canGetUser and (session.User <> nil) then
          begin
            b.Append('<p>You bearer token is '+inttostr(session.UserKey)+'.'+session.User.hash+'. Use this to get access to the secure API without needing OAuth login</p>');
          end;
        end
        else if FSecurePath = '' then
          b.Append('<p>Welcome '+FormatTextToXml(session.SessionName)+'</p>'#13#10)
        else
          b.Append('<p>Welcome '+FormatTextToXml(session.SessionName)+' (or use <a href="https://'+FHost+port(FStatedSSLPort, 443)+FSecurePath+'">Secure API</a>)</p>'#13#10);

    b.Append(
    '<p>'#13#10+
    StringFormat(GetFhirMessage('MSG_HOME_PAGE_1', lang), ['<a href="http://hl7.org/fhir">http://hl7.org/fhir</a>'])+#13#10+
    StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', lang), [s])+' This server defines some <a href="local.hts">extensions to the API</a>, and also offers <a href="/tx">Terminology Services</a>'+#13#10+
    '</p>'#13#10+
    '<hr/>'#13#10+
    ''#13#10+
    '<p>'+GetFhirMessage('SYSTEM_OPERATIONS', lang)+':</p><ul><li> <a href="'+sBaseUrl+'/metadata">'+GetFhirMessage('CONF_PROFILE', lang)+'</a> '+
     '('+GetFhirMessage('OR', lang)+' <a href="'+sBaseUrl+'/metadata?_format=text/xml">as xml</a> ('+GetFhirMessage('OR', lang)+' <a href="'+sBaseUrl+'/metadata?_format=application/json">JSON</a>)</li>'+#13#10);
    if not FIsTerminologyServerOnly then
      b.Append('<li><a class="tag" href="'+sBaseUrl+'/$meta">'+GetFhirMessage('SYSTEM_TAGS', lang)+'</a></li>');
    b.Append('<li><a href="'+sBaseUrl+'/_search">'+GetFhirMessage('GENERAL_SEARCH', lang)+'</a></li>');
    if not FIsTerminologyServerOnly then
      b.Append('<li><a href="'+sBaseUrl+'/_history">'+StringFormat(GetFhirMessage('MSG_HISTORY', lang), [GetFhirMessage('NAME_SYSTEM', lang)])+'</a> (History of all resources)</li>'+#13#10);
    if not FIsTerminologyServerOnly then
      b.Append('<li><a href="#upload">'+GetFhirMessage('NAME_UPLOAD_SERVICES', lang)+'</a></li>'+#13#10);

    if not FIsTerminologyServerOnly then
      b.Append('<li>Create/Edit a new resource based on the profile: <form action="'+sBaseURL+'/_web/Create" method="GET"><select name="profile">'+pol+'</select> <input type="submit" value="GO"></form></li>'+#13#10);

    if (session.canAdministerUsers) then
      b.Append('<li><a href="/scim/web">Manage Users</a></li>'+#13#10);

    b.Append(
    '</ul>'+#13#10+
    ''#13#10+
    '<hr/>'#13#10+
    '<p>'+GetFhirMessage('MSG_HOME_PAGE_3', lang)+'</p>'+#13#10);


    b.Append(
    '<table class="lines">'#13#10+

               '<tr><th>'+GetFhirMessage('NAME_TYPE', lang)+'</th>'+
               '<th>'+GetFhirMessage('NAME_STORED', lang)+'</th>'+
               '<th colspan="4">'+GetFhirMessage('NAME_OPERATIONS', lang)+'</th><td style="border-left: 1px solid grey"/><th>'+GetFhirMessage('NAME_TYPE', lang)+'</th>'+
               '<th>'+GetFhirMessage('NAME_STORED', lang)+'</th>'+
               '<th colspan="4">'+GetFhirMessage('NAME_OPERATIONS', lang)+'</th></tr>'#13#10);

      names := TStringList.create;
      Try
        for a in FServerContext.ValidatorContext.allResourceNames do
        begin
          ix := counts.IndexOf(a);
          if (ix >= 0) and (integer(counts.objects[ix]) > -1) and (FServerContext.ResConfig[a].Supported)  then
            names.Add(a);
        end;

        names.Sort;
        j := 0;
        for i := 0 to names.count div 2 do
        begin
          inc(j);
          if j mod 2 = 1 then
            b.Append('<tr bgcolor="#F0F0F0">')
          else
            b.Append('<tr bgcolor="#FFFFFF">');

          a := names[i];
          ix := counts.IndexOf(a);
          b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseUrl, integer(counts.Objects[ix]), true, true, session.canRead(a)));

          b.Append('<td style="border-left: 1px solid grey"/>');

          if (i + names.count div 2) + 1 < names.count then
          begin
            a := names[1 + i + names.count div 2];
            ix := counts.IndexOf(a);
            b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseUrl, integer(counts.Objects[ix]), true, true, session.canRead(a)));
          end;

          b.Append('</tr>');

        end;
      finally
        names.free;
      end;
      b.Append(
    '</table>'#13#10);
    if not FIsTerminologyServerOnly then
      b.Append(
    '<hr/><h2>'+GetFhirMessage('NAME_UPLOAD_SERVICES', lang)+'</h2>'#13#10+
    '<a name="upload"> </a><form enctype="multipart/form-data" method="POST">'+#13#10+
    '<p><input type="hidden" name="_format" value="text/html"/><br/>'+#13#10+
    ''+GetFhirMessage('MSG_CONTENT_MESSAGE', lang)+'.<br/><br/>'+#13#10+
    ''+GetFhirMessage('MSG_CONTENT_UPLOAD', lang)+': <br/><input type="file" name="file" size="60"/><br/>'+#13#10+
    ''+GetFhirMessage('MSG_CONTENT_PASTE', lang)+':<br/> <textarea name="src" cols="70" rows="5"/>'+#13#10+
    '</textarea><br/><br/>'+#13#10+
    '<table class="none"><tr><td>Operation:</td><td> <select size="1" name="op">'+#13#10+
    ' <option value="transaction">Transaction</option>'+#13#10+
    ' <option value="batch">Batch</option>'+#13#10+
    ' <option value="validation">Validation</option>'+#13#10+
    '</select></td></tr>'+#13#10+
    '<tr><td>Profile:</td><td> <select size="1" name="profile">'+#13#10+
    '<option value=""></option>'+#13#10+
    pol+
    '</select> (if validating, use the selected profile)</td></tr></table><br/>'+#13#10+
    '<input type="submit" value="'+GetFhirMessage('NAME_UPLOAD', lang)+'"/>'#13#10+
    '</p></form>'#13#10);
    b.Append(TFHIRXhtmlComposer.footer(sBaseURL, lang));
    result := b.ToString;
     finally
       b.Free;
     end;
    finally
      profiles.free;
    end;
  finally
    counts.free;
  end;
end;

function TFhirWebServer.BuildFhirUploadPage(lang, host, sBaseURL : String; aType : String; session : TFhirSession): String;
var
  s : String;
begin
  s := host+sBaseURL;

  result :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
'    <title>'+StringFormat(GetFhirMessage('UPLOAD', lang), [aType])+'</title>'#13#10+
'    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
'<div class="header">'#13#10+
'  <a href="http://www.hl7.org/fhir" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img border="0" src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
''#13#10+
'  &copy; HL7.org 2011-2013'#13#10+
'  &nbsp;'#13#10+
'  '+FOwnerName+' '+GetFhirMessage('NAME_SERVER', lang)+''#13#10+
'  &nbsp;'#13#10+
'  FHIR '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+''#13#10;

if session <> nil then
  result := result +'&nbsp;&nbsp;'+FormatTextToXml(Session.SessionName);

result := result +
'  &nbsp;<a href="'+s+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a>'#13#10+
'</div>'#13#10+
''#13#10+
'<div id="div-cnt" class="content">'#13#10+
'<h2>'+StringFormat(GetFhirMessage('UPLOAD', lang), [aType])+'</h2>'#13#10+
'<form action="'+s+lowercase(aType)+'/upload" enctype="multipart/form-data" method="POST">'+#13#10+
'<input type="hidden" name="format" size="text/html"/><br/>'+#13#10+
''+GetFhirMessage('MSG_CONTENT_UPLOAD', lang)+': <input type="file" name="file" size="60"/><br/>'+#13#10+
'<input type="submit" value="Upload"/>'#13#10+
'</form>'#13#10+
''#13#10+
'<p><br/><a href="'+s+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a></p>'+
'</div>'#13#10+
'</body>'#13#10+
'</html>'#13#10+
''#13#10
end;


function TFhirWebServer.loadMultipartForm(const request : TStream; const contentType : String; var upload : boolean) : TMimeMessage;
var
  m : TMimeMessage;
  mp : TMimePart;
begin
  m := TMimeMessage.Create;
  Try
    m.ReadFromStream(request, contentType);
    result := m;
    upload := false;
    for mp in m.parts do
      if SameText(mp.FileName, 'cda.zip') then
        upload := true;
  Except
    on e:exception do
    begin
      m.free;
      recordStack(e);
      raise;
    end;
  End;
end;

function TFhirWebServer.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
var
  store : TFHIROperationEngine;
begin
  store := FServerContext.Storage.createOperationContext(TFHIRRequest(context).Lang);
  try
    result := store.LookupReference(context, id);
    FServerContext.Storage.Yield(store, nil);
  except
    on e : Exception do
    begin
      FServerContext.Storage.Yield(store, e);
      raise;
    end;
  end;
end;

procedure TFhirWebServer.MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  ci : TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  FLock.Lock;
  try
     ci.Activity := request.Command+' '+ request.Document+'?'+request.UnparsedParams;
     ci.Count := ci.Count + 1;
     inc(FTotalCount);
     ci.FStart := GetTickCount;
  finally
    FLock.Unlock;
  end;
end;

procedure TFhirWebServer.MarkExit(AContext: TIdContext);
var
  ci : TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  FLock.Lock;
  try
     ci.Activity := '';
     inc(FTotalTime, getTickCount - ci.FStart);
     ci.FStart := 0;
  finally
    FLock.Unlock;
  end;
end;

function TFhirWebServer.extractFileData(form : TMimeMessage; const name: String; var sContentType : String): TStream;
var
  sLeft, sRight: String;
  iLoop : Integer;
  oPart : TMimePart;
  sHeader : String;
  sName : String;
  sFilename : String;
  sContent : String;
begin
  result := nil;
  For iLoop := 0 To form.Parts.Count - 1 Do
  Begin
    oPart := form.Parts[iLoop];
    sHeader := oPart.ContentDisposition;
    StringSplit(sHeader, ';', sLeft, sHeader);
    If trim(sLeft) = 'form-data' Then
    Begin
      StringSplit(Trim(sHeader), ';', sLeft, sHeader);
      StringSplit(Trim(sLeft), '=', sLeft, sRight);
      If Trim(sLeft) = 'name' Then
        sName := RemoveQuotes(Trim(sRight));
      StringSplit(Trim(sHeader), '=', sLeft, sRight);
      If Trim(sLeft) = 'filename' Then
        sFileName := RemoveQuotes(Trim(sRight));
      If (result = nil) and (sName <> '') And (sFileName <> '') And (oPart.Content.Size > 0) Then
      begin
        result := TBytesStream.Create(oPart.Content.AsBytes);
        sContentType := oPart.Mediatype;
      end
      else if (result = nil) and (sName = 'src') then
      begin
        sContent := BytesAsString(oPart.Content.AsBytes);
        result := TStringStream.create(oPart.Content.AsBytes); // trim
        if StringStartsWith(sContent, '<', false) then
          sContentType := 'application/fhir+xml'
        else if StringStartsWith(sContent, '{', false) then
          sContentType := 'application/fhir+json'
        else
          raise exception.create('unable to determine encoding type for '+sContent);
      end;
    End
  End;
end;

function TFhirWebServer.GetResource(session : TFhirSession; rtype: string; lang, id, ver, op: String): TFhirResource;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  context : TOperationContext;
begin
  result := nil;
  request := TFHIRRequest.create(FServerContext.ValidatorContext.Link, roRest, FServerContext.Indexes.Compartments.Link);
  response := TFHIRResponse.Create;
  try
    request.Session := session.link;
    request.ResourceName := rType;
    request.Lang := lang;
    request.Id := id;
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
    context := TOperationContext.Create;
    try
      ProcessRequest(context, request, response);
    finally
      context.Free;
    end;
    if response.Resource <> nil then
      result := response.resource.link
    else
      raise Exception.Create('Unable to find resource '+rtype+'/'+id+'/'+ver);
  finally
    response.free;
    request.Free;
  end;
end;

function TFhirWebServer.FindResource(session : TFhirSession; rtype: string; lang, params: String): TFhirResource;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  context : TOperationContext;
begin
  result := nil;
  request := TFHIRRequest.create(FServerContext.ValidatorContext.Link, roRest, FServerContext.Indexes.Compartments.Link);
  response := TFHIRResponse.Create;
  try
    request.Session := session.link;
    request.ResourceName := rType;
    request.Lang := lang;
    request.LoadParams(params);
    request.CommandType := fcmdSearch;
    context := TOperationContext.Create;
    try
      ProcessRequest(context, request, response);
    finally
      context.Free;
    end;
    if (response.bundle <> nil) and (response.bundle.entryList.Count = 1) then
      result := response.bundle.entryList[0].resource.link
    else
      raise Exception.Create('Unable to find resource '+rtype+'?'+params);
  finally
    response.free;
    request.Free;
  end;
end;

procedure TFhirWebServer.GetWebUILink(resource: TFhirResource; base, statedType, id, ver: String; var link, text: String);
var
  tail : String;
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
      if statedType = 'Profile'  then
        link := FBasePath+'/_web/StructureDefinition/'+tail
      else
        link := FBasePath+'/_web/Questionnaire/'+tail;
    end;
    if resource.ResourceType = frtStructureDefinition then
    begin
      link := FBasePath+'/_web/StructureDefinition/'+tail;
      text := 'Try out the Profile as a questionnaire based web form';
    end;
    if resource.ResourceType = frtValueSet then
    begin
      link := FBasePath+'/ValueSet/'+id+'/$expand?filter=';
      text := 'Expand this value set';
    end;
    if resource.ResourceType = frtPatient then
    begin
      link := FBasePath+'/_web/Patient/'+id;
      text := 'Patient Record Page';
    end;
  end;
end;

function TFhirWebServer.OAuthPath(secure: boolean): String;
begin
  if secure then
  begin
    if FStatedSSLPort = 443 then
      result := 'https://'+FHost+FSecurePath
    else
      result := 'https://'+FHost+':'+inttostr(FStatedSSLPort)+FSecurePath;
  end
  else
  begin
    if FStatedPort = 80 then
      result := 'http://'+FHost+FSecurePath
    else
      result := 'http://'+FHost+':'+inttostr(FStatedPort)+FSecurePath;
  end;
end;

procedure TFhirWebServer.OnCDSResponse(manager: TCDSHooksManager;
  server: TRegisteredFHIRServer; context: TObject; response: TCDSHookResponse;
  error: String);
var
  ctxt : TFHIRWebServerPatientViewContext;
begin
  ctxt := TFHIRWebServerPatientViewContext(context);

  FLock.Lock;
  try
    if error <> '' then
      ctxt.Errors.add(error+' (from '+server.name+')')
    else
      ctxt.Cards.AddAll(response.cards);
  finally
    FLock.Unlock;
  end;
end;

constructor ERestfulAuthenticationNeeded.Create(const sSender, sMethod, sReason, sMsg: String; aStatus : Word);
begin
  Create(sSender, sMethod, sReason, aStatus, IssueTypeLogin);
  FMsg := sMsg;
end;


function TFhirWebServer.CheckSessionOK(session: TFhirSession; ip : string): Boolean;
var
  id, name, email, msg : String;
begin
  if session.providerCode = apGoogle then
    result := GoogleGetDetails(session.InnerToken, FAuthServer.GoogleAppKey, '', id, name, email, msg)
  else if session.providerCode = apFacebook then
    result := FacebookGetDetails(session.InnerToken, id, name, email, msg)
  else
    result := false;
  if result then
    result := session.Id = id;
  if result then
    FServerContext.SessionManager.MarkSessionChecked(session.Cookie)
  else
    FServerContext.SessionManager.EndSession(session.Cookie, ip);
end;

function TFhirWebServer.ClientAddress(secure: boolean): String;
begin
  if secure then
    result := 'https://localhost:'+IntToStr(FActualSSLPort)+FSecurePath
  else
    result := 'http://localhost:'+IntToStr(FActualPort)+FBasePath;
end;

//procedure TFhirWebServer.ReadTags(Headers: TIdHeaderList; Request: TFHIRRequest);
//var
//  i : integer;
//begin
//  for i := 0 to Headers.Count - 1 do
//    if Headers.Names[i] = 'Category' then
//      ReadTags(Headers.Strings[i], Request);
//end;
//
Procedure TFhirWebServer.ReadTags(header : String; Request : TFHIRRequest);
//var
//  s, s1, l, r, n, v : string;
//  cat : TFHIRAtomCategory;
begin
 // raise Exception.Create('todo');
end;


procedure TFhirWebServer.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: Exception);
var
  op : TFhirTestScriptSetupActionOperation;
begin
  if req.Session = nil then
    exit;
  if req.Session.TestScript = nil then
    exit;
  op := TFhirTestScriptSetupActionOperation.Create;
  req.Session.TestScript.testList.Append.actionList.Append.operation := op;
  if req.CommandType = fcmdOperation then
    op.type_ := TFhirCoding.Create('http://hl7.org/fhir/testscript-operation-codes', req.OperationName)
  else
    op.type_ := TFhirCoding.Create('http://hl7.org/fhir/testscript-operation-codes', CODES_TFHIRCommandType[req.CommandType].ToLower);
  op.resourceElement := TFhirCode.Create(req.ResourceName);
  if resp.Format = ffJson then
    op.accept := ContentTypeJson
  else
    op.accept := ContentTypeXml;
  op.params := req.Parameters.Source;
  op.requestHeaderList.add('Host', req.baseUrl);
  op.requestHeaderList.add('Content-Type', MIMETYPES_TFHIRFormat[req.PostFormat]);
  if (req.lastModifiedDate <> 0) then
    op.requestHeaderList.add('Last-Modified', DateTimeToXMLDateTimeTimeZoneString(req.lastModifiedDate, TimeZoneBias));
  op.requestHeaderList.add('Language', req.Lang);
  op.requestHeaderList.add('if-match', req.IfMatch);
  op.requestHeaderList.add('if-none-match', req.IfNoneMatch);
  if (req.IfModifiedSince <> 0) then
   op.requestHeaderList.add('if-modified-since', DateTimeToXMLDateTimeTimeZoneString(req.IfModifiedSince, TimeZoneBias));
  op.requestHeaderList.add('if-none-exist', req.IfNoneExist);
  if req.Provenance <> nil then
    op.requestHeaderList.add('x-provenance', ComposeJson(FServerContext.ValidatorContext, req.Provenance));
  op.url := req.Url;

end;

procedure TFhirWebServer.ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: Boolean; path: String);
var
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
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
    vars.Add('status.cds.client', inttostr(FPatientHooks.count));
    vars.Add('status.run-time', DescribePeriod((GetTickCount - FStartTime) * DATETIME_MILLISECOND_ONE));
    vars.Add('status.run-time.ms', inttostr(GetTickCount - FStartTime));
    ReturnProcessedFile(response, nil, 'Diagnostics', FSourceProvider.AltFile('/diagnostics.html'), false, vars);
  finally
    vars.free;
  end;

end;

procedure TFhirWebServer.ReturnProcessedFile(response: TIdHTTPResponseInfo; session : TFhirSession; path: String; secure : boolean; variables: TDictionary<String, String> = nil);
begin
  ReturnProcessedFile(response, session, path, path, secure, variables);
end;

procedure TFhirWebServer.ReturnProcessedFile(response: TIdHTTPResponseInfo; session : TFhirSession; claimed, actual: String; secure : boolean; variables: TDictionary<String, String> = nil);
var
  s, n : String;
begin
  logt('script: '+claimed);
  s := FSourceProvider.getSource(actual);
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%specurl%]', FHIR_SPEC_URL, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%admin%]', FAdminEmail, [rfReplaceAll]);
  if (session = nil) then
    s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll])
  else
    s := s.Replace('[%logout%]', '|&nbsp;User: '+session.SessionName+'&nbsp; <a href="/closed/logout" title="Log Out"><img src="/logout.png"></a>  &nbsp;', [rfReplaceAll]);
  if FStatedPort = 80 then
    s := s.Replace('[%host%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', FHost+':'+inttostr(FStatedPort), [rfReplaceAll]);
  if FStatedSSLPort = 443 then
    s := s.Replace('[%securehost%]', FHost, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', FHost+':'+inttostr(FStatedSSLPort), [rfReplaceAll]);
  if s.Contains('[%fitbit-redirect%]') then
    s := s.Replace('[%fitbit-redirect%]', FitBitInitiate(
       FAuthServer.Ini.ReadString(voVersioningNotApplicable, 'fitbit', 'secret', ''), // secret,
       FAuthServer.Ini.ReadString(voVersioningNotApplicable, 'fitbit', 'key', ''), //key
       NewGuidId, //nonce
       'https://local.healthintersections.com.au:961/closed/_web/fitbit.html') //callback
    , [rfReplaceAll]);

  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%'+n+'%]', variables[n], [rfReplaceAll]);

  response.Expires := now + 1;
  response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
  response.FreeContentStream := true;
  response.ContentType := 'text/html; charset=UTF-8';
end;

procedure TFhirWebServer.ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path: String);
begin
  logt('file: '+stated);
  response.Expires := now + 1;
  response.ContentStream := FSourceProvider.asStream(path);
  response.FreeContentStream := true;
  response.ContentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

procedure TFhirWebServer.ReverseProxy(proxy: TReverseProxyInfo; AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
var
  client : TReverseClient;
begin
  client := TReverseClient.create;
  try
    client.proxy := proxy.Link;
    client.context := AContext;
    client.request := request;
    client.response := response;
    if secure then
      client.SecureToken := FSecureToken;
    client.execute;
  finally
    client.free;
  end;
end;

//procedure TFhirWebServer.DoSendFHIR(iMsgKey, SrcID: Integer; request: TFHIRRequest; response: TFHIRResponse);
//var
//  client : TFhirHTTPClient;
//begin
//  client := TFhirHTTPClient.create(FBaseURL, false);
//  try
//    FClientLock.Lock('MakeClient');
//    Try
//      FClients.Add(client);
//    Finally
//      FClientLock.Unlock;
//    End;
//    try
//      if (request.CommandType = fcmdUpdate) and (request.id = '') then
//        request.id := 'test';
//
//      client.doRequest(request, response);
//    finally
//      FClientLock.Lock('CloseClient');
//      Try
//        FClients.Remove(client);
//            Finally
//        FClientLock.Unlock;
//      End;
//    end;
//  finally
//    client.free;
//  end;
//end;
//

function TFhirWebServer.transform1(resource: TFhirResource; lang, xslt: String; saveOnly : boolean): string;
var
  xml : TFHIRXmlComposer;
  msx : TMsXmlParser;
  b : TBytesStream;
  v: variant;
  doc, src : IXMLDOMDocument2;
  xform: IXSLTemplate;
  proc : IXSLProcessor;
  url : String;
begin
//  result := transform2(resource, lang, xslt);
//  exit;

  b := TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create(FServerContext.ValidatorContext.Link, lang);
    try
      xml.Compose(b, resource, false, nil);
    finally
      xml.Free;
    end;
    b.position := 0;
    msx := TMsXmlParser.Create;
    try
      doc := msx.Parse(b);
    finally
      msx.free;
    end;
  finally
    b.Free;
  end;
  logt(doc.documentElement.namespaceURI +', '+doc.documentElement.nodeName);

  v := CreateOLEObject('MSXML2.FreeThreadedDOMDocument.6.0');
  src := IUnknown(TVarData(v).VDispatch) as IXMLDomDocument2;
  src.async := false;
  src.resolveExternals := false;
  src.validateOnParse := false;
  src.setProperty('AllowDocumentFunction', True);
  if not src.loadXML(FSourceProvider.getSource(xslt)) then
    raise Exception.Create('unable to parse XSLT: '+src.parseError.reason);

  v := CreateOLEObject('MSXML2.XSLTemplate.6.0');
  xform := IUnknown(TVarData(v).VDispatch) as IXSLTemplate;
  xform.stylesheet := src;

  Proc := xform.createProcessor;
  Proc.Input := doc;
  Proc.addParameter('useMicrosoft', 'true', '');

  if FStatedPort <> 0 then
    url := 'http://'+FHost+':'+inttostr(FStatedPort)
  else
    url := 'https://'+FHost+':'+inttostr(FStatedSSLPort);

  if saveOnly then
    Proc.addParameter('saveOnly', 'true', '');

  Proc.addParameter('expansionServer', url+FBasePath, '');
  Proc.addParameter('iconPath', url, '');
  Proc.addParameter('jQueryPath', url+'/js', '');

  Proc.Transform;
  result := Proc.Output;
end;


//function TFhirWebServer.transform2(resource: TFhirResource; lang, xslt: String): string;
//var
//  xslt2: AltovaXMLLib_TLB.XSLT2;
//  xml : TFHIRXmlComposer;
//  s : String;
//  AltovaXml : AltovaXMLLib_TLB.Application;
//begin
//  xml := TFHIRXmlComposer.Create(lang);
//  try
//    s := xml.Compose('', '', '', resource, false, nil);
//  finally
//    xml.Free;
//  end;
//
//  AltovaXml := AltovaXMLLib_TLB.CoApplication.Create;
//  xslt2 := AltovaXml.XSLT2;
//  xslt2.InputXMLFromText := s;
//  xslt2.XSLFileName := xslt;
//  result := xslt2.ExecuteAndGetResultAsString;
//  xslt2 := nil;
//  AltovaXml := nil;
//end;

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

{ TFHIRWebServerPatientViewContext }

constructor TFHIRWebServerPatientViewContext.create;
begin
  inherited;
  FCards := TAdvList<TCDSHookCard>.create;
  FErrors := TStringList.create;
end;

destructor TFHIRWebServerPatientViewContext.Destroy;
begin
  FErrors.Free;
  FCards.free;
  FManager.Free;
  inherited;
end;

procedure TFHIRWebServerPatientViewContext.SetManager(const Value: TCDSHooksManager);
begin
  FManager.Free;
  FManager := Value;
end;

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.create(server: TFHIRWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  FLastSweep := now;
  inherited Create;
end;

procedure TFhirServerMaintenanceThread.Execute;
begin
  Writeln('Starting TFhirServerMaintenanceThread');
  try
    FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'starting';
    CoInitialize(nil);
    repeat
      FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'sleeping';
      sleep(1000);
      if not terminated and (FLastSweep < now - (DATETIME_SECOND_ONE * 5)) then
      begin
        try
          FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Sweeping Sessions';
          FServer.FServerContext.Storage.Sweep;
        except
        end;
        FLastSweep := now;
      end;
      if FServer.ServerContext.ForLoad then
      begin
        if (not terminated) then
          try
            FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Building Indexes';
            FServer.FServerContext.TerminologyServer.BuildIndexes(false);
          except
          end;
        if (not terminated) then
          try
            FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'Processing Observations';
            FServer.FServerContext.Storage.ProcessObservations;
          except
          end;
      end;
    until Terminated;
    try
      FServer.ServerContext.TerminologyServer.MaintenanceThreadStatus := 'dead';
    except
    end;
    try
      FServer.FMaintenanceThread := nil;
    except
    end;
    CoUninitialize;
    Writeln('Ending TFhirServerMaintenanceThread');
  except
    Writeln('Failing TFhirServerMaintenanceThread');
  end;
end;

{ TFhirServerSubscriptionThread }

constructor TFhirServerSubscriptionThread.create(server: TFHIRWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create;
end;

procedure TFhirServerSubscriptionThread.Execute;
begin
  Writeln('Starting TFhirServerSubscriptionThread');
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
    until Terminated;
    try
      FServer.ServerContext.TerminologyServer.SubscriptionThreadStatus := 'dead';
    except
    end;
    try
      FServer.FMaintenanceThread := nil;
    except
    end;
    Writeln('Ending TFhirServerSubscriptionThread');
  except
    Writeln('Failing TFhirServerSubscriptionThread');
  end;
end;

{ TFhirServerEmailThread }

constructor TFhirServerEmailThread.create(server: TFHIRWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create;
end;

procedure TFhirServerEmailThread.Execute;
var
  i : integer;
begin
  Writeln('Starting TFhirServerEmailThread');
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
    until Terminated;
    try
      FServer.ServerContext.TerminologyServer.EmailThreadStatus := 'dead';
    except
    end;
    try
      FServer.FMaintenanceThread := nil;
    except
    end;
    Writeln('Ending TFhirServerEmailThread');
  except
    Writeln('Failing TFhirServerEmailThread');
  end;
end;


Initialization
  IdSSLOpenSSLHeaders.Load;
End.






