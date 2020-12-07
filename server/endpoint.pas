unit endpoint;

{$i fhir.inc}

interface

Uses
  SysUtils, Classes, Generics.Collections,
  IdCustomHTTPServer, IdContext, IdOpenSSLX509,
  fsl_base, fsl_threads, fsl_crypto, fsl_stream, fsl_utilities, fsl_http, fsl_json,
  fdb_manager,
  fhir_objects,
  server_config, utilities, session, tx_manager,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  web_event, web_base, web_cache;

type
  TFHIRWebServerClientInfo = class(TFslObject)
  private
    FContext: TIdContext;
    FActivity: String;
    FSession: TFHIRSession;
    FCount: integer;
    FStart: cardinal;
    procedure SetSession(const Value: TFHIRSession);
  public
    destructor Destroy; Override;
    property Context: TIdContext read FContext write FContext;
    property Session: TFHIRSession read FSession write SetSession;
    property Activity: String read FActivity write FActivity;
    property Count: integer read FCount write FCount;
    property Start : cardinal read FStart write FStart;
  end;

  TTokenRedirectManager = class (TFslObject)
  private
    FLock : TFslLock;
    FMap : TDictionary<String, String>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure recordRedirect(token, url : String);
    function getRedirect(token : String; var url : String) : boolean;
    procedure clear;
  end;

  TFhirWebServerEndpoint = class abstract (TFHIRWebServerBase)
  private
    FCode : String;
    FPathWithSlash : String;
    FPathNoSlash : String;
    FOnReturnFile : TWebReturnProcessedFileEvent;
    FOnProcessFile : TWebProcessFileEvent;
//    function EndPointDesc(secure: boolean): String;
  protected
    FTokenRedirects : TTokenRedirectManager;

    function OAuthPath(secure: boolean): String;
    function AbsoluteURL(secure: boolean) : String;
    procedure cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);

    function processFile(session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>) : string; overload;
    procedure returnFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>); overload;
    procedure returnFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean); overload;
    procedure returnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; variables: TFslMap<TFHIRObject>); overload;
    procedure returnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String); overload;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon);
    destructor Destroy; override;
    property PathNoSlash : String read FPathNoSlash;
    property PathWithSlash : String read FPathWithSlash;
    property code : String read FCode;
    function ClientAddress(secure: boolean): String;

    property OnReturnFile : TWebReturnProcessedFileEvent read FOnReturnFile write FOnReturnFile;
    property OnProcessFile : TWebProcessFileEvent read FOnProcessFile write FOnProcessFile;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; virtual; abstract;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; virtual; abstract;
    function description : string; virtual; abstract;
  end;

  { TFHIRServerEndPoint }
  // this is the base for actual end points - they are a pair - a web end point - the class above, a
  // and this class, which is the administrative base

  TFHIRServerEndPoint = class abstract (TFslObject)
  private
    FDatabase : TFDBManager;
    FConfig : TFHIRServerConfigSection;
    FSettings : TFHIRServerSettings;
    FTerminologies : TCommonTerminologies;
    {$IFNDEF NO_JS}
    FOnRegisterJs: TRegisterJavascriptEvent;
    {$ENDIF}
    FWebEndPoint : TFhirWebServerEndpoint;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
    destructor Destroy; override;
    function link : TFHIRServerEndPoint; overload;

    property Database : TFDBManager read FDatabase;
    property Config : TFHIRServerConfigSection read FConfig;
    property Settings : TFHIRServerSettings read FSettings;
    property Terminologies : TCommonTerminologies read FTerminologies;
    property WebEndPoint : TFhirWebServerEndpoint read FWebEndPoint write FWebEndPoint;
    {$IFNDEF NO_JS}
    property OnRegisterJs : TRegisterJavascriptEvent read FOnRegisterJs write FOnRegisterJs;
    {$ENDIF}

    function summary : String; virtual; abstract;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; virtual; abstract;
    function cacheSize : UInt64; virtual;
    procedure clearCache; virtual;
    procedure InstallDatabase; virtual;
    procedure UninstallDatabase; virtual;
    procedure LoadPackages(plist : String); virtual;
    procedure updateAdminPassword; virtual;
    procedure Load; virtual;
    Procedure Unload; virtual;
    procedure internalThread; virtual;
  end;


implementation

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

{ TTokenRedirectManager }

procedure TTokenRedirectManager.clear;
begin
  FLock.Lock('record');
  try
    FMap.clear;
  finally
    FLock.Unlock;
  end;
end;

constructor TTokenRedirectManager.Create;
begin
  inherited;
  FLock := TFslLock.Create('token.redirects');
  FMap := TDictionary<String,String>.create;
end;

destructor TTokenRedirectManager.Destroy;
begin
  FMap.Free;
  FLock.Free;
  inherited;
end;

function TTokenRedirectManager.getRedirect(token: String; var url: String): boolean;
begin
  FLock.Lock('record');
  try
    result := FMap.TryGetValue(token, url);
  finally
    FLock.Unlock;
  end;
end;

procedure TTokenRedirectManager.recordRedirect(token, url: String);
begin
  FLock.Lock('record');
  try
    FMap.AddOrSetValue(token, url);
  finally
    FLock.Unlock;
  end;
end;

{ TFhirWebServerEndpoint }

constructor TFhirWebServerEndpoint.create(code, path: String; common : TFHIRWebServerCommon);
begin
  inherited create(common);
  FCode := code;
  if (path.EndsWith('/')) then
  begin
    FPathWithSlash := path;
    FPathNoSlash := path.Substring(0, path.Length-1);
  end
  else
  begin
    FPathNoSlash := path;
    FPathWithSlash := path+'/';
  end;
  FTokenRedirects := TTokenRedirectManager.create;
end;

destructor TFhirWebServerEndpoint.Destroy;
begin
  FTokenRedirects.Free;
  inherited;
end;

function TFhirWebServerEndPoint.OAuthPath(secure: boolean): String;
begin
  if secure then
  begin
    if Common.ActualSSLPort = 443 then
      result := 'https://' + Common.Host + FPathNoSlash
    else
      result := 'https://' + Common.Host + ':' + inttostr(Common.ActualSSLPort) + FPathNoSlash;
  end
  else
  begin
    if Common.ActualPort = 80 then
      result := 'http://' + Common.Host + FPathNoSlash
    else
      result := 'http://' + Common.Host + ':' + inttostr(Common.ActualPort) + FPathNoSlash;
  end;
end;

function TFhirWebServerEndpoint.processFile(session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>): String;
begin
  FOnProcessFile(self, session, named, path, secure, variables, result);
end;

procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(self, request, response, session, named, path, secure, variables);
end;

procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean);
var
  variables : TFslMap<TFHIRObject>;
begin
  variables := TFslMap<TFHIRObject>.create;
  try
    FOnReturnFile(self, request, response, session, named, path, secure, variables);
  finally
    variables.free;
  end;
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(self, request, response, session, named, path, true, variables);
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String);
var
  variables : TFslMap<TFHIRObject>;
begin
  variables := TFslMap<TFHIRObject>.create;
  try
    FOnReturnFile(self, request, response, session, named, path, true, variables);
  finally
    variables.free;
  end;
end;

function TFhirWebServerEndpoint.ClientAddress(secure: boolean): String;
begin
  if secure then
    result := 'https://'+Common.host+ port(Common.ActualSSLPort, 443) + FPathNoSlash
  else
    result := 'http://'+Common.host+port(Common.ActualPort, 80) + FPathNoSlash;
end;

function TFhirWebServerEndpoint.AbsoluteURL(secure: boolean): String;
begin
  if secure then
    result := 'https://'+common.host+SSLPort+FPathNoSlash
  else
    result := 'http://'+common.host+HTTPPort+FPathNoSlash;
end;

procedure TFhirWebServerEndPoint.cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
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

//function TFhirWebServerEndPoint.EndPointDesc(secure: boolean): String;
//begin
//  result := '';
//  if (secure) then
//  begin
//    if FPathNoSlash <> '' then
//      result := result + ' <li><a href="http://' + Common.Host + port(Common.ActualPort, 80) + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
//        '</a> - direct access with no security considerations</li>'#13#10;
//    if Common.ActualSSLPort <> 0 then
//      result := result + ' <li><a href="' + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
//        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
//  end
//  else
//  begin
//    result := result + ' <li><a href="' + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
//        '</a> - direct access with no security considerations</li>'#13#10;
//    if FPathNoSlash <> '' then
//      result := result + ' <li><a href="https://' + Common.Host + port(Common.ActualSSLPort, 443) + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
//        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
//  end;
//end;

{ TFHIRServerEndPoint }

function TFHIRServerEndPoint.cacheSize: UInt64;
begin
  if WebEndPoint <> nil then
    result := WebEndPoint.FTokenRedirects.sizeInBytes + WebEndPoint.Common.cache.sizeInBytes
  else
    result := 0;
end;

procedure TFHIRServerEndPoint.clearCache;
begin
  if WebEndPoint <> nil then
  begin
    WebEndPoint.FTokenRedirects.clear;
    WebEndPoint.Common.Cache.Clear;
  end;
end;

constructor TFHIRServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
begin
  inherited create;
  FConfig := config;
  FSettings := settings;
  FDatabase := db;
  FTerminologies := common;
end;

destructor TFHIRServerEndPoint.Destroy;
begin
  FTerminologies.Free;
  FConfig.Free;
  FSettings.Free;
  FDatabase.Free;
  inherited;
end;

procedure TFHIRServerEndPoint.InstallDatabase;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.internalThread;
begin
  // nothing
end;

function TFHIRServerEndPoint.link: TFHIRServerEndPoint;
begin
  result := TFHIRServerEndPoint(inherited link);
end;

procedure TFHIRServerEndPoint.Load;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.LoadPackages(plist: String);
begin
 // nothing
end;

procedure TFHIRServerEndPoint.updateAdminPassword;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.UninstallDatabase;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.Unload;
begin
 // nothing
end;

end.


