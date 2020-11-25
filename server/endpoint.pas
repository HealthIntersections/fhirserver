unit endpoint;

{$i fhir.inc}

interface

Uses
  SysUtils, Classes, Generics.Collections,
  IdCustomHTTPServer, IdContext, IdOpenSSLX509,
  fsl_base, fsl_threads, fsl_crypto, fsl_stream, fsl_utilities, fsl_http, fsl_json,
  fdb_manager,
  fhir_objects,
  server_config, utilities, session,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  web_event, web_base;

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
  end;

  TCachedHTTPResponse = class (TFslBuffer)
  private
    FContentType: String;
  public
    function Link : TCachedHTTPResponse; overload;
    property ContentType : String read FContentType write FContentType;
  end;

  THTTPCacheManager = class (TFslObject)
  private
    FLock : TFslLock;
    FCache : TFslMap<TCachedHTTPResponse>;
    function generateKey(req : TIdHTTPRequestInfo) : String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function respond(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo) : boolean;
    procedure recordResponse(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
  end;

  TFhirWebServerEndpoint = class abstract (TFHIRWebServerBase)
  private
    FCode : String;
    FPathWithSlash : String;
    FPathNoSlash : String;
    FOnReturnFile : TReturnProcessFileEvent;


    procedure cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
    function EndPointDesc(secure: boolean): String;


  protected
    FTokenRedirects : TTokenRedirectManager;
    FCache : THTTPCacheManager;

    function OAuthPath(secure: boolean): String;
    function AbsoluteURL(secure: boolean) : String;

//    function HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
//    procedure SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; const lang : THTTPLanguages; message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFHIRIssueType);

//    Procedure ReturnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
//    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
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

    property OnReturnFile : TReturnProcessFileEvent read FOnReturnFile write FOnReturnFile;

//    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
//    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
//    function GetLaunchParameters(request: TIdHTTPRequestInfo; session : TFhirSession; launchContext : String; params : TAuthLaunchParamsSet) : TDictionary<String, String>;

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
    {$IFNDEF NO_JS}
    FOnRegisterJs: TRegisterJavascriptEvent;
    {$ENDIF}
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager);
    destructor Destroy; override;

    property Database : TFDBManager read FDatabase;
    property Config : TFHIRServerConfigSection read FConfig;
    property Settings : TFHIRServerSettings read FSettings;
    {$IFNDEF NO_JS}
    property OnRegisterJs : TRegisterJavascriptEvent read FOnRegisterJs write FOnRegisterJs;
    {$ENDIF}

    function summary : String; virtual; abstract;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; virtual; abstract;
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

{ TCachedHTTPResponse }

function TCachedHTTPResponse.Link: TCachedHTTPResponse;
begin
  result := TCachedHTTPResponse(inherited Link);
end;

{ THTTPCacheManager }

constructor THTTPCacheManager.Create;
begin
  inherited;
  FLock := TFslLock.Create('HTTP.Cache');
  FCache := TFslMap<TCachedHTTPResponse>.create('HTTP.Cache');
end;

destructor THTTPCacheManager.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited;
end;

function THTTPCacheManager.generateKey(req: TIdHTTPRequestInfo): String;
begin
  result := req.RawHTTPCommand+'|'+req.Accept+'|'+req.AuthUsername;
end;

procedure THTTPCacheManager.recordResponse(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  key : String;
  pos : integer;
  co : TCachedHTTPResponse;
begin
  pos := response.ContentStream.Position;
  key := generateKey(request);
  co := TCachedHTTPResponse.Create;
  try
    co.ContentType := response.ContentType;
    co.LoadFromStream(response.ContentStream);
    FLock.Lock;
    try
      FCache.AddOrSetValue(key, co.Link);
    finally
      FLock.Unlock;
    end;
  finally
    co.Free;
  end;
  response.ContentStream.Position := pos;
end;

function THTTPCacheManager.respond(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo): boolean;
var
  co : TCachedHTTPResponse;
begin
  FLock.Lock;
  try
    result := FCache.TryGetValue(generateKey(request), co);
    if result then
      co.Link;
  finally
    FLock.Unlock;
  end;
  if result then
  begin
    try
      response.ContentStream := TMemoryStream.create;
      co.SaveToStream(response.ContentStream);
      response.ContentStream.Position := 0;
      response.ContentType := co.ContentType;
    finally
      co.Free;
    end;
  end;
end;

{ TTokenRedirectManager }

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
  FCache := THTTPCacheManager.create;
end;

destructor TFhirWebServerEndpoint.Destroy;
begin
  FCache.free;
  FTokenRedirects.Free;
//  FAuthServer.Free;
  inherited;
end;

//procedure TFhirWebServerEndpoint.SetAuthServer(const Value: TAuth2Server);
//begin
//  FAuthServer.Free;
//  FAuthServer := Value;
//end;
//


//function TFhirWebServerEndpoint.BuildFhirAuthenticationPage(const lang : THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params : String): String;
//var
//  authurl: string;
//  p : THTTPParameters;
//begin
//  authurl := OAuthPath(secure);
//
//  result := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
//    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
//    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
//    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' + Factory.versionString
//    + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + #13#10 + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 + ''#13#10 +
//    TFHIRXhtmlComposer.header(factory, nil, FPath, lang, SERVER_VERSION) + '<h2>' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + '</h2>'#13#10;
//
//  result := result + '<p>'#13#10 + GetFhirMessage('MSG_AUTH_REQUIRED', lang) + '</p>'#13#10;
//  if (Msg = '') and (params <> '') then
//  begin
//    p := THTTPParameters.Create(params);
//    try
//      msg := p['error_description'];
//    finally
//      p.Free;
//    end;
//  end;
//
//  if Msg <> '' then
//    result := result + '<p><b>' + FormatTextToHTML(Msg) + '</b></p>'#13#10;
//
//  result := result + '<p><a href="' + FAuthServer.BasePath + '/auth?client_id=c.1&response_type=code&scope=openid%20profile%20fhirUser%20user/*.*%20' + SCIM_ADMINISTRATOR
//    + '&redirect_uri=' + authurl + '/internal&aud=' + authurl + '&state=' + FAuthServer.MakeLoginToken(path, apGoogle) + '">Login using OAuth</a></p>' + #13#10;
//
//  if Common.ActualSSLPort <> 0 then
//    result := result + '<p>Or use the <a href="http://' + Host + port(Common.ActualPort, 80) + FPath + '">unsecured API</a>.</p>'#13#10;
//
//  result := result + '<p>&nbsp;</p>'#13#10 +
//    '<p>This server uses <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">Smart App Launch</a> for OAuth logins</p>'#13#10;
//  result := result + TFHIRXhtmlComposer.Footer(factory, path, lang, logid);
//end;

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


procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(request, response, session, named, path, secure, variables);
end;

procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean);
var
  variables : TFslMap<TFHIRObject>;
begin
  variables := TFslMap<TFHIRObject>.create;
  try
    FOnReturnFile(request, response, session, named, path, secure, variables);
  finally
    variables.free;
  end;
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(request, response, session, named, path, true, variables);
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String);
var
  variables : TFslMap<TFHIRObject>;
begin
  variables := TFslMap<TFHIRObject>.create;
  try
    FOnReturnFile(request, response, session, named, path, true, variables);
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


function TFhirWebServerEndPoint.EndPointDesc(secure: boolean): String;
begin
  result := '';
  if (secure) then
  begin
    if FPathNoSlash <> '' then
      result := result + ' <li><a href="http://' + Common.Host + port(Common.ActualPort, 80) + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
        '</a> - direct access with no security considerations</li>'#13#10;
    if Common.ActualSSLPort <> 0 then
      result := result + ' <li><a href="' + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end
  else
  begin
    result := result + ' <li><a href="' + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
        '</a> - direct access with no security considerations</li>'#13#10;
    if FPathNoSlash <> '' then
      result := result + ' <li><a href="https://' + Common.Host + port(Common.ActualSSLPort, 443) + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end;
end;

{ TFHIRServerEndPoint }

constructor TFHIRServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager);
begin
  inherited create;
  FConfig := config;
  FSettings := settings;
  FDatabase := db;
end;

destructor TFHIRServerEndPoint.Destroy;
begin
  FConfig.Free;
  FSettings.Free;
  FDatabase.Free;
  inherited;
end;

procedure TFHIRServerEndPoint.InstallDatabase;
begin

end;

procedure TFHIRServerEndPoint.internalThread;
begin
  // nothing
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

end.


