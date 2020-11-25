unit storage_endpoint;

{$i fhir.inc}

interface

uses
  Sysutils,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_stream, fsl_crypto,
  fhir_objects,
  fdb_manager,
  server_config, utilities,
  storage, user_manager, session, auth_manager, server_context,
  tx_webserver,
  web_base, endpoint;

type
  TStorageEndPoint = class;

  TStorageWebEndpoint = class (TFhirWebServerEndpoint)
  private
    FContext : TFHIRServerContext;
    FEndPoint : TStorageEndPoint;
    FAuthServer: TAuth2Server; // todo - where does this come from?
    FTerminologyWebServer: TTerminologyWebServer; // todo - where does this come from?

    procedure SetTerminologyWebServer(const Value: TTerminologyWebServer);
    Procedure HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    function HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
    procedure ReturnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
    Procedure ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; prefix : String);

  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon; endPoint : TStorageEndPoint);
    destructor Destroy; override;

    property Context : TFHIRServerContext read FContext;
    property TerminologyWebServer: TTerminologyWebServer read FTerminologyWebServer write SetTerminologyWebServer;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; override;
  end;

  TStorageEndPoint = class abstract (TFHIRServerEndPoint)
  private
  protected
    FServerContext : TFHIRServerContext;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager);
    destructor Destroy; override;
  end;


implementation

{ TStorageEndPoint }

constructor TStorageEndPoint.Create(config: TFHIRServerConfigSection; settings: TFHIRServerSettings; db : TFDBManager);
begin
  inherited create(config, settings, db);
end;

destructor TStorageEndPoint.Destroy;
begin
  FServerContext.Free;
  inherited;
end;

{ TStorageWebEndpoint }

constructor TStorageWebEndpoint.Create(code, path: String; common: TFHIRWebServerCommon; endPoint: TStorageEndPoint);
begin
  inherited create(code, path, common);
  FEndPoint := endPoint;
  FContext := FEndPoint.FServerContext;
end;

destructor TStorageWebEndpoint.Destroy;
begin
  FTerminologyWebServer.free;
  inherited;
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

    if request.Document.StartsWith(FAuthServer.path) then
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

    if request.Document.StartsWith(FAuthServer.path) then
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
      result := HandleRequest(AContext, request, response, true, true, PathNoSlash, id, Session, cert)
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




end.
