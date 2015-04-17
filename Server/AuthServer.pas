unit AuthServer;

// to convert .crt to .pem openssl x509 -in mycert.crt -out mycert.pem -outform PEM

interface

uses
  SysUtils, Classes, System.Generics.Collections,

  IniFiles,

  IdContext, IdCustomHTTPServer, IdCookie,

  ParseMap, KDBManager, KDBDialects, KCritSct,

  StringSupport, EncodeSupport, GUIDSupport, DateSupport, AdvObjects, AdvMemories, AdvJSON, JWT,

  FacebookSupport, SCIMServer,

  FHIRDataStore, FHIRSupport, FHIRBase;

Const
  FHIR_COOKIE_NAME = 'fhir-session-idx';

type
  TProcessFileEvent = procedure (response : TIdHTTPResponseInfo; named, path : String; variables: TDictionary<String, String> = nil) of Object;

  TFhirLoginToken = Class (TAdvObject)
  private
    FProvider : TFHIRAuthProvider;
    FPath : String;
    FExpires : TDateTime;
  end;


  // predefined token per user for testing

  // this is a server that lives at /oauth2
  TAuth2Server = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FIni : TIniFile;
    FFhirStore : TFHIRDataStore;
    FOnProcessFile : TProcessFileEvent;
    FFilePath : String;
    FSSLPort : String;
    FHost : String;
    FSSLCert : String;
    FSSLPassword : String;

    FFacebookAppid : String;
    FFacebookAppSecret : String;
    FLoginTokens : TStringList;
    FGoogleAppid : String;
    FGoogleAppSecret : String;
    FGoogleAppKey : String;
//    FAppSecrets : String;
    FHL7Appid : String;
    FHL7AppSecret : String;
    FAdminEmail : String;
    FSCIMServer : TSCIMServer;
    Procedure HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap;response: TIdHTTPResponseInfo);
    function checkNotEmpty(v, n: String): String;
    function isAllowedRedirect(client_id, redirect_uri: String): boolean;
    function isAllowedAud(client_id, aud_uri: String): boolean;
    procedure SetFhirStore(const Value: TFHIRDataStore);
    function BuildLoginList(id : String) : String;
    function AltFile(path: String): String;
    Function CheckLoginToken(state : string; var original : String; var provider : TFHIRAuthProvider):Boolean;

  public
    Constructor Create(ini : String; filePath, Host, SSLPort : String; SCIM : TSCIMServer);
    Destructor Destroy; override;

    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    procedure setCookie(response: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);

    property FHIRStore : TFHIRDataStore read FFhirStore write SetFhirStore;
    property OnProcessFile : TProcessFileEvent read FOnProcessFile write FOnProcessFile;

    property FacebookAppid : String read FFacebookAppid;
    property GoogleAppid : String read FGoogleAppid;
    property GoogleAppKey : String read FGoogleAppKey;
    property HL7Appid : String read FHL7Appid;
    function MakeLoginToken(path: String; provider: TFHIRAuthProvider): String;
    property SSLCert : String read FSSLCert write FSSLCert;
    property SSLPassword : String read FSSLPassword write FSSLPassword;
    property AdminEmail : String read FAdminEmail write FAdminEmail;

    function AuthPath : String;
    function BasePath : String;
    function TokenPath : String;
    Property Ini : TIniFile read FIni;
  end;


implementation

{ TAuth2Server }

constructor TAuth2Server.Create(ini: String; filePath, Host, SSLPort : String; SCIM : TSCIMServer);
begin
  inherited create;
  FSCIMServer := SCIM;
  FIni := TIniFile.Create(ini);
  FFilePath := filePath;
  FHost := host;
  FSSLPort := SSLPort;
  FLock := TCriticalSection.Create('auth-server');
  FLoginTokens := TStringList.create;

  FHL7Appid := FIni.ReadString('hl7.org', 'app-id', '');
  FHL7AppSecret := FIni.ReadString('hl7.org', 'app-secret', '');
  FFacebookAppid := FIni.ReadString('facebook.com', 'app-id', '');
  FFacebookAppSecret := FIni.ReadString('facebook.com', 'app-secret', '');
  FGoogleAppid := FIni.ReadString('google.com', 'app-id', '');
  FGoogleAppSecret := FIni.ReadString('google.com', 'app-secret', '');
  FGoogleAppKey := FIni.ReadString('google.com', 'app-key', '');

end;

destructor TAuth2Server.Destroy;
begin
  FLoginTokens.Free;;
  FLock.Free;
  FFhirStore.Free;
  FIni.Free;
  FSCIMServer.free;
  inherited;
end;


function TAuth2Server.BasePath: String;
begin
  if FSSLPort = '443' then
    result := 'https://'+FHost+'/oauth2'
  else
    result := 'https://'+FHost+':'+FSSLPort+'/oauth2';
end;

function TAuth2Server.MakeLoginToken(path : String; provider : TFHIRAuthProvider): String;
var
  login : TFhirLoginToken;
  i : integer;
  t : TDateTime;
begin
  t := now;
  FLock.Lock;
  try
    login := TFhirLoginToken.create;
    try
      login.FPath := path;
      login.FExpires := now + DATETIME_MINUTE_ONE * 30;
      login.Fprovider := provider;
      result := OAUTH_LOGIN_PREFIX + copy(GUIDToString(CreateGuid), 2, 36);
      FLoginTokens.AddObject(result, login.link);
    finally
      login.free;
    end;
    for i := FLoginTokens.Count - 1 downto 0 do
    begin
      login := TFhirLoginToken(FLoginTokens.Objects[i]);
      if login.FExpires < t then
      begin
        login.free;
        FLoginTokens.Delete(i);
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;




function TAuth2Server.checkNotEmpty(v , n : String) : String;
begin
  if (v = '') then
    raise Exception.Create('Parameter "'+n+'" not found');
  result := v;
end;


function TAuth2Server.AltFile(path : String) : String;
begin
  if path.StartsWith('/') then
    result := FFilePath+path.Substring(1).Replace('/', '\')
  else
    result := '';
end;


function TAuth2Server.isAllowedAud(client_id, aud_uri: String): boolean;
begin
  result := (aud_uri = BasePath);
end;

function TAuth2Server.isAllowedRedirect(client_id, redirect_uri: String): boolean;
var
  i : integer;
  s : String;
begin
  i := 0;
  result := false;
  repeat
    s := FIni.ReadString(client_id, 'redirect'+inttostr(i), '');
    result := s = redirect_uri;
  until result or (s = '');
end;

procedure TAuth2Server.setCookie(response: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);
var
  cookie: TIdCookie;
begin
  cookie := response.Cookies.Add;
  cookie.CookieName := cookiename;
  cookie.Value := cookieval;
  cookie.Domain := domain;
  cookie.Path := '/';  // path;
  cookie.Expires := expiry;
  cookie.Secure := secure;
end;




procedure TAuth2Server.SetFhirStore(const Value: TFHIRDataStore);
begin
  FFhirStore.Free;
  FFhirStore := Value;
end;

function TAuth2Server.TokenPath: String;
begin
 result := '/oauth2/token';
end;

procedure TAuth2Server.HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; params : TParseMap; response: TIdHTTPResponseInfo);
var
  client_id : String;
  scope : String;
  redirect_uri : String;
  state : String;
  aud : String;
  id : String;
  conn : TKDBConnection;
  variables : TDictionary<String,String>;
begin
  if params.GetVar('response_type') <> 'code' then
    raise Exception.Create('Only response_type allowed is ''code''');
  client_id := checkNotEmpty(params.GetVar('client_id'), 'client_id');
  scope := checkNotEmpty(params.GetVar('scope'), 'scope');
  redirect_uri := checkNotEmpty(params.GetVar('redirect_uri'), 'redirect_uri');
  state := checkNotEmpty(params.GetVar('state'), 'state');
  aud := checkNotEmpty(params.GetVar('aud'), 'aud');

  if FIni.ReadString(client_id, 'name', '') = '' then
    raise Exception.Create('Unknown Client Identifier "'+client_id+'"');
  if not isAllowedRedirect(client_id, redirect_uri) then
    raise Exception.Create('Unacceptable Redirect url "'+redirect_uri+'"');
  if not isAllowedAud(client_id, aud) then
    raise Exception.Create('Unacceptable FHIR Server URL "'+aud+'"');

  id := GUIDToString(CreateGUID).ToLower.Substring(1, 36);
  conn := FFhirStore.DB.GetConnection('oatuh2');
  try
    conn.ExecSQL('insert into OAuthLogins (Id, Client, Scope, Redirect, ClientState, Status, DateAdded) values ('''+id+''', '''+client_id+''', '''+SQLWrapString(scope)+''', '''+SQLWrapString(redirect_uri)+''', '''+SQLWrapString(state)+''', 1, '+DBGetDate(conn.Owner.Platform)+')');
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;

  variables := TDictionary<String,String>.create;
  try
    variables.Add('idmethods', BuildLoginList(id));
    variables.Add('client', FIni.ReadString(client_id, 'name', ''));
    OnProcessFile(response, '/oauth_login.html', AltFile('/oauth_login.html'), variables)
  finally
    variables.free;
  end;
end;

procedure TAuth2Server.HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  client_id, name, authurl: String;
  conn : TKDBConnection;
  variables : TDictionary<String,String>;
  c : integer;
  check : boolean;
  session : TFhirSession;
  rights : TStringList;
  redirect, state : String;
begin
  session := nil;
  c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
  if c > -1 then
    FFhirStore.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1), session, check); // actually, in this place, we ignore check.  we just established the session
  if session = nil then
    raise Exception.Create('User Session not found');

  if FSSLPort = '443' then
    authurl := 'https://'+FHost+'/oauth2'
  else
    authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2';

  try
    conn := FFhirStore.DB.GetConnection('OAuth2');
    try
      conn.SQL := 'select Client, Name, Redirect, ClientState from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(session.key)+' and Status = 2 and OAuthLogins.SessionKey = Sessions.SessionKey';
      conn.Prepare;
      conn.Execute;
      if not conn.FetchNext then
        raise Exception.Create('State Error - session "'+inttostr(session.key)+'" not ready for a choice');
      client_id := conn.ColStringByName['Client'];
      name := conn.ColStringByName['Name'];
      redirect := conn.ColStringByName['Redirect'];
      state := conn.ColStringByName['ClientState'];
      conn.Terminate;

      if params.getVar('form') = 'true' then
      begin
        rights := TStringList.create;
        try
          if params.getVar('read') = '1' then
            rights.Add('read');
          if params.getVar('write') = '1' then
            rights.Add('write');
          if params.getVar('user') = '1' then
            rights.Add('user');

          session.JWT := TJWT.Create;
          session.jwt.header['kid'] := authurl+'/auth_key'; // cause we'll sign with our SSL certificate
          session.jwt.issuer := FHost;
          session.jwt.expires := session.Expires;
          session.jwt.issuedAt := now;
          session.jwt.id := FHost+'/sessions/'+inttostr(Session.Key);

          if params.getVar('user') = '1' then
          begin
          // if user rights granted
            session.jwt.subject := Names_TFHIRAuthProvider[session.Provider]+':'+session.id;
            session.jwt.name := session.Name;
            if session.Email <> '' then
              session.jwt.email := session.Email;
          end;
          session.JWTPacked := TJWTUtils.rsa_pack(session.jwt, jwt_hmac_rsa256, ChangeFileExt(FSSLCert, '.key'), FSSLPassword);

          conn.SQL := 'Update OAuthLogins set Status = 3, DateChosen = '+DBGetDate(conn.Owner.Platform)+', Rights = :r, Jwt = :jwt where Id = '''+SQLWrapString(Session.OuterToken)+'''';
          conn.prepare;
          conn.BindBlobFromString('r', rights.CommaText);
          conn.BindBlobFromString('jwt', session.JWTPacked);
          conn.Execute;
          conn.Terminate;

          session.Rights.assign(rights); // marks it ok for use. possible thread safety issue here, but we'll ignore it for now
          response.Redirect(redirect+'?code='+session.OuterToken+'&state='+state);
        finally
          rights.Free;
        end;
      end
      else
      begin
        variables := TDictionary<String,String>.create;
        try
          variables.Add('client', FIni.ReadString(client_id, 'name', ''));
          variables.Add('username', name);
          OnProcessFile(response, '/oauth_choice.html', AltFile('/oauth_choice.html'), variables)
        finally
          variables.free;
        end;
      end;
      conn.Release;
    except
      on e:exception do
      begin
        conn.Error(e);
        raise;
      end;
    end;
  finally
    session.Free;
  end;
end;

procedure TAuth2Server.HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  obj :  TJsonObject;
begin
  obj := TJsonObject.create;
  try
    obj['issuer'] := FHost;
    obj['authorization_endpoint'] := BasePath+'/auth';
    obj['token_endpoint'] := BasePath+'/token';
    obj['jwks_uri'] :=  BasePath+'/auth_key';
    obj['registration_endpoint'] := 'mailto:'+FAdminEmail;
    obj.arr['scopes_supported'] := TJsonArray.create.add('read').add('write').add('user');

    obj['subject_types_supported'] := 'public';
    obj.arr['id_token_signing_alg_values_supported'] := TJsonArray.create.add('RS256');
    obj.arr['response_types_supported'] := TJsonArray.create.add('id_token');
    if FSSLPort = '443' then
      obj['service_documentation'] :=  'https://'+FHost+'/local.hts'
    else
      obj['service_documentation'] :=  'https://'+FHost+':'+FSSLPort+'/local.hts';
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentType := 'application/json';
    response.ContentText := TJSONWriter.writeObjectStr(obj, true);
  finally
    obj.free;
  end;

end;

procedure TAuth2Server.HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  jwk : TJWK;
  authurl : String;
begin
  if FSSLPort = '443' then
    authurl := 'https://'+FHost+'/oauth2'
  else
    authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2';

  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(FSSLCert));
  try
    jwk.obj['alg'] := 'RS256';
    jwk.obj['use'] := 'sig';
    jwk.obj['kid'] := authurl+'/auth_key';

    response.ContentType := 'application/json';
    response.ContentText := TJSONWriter.writeObjectStr(jwk.obj, true);
  finally
    jwk.free;
  end;
end;

procedure TAuth2Server.HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  conn : TKDBConnection;
  id, username, password, domain, state, jwt : String;
  authurl, token, expires, msg, uid, name, email : String;
  session : TFhirSession;
  provider : TFHIRAuthProvider;
  ok : boolean;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  conn := FFhirStore.DB.GetConnection('OAuth2');
  try
    if params.VarExists('id') and params.VarExists('username') and params.VarExists('password') then
    begin
      id := params.GetVar('id');
      username := params.GetVar('username');
      password := params.GetVar('password');

      if not FSCIMServer.CheckLogin(username, password) then
        raise Exception.Create('Login failed');

      if conn.CountSQL('select count(*) from OAuthLogins where Id = '''+SQLWrapString(id)+''' and Status = 1') <> 1 then
        raise Exception.Create('State failed - no login session active');

      session := FFhirStore.RegisterSession(apInternal, '', id, username, '', '', '', '1440', AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else if request.document.startsWith('/oauth2/auth_dest/state/') then
    begin
      // HL7
      if not CheckLoginToken(copy(request.document, 25, $FF), id, provider) then
        raise Exception.Create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)');
      uid := params.GetVar('userid');
      name := params.GetVar('fullName');
      expires := inttostr(60 * 24 * 10); // 10 days
      session := FFhirStore.RegisterSession(aphl7, '', id, uid, name, '', '', expires, AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else if (params.VarExists('state')) then
    begin
      if FSSLPort = '443' then
        authurl := 'https://'+FHost+'/oauth2/auth_dest'
      else
        authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2/auth_dest';

      state := params.GetVar('state');
      if not StringStartsWith(state, OAUTH_LOGIN_PREFIX, false) then
        raise Exception.Create('State Prefix mis-match');
      if not CheckLoginToken(state, id, provider) then
        raise Exception.Create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)');
      if params.VarExists('error') then
        raise Exception.Create('error_description');

      if provider = apGoogle then
      begin
        ok := GoogleCheckLogin(FGoogleAppid, FGoogleAppSecret, authurl, params.GetVar('code'), token, expires, jwt, msg);
        if ok then
          ok := GoogleGetDetails(token, FGoogleAppKey, jwt, uid, name, email, msg);
      end
      else
      begin
        ok := FacebookCheckLogin(FFacebookAppid, FFacebookAppSecret, authurl, params.GetVar('code'), token, expires, msg);
        if ok then
          ok := FacebookGetDetails(token, uid, name, email, msg);
      end;
      if not ok then
        raise Exception.Create('Processing the login failed ('+msg+')');
      session := FFhirStore.RegisterSession(provider, token, id, uid, name, email, '', expires, AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else
      raise Exception.Create('Login attempt not understood');
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TAuth2Server.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  params : TParseMap;
begin
  // cors
  response.CustomHeaders.add('Access-Control-Allow-Origin: *');
  response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
  response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS');
  if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
    response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
  response.ContentType := 'application/json';

  params := TParseMap.createSmart(request.UnparsedParams);
  try
    if (request.Document = '/oauth2/auth') then
      HandleAuth(AContext, request, params, response)
    else if (request.Document.startsWith('/oauth2/auth_dest')) then
      HandleLogin(AContext, request, params, response)
    else if (request.Document = '/oauth2/auth_choice') then
      HandleChoice(AContext, request, params, response)
    else if (request.Document = '/oauth2/token') then
      HandleToken(AContext, request, params, response)
    else if (request.Document = '/oauth2/token_data') then
      HandleTokenData(AContext, request, params, response)
    else if (request.Document = '/oauth2/auth_skype') then
      HandleSkype(AContext, request, params, response)
    else if (request.Document = '/oauth2/auth_key') then
      HandleKey(AContext, request, params, response)
    else if (request.Document = '/oauth2/discovery') then
      HandleDiscovery(AContext, request, response)
    else
      raise Exception.Create('Invalid URL');
  finally
    params.Free;
  end;
end;

procedure TAuth2Server.HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  conn : TKDBConnection;
  token, id, name, email, password, domain : String;
  session : TFhirSession;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));
  conn := FFhirStore.DB.GetConnection('OAuth2');
  try
    if params.getVar('form') <> '' then
    begin
      token := checkNotEmpty(params.GetVar('token'), 'token');
      id := checkNotEmpty(params.GetVar('id'), 'id');
      name := checkNotEmpty(params.GetVar('name'), 'name');
      email := checkNotEmpty(params.GetVar('email'), 'email');
      password := checkNotEmpty(params.GetVar('password'), 'password');

      if FIni.ReadString('admin', 'password', '') <> password then
        raise Exception.Create('Admin Password fail');

      // update the login record
      // create a session
      session := FFhirStore.RegisterSession(apInternal, '', token, id, name, email, '', inttostr(24*60), AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(token)+'''');
      finally
        session.Free;
      end;
      response.ContentText := 'done';
    end
    else if params.getVar('id') <> '' then
    begin
      if not FFhirStore.GetSessionByToken(params.GetVar('id'), session) then
        raise Exception.Create('State Error (1)');
      try
        if conn.CountSQL('Select Count(*) from OAuthLogins where Status = 2 and SessionKey = '+inttostr(session.Key)) <> 1 then
          raise Exception.Create('State Error (2)');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end
    end
    else
      OnProcessFile(response, '/oauth2/auth_skype.html', AltFile('/oauth_skype.html'), nil);
    conn.Release;
  except
    on e:exception do
    begin
      response.ContentText := 'error: '+e.message;
      conn.Error(e);
      raise;
    end;
  end;
end;

function readFromScope(scope, name : String) : String;
var
  i : integer;
var
  list : TStringList;
begin
  result := '';
  list := TStringList.create;
  try
    list.CommaText := scope.Replace(' ', ',');
    for i := 0 to list.Count - 1 do
      if list[i].StartsWith(name+':') then
        result := list[i].Substring(name.Length+1)
  finally
    list.free;
  end;
end;

procedure TAuth2Server.HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  code, clientId, clientSecret, uri, errCode : string;
  session : TFhirSession;
  conn : TKDBConnection;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  launch, scope : String;
begin
  buffer := TAdvMemoryStream.Create;
  try
    try
      errCode := 'invalid_request';
      code := checkNotEmpty(params.getVar('code'), 'code');
      clientId := checkNotEmpty(params.getVar('client_id'), 'client_id');
      clientSecret := params.getVar('client_secret');
      uri := checkNotEmpty(params.getVar('redirect_uri'), 'redirect_uri');
      errCode := 'unsupported_grant_type';
      if params.getVar('grant_type') <> 'authorization_code' then
        raise Exception.Create('Invalid grant_type - must be authorization_code');

      errCode := 'invalid_grant';
      if not FFhirStore.GetSessionByToken(code, session) then
        raise Exception.Create('Authorization Code not recognized');

      errCode := 'invalid_client';
      if FIni.ReadString(clientId, 'secret', '') <> clientSecret then
        raise Exception.Create('Client Id or secret is wrong ("'+clientId+'")');

      try
        errCode := 'invalid_request';
        conn := FFhirStore.DB.GetConnection('OAuth2');
        try
          conn.SQL := 'select Redirect, Scope from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(session.key)+' and Status = 3 and OAuthLogins.SessionKey = Sessions.SessionKey';
          conn.prepare;
          conn.execute;

          errCode := 'invalid_grant';
          if not conn.fetchnext then
            raise Exception.Create('Authorization Code not recognized (2)');

          errCode := 'invalid_request';
          if conn.ColStringByName['Redirect'] <> uri then
            raise Exception.Create('Mismatch between claimed and actual redirection URIs');
          scope := conn.ColStringByName['Scope'];
          launch := readFromScope(scope, 'launch');
          conn.terminate;


          conn.ExecSQL('Update OAuthLogins set Status = 4, DateTokenAccessed = '+DBGetDate(conn.owner.platform)+' where Id = '''+session.OuterToken+'''');

          json := TJsonWriter.create;
          try
            json.Stream := buffer.link;
            json.Start;
            json.Value('access_token', session.Cookie);
            json.Value('token_type', 'Bearer');
            json.Value('expires_in', inttostr(trunc((session.Expires - now) / DATETIME_SECOND_ONE)));
            json.Value('id_token', session.JWTPacked);
            json.Value('patient', launch);
            json.Finish;
          finally
            json.Free;
          end;
          conn.Release;
        except
          on e:exception do
          begin
            conn.Error(e);
            raise;
          end;
        end;
      finally
        session.free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        response.ResponseNo := 500;
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('error', errCode);
          json.Value('error_description', e.Message);
          json.Finish;
        finally
          json.Free;
        end;
      end;
    end;
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;

procedure TAuth2Server.HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
  token, clientId, clientSecret : string;
  session : TFhirSession;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  check : boolean;
begin
  buffer := TAdvMemoryStream.Create;
  try
    try
      if request.AuthUsername <> 'Bearer' then
        raise Exception.Create('OAuth2 Access Token is required in the HTTP Authorization Header (type Bearer)');
      token := checkNotEmpty(params.getVar('token'), 'token');
      if request.AuthPassword <> token then
        raise Exception.Create('Access Token Required');

      clientId := checkNotEmpty(params.getVar('client_id'), 'client_id');
      clientSecret := checkNotEmpty(params.getVar('client_secret'), 'client_secret');

      if FIni.ReadString(clientId, 'secret', '') <> clientSecret then
        raise Exception.Create('Client Id or secret is wrong ("'+clientId+'")');

      if not FFhirStore.GetSession(token, session, check) then
      begin
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('active', false);
          json.Finish;
        finally
          json.Free;
        end;
      end
      else
      try
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('active', true);
          json.Value('token_type', 'Bearer');
          json.Value('exp', inttostr(trunc((session.Expires - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
          json.Value('iat', inttostr(trunc((session.FirstCreated - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
          json.Value('scope', session.rights.CommaText.Replace(',', ' '));
          json.Value('use_count', inttostr(session.useCount));
          if session.Rights.IndexOf('user') > -1 then
          begin
            json.Value('user_id', Names_TFHIRAuthProvider[session.Provider]+':'+session.id);
            json.Value('user_name', session.Name);
            if session.Email <> '' then
              json.Value('email', session.Email);
          end;
          json.Finish;
        finally
          json.Free;
        end;
      finally
        session.free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('error', 'invalid_client');
          json.Value('error_description', e.Message);
          json.Finish;
        finally
          json.Free;
        end;
      end;
    end;
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;


function TAuth2Server.AuthPath: String;
begin
  result := '/oauth2/auth';
end;

function TAuth2Server.BuildLoginList(id : String): String;
var
  authurl : String;
  path : String;
begin
  path := '/oauth2/auth_dest';
  if FSSLPort = '443' then
    authurl := 'https://'+FHost+path
  else
    authurl := 'https://'+FHost+':'+FSSLPort+path;

  result := '';
  if FHL7Appid <> '' then
    result := result +
      '<li><a href="http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FHL7Appid+'&returnURL='+EncodeMime(authurl+'/state/'+MakeLoginToken(id, apHL7))+'">Use your HL7 website account</a></li>'+#13#10;

  if FFacebookAppid <> '' then
    result := result +
      '<li><a href="https://www.facebook.com/dialog/oauth?client_id='+FFacebookAppid+'&redirect_uri='+authurl+'&state='+MakeLoginToken(id, apFacebook)+'">Login through Facebook</a></li>'+#13#10;

  if FGoogleAppid <> '' then
    result := result +
      '<li><a href="https://accounts.google.com/o/oauth2/auth?client_id='+FGoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+authurl+'&state='+MakeLoginToken(id, apGoogle)+'">Login through Google</a></li>'+#13#10;

  result := result +'<li>Authenticate to the Server Administrator directly using Skype (token = '+id+'),<br/> then <a href="/oauth2/auth_skype?id='+id+'">click here</a></li>'+#13#10;
  result := result +
    '<li>Or login directly (if you have an account): <form method="POST" action="'+path+'">'+
    '<input type="hidden" name="id" value="'+id+'"/>'+
    'Username <input type="text" name="username"/> <br/>'+
    'Password <input type="password" name="password"/> <br/>'+
    '<input type="submit" value="Login"/>'+
    '</form></li>'+#13#10;

end;

function TAuth2Server.CheckLoginToken(state: string; var original : String; var provider : TFHIRAuthProvider): Boolean;
var
  i : integer;
  token : TFhirLoginToken;
begin
  FLock.Lock;
  try
    i := FLoginTokens.Indexof(state);
    result := i <> -1;
    if result then
    begin
      token := TFhirLoginToken(FLoginTokens.Objects[i]);
      original := token.FPath;
      provider := token.FProvider;
      token.free;
      FLoginTokens.Delete(i);
    end;
  finally
    FLock.Unlock;
  end;
end;

(*

function TFhirWebServer.ProcessOAuthLogin(path, url, ip : String; request: TFHIRRequest; response: TFHIRResponse; var msg : String; secure : boolean): Boolean;
var
  token, expires, state, id, name, original, email, pname, idt : String;
  provider : TFHIRAuthProvider;
  ok : boolean;
begin
  if url = 'auth-login' then
  begin
    // direct login
    pname := request.Parameters.GetVar('provider');
    token := request.Parameters.GetVar('access_token');
    idt := request.Parameters.GetVar('id_token');
    expires := request.Parameters.GetVar('expires');
    if pname = '' then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Provider Required', HTTP_ERR_BAD_REQUEST);
    if (pname = 'google') then
      provider := apGoogle
    else if (pname = 'facebook') then
      provider := apFacebook
    else if (pname = 'custom') then
      provider := apInternal
    else
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Provider Value invalid (facebook or google)', HTTP_ERR_BAD_REQUEST);
    if (provider <> apInternal) and (token = '') then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Access Token Required', HTTP_ERR_BAD_REQUEST);
    if (pname = 'google') and (idt = '') then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'ID Token required for google logins', HTTP_ERR_BAD_REQUEST);
    if expires = '' then
      expires := '1800'; // 30min
    if provider = apGoogle then
      result := GoogleGetDetails(token, FGoogleAppKey, id, name, email, msg)
    else if provider = apFacebook then
      result := FacebookGetDetails(token, id, name, email, msg)
    else
    begin
      id := request.Parameters.GetVar('id');
      name := request.Parameters.GetVar('name');
      if pos(request.Parameters.GetVar('secret'), FAppSecrets) = 0 then
        raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Application Secret not recognised', HTTP_ERR_BAD_REQUEST);
      result := true;
    end;
    if not result then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Unable to confirm Access Token: '+msg, HTTP_ERR_BAD_REQUEST);
    request.Session := FFhirStore.RegisterSession(provider, token, '', id, name, email, FSecurePath, expires, ip, FFhirStore.defaultRights);
  end
  else if StringStartsWith(url, 'state/', false) and StringStartsWith(copy(url, 7, $FF), OAUTH_LOGIN_PREFIX, false) then
  begin
    // HL7
    result := CheckLoginToken(copy(url, 7, $FF), original, provider);
    if result then
    begin
      // todo: check the signature
      id := request.Parameters.GetVar('userid');
      name := request.Parameters.GetVar('fullName');
      expires := inttostr(60 * 24 * 10); // 10 days
      request.Session := FFhirStore.RegisterSession(aphl7, '', '', id, name, email, original, expires, ip, FFhirStore.defaultRights);
    end;
  end
  else
  begin
    result := request.Parameters.VarExists('state');
    if result then
    begin
      state := request.Parameters.GetVar('state');
      result := StringStartsWith(state, OAUTH_LOGIN_PREFIX, false);
      if result then
      begin
        result := false;
        if not CheckLoginToken(state, original, provider) then
          msg := 'The state does not match. You may be a victim of a cross-site spoof'
        else if request.Parameters.VarExists('error') then
          msg := request.Parameters.GetVar('error_description')
        else
        begin
          if provider = apGoogle then
          begin
            ok := GoogleCheckLogin(FGoogleAppid, FGoogleAppSecret, OAuthPath(secure), request.Parameters.GetVar('code'), token, expires, msg);
            if ok then
              result := GoogleGetDetails(token, FGoogleAppKey, id, name, email, msg);
          end
          else
          begin
            ok := FacebookCheckLogin(FFacebookAppid, FFacebookAppSecret, OAuthPath(secure), request.Parameters.GetVar('code'), token, expires, msg);
            if ok then
              result := FacebookGetDetails(token, id, name, email, msg);
          end;
          if result then
            request.Session := FFhirStore.RegisterSession(provider, token, '', id, name, email, original, expires, ip, FFhirStore.DefaultRights);
        end;
      end;
    end;
  end;
end;


*)

end.
