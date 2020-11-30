unit auth_manager;

// dynreg: https://github.com/smart-on-fhir/smart-on-fhir.github.io/wiki/Dynamic-Client-Registration

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

// to convert .crt to .pem openssl x509 -in mycert.crt -out mycert.pem -outform PEM

interface

uses
  SysUtils, Classes, Generics.Collections, IniFiles,
  IdContext, IdCustomHTTPServer, IdCookie,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_threads,
  fsl_http, fdb_manager, fdb_dialects,
  fsl_oauth, scim_server, fsl_scim, fsl_crypto, fhir_oauth,
  fhir_objects,  fhir_utilities, fhir_common, fhir_factory,
  session, security, server_config,
  user_manager, utilities, server_context, storage,
  jwt;

Const
  FHIR_COOKIE_NAME = 'fhir-session-idx';
  INTERNAL_SECRET = '\8u8J*O{a0Y78.}o%ql9';

type
  TAuthFailReason = (
    afrNone, // no error
    afrInvalidRequest, // The request is missing a required parameter, includes an invalid parameter value, includes a parameter more than once, or is otherwise malformed.
    afrUnauthorizedClient, // The client is not authorized to request an authorization code using this method.
    afrAccessDenied, //The resource owner or authorization server denied the request.
    afrUnsupportedResponseType, // The authorization server does not support obtaining an authorization code using this method.
    afrInvalidScope, // The requested scope is invalid, unknown, or malformed
    afrServerError, // The authorization server encountered an unexpected condition that prevented it from fulfilling the request. This error code is needed because a 500 Internal Server Error HTTP status code cannot be returned to the client via an HTTP redirect.)
    afrTemporarilyUnavailable // The authorization server is currently unable to handle the request due to a temporary overloading or maintenance of the server.  (This error code is needed because a 503 Service Unavailable HTTP status code cannot be returned to the client via an HTTP redirect.)       Values for the "error" parameter MUST NOT include characters        outside the set %x20-21 / %x23-5B / %x5D-7E.
  );

  TAuthLaunchParams = (alpPatient, alpEncounter);
  TAuthLaunchParamsSet = set of TAuthLaunchParams;

const
  AuthLaunchParams_All = [alpPatient..alpEncounter];

type

  EAuthClientException = class (EFHIRException)
  private
    FLocation : String;
    FState : String;
    FReason : TAuthFailReason;
  public
    constructor Create(message : String; reason : TAuthFailReason; location, state : String); overload;
  end;

  TDictionaryStringString = TDictionary<String, String>;
  TGetPatientsEvent = procedure (details : TFslStringDictionary) of object;
  TProcessLaunchParamsEvent = function (request: TIdHTTPRequestInfo; session : TFhirSession; launchContext : String; params : TAuthLaunchParamsSet) : TDictionaryStringString of object;
//  TDoSearchEvent = function (session : TFhirSession; rtype : string; lang, params : String) : TFHIRBundleW of object;

  TFhirLoginToken = Class (TFslObject)
  private
    FProvider : TFHIRAuthProvider;
    FPath : String;
    FExpires : TDateTime;
  public
    Function Link : TFhirLoginToken; overload;
  end;

  // this is a server that lives at /oauth2 (or elsewhere, if configured)
  TAuth2Server = class abstract (TFslObject)
  private
    FFactory : TFHIRFactory;
    FLock : TFslLock;
    FServerContext : TFHIRServerContext;
    FOnProcessFile : TProcessFileEvent;
    FSSLPort : String;
    FHost : String;

    FFacebookAppid : String;
    FFacebookAppSecret : String;
    FLoginTokens : TFslMap<TFhirLoginToken>;
    FGoogleAppid : String;
    FGoogleAppSecret : String;
    FGoogleAppKey : String;
//    FAppSecrets : String;
    FHL7Appid : String;
    FHL7AppSecret : String;
    FAdminEmail : String;
    FUserProvider : TFHIRUserProvider;
    FEndPoint: String;
    FRawPath : String;
    FPath: String;
    FActive : boolean;
    FPassword : String;
    FNonceList : TStringList;
    FOnGetPatients : TGetPatientsEvent;
    FOnProcessLaunchParams : TProcessLaunchParamsEvent;
    FRelPath: String;

    Procedure HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    procedure HandleTokenBearer(AContext: TIdContext; request: TIdHTTPRequestInfo; params: THTTPParameters; response: TIdHTTPResponseInfo);
    procedure HandleTokenOAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
    Procedure HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleKeyToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters;response: TIdHTTPResponseInfo);
    Procedure HandleRegistration(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
    procedure HandleTokenJWT(AContext: TIdContext; request: TIdHTTPRequestInfo; params: THTTPParameters; response: TIdHTTPResponseInfo);
    Procedure HandleEndAllSessions(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);

    function checkNotEmpty(v, n, location, state : String): String;
    function isAllowedRedirect(client : TRegisteredClientInformation; redirect_uri: String): boolean;
    function isAllowedAud(client_id, aud_uri: String): boolean;
    procedure SetServerContext(const Value: TFHIRServerContext);
    function BuildLoginList(id : String) : String;
    Function CheckLoginToken(state : string; var original : String; var provider : TFHIRAuthProvider):Boolean;
    procedure SetUserProvider(const Value: TFHIRUserProvider);
    function nonceIsUnique(nonce : String) : boolean;
    procedure readScopes(scopes: TStringList; params: THTTPParameters);
    procedure loadScopeVariables(variables: TFslMap<TFHIRObject>; scope: String; user : TSCIMUser);
    function GetPatientListAsOptions(launch : String) : String;
    procedure populateFromConsent(consentKey : integer; session : TFhirSession);
  public
    constructor Create(factory : TFHIRFactory; ini : TFHIRServerConfigFile; Host, SSLPort, path : String);
    destructor Destroy; override;

    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
    Procedure HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    procedure setCookie(response: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);

    property ServerContext : TFHIRServerContext read FServerContext write SetServerContext;
    property OnProcessFile : TProcessFileEvent read FOnProcessFile write FOnProcessFile;

    property FacebookAppid : String read FFacebookAppid;
    property GoogleAppid : String read FGoogleAppid;
    property GoogleAppKey : String read FGoogleAppKey;
    property HL7Appid : String read FHL7Appid;
    function MakeLoginToken(path: String; provider: TFHIRAuthProvider): String;
    property AdminEmail : String read FAdminEmail write FAdminEmail;
    property path : String read FPath write FPath;
    property relPath : String read FRelPath write FRelPath;

    function AuthPath : String;
    function BasePath : String;
    function TokenPath : String;
    function RegisterPath : String;
    function ManagePath : String;
    function KeyPath : String;
    function CavsPath : String;
    Property EndPoint : String read FEndPoint write FEndPoint;
    property UserProvider : TFHIRUserProvider read FUserProvider write SetUserProvider;
    property Active : boolean read FActive write FActive;
    property Host : String read FHost write FHost;
    property OnGetPatients : TGetPatientsEvent read FOnGetPatients write FOnGetPatients;
    property OnProcessLaunchParams : TProcessLaunchParamsEvent read FOnProcessLaunchParams write FOnProcessLaunchParams;
  end;


const
  CODES_TAuthFailReason : array [TAuthFailReason] of String = ('', 'invalid_request', 'unauthorized_client', 'access_denied', 'unsupported_response_type', 'invalid_scope', 'server_error', 'temporarily_unavailable');

implementation

uses
  fsl_logging;

{ TAuth2Server }

constructor TAuth2Server.Create(factory : TFHIRFactory; ini : TFHIRServerConfigFile; Host, SSLPort, path : String);
begin
  inherited create;
  FFactory := factory;
  FHost := host;
  FSSLPort := SSLPort;
  FLock := TFslLock.Create('auth-server');
  FLoginTokens := TFslMap<TFhirLoginToken>.create('auth.server.login');
  FNonceList := TStringList.create;
  FNonceList.Sorted := true;

  FHL7Appid := ini.identityProviders.section['hl7.org']['app-id'].value;
  FHL7AppSecret := ini.identityProviders.section['hl7.org']['app-secret'].value;
  FFacebookAppid := ini.identityProviders.section['facebook.com']['app-id'].value;
  FFacebookAppSecret := ini.identityProviders.section['facebook.com']['app-secret'].value;
  FGoogleAppid := ini.identityProviders.section['google.com']['app-id'].value;
  FGoogleAppSecret := ini.identityProviders.section['google.com']['app-secret'].value;
  FGoogleAppKey := ini.identityProviders.section['google.com']['app-key'].value;
  FPassword := ini.admin['password'].value;
  FRawPath := path;
  FPath := path+'/auth';
  FRelPath := '/auth';
end;

destructor TAuth2Server.Destroy;
begin
  FNonceList.Free;
  FLoginTokens.Free;;
  FLock.Free;
  FServerContext.Free;
  FUserProvider.free;
  FFactory.Free;
  inherited;
end;


function TAuth2Server.BasePath: String;
begin
  if FSSLPort = '443' then
    result := 'https://'+FHost+FPath
  else
    result := 'https://'+FHost+':'+FSSLPort+FPath;
end;

function TAuth2Server.MakeLoginToken(path : String; provider : TFHIRAuthProvider): String;
var
  login : TFhirLoginToken;
  t : TDateTime;
  st : TStringlist;
  s : String;
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
      FLoginTokens.Add(result, login.link);
    finally
      login.free;
    end;
    st := TStringList.Create;
    try
      for s in FLoginTokens.Keys do
      begin
        login := FLoginTokens[s];
        if login.FExpires < t then
          st.Add(s)
      end;
      for s in st do
        FLoginTokens.Remove(s);
    finally
      st.Free;
    end;
  finally
    FLock.Unlock;
  end;
end;


function TAuth2Server.ManagePath: String;
begin
  result := FRelPath+'/manage';
end;

function TAuth2Server.nonceIsUnique(nonce: String): boolean;
var
  i : integer;
begin
  FLock.Lock;
  try
    result := not FNonceList.Find(nonce, i);
    if result then
      FNonceList.Add(nonce);
  finally
    FLock.Unlock;
  end;
end;

function TAuth2Server.checkNotEmpty(v, n, location, state : String) : String;
begin
  if (v = '') then
    raise EAuthClientException.create('Parameter "'+n+'" not found', afrInvalidRequest, location, state);
  result := v;
end;

function TAuth2Server.isAllowedAud(client_id, aud_uri: String): boolean;
begin
  result := (aud_uri = EndPoint);
end;

function TAuth2Server.isAllowedRedirect(client : TRegisteredClientInformation; redirect_uri: String): boolean;
begin
  result := (client <> nil) and (client.mode = rcmOAuthClient) and (client.redirects.indexof(redirect_uri) > -1);
end;

function TAuth2Server.KeyPath: String;
begin
  result := BasePath+'/auth_key';
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

procedure TAuth2Server.SetServerContext(const Value: TFHIRServerContext);
begin
  FServerContext.Free;
  FServerContext := Value;
end;

procedure TAuth2Server.SetUserProvider(const Value: TFHIRUserProvider);
begin
  FUserProvider.Free;
  FUserProvider := Value;
end;

function TAuth2Server.TokenPath: String;
begin
 result := FRelPath+'/token';
end;

procedure TAuth2Server.HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : THTTPParameters; response: TIdHTTPResponseInfo);
var
  client_id : String;
  scope : String;
  redirect_uri : String;
  state : String;
  aud : String;
  id : String;
  message : String;
  b : TStringBuilder;
  ok : boolean;
  variables : TFslMap<TFHIRObject>;
  client : TRegisteredClientInformation;
begin
  client_id := checkNotEmpty(params['client_id'], 'client_id', '', '');
  client := ServerContext.Storage.getClientInfo(client_id);
  try
    if client = nil then
      raise EAuthClientException.create('Unknown Client Identifier "'+client_id+'"', afrNone, '', '');
    redirect_uri := checkNotEmpty(params['redirect_uri'], 'redirect_uri', '', '');
    if not ((client_id = 'c.1') and (redirect_uri = ServerContext.FormalURLSecure+'/internal')) then
      if not isAllowedRedirect(client, redirect_uri) then
        raise EAuthClientException.create('Unacceptable Redirect url "'+redirect_uri+'"', afrNone, '', '');
    state := checkNotEmpty(params['state'], 'state', redirect_uri, state);
    if params['response_type'] <> 'code' then
      raise EAuthClientException.create('Only response_type allowed is ''code''', afrUnauthorizedClient, redirect_uri, state);
    scope := checkNotEmpty(params['scope'], 'scope', redirect_uri, state);
    aud := checkNotEmpty(params['aud'], 'aud', redirect_uri, state);
    if not isAllowedAud(client_id, aud) then
      raise EAuthClientException.create('Unacceptable FHIR Server URL "'+aud+'" (should be '+EndPoint+')', afrInvalidRequest, redirect_uri, state);

    id := newguidid;
    ServerContext.Storage.recordOAuthLogin(id, client_id, scope, redirect_uri, state, params['launch']);
    b := TStringBuilder.Create;
    try
      ok := true;
      variables := TFslMap<TFHIRObject>.create('scim.vars');
      try
        variables.Add('/oauth2', TFHIRSystemString.Create(FPath));
        variables.Add('idmethods', TFHIRSystemString.Create(BuildLoginList(id)));
        variables.Add('client', TFHIRSystemString.Create(client.name));
        variables.Add('client-notes', TFHIRSystemString.Create(message));
        if ok then
          OnProcessFile(request, response, session, '/oauth_login.html', true, variables)
        else
          OnProcessFile(request, response, session, '/oauth_login_denied.html', true, variables)
      finally
        variables.free;
      end;
    finally
      b.Free;
    end;
  finally
    client.free;
  end;
end;

function TAuth2Server.RegisterPath: String;
begin
  result := FRelPath+'/register';
end;

procedure TAuth2Server.HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  client_id, name, authurl: String;
  variables : TFslMap<TFHIRObject>;
  scopes : TStringList;
  redirect, state, scope, launch : String;
begin
  if session = nil then
    raise EAuthClientException.create('User Session not found');

  if FSSLPort = '443' then
    authurl := 'https://'+FHost+FPath
  else
    authurl := 'https://'+FHost+':'+FSSLPort+FPath;

  if not ServerContext.Storage.fetchOAuthDetails(session.key, 2, client_id, name, redirect, state, scope, launch) then
    raise EAuthClientException.create('State Error - session "'+inttostr(session.key)+'" not ready for a choice', afrInvalidRequest, '', '');

  if params['form'] = 'true' then
  begin
    scopes := TStringList.create;
    try
      readScopes(scopes, params);

      session.JWT := TJWT.Create;
      session.jwt.header['kid'] := authurl+'/auth_key'; // cause we'll sign with our SSL certificate
      session.jwt.issuer := FHost;
      session.jwt.expires := session.Expires;
      session.jwt.issuedAt := now;
      session.jwt.id := FHost+'/sessions/'+inttostr(Session.Key);

      if params['user'] = '1' then
      begin
      // if user rights granted
        session.jwt.subject := Names_TFHIRAuthProvider[session.ProviderCode]+':'+session.id;
        session.jwt.name := session.UserName;
        if session.Email <> '' then
          session.jwt.email := session.Email;
      end;
      session.JWTPacked := ServerContext.JWTServices.pack(session.JWT);

      if params['log'] = '1' then
        ServerContext.Storage.SetupRecording(session);

      ServerContext.Storage.recordOAuthChoice(Session.OuterToken, scopes.CommaText, session.JWTPacked, params['patient']);
      if params['patient'] <> '' then
// Will these compartments be freed?
        session.Compartments.Add(TFHIRCompartmentId.Create('Patient', params['patient']));

      session.scopes := scopes.CommaText.Replace(',', ' ');
      ServerContext.Storage.RegisterConsentRecord(session);
      if redirect = 'urn:ietf:wg:oauth:2.0:oob' then
      begin
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentText := 'Redirect Code is not valid redirect? ('+redirect+') - Authorization code: '+session.OuterToken;
      end
      else
        response.Redirect(redirect+'?code='+session.OuterToken+'&state='+state);
    finally
      scopes.Free;
    end;
  end
  else
  begin
    variables := TFslMap<TFHIRObject>.create('scim.vars');
    try
      variables.Add('client', TFHIRSystemString.Create(ServerContext.Storage.getClientName(client_id)));
      variables.Add('/oauth2', TFHIRSystemString.Create(FPath));
      variables.Add('username', TFHIRSystemString.Create(name));
      variables.Add('patient-list', TFHIRSystemString.Create(GetPatientListAsOptions(launch)));
      loadScopeVariables(variables, scope, session.User);
      OnProcessFile(request, response, session, '/oauth_choice.html', true, variables)
    finally
      variables.free;
    end;
  end;
end;

procedure TAuth2Server.HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  obj :  TJsonObject;
begin
  if not FActive then
  begin
    response.ResponseNo := 401;
    response.ResponseText := 'Not Found';
  end
  else
  begin
    obj := TJsonObject.create;
    try
      obj['issuer'] := FHost;
      obj['authorization_endpoint'] := BasePath+'/auth';
      obj['token_endpoint'] := BasePath+'/token';
      obj['register_endpoint'] := BasePath+'/register';
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
end;

procedure TAuth2Server.HandleEndAllSessions(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; response: TIdHTTPResponseInfo);
var
  params : THTTPParameters;
begin
  params := THTTPParameters.Create(request.UnparsedParams);
  try
    ServerContext.SessionManager.EndAllSessions(params['cookie'], request.RemoteIP);
    response.Redirect(FRawPath);
  finally
    params.Free;
  end;
end;

procedure TAuth2Server.HandleKeyToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
begin
  response.ContentType := 'application/jwt';
  response.ContentText := ServerContext.JWTServices.makeJWT;
end;

procedure TAuth2Server.HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
begin
  response.ContentType := 'application/json';
  response.ContentText := ServerContext.JWTServices.makeJWK;
end;

procedure TAuth2Server.HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  id, username, password, domain, state, jwt, redirect, scope, launch : String;
  authurl, token, expires, msg, uid, name, email, client_id : String;
  provider : TFHIRAuthProvider;
  ok : boolean;
  key : integer;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  if params.has('id') and params.has('username') and params.has('password') then
  begin
    id := params['id'];
    ServerContext.Storage.fetchOAuthDetails(id, client_id, redirect, state, scope, launch);

    username := params['username'];
    password := params['password'];

    if not FUserProvider.CheckLogin(username, password, key) then
      raise EAuthClientException.create('Login failed', afrAccessDenied, redirect, state);

    if not ServerContext.Storage.hasOAuthSession(id, 1) then
      raise EAuthClientException.create('State failed - no login session active', afrAccessDenied, redirect, state);

    session := ServerContext.SessionManager.RegisterSession(userLogin, apInternal, '', id, username, '', '', '', '1440', AContext.Binding.PeerIP, '');
    try
      ServerContext.Storage.UpdateOAuthSession(id, 2, session.key, client_id);
      session.SystemName := client_id;
      session.SystemEvidence := systemFromOAuth;
      setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
      response.Redirect(FPath+'/auth_choice');
    finally
      session.Free;
    end;
  end
  else if request.document.startsWith(FPath+'/auth_dest/state/') then
  begin
    // HL7
    if not CheckLoginToken(copy(request.document, 26, $FF), id, provider) then
      raise EAuthClientException.create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)', afrInvalidRequest, redirect, state);
    uid := params['userid'];
    name := params['fullName'];
    expires := inttostr(60 * 24 * 10); // 10 days
    session := ServerContext.SessionManager.RegisterSession(userExternalOAuth, aphl7, '', id, uid, name, '', '', expires, AContext.Binding.PeerIP, '');
    try
      ServerContext.Storage.updateOAuthSession(id, 2, session.key, client_id);
      session.SystemName := client_id;
      session.SystemEvidence := systemFromOAuth;
      setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
      response.Redirect(FPath+'/auth_choice');
    finally
      session.Free;
    end;
  end
  else if (params.has('state')) then
  begin
    if FSSLPort = '443' then
      authurl := 'https://'+FHost+FPath+'/auth_dest'
    else
      authurl := 'https://'+FHost+':'+FSSLPort+FPath+'/auth_dest';

    state := params['state'];
    if not StringStartsWith(state, OAUTH_LOGIN_PREFIX, false) then
      raise EAuthClientException.create('State Prefix mis-match', afrInvalidRequest, redirect, state);
    if not CheckLoginToken(state, id, provider) then
      raise EAuthClientException.create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)', afrInvalidRequest, redirect, state);
    if params.has('error') then
      raise EAuthClientException.create('error_description', afrServerError, redirect, state);

    if provider = apGoogle then
    begin
      ok := GoogleCheckLogin(FGoogleAppid, FGoogleAppSecret, authurl, params['code'], token, expires, jwt, msg);
      if ok then
        ok := GoogleGetDetails(token, FGoogleAppKey, jwt, uid, name, email, msg);
    end
    else
    begin
      ok := FacebookCheckLogin(FFacebookAppid, FFacebookAppSecret, authurl, params['code'], token, expires, msg);
      if ok then
        ok := FacebookGetDetails(token, uid, name, email, msg);
    end;
    if not ok then
      raise EAuthClientException.create('Processing the login failed ('+msg+')', afrAccessDenied, redirect, state);
    session := ServerContext.SessionManager.RegisterSession(userExternalOAuth, provider, token, id, uid, name, email, '', expires, AContext.Binding.PeerIP, '');
    try
      ServerContext.Storage.updateOAuthSession(id, 2, session.key, client_id);
      session.SystemName := client_id;
      session.SystemEvidence := systemFromOAuth;
      setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
      response.Redirect(FPath+'/auth_choice');
    finally
      session.Free;
    end;
  end
  else
    raise EAuthClientException.create('Login attempt not understood', afrInvalidRequest, redirect, state);
end;

//  "grant_types" : "client_credentials",
//  "jwks" : {
//    "keys" : [
//      {
//        "e" : "AQAB",
//        "kty" : "RSA",
//        "n" : "pWGCPEp8PNjfiTHNM_iB5JqC4SyfVJoAR8urI1guoFtfPPlH2c_ZO1p4S0W1rAy8qi_lLLOKmiTL2JDqd6xUA6AHcf8Fr7cwAJiqQBd-3AMOfm7fSIgVgKQXCFQAzTAefJYq4f7ydnkocrDuKSH29QhhlBqVIu3TwKNQUGs6Owk1HRRtxIlxHJRCNEbA-AFdjT4A5JKztyYaA5IiWr2cgU-q71_SeMDWUNHZNeBO7HJu0Jh7uX-9xhpSr8J3zt9kMBbFzIwH1ycglJ9e4yhFA5y5VE-ZjBvSnDrfC5J22IoEvzBOSuzy33D6Y4vbH26g7slh90atsMbzuTHJu7iaQQ"
//      }
//    ]
//  },
//  "response_types" : "token",
//  "token_endpoint_auth_method" : "client_secret_post"

procedure TAuth2Server.HandleRegistration(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; response: TIdHTTPResponseInfo);
var
  json, resp : TJsonObject;
  ac, ar : TJsonArray;
  n : TJsonNode;
  u : String;
  client : TRegisteredClientInformation;
  function checkPresent(name : String) : String;
  begin
    result := json.str[name];
    if result = '' then
      raise EAuthClientException.create('A '+name+' field is required in the json token');
  end;
  procedure checkValue(name, value : String);
  begin
    if json.str[name] <> value then
      raise EAuthClientException.create('The '+name+' field must have the value "'+value+'" but has the value "'+json.str[name]+'"');
  end;
  function checkPresentA(name : String) : TJsonArray;
  begin
    result := json.arr[name];
    if (result = nil) or (result.Count < 1) then
      raise EAuthClientException.create('A value for '+name+' field is required in the json token');
  end;
  procedure checkValueA(name, value, opt : String);
  var
    arr : TJsonArray;
    e : TJsonNode;
    s : String;
  begin
    arr := json.arr[name];
    if (arr = nil) or (arr.Count < 1) then
      raise EAuthClientException.create('The '+name+' field must have a value "'+value);
    for e in arr do
    begin
      s := (e as TJsonString).value;
      if (s = value) then
        exit;
      if (s <> opt) and (opt <> '') then
        raise EAuthClientException.create('The '+name+' field has an unacceptable value "'+s+'"');
    end;
    raise EAuthClientException.create('The '+name+' field must have the value "'+value+'" but it was not found');
  end;
begin
  try
    // parse the json
    json := TJSONParser.Parse(request.PostStream);
    try
      resp := TJsonObject.Create;
      try
        client := TRegisteredClientInformation.Create;
        try
//          disabled after discussion with Luis Maas - pending further discussion
//             - determine when this should be done
//          if Session = nil then
//            raise EFHIRException.create('User must be identified; log in to the server using the web interface, and get a token that can be used to register the client');
          // check that it meets business rules
          client.name := checkPresent('client_name');
          resp.str['client_name'] := client.name;

          client.url := json.str['client_uri'];
          resp.str['client_uri'] := client.url;

          client.logo := json.str['logo_uri'];
          resp.str['logo_uri'] := client.logo;

          client.softwareId := json.str['software_id'];
          resp.str['software_id'] := client.softwareId;

          client.softwareVersion := json.str['software_version'];
          resp.str['software_version'] := client.softwareVersion;

          ac := json.arr['redirect_uris'];
          ar := resp.forceArr['redirect_uris'];
          if (ac <> nil) then
          begin
            for n in ac do
            begin
              u := (n as TJsonString).value;
              client.redirects.Add(u);
              ar.add(u);
            end;
          end;

          checkPresentA('grant_types');
          checkPresentA('response_types');
          checkPresent('token_endpoint_auth_method');
          if (json.str['token_endpoint_auth_method'] = 'private_key_jwt') then // backend services
          begin
            client.mode := rcmBackendServices;
            checkValueA('grant_types', 'client_credentials', '');
            resp.forceArr['grant_types'].add('client_credentials');
            checkValueA('response_types', 'token', '');
            resp.forceArr['response_types'].add('token');
            if json.obj['jwks'] = nil then
              raise EAuthClientException.create('No jwks found');
            if json.obj['jwks'].arr['keys'] = nil then
              raise EAuthClientException.create('No keys found in jwks');
            if json.obj['jwks'].arr['keys'].Count = 0 then
              raise EAuthClientException.create('No Keys found in jwks.keys');
            client.publicKey := TJsonWriter.writeObjectStr(json.obj['jwks']);
            client.issuer := checkPresent('issuer');
          end
          else if (json.str['token_endpoint_auth_method'] = 'client_secret_basic') then // confidential
          begin
            checkValueA('grant_types', 'authorization_code', 'refresh_token');
            resp.forceArr['grant_types'].add('authorization_code');
            checkValueA('response_types', 'code', '');
            resp.forceArr['response_types'].add('code');
            client.mode := rcmBackendServices;
            client.secret := newGuidId;
            resp.str['client_secret'] := client.secret;
          end
          else if (json.str['token_endpoint_auth_method'] = 'none') then // public
          begin
            checkValueA('grant_types', 'authorization_code', 'refresh_token');
            resp.forceArr['grant_types'].add('authorization_code');
            checkValueA('response_types', 'code', '');
            resp.forceArr['response_types'].add('code');
            client.mode := rcmBackendServices;
            // no secret
          end
          else
            raise EAuthClientException.create('Unable to recognise client mode');
          client.patientContext := json.bool['fhir_patient_context'];
          resp.str['client_id'] := ServerContext.Storage.storeClient(client, 0 {session.Key});
          resp.str['client_id_issued_at'] := IntToStr(DateTimeToUnix(now));
          response.ContentText := TJSONWriter.writeObjectStr(resp);
          response.ResponseNo := 201;
          response.ResponseText := 'OK';
          response.ContentType := 'application/json';
        finally
          client.Free;
        end;
      finally
        resp.Free;
      end;
    finally
      json.Free;
    end;
  except
    on e : Exception do
    begin
      json := TJsonObject.create;
      try
        json.str['error'] := 'invalid_client';
        json.str['error_description'] := e.Message;
        response.ContentText := TJSONWriter.writeObjectStr(json);
        response.ResponseNo := 400;
        response.ResponseText := 'Client Error';
        response.ContentType := 'application/json';
      finally
        json.Free;
      end;
    end;
  end;
end;

procedure TAuth2Server.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
var
  params : THTTPParameters;
begin
  if not FActive then
  begin
    response.ResponseNo := 401;
    response.ResponseText := 'Not Found';
  end
  else
  begin
    Logging.log('Auth: '+request.Document);
    try
      // cors
      response.CustomHeaders.add('Access-Control-Allow-Origin: *');
      response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
      response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS');
      if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
        response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
      response.ContentType := 'application/json';

      params := THTTPParameters.create(request.UnparsedParams);
      try
        if (secure and (request.Document = FPath+'/auth')) then
          HandleAuth(AContext, request, session, params, response)
        else if (secure and (request.Document.startsWith(FPath+'/auth_dest'))) then
          HandleLogin(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/auth_choice')) then
          HandleChoice(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/token')) then
          HandleToken(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/token_data')) then
          HandleTokenData(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/auth_skype')) then
          HandleSkype(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/auth_key')) then
          HandleKey(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/auth_jwt')) then
          HandleKeyToken(AContext, request, session, params, response)
        else if (secure and (request.Document = FPath+'/discovery')) then
          HandleDiscovery(AContext, request, response)
        else if (request.Document = FPath+'/register') then
          HandleRegistration(AContext, request, session, response)
        else if (secure and (request.Document = FPath+'/userdetails')) then
          HandleUserDetails(AContext, request, session, params, response)
        else if (request.Document = FPath+'/logout-all') then
          HandleEndAllSessions(AContext, request, session, response)
        else
          raise EAuthClientException.create('Invalid URL');
      finally
        params.Free;
      end;
    except
      on e : EAuthClientException do
      begin
        if (e.FLocation = '') or (e.FLocation = 'urn:ietf:wg:oauth:2.0:oob') then
        begin
          response.ResponseNo := 400;
          response.ResponseText := 'Bad Request';
          response.ContentText := e.Message;
        end
        else
          response.Redirect(e.FLocation+'?error='+CODES_TAuthFailReason[e.FReason]+'&error_description='+EncodeMIME(e.Message)+'&state='+e.FState);
      end;
      on e : Exception do
      begin
        Logging.log('Auth Exception: '+e.Message);
        recordStack(e);
        raise;
      end;
    end;
  end;
end;

procedure TAuth2Server.HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  token, id, name, email, password, domain, client_id : String;
  variables : TFslMap<TFHIRObject>;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));
  if params['form'] <> '' then
  begin
    token := checkNotEmpty(params['token'], 'token', '', '');
    id := checkNotEmpty(params['id'], 'id', '', '');
    name := checkNotEmpty(params['name'], 'name', '', '');
    email := checkNotEmpty(params['email'], 'email', '', '');
    password := checkNotEmpty(params['password'], 'password', '', '');

    if FPassword <> password then
      raise EAuthClientException.create('Admin Password fail');

    // update the login record
    // create a session
    session := ServerContext.SessionManager.RegisterSession(userExternalOAuth, apInternal, '', token, id, name, email, '', inttostr(24*60), AContext.Binding.PeerIP, '');
    try
      ServerContext.Storage.updateOAuthSession(token, 2, session.Key, client_id);
      session.SystemName := client_id;
      session.SystemEvidence := systemFromOAuth;
    finally
      session.Free;
    end;
    response.ContentText := 'done';
  end
  else if params['id'] <> '' then
  begin
    if not ServerContext.SessionManager.GetSessionByToken(params['id'], session) then
      raise EAuthClientException.create('State Error (1)');
    try
      if not ServerContext.Storage.hasOAuthSessionByKey(session.Key, 2) then
        raise EAuthClientException.create('State Error (2)');
      setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
      response.Redirect(FPath+'/auth_choice');
    finally
      session.Free;
    end
  end
  else
  begin
    variables := TFslMap<TFHIRObject>.create('scim.vars');
    try
      variables.Add('/oauth2', TFHIRSystemString.create(FPath));
      OnProcessFile(request, response, session, FPath+'/auth_skype.html', true, variables);
    finally
      variables.free;
    end;
  end;
end;

function readFromScope(scope, name : String) : String;
var
  i : integer;
  list : TStringList;
begin
  result := '';
  list := TStringList.create;
  try
    list.CommaText := scope.Replace(' ', ',');
    for i := 0 to list.Count - 1 do
      if (list[i].StartsWith(name+':')) then
        result := list[i].Substring(name.Length+1)
  finally
    list.free;
  end;
end;

function readLaunchScopes(scope : String): TAuthLaunchParamsSet;
var
  i : integer;
  list : TStringList;
begin
  result := [];
  list := TStringList.create;
  try
    list.CommaText := scope.Replace(' ', ',');
    for i := 0 to list.count - 1 do
      if list[i] = 'launch' then
        result := result + AuthLaunchParams_All
      else if list[i] = 'launch/patient' then
        result := result + [alpPatient]
      else if list[i] = 'launch/encounter' then
        result := result + [alpEncounter];
  finally
    list.free;
  end;
end;

procedure TAuth2Server.HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  json : TJSONWriter;
begin
  if params['grant_type'] = 'authorization_code' then
    HandleTokenOAuth(AContext, request, session, params, response)
  else if params['grant_type'] = 'client_credentials' then
  begin
     if params['client_assertion_type'] = 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer' then
       HandleTokenJWT(AContext, request, params, response) // smart backend services
     else
       HandleTokenBearer(AContext, request, params, response)
  end
  else
  begin
    response.ResponseNo := 500;
    response.ContentType := 'application/json';
    json := TJsonWriterDirect.create;
    try
      json.Start(true);
      json.Value('error', 'unsupported_grant_type');
      json.Value('error_description', 'Unknown Grant Type '+params['grant_type']);
      json.Finish(true);
      response.ContentText := json.ToString;
    finally
      json.Free;
    end;
  end;
end;

procedure TAuth2Server.HandleTokenBearer(AContext: TIdContext; request: TIdHTTPRequestInfo; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  errCode, jwtStored, domain : string;
  json : TJsonWriter;
  jwt : TJWT;
  expiry : TDateTime;
  PatientId : String;
  ConsentKey, SessionKey : integer;
  uuid : string;
  session : TFhirSession;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  response.ContentType := 'application/json';
  try
    errCode := 'invalid_request';
    if not (request.AuthUsername = INTERNAL_SECRET) then
      raise EAuthClientException.create('Can only call grant_type=client_credentials if the Authorization Header has a Bearer Token');
    try
      jwt := TJWTUtils.unpack(request.AuthPassword, false, nil); // todo:
    except
      on e : exception do
        raise EAuthClientException.create('Error reading JWT: '+e.message);
    end;
    try
      uuid := params['scope'];
      if not ServerContext.Storage.FetchAuthorization(uuid, PatientId, ConsentKey, SessionKey, Expiry, jwtStored) then
        raise EAuthClientException.create('Unrecognised Token');
      if (jwtStored <> request.AuthPassword) then
        raise EAuthClientException.create('JWT mismatch');
      if (expiry < now) then
        raise EAuthClientException.create('The authorization has expired');

      session := ServerContext.SessionManager.RegisterSessionFromPastSession(userBearerJWT, sessionKey, expiry, AContext.Binding.PeerIP);
      try
        session.SystemName := jwt.subject;
        session.SystemEvidence := systemFromJWT;
        session.Compartments.Add(TFHIRCompartmentId.Create('Patient', PatientId));
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        if FFactory.version <> fhirVersionRelease2 then
          populateFromConsent(consentKey, session);
        json := TJsonWriterDirect.create;
        try
          json.Start;
          json.Value('access_token', session.Cookie);
          json.Value('token_type', 'Bearer');
          json.Value('expires_in', trunc((session.Expires - now) / DATETIME_SECOND_ONE));
//          json.Value('id_token', session.JWTPacked);
          json.Value('scope', session.scopes);
          json.Value('patient', session.Compartments[0].id);
          json.Finish;
          response.ContentText := json.ToString;
        finally
          json.Free;
        end;
      finally
        session.Free;
      end;
    finally
      jwt.Free;
    end;
    response.ResponseNo := 200;
  except
    on e : Exception do
    begin
      response.ResponseNo := 500;
      json := TJsonWriterDirect.create;
      try
        json.Start;
        json.Value('error', errCode);
        json.Value('error_description', e.Message);
        json.Finish;
        response.ContentText := json.ToString;
      finally
        json.Free;
      end;
    end;
  end;
end;

procedure TAuth2Server.HandleTokenJWT(AContext: TIdContext; request: TIdHTTPRequestInfo; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
//  jwtStored,
  errCode, domain : string;
  json : TJsonWriter;
  jwt : TJWT;
  jwk : TJWKList;
//  expiry : TDateTime;
//  PatientId : String;
//  ConsentKey, SessionKey : integer;
  session : TFhirSession;
  client : TRegisteredClientInformation;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  response.ContentType := 'application/json';
  try
    errCode := 'invalid_request';
    try
      jwt := TJWTUtils.unpack(params['client_assertion'], false, nil); // todo:
    except
      on e : exception do
        raise EAuthClientException.create('Error reading JWT: '+e.message);
    end;
    try
      client := ServerContext.Storage.getClientInfo(jwt.subject);
      try
        // validate the signature on the JWT
        // check that the JWT exp is valid
        // check that this is not a jti value seen before (prevention of replay attacks)
        // ensure that the client_id provided is valid etc
        if client = nil then
          raise EAuthClientException.create('Unknown client_id "'+jwt.subject+'"');
        if jwt.issuer <> client.issuer then
          raise EAuthClientException.create('Stated Issuer does not match registered issuer');
        if not nonceIsUnique(jwt.id) then
          raise EAuthClientException.create('Repeat Nonce Token - not allowed');
        if (jwt.expires > TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE)then
          raise EAuthClientException.create('JWT Expiry is too far in the future - ('+FormatDateTime('c', jwt.expires)+', must be < 5 minutes)');
        if (jwt.expires < TFslDateTime.makeUTC.DateTime) then
          raise EAuthClientException.create('JWT expiry ('+TFslDateTime.make(jwt.expires, dttzUTC).toXML+') is too old');

        jwk := TJWKList.create(client.publicKey);
        try
          TJWTUtils.unpack(params['client_assertion'], true, jwk).Free;
        finally
          jwk.Free;
        end;
        // all done... now create the session
        session := ServerContext.SessionManager.CreateImplicitSession(AContext.Binding.PeerIP, jwt.subject, 'n/a', systemFromJWT, false, true);
        try
          session.SystemName := jwt.subject;
          session.SystemEvidence := systemFromJWT;
          session.scopes := params['scope'];
          json := TJsonWriterDirect.create;
          try
            json.Start;
            json.Value('access_token', session.Cookie);
            json.Value('token_type', 'Bearer');
            json.Value('expires_in', trunc((session.Expires - TFslDateTime.makeUTC.DateTime) / DATETIME_SECOND_ONE));
            json.Value('scope', session.scopes);
            json.Finish;
            response.ContentText := json.ToString;
          finally
            json.Free;
          end;
        finally
          session.Free;
        end;
      finally
        client.free;
      end;
    finally
      jwt.Free;
    end;
    response.ResponseNo := 200;
  except
    on e : Exception do
    begin
      response.ResponseNo := 500;
      json := TJsonWriterDirect.create;
      try
        json.Start;
        json.Value('error', errCode);
        json.Value('error_description', e.Message);
        json.Finish;
        response.ContentText := json.ToString;
      finally
        json.Free;
      end;
    end;
  end;
end;

procedure TAuth2Server.HandleTokenOAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);

var
  code, clientId, clientSecret, uri, errCode, client_id, n : string;
  pclientid, pname, predirect, pstate, pscope : String;
  json : TJSONWriter;
  buffer : TFslMemoryStream;
  launch, scope : String;
  launchParams : TAuthLaunchParamsSet;
  launchParamValues : TDictionary<String, String>;
  client : TRegisteredClientInformation;
begin
  buffer := TFslMemoryStream.Create;
  try
    try
      // first, we check the fixed value

      // now, check the code
      errCode := 'invalid_request';
      code := checkNotEmpty(params['code'], 'code', '', '');
      if not ServerContext.SessionManager.GetSessionByToken(code, session) then // todo: why is session passed in too?
        raise EAuthClientException.create('Authorization Code not recognized');
      try
        if not ServerContext.Storage.fetchOAuthDetails(session.key, 3, pclientid, pname, predirect, pstate, pscope, launch) then
          raise EAuthClientException.create('Authorization Code not recognized (2)');
        client := ServerContext.Storage.getClientInfo(pclientid);
        try
          // what happens now depends on whether there's a client secret or not
          if (client.secret = '') then
          begin
            // user must supply the correct client id
            errCode := 'invalid_client';
            clientId := checkNotEmpty(params['client_id'], 'client_id', '', '');
            if clientId <> pclientid then
              raise EAuthClientException.create('Client Id is wrong ("'+clientId+'") is wrong in parameter');
          end
          else
          begin
            // client id and client secret must be in the basic header. Check them
            clientId := request.AuthUsername;
            clientSecret := request.AuthPassword;
            if (clientId = '') and (clientSecret = '') then // R support
            begin
              clientId := params['client_id'];
              clientSecret := params['client_secret'];
            end;
            if clientId <> pclientid then
              raise EAuthClientException.create('Client Id is wrong ("'+clientId+'") in Authorization Header');
            if clientSecret <> client.secret then
              raise EAuthClientException.create('Client Secret in Authorization header is wrong ("'+clientSecret+'")');
          end;

          // now check the redirect URL
          uri := checkNotEmpty(params['redirect_uri'], 'redirect_uri', '', '');
          errCode := 'invalid_request';
          if predirect <> uri then
            raise EAuthClientException.create('Mismatch between claimed and actual redirection URIs');

          // ok, well, it's passed.
          scope := pscope;
          ServerContext.Storage.updateOAuthSession(session.OuterToken, 4, session.Key, client_id);
          if (session.SystemName <> client_id) or (session.SystemName <> clientId) then
            raise EAuthClientException.create('Session client id mismatch ("'+session.SystemName+'"/"'+client_id+'"/"'+clientId+'")');

          launchParams := readLaunchScopes(scope);
          launchParamValues := FOnProcessLaunchParams(request, session, launch, launchParams);
          try
            json := TJsonWriterDirect.create;
            try
              json.Stream := buffer.link;
              json.Start;
              json.Value('access_token', session.Cookie);
              json.Value('token_type', 'Bearer');
              json.Value('expires_in', trunc((session.Expires - now) / DATETIME_SECOND_ONE));
              if session.canGetUser then
                json.Value('id_token', session.JWTPacked);
              json.Value('scope', scope);
              for n in launchParamValues.Keys do
                json.Value(n, launchParamValues[n]);
              json.Finish;
            finally
              json.Free;
            end;
          finally
            launchParamValues.Free;
          end;
        finally
          client.Free;
        end;
      finally
        session.free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        Logging.log('Error in OAuth Token call: '+e.Message+' (params = '+params.Source+')');
        response.ResponseNo := 400;
        json := TJsonWriterDirect.create;
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
    response.ContentType := 'application/json';
    response.ContentText := buffer.Buffer.AsText;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;

procedure TAuth2Server.HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  token, clientId, clientSecret : string;
  json : TJSONWriter;
  buffer : TFslMemoryStream;
  check : boolean;
  client : TRegisteredClientInformation;
begin
  buffer := TFslMemoryStream.Create;
  try
    try
      if request.AuthUsername <> 'Bearer' then
        raise EAuthClientException.create('OAuth2 Access Token is required in the HTTP Authorization Header (type Bearer)');
      token := checkNotEmpty(params['token'], 'token', '', '');
      if request.AuthPassword <> token then
        raise EAuthClientException.create('Access Token Required');

      clientId := checkNotEmpty(params['client_id'], 'client_id', '', '');
      clientSecret := checkNotEmpty(params['client_secret'], 'client_secret', '', '');

      client := ServerContext.Storage.getClientInfo(clientId);
      try
        if client.secret <> clientSecret then
          raise EAuthClientException.create('Client Id or secret is wrong ("'+clientId+'")');

        if not ServerContext.SessionManager.GetSession(token, session, check) then
        begin
          json := TJsonWriterDirect.create;
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
          json := TJsonWriterDirect.create;
          try
            json.Stream := buffer.link;
            json.Start;
            json.Value('active', true);
            json.Value('token_type', 'Bearer');
            json.Value('exp', inttostr(trunc((session.Expires - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
            json.Value('iat', inttostr(trunc((session.FirstCreated - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
            json.Value('scope', session.scopes);
            json.Value('use_count', inttostr(session.useCount));
            if session.canGetUser then
            begin
              json.Value('user_id', Names_TFHIRAuthProvider[session.ProviderCode]+':'+session.id);
              json.Value('user_name', session.UserName);
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
      finally
        client.Free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        json := TJsonWriterDirect.create;
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
    response.ContentText := buffer.Buffer.AsText;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;


procedure TAuth2Server.HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: THTTPParameters; response: TIdHTTPResponseInfo);
var
  variables : TFslMap<TFHIRObject>;
begin
  if session = nil then
    response.Redirect(FPath+'/auth?client_id=web&response_type=code&scope=openid%20profile%20fhirUser%20user/*.*%20'+SCIM_ADMINISTRATOR+'&redirect_uri='+EndPoint+'/internal&aud='+EndPoint+'&state='+MakeLoginToken(EndPoint, apGoogle))
  else
  begin
    if params['form'] = 'true' then
    begin
      raise EFHIRTodo.create('TAuth2Server.HandleUserDetails');
    end
    else
    begin
      variables := TFslMap<TFHIRObject>.create('scim.vars');
      try
        variables.Add('username', TFHIRSystemString.create(session.User.username));
        variables.Add('/oauth2', TFHIRSystemString.create(FPath));
        OnProcessFile(request, response, session, '/oauth_userdetails.html', true, variables)
      finally
        variables.free;
      end;
    end;
  end;
end;

function TAuth2Server.AuthPath: String;
begin
  result := FRelPath+'/auth';
end;

function TAuth2Server.BuildLoginList(id : String): String;
var
  authurl : String;
  path : String;
begin
  path := FPath+'/auth_dest';
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

  result := result +'<li>Authenticate to the Server Administrator directly using Skype (token = '+id+'),<br/> then <a href="'+FPath+'/auth_skype?id='+id+'">click here</a></li>'+#13#10;
  result := result +
    '<li>Or login directly (if you have an account): <form method="POST" action="'+path+'">'+
    '<input type="hidden" name="id" value="'+id+'"/>'+
    'Username <input type="text" name="username"/> <br/>'+
    'Password <input type="password" name="password"/> <br/>'+
    '<input type="submit" value="Login"/>'+
    '</form></li>'+#13#10;

end;

function TAuth2Server.CavsPath: String;
begin
  result := BasePath+'/cavs';
end;

function TAuth2Server.CheckLoginToken(state: string; var original : String; var provider : TFHIRAuthProvider): Boolean;
var
  token : TFhirLoginToken;
begin
  FLock.Lock;
  try
    result := FLoginTokens.TryGetValue(state, token);
    if result then
    begin
      original := token.FPath;
      provider := token.FProvider;
      FLoginTokens.remove(state);
    end;
  finally
    FLock.Unlock;
  end;
end;

{ TFhirLoginToken }

function TFhirLoginToken.Link: TFhirLoginToken;
begin
  result := TFhirLoginToken(inherited link);
end;

function TAuth2Server.GetPatientListAsOptions(launch : String): String;
var
  b : TStringBuilder;
  dict : TFslStringDictionary;
  s : String;
begin
  dict := TFslStringDictionary.Create;
  try
    FOnGetPatients(dict);
    b := TStringBuilder.create;
    try
      b.Append('<option value=""/>');
      for s in dict.Keys do
      begin
        b.Append('<option value="');
        b.Append(s);
        b.Append('"');
        if ((launch <> '') and (launch = 'Patient/'+s)) then
          b.Append(' selected');
        b.Append('>');
        b.Append(dict[s]);
        b.Append(' (');
        b.Append(s);

        b.Append(')</option>');
      end;
      result := b.ToString;
    finally
      b.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TAuth2Server.populateFromConsent(consentKey: integer; session: TFhirSession);
//var
//  consent : TFhirConsent;
//  c : TFhirCoding;
//  s : String;
begin
//  consent := ServerContext.Storage.FetchResource(consentKey) as TFhirConsent;
//  try
//    s := '';
//    !{$IFDEF FHIR3}
//    for c in consent.except_List[0].class_List do
//      if (c.System = 'http://smarthealthit.org/fhir/scopes') then
//        s := s +' '+c.code;
//    {$ELSE}
//    raise EFHIRException.create('This operation is only supported in R3 for now');
//    {$ENDIF}
//    session.scopes := s.Trim;
//  finally
//    consent.Free;
//  end;
//  raise EFslException.Create('Needs further development right now');
end;

procedure TAuth2Server.loadScopeVariables(variables : TFslMap<TFHIRObject>; scope : String; user : TSCIMUser);
//patient/*.read  Permission to read any resource for the current patient
//user/*.*  Permission to read and write all resources that the current user can access
//openid profile  Permission to retrieve information about the current logged-in user
//launch  Permission to obtain launch context when app is launched from an EHR
//launch/patient  When launching outside the EHR, ask for a patient to be selected at launch time
var
  security : TFHIRSecurityRights;
  s, c : String;
begin
  variables.add('userlevel', TFHIRSystemString.create(''));
  variables.add('userinfo', TFHIRSystemString.create(''));
  security := TFHIRSecurityRights.create(ServerContext.ValidatorContext.Link, user, scope, true);
  try
    if security.canAdministerUsers then
      variables.add('useradmin', TFHIRSystemString.create('<input type="checkbox" name="useradmin" value="1"/> Administer Users'))
    else
      variables.add('useradmin', TFHIRSystemString.create(''));
    if security.canGetUserInfo then
      variables['userinfo'] := TFHIRSystemString.create('checked');

  for s in FFactory.ResourceNames do
  begin
    c := CODES_TTokenCategory[FFactory.resCategory(s)];
    variables.AddOrSetValue('read'+c, TFHIRSystemString.create(''));
    variables.AddOrSetValue('write'+c, TFHIRSystemString.create(''));
    variables.AddOrSetValue('read'+c+'disabled', TFHIRSystemString.create('disabled'));
    variables.AddOrSetValue('write'+c+'disabled', TFHIRSystemString.create('disabled'));
  end;

    for s in FFactory.ResourceNames do
    begin
      c := CODES_TTokenCategory[FFactory.resCategory(s)];
      if security.canRead(s) then
      begin
        variables['read'+c] := TFHIRSystemString.create('checked');
        variables['read'+c+'disabled'] := TFHIRSystemString.create('');
      end;
      if security.canWrite(s) then
      begin
        variables['write'+c] := TFHIRSystemString.create('checked');
        variables['write'+c+'disabled'] := TFHIRSystemString.create('');
      end;
    end;
  finally
    security.free;
  end;
end;


procedure TAuth2Server.readScopes(scopes : TStringList; params : THTTPParameters);
var
  pfx : String;
  s : String;
  all : boolean;
begin
  scopes.clear;
  if (params['userInfo'] = '1') then
  begin
    scopes.add('openid');
    scopes.add('profile');
  end;
  if (params['useradmin'] = '1') then
    scopes.add(SCIM_ADMINISTRATOR);

  // if there's a patient, then the scopes a patient specific
  if (params['patient'] = '') then
    pfx := 'User/'
  else
    pfx := 'Patient/';

  all := true;
  for s in FFactory.ResourceNames do
    if (params['read'+CODES_TTokenCategory[Ffactory.resCategory(s)]] <> '1') or
      (params['write'+CODES_TTokenCategory[Ffactory.resCategory(s)]] <> '1') then
      all := false;

  if all then
    scopes.Add(pfx+'*.*')
  else
  begin
  for s in FFactory.ResourceNames do
    begin
      if params['read'+CODES_TTokenCategory[Ffactory.resCategory(s)]] = '1' then
        scopes.Add(pfx+s+'.read');
      if params['write'+CODES_TTokenCategory[Ffactory.resCategory(s)]] = '1' then
        scopes.Add(pfx+s+'.write');
    end;
  end;
end;

{ EAuthClientException }

constructor EAuthClientException.Create(message: String; reason: TAuthFailReason; location, state: String);
begin
  inherited Create(message);
  FReason := reason;
  FState := state;
  FLocation := location;
end;

end.
