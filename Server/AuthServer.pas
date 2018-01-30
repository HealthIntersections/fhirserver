unit AuthServer;

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


// to convert .crt to .pem openssl x509 -in mycert.crt -out mycert.pem -outform PEM

interface

uses
  SysUtils, Classes, System.Generics.Collections, IniFiles,
  IdContext, IdCustomHTTPServer, IdCookie,
  ParseMap, KDBManager, KDBDialects, KCritSct, StringSupport, EncodeSupport, GUIDSupport, DateSupport, TextUtilities,
  AdvObjects, AdvMemories, AdvJSON, AdvExceptions, AdvGenerics,
  FacebookSupport, SCIMServer, SCIMObjects, JWT, SmartOnFhirUtilities,
  FHIRSupport, FHIRBase, FHIRTypes, FHIRResources, FHIRConstants, FHIRSecurity, FHIRUtilities,
  FHIRUserProvider, ServerUtilities, FHIRServerContext, FHIRStorageService, ClientApplicationVerifier,
  JWTService, ApplicationCache;

Const
  FHIR_COOKIE_NAME = 'fhir-session-idx';
  INTERNAL_SECRET = '\8u8J*O{a0Y78.}o%ql9';

type
  TDoSearchEvent = function (session : TFhirSession; rtype : string; lang, params : String) : TFHIRBundle of object;

  TFhirLoginToken = Class (TAdvObject)
  private
    FProvider : TFHIRAuthProvider;
    FPath : String;
    FExpires : TDateTime;
  public
    Function Link : TFhirLoginToken; overload;
  end;

  // predefined token per user for testing

  // this is a server that lives at /oauth2 (or elsewhere, if configured)
  TAuth2Server = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FServerContext : TFHIRServerContext;
    FOnProcessFile : TProcessFileEvent;
    FSSLPort : String;
    FHost : String;

    FFacebookAppid : String;
    FFacebookAppSecret : String;
    FLoginTokens : TAdvMap<TFhirLoginToken>;
    FGoogleAppid : String;
    FGoogleAppSecret : String;
    FGoogleAppKey : String;
//    FAppSecrets : String;
    FHL7Appid : String;
    FHL7AppSecret : String;
    FAdminEmail : String;
    FUserProvider : TFHIRUserProvider;
    FEndPoint: String;
    FOnDoSearch : TDoSearchEvent;
    FPath: String;
    FActive : boolean;
    FPassword : String;
    FNonceList : TStringList;

    function GetPatientListAsOptions : String;
    {$IFNDEF FHIR2}
    procedure populateFromConsent(consentKey : integer; session : TFhirSession);
    {$ENDIF}

    Procedure HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleCAVS(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    procedure HandleTokenBearer(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
    procedure HandleTokenOAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
    Procedure HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleKeyToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleRegistration(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
    procedure HandleTokenJWT(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
    function checkNotEmpty(v, n: String): String;
    function isAllowedRedirect(client : TRegisteredClientInformation; redirect_uri: String): boolean;
    function isAllowedAud(client_id, aud_uri: String): boolean;
    procedure SetServerContext(const Value: TFHIRServerContext);
    function BuildLoginList(id : String) : String;
    Function CheckLoginToken(state : string; var original : String; var provider : TFHIRAuthProvider):Boolean;
    procedure loadScopeVariables(variables: TDictionary<String, String>; scope: String; user : TSCIMUser);
    procedure readScopes(scopes: TStringList; params: TParseMap);
    procedure SetUserProvider(const Value: TFHIRUserProvider);
    function nonceIsUnique(nonce : String) : boolean;

  public
    Constructor Create(ini : TFHIRServerIniFile; Host, SSLPort : String);
    Destructor Destroy; override;

    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
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

    function AuthPath : String;
    function BasePath : String;
    function TokenPath : String;
    function RegisterPath : String;
    function KeyPath : String;
    function CavsPath : String;
    Property EndPoint : String read FEndPoint write FEndPoint;
    property OnDoSearch : TDoSearchEvent read FOnDoSearch write FOnDoSearch;
    property UserProvider : TFHIRUserProvider read FUserProvider write SetUserProvider;
    property Active : boolean read FActive write FActive;
    property Host : String read FHost write FHost;
  end;


implementation

uses
  FHIRLog, FHIRAuthMap;


{ TAuth2Server }

constructor TAuth2Server.Create(ini : TFHIRServerIniFile; Host, SSLPort : String);
begin
  inherited create;
  FHost := host;
  FSSLPort := SSLPort;
  FLock := TCriticalSection.Create('auth-server');
  FLoginTokens := TAdvMap<TFhirLoginToken>.create;
  FNonceList := TStringList.create;
  FNonceList.Sorted := true;

  FHL7Appid := ini.ReadString(voVersioningNotApplicable, 'hl7.org', 'app-id', '');
  FHL7AppSecret := ini.ReadString(voVersioningNotApplicable, 'hl7.org', 'app-secret', '');
  FFacebookAppid := ini.ReadString(voVersioningNotApplicable, 'facebook.com', 'app-id', '');
  FFacebookAppSecret := ini.ReadString(voVersioningNotApplicable, 'facebook.com', 'app-secret', '');
  FGoogleAppid := ini.ReadString(voVersioningNotApplicable, 'google.com', 'app-id', '');
  FGoogleAppSecret := ini.ReadString(voVersioningNotApplicable, 'google.com', 'app-secret', '');
  FGoogleAppKey := ini.ReadString(voVersioningNotApplicable, 'google.com', 'app-key', '');
  FPassword := ini.ReadString(voVersioningNotApplicable, 'admin', 'password', '');
  FPath := '/auth';
end;

destructor TAuth2Server.Destroy;
begin
  FNonceList.Free;
  FLoginTokens.Free;;
  FLock.Free;
  FServerContext.Free;
  FUserProvider.free;
  inherited;
end;


function TAuth2Server.GetPatientListAsOptions: String;
var
  bundle : TFhirBundle;
  entry : TFhirBundleEntry;
  patient : TFhirPatient;
  b : TStringBuilder;
begin
  bundle := OnDoSearch(nil, 'Patient', 'en', '_summary=true&__wantObject=true');
  b := TStringBuilder.create;
  try
    b.Append('<option value=""/>');
    for entry in bundle.entryList do
    begin
      patient := entry.resource as TFhirPatient;
      b.Append('<option value="');
      b.Append(patient.id);
      b.Append('">');
      b.Append(HumanNamesAsText(patient.nameList));
      b.Append(' (');
      b.Append(patient.id);
      b.Append(')</option>');
    end;
    result := b.ToString;
  finally
    b.Free;
    bundle.Free;
  end;
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

{$IFNDEF FHIR2}
procedure TAuth2Server.populateFromConsent(consentKey: integer; session: TFhirSession);
var
  consent : TFhirConsent;
  c : TFhirCoding;
  s : String;
begin
  consent := ServerContext.Storage.FetchResource(consentKey) as TFhirConsent;
  try
    s := '';
    {$IFDEF FHIR3}
    for c in consent.except_List[0].class_List do
      if (c.System = 'http://smarthealthit.org/fhir/scopes') then
        s := s +' '+c.code;
    {$ELSE}
    raise Exception.Create('This operation is only supported in R3 for now');
    {$ENDIF}
    session.scopes := s.Trim;
  finally
    consent.Free;
  end;
end;
{$ENDIF}

function TAuth2Server.checkNotEmpty(v , n : String) : String;
begin
  if (v = '') then
    raise Exception.Create('Parameter "'+n+'" not found');
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
 result := FPath+'/token';
end;

procedure TAuth2Server.HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap; response: TIdHTTPResponseInfo);
var
  client_id : String;
  scope : String;
  redirect_uri : String;
  state : String;
  aud : String;
  id : String;
  jwt : TJWT;
  message : String;
  b : TStringBuilder;
  ok : boolean;
  variables : TDictionary<String,String>;
  client : TRegisteredClientInformation;
begin
  if params.GetVar('response_type') <> 'code' then
    raise Exception.Create('Only response_type allowed is ''code''');
  client_id := checkNotEmpty(params.GetVar('client_id'), 'client_id');
  client := ServerContext.Storage.getClientInfo(client_id);
  try
    if client = nil then
      raise Exception.Create('Unknown Client Identifier "'+client_id+'"');
    redirect_uri := checkNotEmpty(params.GetVar('redirect_uri'), 'redirect_uri');
    if not ((client_id = 'c.1') and (redirect_uri = ServerContext.FormalURLSecureClosed+'/internal')) then
      if not isAllowedRedirect(client, redirect_uri) then
      raise Exception.Create('Unacceptable Redirect url "'+redirect_uri+'"');
    scope := checkNotEmpty(params.GetVar('scope'), 'scope');
    state := checkNotEmpty(params.GetVar('state'), 'state');
    aud := checkNotEmpty(params.GetVar('aud'), 'aud');
    if not isAllowedAud(client_id, aud) then
      raise Exception.Create('Unacceptable FHIR Server URL "'+aud+'" (should be '+EndPoint+')');

    id := newguidid;
    ServerContext.Storage.recordOAuthLogin(id, client_id, scope, redirect_uri, state);
    b := TStringBuilder.Create;
    try
      ok := true;
      if (client.jwt <> '') and (ServerContext.ClientApplicationVerifier <> nil) then
      begin
        jwt := TJWTUtils.unpack(client.jwt, false, nil);
        try
          ok := ServerContext.ClientApplicationVerifier.check(jwt, b, message);
        finally
          jwt.Free;
        end;
      end
      else
        message := ' <li>The Client Application Service has not checked this client</li>';

      variables := TDictionary<String,String>.create;
      try
        variables.Add('/oauth2', FPath);
        variables.Add('idmethods', BuildLoginList(id));
        variables.Add('client', client.name);
        variables.Add('client-notes', message);
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

procedure TAuth2Server.loadScopeVariables(variables : TDictionary<String,String>; scope : String; user : TSCIMUser);
//patient/*.read	Permission to read any resource for the current patient
//user/*.*	Permission to read and write all resources that the current user can access
//openid profile	Permission to retrieve information about the current logged-in user
//launch	Permission to obtain launch context when app is launched from an EHR
//launch/patient	When launching outside the EHR, ask for a patient to be selected at launch time
var
  security : TFHIRSecurityRights;
  t : TFhirResourceType;
begin
  variables.add('userlevel', '');
  variables.add('userinfo', '');
  security := TFHIRSecurityRights.create(ServerContext.ValidatorContext.Link, user, scope, true);
  try
    if security.canAdministerUsers then
      variables.add('useradmin', '<input type="checkbox" name="useradmin" value="1"/> Administer Users')
    else
      variables.add('useradmin', '');
    if security.canGetUserInfo then
      variables['userinfo'] := 'checked';

    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      variables.AddOrSetValue('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]], '');
      variables.AddOrSetValue('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]], '');
      variables.AddOrSetValue('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled', 'disabled');
      variables.AddOrSetValue('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled', 'disabled');
    end;

    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      if security.canRead(CODES_TFHIRResourceType[t]) then
      begin
        variables['read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]] := 'checked';
        variables['read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled'] := '';
      end;
      if security.canWrite(CODES_TFHIRResourceType[t]) then
      begin
        variables['write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]] := 'checked';
        variables['write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled'] := '';
      end;
    end;
  finally
    security.free;
  end;
end;

procedure TAuth2Server.readScopes(scopes : TStringList; params : TParseMap);
var
  pfx : String;
  t : TFhirResourceType;
  all : boolean;
begin
  scopes.clear;
  if (params.getVar('userInfo') = '1') then
  begin
    scopes.add('openid');
    scopes.add('profile');
  end;
  if (params.getVar('useradmin') = '1') then
    scopes.add(SCIM_ADMINISTRATOR);

  // if there's a patient, then the scopes a patient specific
  if (params.getVar('patient') = '') then
    pfx := 'User/'
  else
    pfx := 'Patient/';

  all := true;
  for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    if (params.GetVar('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) <> '1') or
      (params.GetVar('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) <> '1') then
      all := false;

  if all then
    scopes.Add(pfx+'*.*')
  else
  begin
    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      if params.GetVar('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) = '1' then
        scopes.Add(pfx+CODES_TFHIRResourceType[t]+'.read');
      if params.GetVar('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) = '1' then
        scopes.Add(pfx+CODES_TFHIRResourceType[t]+'.write');
    end;
  end;
end;

function TAuth2Server.RegisterPath: String;
begin
  result := FPath+'/register';
end;

const
  MAGIC_OBS = 'http://healthintersections.com.au/fhir/codes/obs';

procedure TAuth2Server.HandleCAVS(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  pbody : TParseMap;
  json, item, endorser : TJsonObject;
  jwt : TJWT;
  jwk : TJWKList;
  endorsements: TAdvList<TEndorsement>;
  endorsement : TEndorsement;
  app : TFhirDevice;
  list : TJsonArray;
  c : TFhirContactPoint;
begin
  pbody := TParseMap.create(request.formParams);
  try
    if not pbody.VarExists('jwt') then
      raise Exception.Create('Unable to understand post body - no jwt parameter found');
    jwk := ServerContext.JWTServices.jwkList;
    try
      json := TJsonObject.Create;
      try
        try
          jwt := TJWTUtils.unpack(pbody.GetVar('jwt'), true, jwk);
        except
          on e : exception do
          begin
            jwt := nil;
            json.str['status'] := 'invalid';
            json.str['message'] := e.Message;
          end;
        end;
        try
          if jwt <> nil then
          begin
            endorsements := TAdvList<TEndorsement>.create;
            try
              app := ServerContext.ApplicationCache.recogniseJWT(jwt.originalSource, endorsements);
              try
                if (app = nil) then
                begin
                  json.str['status'] := 'unknown';
                  json.str['message'] := 'Unknown JWT';
                end
                else if not (app.status in [{$IFDEF FHIR2}DevicestatusAvailable{$ELSE}DeviceStatusActive{$ENDIF}]) then
                begin
                  json.str['status'] := 'unsuitable';
                  json.str['message'] := 'This Application cannot be used because it''s status is '+CODES_TFhirDeviceStatusEnum[app.status];
                end
                else
                begin
                  json.str['status'] := 'approved';
                  json.str['message'] := 'Approved for use by '+FHost;
                  list := json.forceArr['endorsements'];
                  for endorsement in endorsements do
                  begin
                    item := list.addObject;
                    endorser := item.forceObj['endorser'];
                    endorser.str['name'] := endorsement.Organization.name;
                    for c in endorsement.Organization.telecomList do
                      if c.system = {$IFDEF FHIR2} ContactPointSystemOther {$ELSE} ContactPointSystemUrl {$ENDIF} then
                        endorser.str['url'] := c.value;
                    if (endorsement.Observation.code = nil) or (endorsement.Observation.code.codingList.Count = 0) or (endorsement.Observation.code.codingList[0].system <> MAGIC_OBS) then
                      endorser.str['type'] := 'usage-note' // ???
                    else
                      endorser.str['type'] := endorsement.Observation.code.codingList[0].code;
                    endorser.str['comment'] := endorsement.Observation.{$IFDEF FHIR2}comments{$ELSE}comment{$ENDIF};
                  end;
                end;
              finally
                app.Free;
              end;
            finally
              endorsements.Free;
            end;
          end;
        finally
          jwt.free;
        end;
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentText := TJSONWriter.writeObjectStr(json, true);
      finally
        json.free;
      end;
    finally
      jwk.Free;
    end;
  finally
    pBody.Free;
  end;
end;

procedure TAuth2Server.HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  client_id, name, authurl: String;
  variables : TDictionary<String,String>;
  scopes : TStringList;
  redirect, state, scope : String;
begin
  if session = nil then
    raise Exception.Create('User Session not found');

  if FSSLPort = '443' then
    authurl := 'https://'+FHost+FPath
  else
    authurl := 'https://'+FHost+':'+FSSLPort+FPath;

  if not ServerContext.Storage.fetchOAuthDetails(session.key, 2, client_id, name, redirect, state, scope) then
    raise Exception.Create('State Error - session "'+inttostr(session.key)+'" not ready for a choice');

  if params.getVar('form') = 'true' then
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

      if params.getVar('user') = '1' then
      begin
      // if user rights granted
        session.jwt.subject := Names_TFHIRAuthProvider[session.ProviderCode]+':'+session.id;
        session.jwt.name := session.UserName;
        if session.Email <> '' then
          session.jwt.email := session.Email;
      end;
      session.JWTPacked := ServerContext.JWTServices.pack(session.JWT);

      ServerContext.Storage.recordOAuthChoice(Session.OuterToken, scopes.CommaText, session.JWTPacked, params.GetVar('patient'));
      if params.GetVar('patient') <> '' then
        session.Compartments.Add(TFHIRCompartmentId.Create(frtPatient, params.GetVar('patient')));

      session.scopes := scopes.CommaText.Replace(',', ' ');
      ServerContext.Storage.RegisterConsentRecord(session);
      response.Redirect(redirect+'?code='+session.OuterToken+'&state='+state);
    finally
      scopes.Free;
    end;
  end
  else
  begin
    variables := TDictionary<String,String>.create;
    try
      variables.Add('client', ServerContext.Storage.getClientName(client_id));
      variables.Add('/oauth2', FPath);
      variables.Add('username', name);
      variables.Add('patient-list', GetPatientListAsOptions);
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

procedure TAuth2Server.HandleKeyToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
begin
  response.ContentType := 'application/jwt';
  response.ContentText := ServerContext.JWTServices.makeJWT;
end;

procedure TAuth2Server.HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
begin
  response.ContentType := 'application/json';
  response.ContentText := ServerContext.JWTServices.makeJWK;
end;

procedure TAuth2Server.HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  id, username, password, domain, state, jwt : String;
  authurl, token, expires, msg, uid, name, email, client_id : String;
  provider : TFHIRAuthProvider;
  ok : boolean;
  key : integer;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  if params.VarExists('id') and params.VarExists('username') and params.VarExists('password') then
  begin
    id := params.GetVar('id');
    username := params.GetVar('username');
    password := params.GetVar('password');

    if not FUserProvider.CheckLogin(username, password, key) then
      raise Exception.Create('Login failed');

    if not ServerContext.Storage.hasOAuthSession(id, 1) then
      raise Exception.Create('State failed - no login session active');

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
    if not CheckLoginToken(copy(request.document, 25, $FF), id, provider) then
      raise Exception.Create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)');
    uid := params.GetVar('userid');
    name := params.GetVar('fullName');
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
  else if (params.VarExists('state')) then
  begin
    if FSSLPort = '443' then
      authurl := 'https://'+FHost+FPath+'/auth_dest'
    else
      authurl := 'https://'+FHost+':'+FSSLPort+FPath+'/auth_dest';

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
    raise Exception.Create('Login attempt not understood');
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
  client : TRegisteredClientInformation;
  function checkPresent(name : String) : String;
  begin
    result := json.str[name];
    if result = '' then
      raise Exception.Create('A '+name+' field is required in the json token');
  end;
  procedure checkValue(name, value : String);
  begin
    if json.str[name] <> value then
      raise Exception.Create('The '+name+' field must have the value "'+value+'" but has the value "'+json.str[name]+'"');
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
//            raise Exception.Create('User must be identified; log in to the server using the web interface, and get a token that can be used to register the client');
          // check that it meets business rules
          client.name := checkPresent('client_name');
          client.url := json.str['client_uri'];
          client.logo := json.str['logo_uri'];
          client.softwareId := json.str['software_id'];
          client.softwareVersion := json.str['software_version'];
          checkPresent('grant_types');
          checkPresent('response_types');
          checkPresent('token_endpoint_auth_method');
          if (json.str['token_endpoint_auth_method'] = 'private_key_jwt') then // backend services
          begin
            client.mode := rcmBackendServices;
            checkValue('grant_types', 'client_credentials');
            checkValue('response_types', 'token');
            if json.obj['jwks'] = nil then
              raise Exception.Create('No jwks found');
            if json.obj['jwks'].arr['keys'] = nil then
              raise Exception.Create('No keys found in jwks');
            if json.obj['jwks'].arr['keys'].Count = 0 then
              raise Exception.Create('No Keys found in jwks.keys');
            client.publicKey := TJsonWriter.writeObjectStr(json.obj['jwks']);
            client.issuer := checkPresent('issuer');
          end
          else if (json.str['token_endpoint_auth_method'] = 'client_secret_basic') then // confidential
          begin
            checkValue('grant_types', 'authorization_code');
            checkValue('response_types', 'code');
            client.mode := rcmBackendServices;
            client.secret := newGuidId;
            resp.str['client_secret'] := client.secret;
          end
          else if (json.str['token_endpoint_auth_method'] = 'none') then // confidential
          begin
            checkValue('grant_types', 'authorization_code');
            checkValue('response_types', 'code');
            client.mode := rcmBackendServices;
            // no secret
          end
          else
            raise Exception.Create('Unable to recognise client mode');
          resp.str['client_id'] := ServerContext.Storage.storeClient(client, 0 {session.Key});
          resp.str['client_id_issued_at'] := IntToStr(DateTimeToUnix(now));
          client.patientContext := json.bool['fhir_patient_context'];
          response.ContentText := TJSONWriter.writeObjectStr(resp);
          response.ResponseNo := 200;
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

procedure TAuth2Server.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
var
  params : TParseMap;
begin
  if not FActive then
  begin
    response.ResponseNo := 401;
    response.ResponseText := 'Not Found';
  end
  else
  begin
    logt('Auth: '+request.Document);
    try
      // cors
      response.CustomHeaders.add('Access-Control-Allow-Origin: *');
      response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
      response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS');
      if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
        response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
      response.ContentType := 'application/json';

      params := TParseMap.create(request.UnparsedParams);
      try
        if (request.Document = FPath+'/auth') then
          HandleAuth(AContext, request, session, params, response)
        else if (request.Document.startsWith(FPath+'/auth_dest')) then
          HandleLogin(AContext, request, session, params, response)
        else if (request.Document = FPath+'/auth_choice') then
          HandleChoice(AContext, request, session, params, response)
        else if (request.Document = FPath+'/token') then
          HandleToken(AContext, request, session, params, response)
        else if (request.Document = FPath+'/token_data') then
          HandleTokenData(AContext, request, session, params, response)
        else if (request.Document = FPath+'/auth_skype') then
          HandleSkype(AContext, request, session, params, response)
        else if (request.Document = FPath+'/auth_key') then
          HandleKey(AContext, request, session, params, response)
        else if (request.Document = FPath+'/auth_jwt') then
          HandleKeyToken(AContext, request, session, params, response)
        else if (request.Document = FPath+'/discovery') then
          HandleDiscovery(AContext, request, response)
        else if (request.Document = FPath+'/register') then
          HandleRegistration(AContext, request, session, response)
        else if (request.Document = FPath+'/userdetails') then
          HandleUserDetails(AContext, request, session, params, response)
        else if (request.Document = FPath+'/cavs') then
          HandleCAVS(AContext, request, session, params, response)
        else
          raise Exception.Create('Invalid URL');
      finally
        params.Free;
      end;
    except
      on e : Exception do
      begin
        logt('Auth Exception: '+e.Message);
        recordStack(e);
        raise;
      end;
    end;
  end;
end;

procedure TAuth2Server.HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  token, id, name, email, password, domain, client_id : String;
  variables : TDictionary<String,String>;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));
  if params.getVar('form') <> '' then
  begin
    token := checkNotEmpty(params.GetVar('token'), 'token');
    id := checkNotEmpty(params.GetVar('id'), 'id');
    name := checkNotEmpty(params.GetVar('name'), 'name');
    email := checkNotEmpty(params.GetVar('email'), 'email');
    password := checkNotEmpty(params.GetVar('password'), 'password');

    if FPassword <> password then
      raise Exception.Create('Admin Password fail');

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
  else if params.getVar('id') <> '' then
  begin
    if not ServerContext.SessionManager.GetSessionByToken(params.GetVar('id'), session) then
      raise Exception.Create('State Error (1)');
    try
      if not ServerContext.Storage.hasOAuthSessionByKey(session.Key, 2) then
        raise Exception.Create('State Error (2)');
      setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
      response.Redirect(FPath+'/auth_choice');
    finally
      session.Free;
    end
  end
  else
  begin
    variables := TDictionary<String,String>.create;
    try
      variables.Add('/oauth2', FPath);
      OnProcessFile(request, response, session, FPath+'/auth_skype.html', true, variables);
    finally
      variables.free;
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

procedure TAuth2Server.HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  json : TJSONWriter;
begin
  if params.getVar('grant_type') = 'authorization_code' then
    HandleTokenOAuth(AContext, request, session, params, response)
  else if params.getVar('grant_type') = 'client_credentials' then
  begin
     if params.GetVar('client_assertion_type') = 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer' then
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
      json.Start;
      json.Value('error', 'unsupported_grant_type');
      json.Value('error_description', 'Unknown Grant Type '+params.getVar('grant_type'));
      json.Finish;
      response.ContentText := json.ToString;
    finally
      json.Free;
    end;
  end;
end;

procedure TAuth2Server.HandleTokenBearer(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
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
      raise Exception.Create('Can only call grant_type=client_credentials if the Authorization Header has a Bearer Token');
    try
      jwt := TJWTUtils.unpack(request.AuthPassword, false, nil); // todo:
    except
      on e : exception do
        raise Exception.Create('Error reading JWT: '+e.message);
    end;
    try
      uuid := params.getVar('scope');
      if not ServerContext.Storage.FetchAuthorization(uuid, PatientId, ConsentKey, SessionKey, Expiry, jwtStored) then
        raise Exception.Create('Unrecognised Token');
      if (jwtStored <> request.AuthPassword) then
        raise Exception.Create('JWT mismatch');
      if (expiry < now) then
        raise Exception.Create('The authorization has expired');

      session := ServerContext.SessionManager.RegisterSessionFromPastSession(userBearerJWT, sessionKey, expiry, AContext.Binding.PeerIP);
      try
        session.SystemName := jwt.subject;
        session.SystemEvidence := systemFromJWT;
        session.Compartments.Add(TFHIRCompartmentId.Create(frtPatient, PatientId));
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        {$IFNDEF FHIR2}
        populateFromConsent(consentKey, session);
        {$ENDIF}
        json := TJsonWriterDirect.create;
        try
          json.Start;
          json.Value('access_token', session.Cookie);
          json.Value('token_type', 'Bearer');
          json.Value('expires_in', inttostr(trunc((session.Expires - now) / DATETIME_SECOND_ONE)));
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

procedure TAuth2Server.HandleTokenJWT(AContext: TIdContext; request: TIdHTTPRequestInfo; params: TParseMap; response: TIdHTTPResponseInfo);
var
//  jwtStored,
  errCode, domain : string;
  json : TJsonWriter;
  jwt, jwtv : TJWT;
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
      jwt := TJWTUtils.unpack(params.GetVar('client_assertion'), false, nil); // todo:
    except
      on e : exception do
        raise Exception.Create('Error reading JWT: '+e.message);
    end;
    try
      client := ServerContext.Storage.getClientInfo(jwt.subject);
      try
        // validate the signature on the JWT
        // check that the JWT exp is valid
        // check that this is not a jti value seen before (prevention of replay attacks)
        // ensure that the client_id provided is valid etc
        if client = nil then
          raise Exception.Create('Unknown client_id "'+jwt.subject+'"');
        if jwt.issuer <> client.issuer then
          raise Exception.Create('Stated Issuer does not match registered issuer');
        if not nonceIsUnique(jwt.id) then
          raise Exception.Create('Repeat Nonce Token - not allowed');
        if (jwt.expires > TDateTimeEx.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE)then
          raise Exception.Create('JWT Expiry is too far in the future - ('+FormatDateTime('c', jwt.expires)+', must be < 5 minutes)');
        if (jwt.expires < TDateTimeEx.makeUTC.DateTime) then
          raise Exception.Create('JWT expiry ('+TDateTimeEx.make(jwt.expires, dttzUTC).toXML+') is too old');

        jwk := TJWKList.create(client.publicKey);
        try
          TJWTUtils.unpack(params.GetVar('client_assertion'), true, jwk).Free;
        finally
          jwk.Free;
        end;
        // all done... now create the session
        session := ServerContext.SessionManager.CreateImplicitSession(AContext.Binding.PeerIP, jwt.subject, 'n/a', systemFromJWT, false, true);
        try
          session.SystemName := jwt.subject;
          session.SystemEvidence := systemFromJWT;
          session.scopes := params.GetVar('scope');
          json := TJsonWriterDirect.create;
          try
            json.Start;
            json.Value('access_token', session.Cookie);
            json.Value('token_type', 'Bearer');
            json.Value('expires_in', trunc((session.Expires - TDateTimeEx.makeUTC.DateTime) / DATETIME_SECOND_ONE));
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

procedure TAuth2Server.HandleTokenOAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);

var
  code, clientId, clientSecret, uri, errCode, client_id : string;
  pclientid, pname, predirect, pstate, pscope : String;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  launch, scope : String;
  client : TRegisteredClientInformation;
begin
  buffer := TAdvMemoryStream.Create;
  try
    try
      // first, we check the fixed value

      // now, check the code
      errCode := 'invalid_request';
      code := checkNotEmpty(params.getVar('code'), 'code');
      if not ServerContext.SessionManager.GetSessionByToken(code, session) then // todo: why is session passed in too?
        raise Exception.Create('Authorization Code not recognized');
      try
        if not ServerContext.Storage.fetchOAuthDetails(session.key, 3, pclientid, pname, predirect, pstate, pscope) then
          raise Exception.Create('Authorization Code not recognized (2)');
        client := ServerContext.Storage.getClientInfo(pclientid);
        try
          // what happens now depends on whether there's a client secret or not
          if (client.secret = '') then
          begin
            // user must supply the correct client id
            errCode := 'invalid_client';
            clientId := checkNotEmpty(params.getVar('client_id'), 'client_id');
            if clientId <> pclientid then
              raise Exception.Create('Client Id is wrong ("'+clientId+'") is wrong in parameter');
          end
          else
          begin
            // client id and client secret must be in the basic header. Check them
            clientId := request.AuthUsername;
            clientSecret := request.AuthPassword;
            if clientId <> pclientid then
              raise Exception.Create('Client Id is wrong ("'+clientId+'") in Authorization Header');
            if clientSecret <> client.secret then
              raise Exception.Create('Client Secret in Authorization header is wrong ("'+clientSecret+'")');
          end;

          // now check the redirect URL
          uri := checkNotEmpty(params.getVar('redirect_uri'), 'redirect_uri');
          errCode := 'invalid_request';
          if predirect <> uri then
            raise Exception.Create('Mismatch between claimed and actual redirection URIs');

          // ok, well, it's passed.
          scope := pscope;
          launch := readFromScope(scope, 'launch');
          ServerContext.Storage.updateOAuthSession(session.OuterToken, 4, session.Key, client_id);
          if (session.SystemName <> client_id) or (session.SystemName <> clientId) then
            raise Exception.Create('Session client id mismatch ("'+session.SystemName+'"/"'+client_id+'"/"'+clientId+'")');

          json := TJsonWriterDirect.create;
          try
            json.Stream := buffer.link;
            json.Start;
            json.Value('access_token', session.Cookie);
            json.Value('token_type', 'Bearer');
            json.Value('expires_in', inttostr(trunc((session.Expires - now) / DATETIME_SECOND_ONE)));
            json.Value('id_token', session.JWTPacked);
            json.Value('scope', scope);
            json.Value('patient', launch);
            json.Finish;
          finally
            json.Free;
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
        response.ResponseNo := 500;
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
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;

procedure TAuth2Server.HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  token, clientId, clientSecret : string;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  check : boolean;
  client : TRegisteredClientInformation;
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

      client := ServerContext.Storage.getClientInfo(clientId);
      try
        if client.secret <> clientSecret then
          raise Exception.Create('Client Id or secret is wrong ("'+clientId+'")');

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
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;


procedure TAuth2Server.HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  variables : TDictionary<String,String>;
begin
  if session = nil then
    response.Redirect(FPath+'/auth?client_id=web&response_type=code&scope=openid%20profile%20user/*.*%20'+SCIM_ADMINISTRATOR+'&redirect_uri='+EndPoint+'/internal&aud='+EndPoint+'&state='+MakeLoginToken(EndPoint, apGoogle))
  else
  begin
    if params.getVar('form') = 'true' then
    begin
      raise Exception.Create('Not done yet');
    end
    else
    begin
      variables := TDictionary<String,String>.create;
      try
        variables.Add('username', session.User.username);
        variables.Add('/oauth2', FPath);
        OnProcessFile(request, response, session, '/oauth_userdetails.html', true, variables)
      finally
        variables.free;
      end;
    end;
  end;
end;

function TAuth2Server.AuthPath: String;
begin
  result := FPath+'/auth';
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

end.
