unit session_manager;

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

interface

uses
  SysUtils, Classes, fsl_threads,
  fsl_base, fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects,  fhir_common, fhir_factory,
  scim_server,
  session, security, user_manager, utilities, storage;

Const
  IMPL_COOKIE_PREFIX = 'implicit-';

Type
  TFHIRSessionManager = class (TFHIRServerWorker)
  private
    FLock: TFslLock;
    FSessions: TFslMap<TFHIRSession>;
    FLastSessionKey: integer;
    function factory : TFHIRFactory;
    Procedure EndSession(session : TFHIRSession; ip: String); overload;
  public
    constructor Create(ServerSettings : TFHIRServerSettings; ServerContext : TFslObject);
    destructor Destroy; override;

    property LastSessionKey: integer read FLastSessionKey write FLastSessionKey;

    procedure CloseAll;
    Function CreateImplicitSession(ClientIp, ClientSystemId, UserName : String; SystemEvidence : TFHIRSystemIdEvidence; server, useGUID: Boolean) : TFhirSession;
    function getSessionFromJWT(ClientIp, SystemName : String; SystemEvidence : TFHIRSystemIdEvidence; JWT : TJWT) : TFhirSession;

    Procedure EndSession(sCookie, ip: String); overload;
    function GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
    function GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
    function RegisterSession(userEvidence : TFHIRUserIdEvidence; provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
    function RegisterSessionFromPastSession(userEvidence : TFHIRUserIdEvidence; pastSessionKey : integer; expires : TDateTime; ip : String) : TFhirSession;
    procedure MarkSessionChecked(sCookie: String);
    function isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
    function isOkSession(session: TFhirSession): Boolean;
    function GetSessionByKey(userkey : integer) : TFhirSession;
    procedure Sweep;
    function DumpSessions : String;
    procedure EndAllSessions(cookie, ip : String);
    function buildTable : String;
  end;

implementation

uses
  server_context;

{ TFHIRSessionManager }

constructor TFHIRSessionManager.Create(ServerSettings : TFHIRServerSettings; ServerContext : TFslObject);
begin
  inherited Create(ServerContext);
  FLock := TFslLock.Create('session-manager');
  FSessions := TFslMap<TFHIRSession>.Create('session');
end;

destructor TFHIRSessionManager.Destroy;
begin
  FSessions.free;
  FLock.free;
  inherited;
end;

function TFHIRSessionManager.buildTable: String;
var
  b : TFslStringBuilder;
  session: TFhirSession;
begin
  b := TFslStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.Append(' <tr><td><b>Type</b></td><td><b>Name</b></td><td><b>User</b></td><td><b>System</b></td><td><b>Provider</b></td>'+
        '<td><b>Created</b></td><td><b>Expires</b></td><td><b>Rights</b></td><td><b>Recording?</b></td><td><b>Actions</b></td></tr>'#13#10);
    FLock.Lock('table');
    try
      for session in FSessions.Values do
        session.buildRow(b);
    finally
      FLock.Unlock;
    end;
    b.Append('</table>'#13#10);
    result := b.AsString;
  finally
    b.free;
  end;
end;

procedure TFHIRSessionManager.CloseAll;
begin
  FLock.Lock('close all');
  try
    FSessions.Clear;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.CreateImplicitSession(ClientIp, ClientSystemId, UserName : String; SystemEvidence : TFHIRSystemIdEvidence; server, useGUID: Boolean): TFhirSession;
var
  session: TFhirSession;
  dummy: Boolean;
  new: Boolean;
  se: TFhirAuditEventW;
  key : integer;
  intcookie : String;
begin
  if useGUID then
    intcookie := NewGuidURN
  else
    intcookie := IMPL_COOKIE_PREFIX + inttostr(HashStringToCode32(ClientIp+':'+ClientSystemId+':'+UserName));
  session := nil;
  try
    new := false;
    FLock.Lock('CreateImplicitSession');
    try
      if not GetSession(intcookie, result, dummy) then
      begin
        new := true;
        session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.link, false);
        inc(FLastSessionKey);
        session.key := FLastSessionKey;
        session.id := '';
        session.UserName := userName;
        session.SystemName := ClientSystemId;
        session.SystemEvidence := SystemEvidence;
        session.SessionName := session.UserName+' ('+session.SystemName+')';
        if server then
          session.UserEvidence := userInternal
        else
          session.UserEvidence := userAnonymous;
        session.expires := TFslDateTime.makeUTC.DateTime + DATETIME_SECOND_ONE * 60 * 60;
        // 1 hour
        session.Cookie := intcookie;
        session.providerCode := apNone;
        session.originalUrl := '';
        session.email := '';
        session.userkey := 0;

        FSessions.Add(session.Cookie, session.Link);
        result := session.Link as TFhirSession;
      end;
    finally
      FLock.Unlock;
    end;
    if new then
    begin
      if server then
        session.User := TFHIRServerContext(serverContext).UserProvider.loadUser(SCIM_SYSTEM_USER, key)
      else
        session.User := TFHIRServerContext(serverContext).UserProvider.loadUser(SCIM_ANONYMOUS_USER, key);
      session.UserName := session.User.username;
      session.SessionName := session.UserName+' ('+session.SystemName+')';
      session.UserKey := key;
      session.scopes := TFHIRSecurityRights.allScopes;
      // though they'll only actually get what the user allows
      TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
      se := factory.wrapAuditEvent(factory.makeResource('AuditEvent'));
      try
        se.success;
        se.eventType('http://nema.org/dicom/dcid', '110114', 'User Authentication');
        se.eventSubType('http://nema.org/dicom/dcid', '110122', 'Login');
        se.source(TFHIRServerContext(serverContext).Globals.OwnerName, 'urn:ietf:rfc:3986', TFHIRServerContext(serverContext).DatabaseId);
        se.sourceType('http://hl7.org/fhir/security-source-type', '3', 'Web Server');
        se.participantId(TFHIRServerContext(serverContext).DatabaseId, inttostr(session.key), session.id, session.SessionName);
        if clientIp <> '' then
          se.participantIp(clientIp);
        TFHIRServerContext(serverContext).Storage.QueueResource(session, se.resource, se.dateTime);
      finally
        se.free;
      end;
    end;
  finally
    session.free;
  end;
end;

procedure TFHIRSessionManager.EndSession(session : TFhirSession; ip: String);
var
  se: TFhirAuditEventW;
  key: integer;
begin
  assert(FLock.LockedToMe);
  se := factory.wrapAuditEvent(factory.makeResource('AuditEvent'));
  try
    se.success;
    se.eventType('http://nema.org/dicom/dcid', '110114', 'User Authentication');
    se.eventSubType('http://nema.org/dicom/dcid', '110123', 'Logout');
    se.source(TFHIRServerContext(serverContext).Globals.OwnerName, 'urn:ietf:rfc:3986', TFHIRServerContext(serverContext).DatabaseId);
    se.sourceType('http://hl7.org/fhir/security-source-type', '3', 'Web Server');
    se.participantId(TFHIRServerContext(serverContext).DatabaseId, inttostr(session.key), session.id, session.SessionName);
    if (ip <> '') then
      se.participantIp(ip);
    if session.TestScript <> nil then
      TFHIRServerContext(serverContext).Storage.QueueResource(session, session.TestScript.Resource, se.dateTime);
    TFHIRServerContext(serverContext).Storage.QueueResource(session, se.Resource, se.dateTime);
  finally
    se.free;
  end;
  key := session.key;
  FSessions.Remove(session.Cookie);
  TFHIRServerContext(serverContext).Storage.CloseFhirSession(key);
end;

procedure TFHIRSessionManager.EndAllSessions(cookie, ip : String);
var
  session : TFhirSession;
  list : TFslList<TFhirSession>;
begin
  list := TFslList<TFhirSession>.create;
  try
    FLock.Lock('sweep2');
    try
      for session in FSessions.Values do
        if (cookie = '') or (cookie = session.Cookie) then
          list.add(session.Link);
      for session in list do
        EndSession(session, ip);
    finally
      FLock.Unlock;
    end;
  finally
    list.Free;
  end;

end;

procedure TFHIRSessionManager.EndSession(sCookie, ip: String);
var
  session: TFhirSession;
begin
  FLock.Lock('EndSession');
  try
    if FSessions.TryGetValue(sCookie, session) then
    begin
      EndSession(session, ip);
    end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.factory: TFHIRFactory;
begin
  result := TFHIRServerContext(serverContext).Factory;
end;

function TFHIRSessionManager.GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
var
  key: integer;
begin
  key := 0;
  FLock.Lock('GetSession');
  try
    result := FSessions.TryGetValue(sCookie, session);
    if result then
    begin
      session.useCount := session.useCount + 1;
      if session.expires > TFslDateTime.makeUTC.DateTime then
      begin
        session.Link;
        check := (session.providerCode in [apFacebook, apGoogle]) and
          (session.NextTokenCheck < TFslDateTime.makeUTC.DateTime);
      end
      else
      begin
        result := false;
        key := session.key;
        FSessions.Remove(sCookie);
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    TFHIRServerContext(serverContext).Storage.CloseFhirSession(key);
end;

function TFHIRSessionManager.GetSessionByKey(userkey: integer): TFhirSession;
var
  c : integer;
  session : TFHIRSession;
begin
  c := 0;
  result := nil;
  FLock.Lock('GetSession');
  try
    for session in FSessions.Values do
      if session.UserKey = userkey then
      begin
        session.useCount := session.useCount + 1;
        c := session.Key;
        if (session.expires > TFslDateTime.makeUTC.DateTime) and not ((session.providerCode in [apFacebook, apGoogle]) and (session.NextTokenCheck < TFslDateTime.makeUTC.DateTime)) then
          result := session.Link
        else
          FSessions.Remove(session.Cookie);
      end;
  finally
    FLock.Unlock;
  end;
  if (result = nil) and (c <> 0) then
    TFHIRServerContext(serverContext).Storage.CloseFhirSession(c);
  if result = nil then
  begin
    result := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
    try
      result.innerToken := NewGuidURN;
      result.outerToken := NewGuidURN;
      result.id := NewGuidURN;
      result.UserKey := userkey;
      result.User := TFHIRServerContext(serverContext).UserProvider.loadUser(userkey);
      result.UserName := result.User.formattedName;
      result.SystemName := 'unknown';
      result.SessionName := result.UserName+' ('+result.SystemName+')';
      result.expires := TFslDateTime.makeLocal.DateTime + DATETIME_SECOND_ONE * 500;
      result.Cookie := NewGuidUrn;
      result.providerCode := apInternal;
      result.ProviderName := 'Internal';
      result.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
      result.scopes := TFHIRSecurityRights.allScopes;
      FLock.Lock('RegisterSession2');
      try
        inc(FLastSessionKey);
        result.key := FLastSessionKey;
        FSessions.Add(result.Cookie, result.Link);
      finally
        FLock.Unlock;
      end;
      TFHIRServerContext(serverContext).Storage.RegisterAuditEvent(result, 'Subscription.Hook');
      result.Link;
    finally
      result.Free;
    end;
    TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
  end;
end;

function TFHIRSessionManager.GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
var
  t : TFHIRSession;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for t in FSessions.Values do
      if (t.outerToken = outerToken) or (t.JWTPacked = outerToken) then
      begin
        result := true;
        session := t.Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.getSessionFromJWT(ClientIp, SystemName: String; SystemEvidence: TFHIRSystemIdEvidence; JWT: TJWT): TFhirSession;
var
  id : String;
//var
//  session: TFhirSession;
  dummy: Boolean;
  new: Boolean;
  se: TFhirAuditEventW;
  key : integer;
//  intcookie : String;
begin
  id := jwt.id;
  if id = '' then
    id := inttostr(HashStringToCode32(TJSONWriter.writeObjectStr(jwt.payload, false)));
  result := nil;
  try
    new := false;
    FLock.Lock('getSessionFromJWT');
    try
      if not GetSession('jwt:'+id, result, dummy) then
      begin
        new := true;
        result := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.link, false);
        inc(FLastSessionKey);
        result.key := FLastSessionKey;
        result.id := '';
        result.UserEvidence := userBearerJWT;
        result.UserName := jwt.userName;
        result.SystemName := SystemName;
        result.SystemEvidence := SystemEvidence;
        result.SessionName := result.UserName+' ('+result.SystemName+')';
        result.expires := JWT.expires;
        result.Cookie := 'jwt:'+id;
        result.providerName := jwt.issuer;
        result.originalUrl := '';
        result.email := jwt.email;
        result.userkey := 0;
        FSessions.Add(result.Cookie, result.Link);
        result := result.Link as TFhirSession;
      end;
    finally
      FLock.Unlock;
    end;
    if new then
    begin
      result.User := TFHIRServerContext(serverContext).UserProvider.loadUser(SCIM_ANONYMOUS_USER, key);
      result.UserKey := key;
      result.scopes := TFHIRSecurityRights.allScopes;
      TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
      se := factory.wrapAuditEvent(factory.makeResource('AuditEvent'));
      try
        se.success;
        se.eventType('http://nema.org/dicom/dcid', '110114', 'User Authentication');
        se.eventSubType('http://nema.org/dicom/dcid', '110122', 'Login');
        se.source(TFHIRServerContext(serverContext).Globals.OwnerName, 'urn:ietf:rfc:3986', TFHIRServerContext(serverContext).DatabaseId);
        se.sourceType('http://hl7.org/fhir/security-source-type', '3', 'Web Server');
        se.participantId(TFHIRServerContext(serverContext).DatabaseId, inttostr(result.key), result.id, result.SessionName);
        if (ClientIp <> '') then
          se.participantIp(ClientIp);

        TFHIRServerContext(serverContext).Storage.QueueResource(result, se.Resource, se.dateTime);
      finally
        se.free;
      end;
    end;
  finally
    result.free;
  end;
end;

function TFHIRSessionManager.isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
var
  id, hash, username, password: String;
  key: integer;
  se: TFhirAuditEventW;
  t : TFHIRSession;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for t in FSessions.values do
      if (t.innerToken = token) and (t.outerToken = '$BEARER') then
      begin
        result := true;
        session := t.Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
  if (not result) then
  begin
    StringSplit(token, '.', id, hash);
    result := StringIsInteger32(id) and TFHIRServerContext(ServerContext).UserProvider.CheckId(id, username,
      password);
    if (result and (password = hash)) then
    begin
      session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
      try
        session.innerToken := token;
        session.outerToken := '$BEARER';
        session.Cookie := token;
        session.id := id;
        session.User := TFHIRServerContext(ServerContext).UserProvider.loadUser(username, key);
        session.UserKey := key;
        session.UserName := session.User.bestName;
        session.SystemName := 'unknown';
        session.SessionName := session.UserName+' ('+session.SystemName+')';
        session.expires := TFslDateTime.makeLocal.DateTime + DATETIME_SECOND_ONE * 0.25;
        session.providerCode := apInternal;
        session.providerName := 'Internal';
        session.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
        session.scopes := TFHIRSecurityRights.allScopes;
        if (session.User.emails.Count > 0) then
          session.email := session.User.emails[0].value;
        // session.scopes := ;
        FLock.Lock('isOkBearer');
        try
          inc(FLastSessionKey);
          session.key := FLastSessionKey;
          FSessions.Add(session.Cookie, session.Link);
          session.Link;
        finally
          FLock.Unlock;
        end;
      finally
        session.free;
      end;
      TFHIRServerContext(ServerContext).Storage.RecordFhirSession(session);
      se := factory.wrapAuditEvent(factory.makeResource('AuditEvent'));
      try
        se.success;
        se.eventType('http://nema.org/dicom/dcid', '110114', 'User Authentication');
        se.eventSubType('http://nema.org/dicom/dcid', '110122', 'Login');
        se.source(TFHIRServerContext(serverContext).Globals.OwnerName, 'urn:ietf:rfc:3986', TFHIRServerContext(serverContext).DatabaseId);
        se.sourceType('http://hl7.org/fhir/security-source-type', '3', 'Web Server');
        se.participantId(TFHIRServerContext(serverContext).DatabaseId, inttostr(session.key), session.id, session.SessionName);
        if (clientInfo <> '') then
          se.participantIp(clientInfo);
        TFHIRServerContext(ServerContext).Storage.QueueResource(session, se.Resource, se.dateTime);
      finally
        se.free;
      end;
    end
    else
      result := false;
  end;
end;

function TFHIRSessionManager.isOkSession(session: TFhirSession): Boolean;
begin
  FLock.Lock('MarkSessionChecked');
  try
    result := FSessions.ContainsKey(session.Cookie);
    if result then
    begin
//      session.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
      session.useCount := session.useCount + 1;
    end;
  finally
    FLock.Unlock;
  end;

end;

procedure TFHIRSessionManager.MarkSessionChecked(sCookie: String);
var
  session: TFhirSession;
begin
  FLock.Lock('MarkSessionChecked');
  try
    if FSessions.TryGetValue(sCookie, session) then
      session.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.RegisterSession(userEvidence : TFHIRUserIdEvidence; provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
var
  session: TFhirSession;
  key : integer;
begin
  session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
  try
    session.innerToken := innerToken;
    session.outerToken := outerToken;
    session.id := id;
    session.expires := TFslDateTime.makeLocal.DateTime + DATETIME_SECOND_ONE * StrToInt(expires);
    session.Cookie := OAUTH_SESSION_PREFIX + NewGuidId;
    session.providerCode := provider;
    session.providerName := Names_TFHIRAuthProvider[provider];
    session.originalUrl := original;
    session.email := email;
    session.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
    if provider = apInternal then
      session.User := TFHIRServerContext(serverContext).UserProvider.loadUser(id, key)
    else
      session.User := TFHIRServerContext(serverContext).UserProvider.loadOrCreateUser(USER_SCHEME_PROVIDER[provider] + '#' + id, name, email, key);
    session.UserEvidence := userEvidence;
    session.UserKey := key;
    session.UserName := name;
    if session.UserName = '' then
      session.UserName := session.User.userName;
    session.SystemName := 'unknown';
    session.SessionName := session.UserName+' ('+session.SystemName+')';
    if (session.email = '') and (session.User.emails.Count > 0) then
      session.email := session.User.emails[0].value;

    session.scopes := rights;
    // empty, mostly - user will assign them later when they submit their choice

    FLock.Lock('RegisterSession');
    try
      inc(FLastSessionKey);
      session.key := FLastSessionKey;
      FSessions.Add(session.Cookie, session.Link);
    finally
      FLock.Unlock;
    end;

    TFHIRServerContext(serverContext).Storage.RegisterAuditEvent(session, ip);

    result := session.Link as TFhirSession;
  finally
    session.free;
  end;
  TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
end;

function TFHIRSessionManager.RegisterSessionFromPastSession(userEvidence: TFHIRUserIdEvidence; pastSessionKey: integer; expires: TDateTime; ip: String): TFhirSession;
var
  session: TFhirSession;
  UserKey, Provider: integer;
  Id, Name, Email: String;
begin
  if not TFHIRServerContext(serverContext).Storage.RetrieveSession(pastSessionKey, UserKey, Provider, Id, Name, Email) then
    raise EFHIRException.create('Unable to retrieve past session '+inttostr(pastSessionKey));

  session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
  try
    session.innerToken := NewGuidURN;
    session.outerToken := NewGuidURN;
    session.id := id; // same id?
    session.expires := expires;
    session.Cookie := OAUTH_SESSION_PREFIX + NewGuidId;
    session.providerCode := TFHIRAuthProvider(provider);
    session.providerName := Names_TFHIRAuthProvider[session.providerCode];
    session.email := email;
    session.NextTokenCheck := TFslDateTime.makeUTC.DateTime + 5 * DATETIME_MINUTE_ONE;
    session.User := TFHIRServerContext(serverContext).UserProvider.loadUser(UserKey);
    session.UserEvidence := userEvidence;
    session.UserKey := UserKey;
    session.UserName := name;
    if session.UserName = '' then
      session.UserName := session.User.userName;
    session.SystemName := 'unknown';
    session.SessionName := session.UserName+' ('+session.SystemName+')';
    session.email := email;
    if (session.email = '') and (session.User.emails.Count > 0) then
      session.email := session.User.emails[0].value;

    FLock.Lock('RegisterSession');
    try
      inc(FLastSessionKey);
      session.key := FLastSessionKey;
      FSessions.Add(session.Cookie, session.Link);
    finally
      FLock.Unlock;
    end;

    TFHIRServerContext(serverContext).Storage.RegisterAuditEvent(session, ip);

    result := session.Link as TFhirSession;
  finally
    session.free;
  end;
  TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
end;

procedure TFHIRSessionManager.Sweep;
var
  session : TFhirSession;
  list : TFslList<TFhirSession>;
  d: TDateTime;
begin
  list := TFslList<TFhirSession>.create;
  try
    d := TFslDateTime.makeUTC.DateTime;
    FLock.Lock('sweep2');
    try
      for session in FSessions.Values do
      begin
        if session.expires < d then
        begin
          list.add(session.Link);
        end;
      end;
      for session in list do
        EndSession(session, '');
    finally
      FLock.Unlock;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRSessionManager.DumpSessions: String;
var
  session: TFhirSession;
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.Append('<tr>');
    b.Append('<td>Session Key</td>');
    b.Append('<td>user Identity</td>');
    b.Append('<td>UserKey</td>');
    b.Append('<td>Name</td>');
    b.Append('<td>Created</td>');
    b.Append('<td>Expires</td>');
    b.Append('<td>Check Time</td>');
    b.Append('<td>Use Count</td>');
    b.Append('<td>Scopes</td>');
    b.Append('<td>Component</td>');
    b.Append('</tr>'#13#10);

    FLock.Lock('DumpSessions');
    try
      for session in FSessions.values do
      begin
        session.describe(b);
        b.Append(#13#10);
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

end.

