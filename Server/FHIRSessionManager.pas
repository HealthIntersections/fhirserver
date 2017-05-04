unit FHIRSessionManager;

interface

uses
  SysUtils, Classes, kCritSct,
  AdvObjects, DateSupport, GuidSupport, StringSupport,
  SCIMServer, DateAndTime,
  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, FHIRUtilities, FHIRSecurity,
  ServerUtilities, FHIRStorageService, ServerValidator;

Const
  IMPL_COOKIE_PREFIX = 'implicit-';

Type
  TFHIRSessionManager = class (TFHIRServerWorker)
  private
    FLock: TCriticalSection;
    FSessions: TStringList;
    FLastSessionKey: integer;
  public
    constructor Create(ServerContext : TAdvObject);
    destructor Destroy; override;

    property LastSessionKey: integer read FLastSessionKey write FLastSessionKey;

    procedure CloseAll;
    Function CreateImplicitSession(clientInfo: String; server: Boolean) : TFhirSession;
    Procedure EndSession(sCookie, ip: String);
    function GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
    function GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
    function RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
    procedure MarkSessionChecked(sCookie, sName: String);
    function isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
    function GetSessionByKey(userkey : integer) : TFhirSession;
    procedure Sweep;
    function DumpSessions : String;
  end;

implementation

uses
  FHIRServerContext;

{ TFHIRSessionManager }

constructor TFHIRSessionManager.Create;
begin
  inherited Create(ServerContext);
  FSessions := TStringList.Create;
  FLock := TCriticalSection.Create('session-manager');
end;

destructor TFHIRSessionManager.Destroy;
begin
  FSessions.free;
  FLock.free;
  inherited;
end;

procedure TFHIRSessionManager.CloseAll;
var
  i: integer;
  session: TFhirSession;
begin
  FLock.Lock('close all');
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.free;
      FSessions.Delete(i);
    end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.CreateImplicitSession(clientInfo: String; server: Boolean): TFhirSession;
var
  session: TFhirSession;
  dummy: Boolean;
  new: Boolean;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
  key : integer;
begin
  new := false;
  FLock.Lock('CreateImplicitSession');
  try
    if not GetSession(IMPL_COOKIE_PREFIX + clientInfo, result, dummy) then
    begin
      new := true;
      session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.link, false);
      try
        inc(FLastSessionKey);
        session.key := FLastSessionKey;
        session.id := '';
        session.name := clientInfo;
        session.expires := UniversalDateTime + DATETIME_SECOND_ONE * 60 * 60;
        // 1 hour
        session.Cookie := '';
        session.provider := apNone;
        session.originalUrl := '';
        session.email := '';
        session.anonymous := true;
        session.userkey := 0;
        FSessions.AddObject(IMPL_COOKIE_PREFIX + clientInfo, session.Link);
        result := session.Link as TFhirSession;
      finally
        session.free;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if new then
  begin
    if server then
      session.User := TFHIRServerContext(serverContext).SCIMServer.loadUser(SCIM_SYSTEM_USER, key)
    else
      session.User := TFHIRServerContext(serverContext).SCIMServer.loadUser(SCIM_ANONYMOUS_USER, key);
    session.name := session.User.username + ' (' + clientInfo + ')';
    session.UserKey := key;
    session.scopes := TFHIRSecurityRights.allScopes;
    // though they'll only actually get what the user allows
    TFHIRServerContext(serverContext).Storage.RecordFhirSession(result);
    se := TFhirAuditEvent.Create;
    try
      se.event := TFhirAuditEventEvent.Create;
      se.event.type_ := TFHIRCoding.Create;
      C := se.event.type_;
      C.code := '110114';
      C.system := 'http://nema.org/dicom/dcid';
      C.Display := 'User Authentication';
      C := se.event.subtypeList.append;
      C.code := '110122';
      C.system := 'http://nema.org/dicom/dcid';
      C.Display := 'Login';
      se.event.action := AuditEventActionE;
      se.event.outcome := AuditEventOutcome0;
      se.event.dateTime := NowUTC;
      se.source := TFhirAuditEventSource.Create;
      se.source.site := TFHIRServerContext(serverContext).OwnerName;
      se.source.identifier := TFhirIdentifier.Create;
      se.source.identifier.system := 'urn:ietf:rfc:3986';
      se.source.identifier.value := TFHIRServerContext(serverContext).SystemId;

      C := se.source.type_List.append;
      C.code := '3';
      C.Display := 'Web Server';
      C.system := 'http://hl7.org/fhir/security-source-type';

      // participant - the web browser / user proxy
      p := se.participantList.append;
      p.network := TFhirAuditEventParticipantNetwork.Create;
      p.network.address := clientInfo;
      p.network.type_ := NetworkType2;

      TFHIRServerContext(serverContext).Storage.QueueResource(se, se.event.dateTime);
    finally
      se.free;
    end;
  end;
end;


procedure TFHIRSessionManager.EndSession(sCookie, ip: String);
var
  i: integer;
  session: TFhirSession;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
  key: integer;
begin
  key := 0;
  FLock.Lock('EndSession');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      try
        se := TFhirAuditEvent.Create;
        try
          se.event := TFhirAuditEventEvent.Create;
          se.event.type_ := TFHIRCoding.Create;
          C := se.event.type_;
          C.code := '110114';
          C.system := 'http://nema.org/dicom/dcid';
          C.Display := 'User Authentication';
          C := se.event.subtypeList.append;
          C.code := '110123';
          C.system := 'http://nema.org/dicom/dcid';
          C.Display := 'Logout';
          se.event.action := AuditEventActionE;
          se.event.outcome := AuditEventOutcome0;
          se.event.dateTime := NowUTC;
          se.source := TFhirAuditEventSource.Create;
          se.source.site := TFHIRServerContext(serverContext).OwnerName;
          se.source.identifier := TFhirIdentifier.Create;
          se.source.identifier.system := 'urn:ietf:rfc:3986';
          se.source.identifier.value := TFHIRServerContext(serverContext).SystemId;
          C := se.source.type_List.append;
          C.code := '3';
          C.Display := 'Web Server';
          C.system := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.append;
          p.userId := TFhirIdentifier.Create;
          p.userId.system := TFHIRServerContext(serverContext).SystemId;
          p.userId.value := inttostr(session.key);
          p.altId := session.id;
          p.name := session.name;
          if (ip <> '') then
          begin
            p.network := TFhirAuditEventParticipantNetwork.Create;
            p.network.address := ip;
            p.network.type_ := NetworkType2;
            p.requestor := true;
          end;

          TFHIRServerContext(serverContext).Storage.QueueResource(se, se.event.dateTime);
        finally
          se.free;
        end;
        key := session.key;
        FSessions.Delete(i);
      finally
        session.free;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    TFHIRServerContext(serverContext).Storage.CloseFhirSession(key);
end;

function TFHIRSessionManager.GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
var
  key, i: integer;
begin
  key := 0;
  FLock.Lock('GetSession');
  try
    i := FSessions.IndexOf(sCookie);
    result := i > -1;
    if result then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.useCount := session.useCount + 1;
      if session.expires > UniversalDateTime then
      begin
        session.Link;
        check := (session.provider in [apFacebook, apGoogle]) and
          (session.NextTokenCheck < UniversalDateTime);
      end
      else
      begin
        result := false;
        try
          key := session.key;
          FSessions.Delete(i);
        finally
          session.free;
        end;
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
  c, i, key: integer;
begin
  c := -1;
  key := 0;
  result := nil;
  FLock.Lock('GetSession');
  try
    for i := 0 to FSessions.Count - 1 do
      if TFhirSession(FSessions.Objects[i]).UserKey = userkey then
        c := i;
    if (c <> -1) then
    begin
      result := FSessions.Objects[c] as TFhirSession;
      result.useCount := result.useCount + 1;
      if (result.expires > UniversalDateTime) and not ((result.provider in [apFacebook, apGoogle]) and (result.NextTokenCheck < UniversalDateTime)) then
        result.Link
      else
      begin
        key := result.Key;
        FSessions.Delete(c);
        result.Free;
        result := nil;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if c > 0 then
    TFHIRServerContext(serverContext).Storage.CloseFhirSession(c);
  if result = nil then
  begin
    result := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
    try
      result.innerToken := NewGuidURN;
      result.outerToken := NewGuidURN;
      result.id := NewGuidURN;
      result.UserKey := userkey;
      result.User := TFHIRServerContext(serverContext).SCIMServer.loadUser(userkey);
      result.name := result.User.formattedName;
      result.expires := LocalDateTime + DATETIME_SECOND_ONE * 500;
      result.Cookie := NewGuidURN;
      result.provider := apInternal;
      result.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
      result.scopes := TFHIRSecurityRights.allScopes;
      FLock.Lock('RegisterSession2');
      try
        inc(FLastSessionKey);
        result.key := FLastSessionKey;
        FSessions.AddObject(result.Cookie, result.Link);
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

function TFHIRSessionManager.GetSessionByToken(outerToken: String;
  var session: TFhirSession): Boolean;
var
  i: integer;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.Count - 1 do
      if (TFhirSession(FSessions.Objects[i]).outerToken = outerToken) or
        (TFhirSession(FSessions.Objects[i]).JWTPacked = outerToken) then
      begin
        result := true;
        session := TFhirSession(FSessions.Objects[i]).Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRSessionManager.isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
var
  id, hash, username, password: String;
  i, key: integer;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.Count - 1 do
      if (TFhirSession(FSessions.Objects[i]).innerToken = token) and
        (TFhirSession(FSessions.Objects[i]).outerToken = '$BEARER') then
      begin
        result := true;
        session := TFhirSession(FSessions.Objects[i]).Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
  if (not result) then
  begin
    StringSplit(token, '.', id, hash);
    result := StringIsInteger32(id) and TFHIRServerContext(ServerContext).SCIMServer.CheckId(id, username,
      password);
    if (result and (password = hash)) then
    begin
      session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
      try
        session.innerToken := token;
        session.outerToken := '$BEARER';
        session.id := id;
        session.User := TFHIRServerContext(ServerContext).SCIMServer.loadUser(username, key);
        session.UserKey := key;
        session.name := session.User.bestName;
        session.expires := LocalDateTime + DATETIME_SECOND_ONE * 0.25;
        session.provider := apInternal;
        session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
        session.scopes := TFHIRSecurityRights.allScopes;
        if (session.User.emails.Count > 0) then
          session.email := session.User.emails[0].value;
        // session.scopes := ;
        FLock.Lock('CreateImplicitSession');
        try
          inc(FLastSessionKey);
          session.key := FLastSessionKey;
          FSessions.AddObject(token, session.Link);
          session.Link;
        finally
          FLock.Unlock;
        end;
      finally
        session.free;
      end;
      TFHIRServerContext(ServerContext).Storage.RecordFhirSession(session);
      se := TFhirAuditEvent.Create;
      try
        se.event := TFhirAuditEventEvent.Create;
        se.event.type_ := TFHIRCoding.Create;
        C := se.event.type_;
        C.code := '110114';
        C.system := 'http://nema.org/dicom/dcid';
        C.Display := 'User Authentication';
        C := se.event.subtypeList.append;
        C.code := '110122';
        C.system := 'http://nema.org/dicom/dcid';
        C.Display := 'Login';
        se.event.action := AuditEventActionE;
        se.event.outcome := AuditEventOutcome0;
        se.event.dateTime := NowUTC;
        se.source := TFhirAuditEventSource.Create;
        se.source.site := TFHIRServerContext(serverContext).OwnerName;
        se.source.identifier := TFhirIdentifier.Create;
        se.source.identifier.system := 'urn:ietf:rfc:3986';
        se.source.identifier.value := TFHIRServerContext(serverContext).SystemId;
        C := se.source.type_List.append;
        C.code := '3';
        C.Display := 'Web Server';
        C.system := 'http://hl7.org/fhir/security-source-type';

        // participant - the web browser / user proxy
        p := se.participantList.append;
        p.userId := TFhirIdentifier.Create;
        p.userId.system := TFHIRServerContext(serverContext).SystemId;
        p.userId.value := inttostr(session.key);
        p.network := TFhirAuditEventParticipantNetwork.Create;
        p.network.address := clientInfo;
        p.network.type_ := NetworkType2;
        TFHIRServerContext(ServerContext).Storage.QueueResource(se, se.event.dateTime);
      finally
        se.free;
      end;
    end
    else
      result := false;
  end;
end;

procedure TFHIRSessionManager.MarkSessionChecked(sCookie, sName: String);
var
  i: integer;
  session: TFhirSession;
begin
  FLock.Lock('MarkSessionChecked');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
      session.name := sName;
    end;
  finally
    FLock.Unlock;
  end;

end;

function TFHIRSessionManager.RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
var
  session: TFhirSession;
  key : integer;
begin
  session := TFhirSession.Create(TFHIRServerContext(serverContext).ValidatorContext.Link, true);
  try
    session.innerToken := innerToken;
    session.outerToken := outerToken;
    session.id := id;
    session.name := name;
    session.expires := LocalDateTime + DATETIME_SECOND_ONE * StrToInt(expires);
    session.Cookie := OAUTH_SESSION_PREFIX +
      copy(GUIDToString(CreateGuid), 2, 36);
    session.provider := provider;
    session.originalUrl := original;
    session.email := email;
    session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
    if provider = apInternal then
      session.User := TFHIRServerContext(serverContext).SCIMServer.loadUser(id, key)
    else
      session.User := TFHIRServerContext(serverContext).SCIMServer.loadOrCreateUser(USER_SCHEME_PROVIDER[provider] + '#' + id, name, email, key);
    session.UserKey := key;
    if session.name = '' then
      session.name := session.User.bestName;
    if (session.email = '') and (session.User.emails.Count > 0) then
      session.email := session.User.emails[0].value;

    session.scopes := rights;
    // empty, mostly - user will assign them later when they submit their choice

    FLock.Lock('RegisterSession');
    try
      inc(FLastSessionKey);
      session.key := FLastSessionKey;
      FSessions.AddObject(session.Cookie, session.Link);
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
  i, key : integer;
  session : TFhirSession;
  d: TDateTime;
begin
  key := 0;
  d := UniversalDateTime;
  FLock.Lock('sweep2');
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      session := TFhirSession(FSessions.Objects[i]);
      if session.expires < d then
      begin
        try
          key := session.key;
          FSessions.Delete(i);
          break;
        finally
          session.free;
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    TFHIRServerContext(serverContext).Storage.CloseFhirSession(key);
end;

function TFHIRSessionManager.DumpSessions: String;
var
  i: integer;
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
      for i := FSessions.Count - 1 downto 0 do
      begin
        session := TFhirSession(FSessions.Objects[i]);
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

