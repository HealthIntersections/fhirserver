unit FHIRSubscriptionManager;

interface

uses
  SysUtils, Classes, DateSupport, StringSupport,
  kCritSct, KDBManager, KDBDialects, KDate, ParseMap,
  AdvObjects, AdvObjectLists,
  IdHTTP, IdSSLOpenSSL, IdSMTP, IdMessage, IdExplicitTLSClientServerBase, idGlobal,
  FHIRBase, FhirResources, FHIRComponents, FHIRTypes, FHIRConstants, FHIRUtilities, FHIRClient, FHIRAtomFeed,
  FhirSupport, FHIRIndexManagers, FHIRServerUtilities, FHIRParser, FHIRParserBase;

Type
  TSubscriptionTracker = class (TAdvObject)
  private
    FKey: Integer;
    FErrorCount: Integer;
    FInfo: String;
  public
    procedure UpdateForInfo(info : String);

    Property Key : Integer read FKey write FKey;
    Property Info : String read FInfo write FInfo;
    Property ErrorCount : Integer read FErrorCount write FErrorCount;
  end;

  TSubscriptionTrackerList = class (TAdvObjectList)
  private
    function GetTracker(i: integer): TSubscriptionTracker;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    procedure addOrUpdate(Key : integer; info : String; status : TFhirSubscriptionStatus); overload;
    function getByKey(Key : integer) : TSubscriptionTracker;
    property TrackerItem[i : integer] : TSubscriptionTracker read GetTracker; default;
  end;

  TSubscriptionEntry = class (TAdvObject)
  private
    FSubscription : TFhirSubscription;
    FKey : Integer;
    FVersionKey : Integer;
    FResourceType: Integer;
    FId: String;
    procedure SetSubscription(const Value: TFhirSubscription);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Property Key : integer read FKey write FKey;
    Property VersionKey : integer read FVersionKey write FVersionKey;
    Property Id : String read FId write FId;
    Property ResourceType : Integer read FResourceType write FResourceType;
    Property Subscription : TFhirSubscription read FSubscription write SetSubscription;
  end;

  TSubscriptionEntryList = class (TAdvObjectList)
  private
    function GetEntry(i: integer): TSubscriptionEntry;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    procedure add(Key, VersionKey, ResourceType : Integer; id : String; Subscription : TFhirSubscription); overload;
    function getByKey(Key : integer) : TSubscriptionEntry;
    property EntryItem[i : integer] : TSubscriptionEntry read GetEntry; default;
  end;

  TExecuteOperationEvent = procedure(request : TFHIRRequest; response : TFHIRResponse; bWantSession : boolean) of object;
  TExecuteSearchEvent = function (typekey : integer; compartmentId, compartments : String; params : TParseMap; conn : TKDBConnection): String of object;

  TSubscriptionManager = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FSubscriptions : TSubscriptionEntryList;
    FSubscriptionTrackers  : TSubscriptionTrackerList;
    FLastSubscriptionKey, FLastNotificationQueueKey : integer;
    FDatabase: TKDBManager;
    EmptyQueue : boolean;
    FSMTPPort: String;
    FSMTPPassword: String;
    FSMTPHost: String;
    FSMTPSender: String;
    FSMTPUsername: String;
    FOnExecuteOperation : TExecuteOperationEvent;
    FOnExecuteSearch : TExecuteSearchEvent;
    FSMTPUseTLS: boolean;
    FSMSFrom: String;
    FSMSToken: String;
    FSMSAccount: String;

//    FMessageQueue : TNotification;
    function determineResourceTypeKey(criteria : String; conn : TKDBConnection) : integer;
    procedure checkAcceptable(subscription : TFhirSubscription; session : TFHIRSession);
    procedure SeeNewSubscription(key, vkey : Integer; id : String; subscription : TFhirSubscription; session : TFHIRSession; conn : TKDBConnection);
    function ProcessSubscription(conn : TKDBConnection): Boolean;
    function ProcessNotification(conn : TKDBConnection): Boolean;
    function MeetsCriteria(criteria : String; typekey, key : integer; conn : TKDBConnection) : boolean;
    procedure createNotification(vkey, skey : integer; conn : TKDBConnection);
    function LoadResourceFromDBByKey(conn : TKDBConnection; key: integer) : TFhirResource;
    function LoadResourceFromDBByVer(conn : TKDBConnection; vkey: integer; var id : String) : TFhirResource;
    procedure sendByRest(id : String; res : TFhirResource; subst : TFhirSubscription);
    procedure sendByEmail(id : String; res : TFhirResource; subst : TFhirSubscription);
    procedure sendBySms(id : String; res : TFhirResource; subst : TFhirSubscription);
    procedure saveTags(conn : TKDBConnection; ResourceKey : integer; res : TFHIRResource);
    procedure NotifySuccess(SubscriptionKey : integer);
    procedure NotifyFailure(SubscriptionKey : integer; message : string);
    procedure DoDropResource(key, vkey : Integer; internal : boolean);
    function getSummaryForChannel(subst : TFhirSubscription) : String;
    procedure ApplyUpdateToResource(id : String; resource : TFhirResource);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure loadQueue(conn : TKDBConnection);
    procedure SeeResource(key, vkey : Integer; id : String; resource : TFHIRResource; conn : TKDBConnection; reload : boolean; session : TFHIRSession);
    procedure DropResource(key, vkey : Integer);
    procedure Process; // spend up to a minute working on subscriptions

    Property Database : TKDBManager read FDatabase write FDatabase;

    Property SMTPHost : String read FSMTPHost write FSMTPHost;
    Property SMTPPort : String read FSMTPPort write FSMTPPort;
    Property SMTPUsername : String read FSMTPUsername write FSMTPUsername;
    Property SMTPPassword : String read FSMTPPassword write FSMTPPassword;
    Property SMTPSender : String read FSMTPSender write FSMTPSender;
    Property SMTPUseTLS : boolean read FSMTPUseTLS write FSMTPUseTLS;
    Property SMSAccount : String read FSMSAccount write FSMSAccount;
    Property SMSToken : String read FSMSToken write FSMSToken;
    Property SMSFrom : String read FSMSFrom write FSMSFrom;
    Property OnExecuteOperation : TExecuteOperationEvent read FOnExecuteOperation write FOnExecuteOperation;
    Property OnExecuteSearch : TExecuteSearchEvent read FOnExecuteSearch write FOnExecuteSearch;
  end;

implementation

uses
  FHIROperation,
  TwilioClient;

{ TSubscriptionManager }

constructor TSubscriptionManager.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('Subscriptions');
  FSubscriptions := TSubscriptionEntryList.Create;
  FSubscriptionTrackers := TSubscriptionTrackerList.Create;
end;

destructor TSubscriptionManager.Destroy;
begin
  FSubscriptionTrackers.Free;
  FSubscriptions.Free;
  FLock.Free;
  inherited;
end;


procedure TSubscriptionManager.ApplyUpdateToResource(id: String; resource: TFhirResource);
var
  request : TFHIRRequest;
  response : TFHIRResponse;
begin
  request := TFHIRRequest.Create;
  response := TFHIRResponse.Create;
  try
    request.Id := id;
    request.Session := nil;
    request.Resource := resource.Link;
    request.ResourceType := resource.ResourceType;
    request.CommandType := fcmdUpdate;
    request.LoadParams('');
    OnExecuteOperation(request, response, true);
    // and we ignore the outcome, because what can we do?   
  finally
    request.Free;
    response.Free;
  end;
end;

procedure TSubscriptionManager.checkAcceptable(subscription: TFhirSubscription; session: TFHIRSession);
begin
  // permissions. Anyone who is authenticated can set up a subscription
//  if (session <> nil) and (session.Name <> 'server') and (session.Name <> 'service') then // session is nil if we are reloading. Service is always allowed to create a subscription
//  begin
//    if session.Email = '' then
//      raise Exception.Create('This server does not accept subscription request unless an email address is the client login is associated with an email address');
//    ok := false;
//    for i := 0 to subscription.contactList.Count - 1 do
//      ok := ok or ((subscription.contactList[i].systemST = ContactSystemEmail) and (subscription.contactList[i].valueST = session.Email));
//    if not ok then
//      raise Exception.Create('The subscription must explicitly list the logged in user email ('+session.Email+') as a contact');
//  end;                                                                           9

  // basic setup stuff
  if subscription.channel.type_ = SubscriptionChannelTypeNull then
    raise Exception.Create('A channel type must be specified');
  if subscription.channel.type_ in [SubscriptionChannelTypeWebsocket, SubscriptionChannelTypeMessage] then
    raise Exception.Create('The channel type '+CODES_TFhirSubscriptionChannelType[subscription.channel.type_]+' is not supported');
  if subscription.channel.url = '' then
    raise Exception.Create('A channel URL must be specified');
  if (subscription.channel.type_ = SubscriptionChannelTypeSms) and not subscription.channel.url.StartsWith('tel:') then
    raise Exception.Create('When the channel type is "sms", then the URL must start with "tel:"');
end;


procedure TSubscriptionManager.DropResource(key, vkey: Integer);
begin
  DoDropResource(key, vKey, false);
end;

function TSubscriptionManager.getSummaryForChannel(subst: TFhirSubscription): String;
begin
  result := subst.channel.type_Element.value+#1+subst.channel.url+#1+subst.channel.payload+#0+subst.channel.header;
end;

procedure TSubscriptionManager.DoDropResource(key, vkey: Integer; internal : boolean);
var
  i : integer;
  dodelete : boolean;
  conn : TKDBConnection;
begin
  dodelete := false;
  FLock.Enter('DropResource');
  try
    if not internal then
    begin
      for i := FSubscriptionTrackers.Count - 1 downto 0 do
        if FSubscriptionTrackers[i].FKey = key then
        begin
          dodelete := true;
          FSubscriptionTrackers.DeleteByIndex(i);
        end;
    end;
    for i := FSubscriptions.Count - 1 downto 0 do
      if FSubscriptions[i].FKey = key then
        FSubscriptions.DeleteByIndex(i);
  finally      
    FLock.Leave;
  end;
  if not internal and dodelete then
  begin  
    conn := FDatabase.GetConnection('drop subscription');
    try
      conn.ExecSQL('update NotificationQueue set Abandoned = '+DBGetDate(conn.Owner.Platform)+' where SubscriptionKey = '+inttostr(key));    
      conn.Release;
    except
      on e:exception do
      begin
        conn.Error(e);
        raise;
      end;
    end;
  end;
end;

procedure TSubscriptionManager.SeeNewSubscription(key, vkey: Integer; id : String; subscription: TFhirSubscription; session: TFHIRSession; conn : TKDBConnection);
begin
  if subscription.status in [SubscriptionStatusActive, SubscriptionStatusError] then
  begin
    CheckAcceptable(subscription, session);
    DoDropResource(Key, 0, true); // delete any previously existing entry for this subscription
    FSubscriptions.add(key, vkey, determineResourceTypeKey(subscription.criteria, conn), id, subscription.Link);
    FSubscriptionTrackers.addorUpdate(key, getSummaryForChannel(subscription), subscription.status);
  end;
end;

procedure TSubscriptionManager.SeeResource(key, vkey: Integer; id : String; resource: TFHIRResource; conn : TKDBConnection; reload: boolean; session : TFHIRSession);
begin
  FLock.Enter('SeeResource');
  try
    if not reload then
      // we evaluate the criteria retrospectively in a different thead, so for now, all we do is add the entry to a queue
    begin
      inc(FLastSubscriptionKey);
      conn.ExecSQL('Insert into SubscriptionQueue (SubscriptionQueueKey, ResourceKey, ResourceVersionKey, Entered) values ('+inttostr(FLastSubscriptionKey)+', '+inttostr(key)+', '+inttostr(vkey)+', '+DBGetDate(conn.Owner.Platform)+')');
      EmptyQueue := false;
    end;

    if resource is TFhirSubscription then
      SeeNewSubscription(key, vkey, id, resource as TFhirSubscription, session, conn);
  finally
    FLock.Leave;
  end;
end;


procedure TSubscriptionManager.sendByEmail(id : String; res: TFhirResource; subst: TFhirSubscription);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  comp : TFHIRComposer;
  bs : TBytesStream;
begin
  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := SMTPHost;
    if SMTPPort <> '' then
      sender.port := StrToInt(SMTPPort);
    sender.Username := SMTPUsername;
    sender.Password := SMTPPassword;
    if SMTPUseTLS then
    begin
      ssl := TIdSSLIOHandlerSocketOpenSSL.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := SMTPHost+':'+SMTPPort;
      ssl.Host := SMTPHost;
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(SMTPPort);
      ssl.SSLOptions.Method := sslvTLSv1;
      ssl.SSLOptions.Mode := sslmUnassigned;
      ssl.SSLOptions.VerifyMode := [];
      ssl.SSLOptions.VerifyDepth := 0;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := subst.channel.header;
      msg.Recipients.EMailAddresses := subst.channel.url.Replace('mailto:', '');
      msg.From.Text := SMTPSender;
      if subst.channel.payload = '' then
        msg.Body.Text := 'An update has occurred'
      else
      begin
        comp := MakeComposer('en', subst.channel.payload);
        try
          bs := TBytesStream.Create;
          try
            comp.Compose(bs, res, true, nil);
            msg.Body.Text := TEncoding.UTF8.GetString(bs.Bytes);
          finally
            bs.Free;
          end;
        finally
          comp.Free;
        end;
      end;
      sender.Send(msg);
    Finally
      msg.Free;
    End;
    sender.Disconnect;
  Finally
    sender.IOHandler.free;
    sender.Free;
  End;
end;


procedure TSubscriptionManager.sendByRest(id : String; res: TFhirResource; subst: TFhirSubscription);
var
  http : TIdHTTP;
  client : TFHIRClient;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  stream : TMemoryStream;
begin
  if subst.channel.payload = '' then
  begin
    stream := TMemoryStream.Create;
    http := TIdHTTP.create(nil);
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      if subst.channel.header <> '' then
        http.Request.RawHeaders.Add(subst.channel.header);
      http.Post(subst.channel.url, stream);
    finally
      ssl.Free;
      http.Free;
      stream.Free;
    end;
  end
  else
  begin
    client := TFhirClient.create(subst.channel.url, subst.channel.payload.Contains('json'));
    try
      client.updateResource(id, res);
    finally
      client.Free;
    end;
  end;
end;

procedure TSubscriptionManager.sendBySms(id: String; res: TFhirResource; subst: TFhirSubscription);
var
  client : TTwilioClient;
begin
  client := TTwilioClient.Create;
  try
    client.Account := SMSAccount;
    client.Token := SMSToken;
    client.From := SMSFrom;
    if subst.channel.url.StartsWith('tel:') then
      client.dest := subst.channel.url.Substring(4)
    else
      client.dest := subst.channel.url;
    if subst.channel.payload <> '' then
      client.Body := subst.channel.payload
    else
      client.Body := 'A new matching resource has been received';
    client.send;
  finally
    client.Free;
  end;
end;


procedure TSubscriptionManager.Process;
var
  finish : TDateTime;
  found : boolean;
  conn : TKDBConnection;
begin
//  if EmptyQueue then
    exit;
  try
    finish := now + DATETIME_MINUTE_ONE;
    repeat
      conn := FDatabase.GetConnection('process subscription');
      try
        // 2:1 process rate, AND process notification afterwards
        found := ProcessNotification(conn);
        found := ProcessSubscription(conn) or found;
        found := ProcessNotification(conn) or found;
        conn.Release;
      except
        on e : Exception do
        begin
          conn.Error(e);
          raise;
        end;
      end;
    until not found or (now > finish);
    if not found then
      EmptyQueue := true;
  except
    on e:exception do
      WriteLn('Error handling subscriptions: '+e.Message);
  end;
end;

function TSubscriptionManager.ProcessNotification(conn : TKDBConnection): Boolean;
var
  NotificationnQueueKey, ResourceKey, SubscriptionKey : integer;
  res : TFhirResource;
  id : string;
  subst : TFhirSubscription;
  done : boolean;
  tnow, dnow : TDateTime;
  i: Integer;
begin
  NotificationnQueueKey := 0;
  SubscriptionKey := 0;
  ResourceKey := 0;
  tNow := now;

  conn.SQL := 'Select NotificationQueueKey, ResourceVersionKey, SubscriptionKey, LastTry, ErrorCount from NotificationQueue where '+
     'Handled is null and Abandoned is null order by NotificationQueueKey';
  conn.Prepare;
  try
    conn.Execute;
    result := false;
    done := false;
    repeat
      if conn.FetchNext then
      begin
        dnow := TSToDateTime(conn.ColTimeStampByName['LastTry']) + DATETIME_MINUTE_ONE * conn.ColIntegerByName['ErrorCount'];
        result := conn.ColNullByName['LastTry'] or (tnow >= dnow)
      end
      else
        done := true;
    until done or result;
    if result then
    begin
      NotificationnQueueKey := conn.ColIntegerByName['NotificationQueueKey'];
      SubscriptionKey := conn.ColIntegerByName['SubscriptionKey'];
      ResourceKey := conn.ColIntegerByName['ResourceVersionKey'];
    end;
  finally
    conn.Terminate;
  end;
  if result then
  begin
    subst := LoadResourceFromDBByKey(conn, SubscriptionKey) as TFhirSubscription;
    try
      res := LoadResourceFromDBByVer(conn, ResourceKey, id);
      try
        try
          for i := 0 to subst.meta.tagList.Count - 1 do
            if (subst.meta.tagList[i].code <> '') and (subst.meta.tagList[i].system <> '') then
              if not res.meta.HasTag(subst.meta.tagList[i].system, subst.meta.tagList[i].code) then
                res.meta.tagList.AddValue(subst.meta.tagList[i].code, subst.meta.tagList[i].system, subst.meta.tagList[i].code);
          case subst.channel.type_ of
            SubscriptionChannelTypeRestHook: sendByRest(id, res, subst);
            SubscriptionChannelTypeEmail: sendByEmail(id, res, subst);
            SubscriptionChannelTypeSms: sendBySms(id, res, subst);
          end;

          saveTags(conn, ResourceKey, res);
          conn.ExecSQL('update NotificationQueue set Handled = '+DBGetDate(conn.Owner.Platform)+' where NotificationQueueKey = '+inttostr(NotificationnQueueKey));
          NotifySuccess(SubscriptionKey);
        except
          on e:exception do
          begin
            conn.ExecSQL('update NotificationQueue set LastTry = '+DBGetDate(conn.Owner.Platform)+', ErrorCount = ErrorCount + 1 where NotificationQueueKey = '+inttostr(NotificationnQueueKey));
            NotifyFailure(SubscriptionKey, e.message);
          end;
        end;
      finally
        subst.Free;
      end;
    finally
      res.free;
    end;
  end;
end;


function TSubscriptionManager.ProcessSubscription(conn: TKDBConnection): Boolean;
var
  SubscriptionQueueKey, ResourceKey, ResourceVersionKey, ResourceTypeKey : integer;
  i : integer;
  list : TSubscriptionEntryList;
begin
  SubscriptionQueueKey := 0;
  ResourceKey := 0;
  ResourceVersionKey := 0;
  ResourceTypeKey := 0;

  conn.SQL := 'Select Top 1 SubscriptionQueueKey, Ids.ResourceKey, SubscriptionQueue.ResourceVersionKey, Ids.ResourceTypeKey from SubscriptionQueue, Ids where '+
    'Handled is null and Ids.ResourceKey = SubscriptionQueue.ResourceKey order by SubscriptionQueueKey';
  conn.Prepare;
  try
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      SubscriptionQueueKey := conn.ColIntegerByName['SubscriptionQueueKey'];
      ResourceKey := conn.ColIntegerByName['ResourceKey'];
      ResourceVersionKey := conn.ColIntegerByName['ResourceVersionKey'];
      ResourceTypeKey := conn.ColIntegerByName['ResourceTypeKey'];
    end;
  finally
    conn.Terminate;
  end;
  if result then
  begin
    list := TSubscriptionEntryList.Create;
    try
      // keep the lock short
      FLock.Lock('List Subscriptions');
      try
        for i := 0 to FSubscriptions.Count - 1 do
          list.Add(FSubscriptions[i].FKey, FSubscriptions[i].FVersionKey, FSubscriptions[i].FResourceType, FSubscriptions[i].FId, FSubscriptions[i].FSubscription.Link); // not clone
      finally
        FLock.Leave;
      end;
      conn.StartTransact;
      try
        for i := 0 to list.Count - 1 do
          if ((list[i].FResourceType = 0) or (list[i].FResourceType = ResourceTypeKey) ) and MeetsCriteria(list[i].Subscription.criteria, list[i].FResourceType, ResourceKey, conn) then
            CreateNotification(ResourceVersionKey, list[i].FKey, conn);
         conn.ExecSQL('Update SubscriptionQueue set Handled = '+DBGetDate(conn.Owner.Platform)+' where SubscriptionQueueKey = '+inttostr(SubscriptionQueueKey));
         conn.Commit;
      except
        conn.Rollback;
        raise;
      end;
    finally
      list.free;
    end;
  end;
end;

procedure TSubscriptionManager.saveTags(conn: TKDBConnection; ResourceKey: integer; res : TFHIRResource);
begin
raise Exception.Create('Error Message');
{  conn.SQL := 'Update Versions set content = :t where ResourceVersionKey = '+inttostr(ResourceKey);
  conn.Prepare;
  try
    conn.BindBlobFromBytes('t', tags.Json);
    conn.Execute;
  finally
    conn.Terminate;
  end;
  }
end;

procedure TSubscriptionManager.loadQueue(conn: TKDBConnection);
begin
  FLastSubscriptionKey := conn.CountSQL('select max(SubscriptionQueueKey) from SubscriptionQueue');
  FLastNotificationQueueKey := conn.CountSQL('select max(NotificationQueueKey) from NotificationQueue');
end;

function TSubscriptionManager.LoadResourceFromDBByVer(conn: TKDBConnection; vkey: integer; var id : String): TFhirResource;
var
  parser : TFHIRParser;
begin
  conn.SQL := 'select ResourceName, Ids.Id, Tags, Content From Versions, Ids, Types where ResourceVersionKey = '+inttostr(vkey)+' and Versions.ResourceKey = IDs.ResourceKey and IDs.ResourceTypeKey = Types.ResourceTypeKey';
  conn.prepare;
  try
    conn.Execute;
    if not conn.FetchNext then
      raise Exception.Create('Cannot find resource');
    id := conn.ColStringByName['Id'];
    if conn.ColStringByName['ResourceName'] = 'Binary' then
      result := LoadBinaryResource('en', conn.ColBlobByName['Content'])
    else
    begin
      parser := MakeParser('en', ffXml, ZDecompressBytes(conn.ColBlobByName['Content']), xppDrop);
      try
        result := parser.resource.Link as TFHIRResource;
      finally
        parser.free;
      end;
    end;
  finally
    conn.terminate;
  end;
end;

function TSubscriptionManager.LoadResourceFromDBByKey(conn: TKDBConnection; key: integer): TFhirResource;
var
  parser : TFHIRParser;
begin
  conn.SQL := 'select ResourceName, Ids.Id, Tags, Content From Versions, Ids, Types where IDs.ResourceKey = '+inttostr(key)+' and Versions.ResourceVersionKey = IDs.MostRecent and IDs.ResourceTypeKey = Types.ResourceTypeKey';
  conn.prepare;
  try
    conn.Execute;
    if not conn.FetchNext then
      raise Exception.Create('Cannot find resource');
    if conn.ColStringByName['ResourceName'] = 'Binary' then
      result := LoadBinaryResource('en', conn.ColBlobByName['Content'])
    else
    begin
      parser := MakeParser('en', ffXml, ZDecompressBytes(conn.ColBlobByName['Content']), xppDrop);
      try
        result := parser.resource.Link as TFHIRResource;
      finally
        parser.free;
      end;
    end;
  finally
    conn.terminate;
  end;
end;

function TSubscriptionManager.determineResourceTypeKey(criteria: String; conn: TKDBConnection): integer;
var
  t : string;
begin
  if criteria.StartsWith('?') then
    result := 0
  else
  begin
    if criteria.IndexOf('?') = -1 then
      t := criteria.Substring(1)
    else
      t := criteria.Substring(1, criteria.IndexOf('?')-1);
    result := conn.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(t)+'''')
  end;
end;

function TSubscriptionManager.MeetsCriteria(criteria: String; typekey, key: integer; conn: TKDBConnection): boolean;
var
  l, r, sql : String;
  p : TParseMap;
begin
  StringSplit(criteria, '?', l, r);
  p := TParseMap.createSmart(r, true);
  try
    sql := FOnExecuteSearch(typekey, '', '', p, conn);
    result := conn.CountSQL('select count(*) from Ids where not MostRecent is null and ResourceKey = '+inttostr(key)+' and '+sql) > 0;
  finally
    p.Free;
  end;
end;

procedure TSubscriptionManager.NotifySuccess(SubscriptionKey: integer);
var
  tracker : TSubscriptionTracker;
  notify : boolean;
  entry : TSubscriptionEntry;
  subst : TFhirSubscription;
  id : string;
begin
  subst := nil;
  notify := false;
  FLock.Lock('notifysuccess');
  try
    tracker := FSubscriptionTrackers.getByKey(SubscriptionKey);
    if (tracker <> nil) then
      notify := tracker.FErrorCount > 0;
    if notify then
    begin
      tracker.FErrorCount := 0;
      entry := FSubscriptions.getBykey(SubscriptionKey);
      id := entry.Id;
      subst := entry.FSubscription.Link;
    end;
  finally
    FLock.Unlock;
  end;
  if notify then
  begin
    subst.status := SubscriptionStatusActive;
    subst.error := '';
    ApplyUpdateToResource(id, subst);
  end;
end;

procedure TSubscriptionManager.NotifyFailure(SubscriptionKey: integer; message: string);
var
  tracker : TSubscriptionTracker;
  entry : TSubscriptionEntry;
  subst : TFhirSubscription;
  action : integer;
  id : string;
begin
  action := 0;
  subst := nil;

  FLock.Lock('notifysuccess');
  try
    tracker := FSubscriptionTrackers.getByKey(SubscriptionKey);
    if (tracker <> nil) then
    begin
      if tracker.FErrorCount = 0 then
        action := 1
      else if tracker.FErrorCount = 20 then
        action := 2
      else
        inc(tracker.FErrorCount);
    end;
    if action in [1..2] then
    begin
      inc(tracker.FErrorCount);
      entry := FSubscriptions.getBykey(SubscriptionKey);
      id := entry.Id;
      subst := entry.FSubscription.Link;
    end;
  finally
    FLock.Unlock;
  end;
  if action >  0then
  begin
    if action = 1 then
      subst.status := SubscriptionStatusError
    else
      subst.status := SubscriptionStatusOff;
    subst.error := message;
    ApplyUpdateToResource(id, subst);
  end;
end;

procedure TSubscriptionManager.createNotification(vkey, skey: integer; conn : TKDBConnection);
var
  k : integer;
begin
  FLock.Lock('createNotification');
  try
    inc(FLastNotificationQueueKey);
    k := FLastNotificationQueueKey;
  finally
    FLock.Unlock;
  end;
  conn.ExecSQL('insert into NotificationQUeue (NotificationQueueKey, SubscriptionKey, ResourceVersionKey, Entered, ErrorCount) values '+
       '('+inttostr(k)+', '+inttostr(skey)+', '+inttostr(vkey)+', '+DBGetDate(conn.Owner.Platform)+', 0)');
end;

{ TSubscriptionEntryList }

procedure TSubscriptionEntryList.add(Key, VersionKey, ResourceType: Integer; Id : String; Subscription: TFhirSubscription);
var
  entry : TSubscriptionEntry;
begin
  entry := TSubscriptionEntry.Create;
  try
    entry.Key := key;
    entry.VersionKey := Versionkey;
    entry.resourceType := ResourceType;
    entry.Subscription := subscription;
    entry.Id := id;
    add(entry.link);
  finally
    entry.free;
  end;
end;

function TSubscriptionEntryList.getByKey(Key: integer): TSubscriptionEntry;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1  do
    if EntryItem[i].Key = key then
    begin
      result := EntryItem[i];
      exit;
    end;
end;

function TSubscriptionEntryList.GetEntry(i: integer): TSubscriptionEntry;
begin
  result := TSubscriptionEntry(ObjectByIndex[i]);
end;

function TSubscriptionEntryList.ItemClass: TAdvObjectClass;
begin
  result := TSubscriptionEntry;
end;

{ TSubscriptionTrackerList }

procedure TSubscriptionTrackerList.addOrUpdate(Key: integer; info: String; status : TFhirSubscriptionStatus);
var
  t : TSubscriptionTracker;
begin
  t := getByKey(key);
  if t = nil then
  begin
    t := TSubscriptionTracker.Create;
    try
      t.FKey := key;
      t.FInfo := info;
      if status = SubscriptionStatusError then
        t.FErrorCount := 1;
        
      add(t.Link);
    finally
      t.Free;
    end;
  end
  else
    t.UpdateForInfo(info);
end;

function TSubscriptionTrackerList.getByKey(Key: integer): TSubscriptionTracker;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1  do
    if TrackerItem[i].Key = key then
    begin
      result := TrackerItem[i];
      exit;
    end;
end;

function TSubscriptionTrackerList.GetTracker(i: integer): TSubscriptionTracker;
begin
  result := TSubscriptionTracker(ObjectByIndex[i]);
end;

function TSubscriptionTrackerList.ItemClass: TAdvObjectClass;
begin
  result := TSubscriptionTracker;
end;

{ TSubscriptionEntry }

constructor TSubscriptionEntry.Create;
begin
  inherited;
end;

destructor TSubscriptionEntry.Destroy;
begin
  FSubscription.Free;
  inherited;
end;

procedure TSubscriptionEntry.SetSubscription(const Value: TFhirSubscription);
begin
  FSubscription.Free;
  FSubscription := Value;
end;

{ TSubscriptionTracker }

procedure TSubscriptionTracker.UpdateForInfo(info: String);
begin
  if FInfo <> info then
  begin
    FInfo := info;
    FErrorCount := 0; // reset error count if channel details change
  end;
end;

end.


