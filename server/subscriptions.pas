unit subscriptions;


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

(*

Notes from Thurs Q0 Atlanta 2015
- when content is sent as a response to a notification, it should be a bundle - this is for rest-hook, websockets, and messaging
- payload is mandatory, since a bundle will always be sent
- the subscription id should be one of the links in the bundle (rel to be determined)
- unless otherwise specified, the bundle will be empty
- specify a list of GET operations using the template syntax in the URLS as defined by cds-hook (find somewhere common to specify that)
- server puts a bundle entry in the bundle for each GET operation
- same approach to be used in cds-hook

*)
interface

uses
  SysUtils, Classes, SyncObjs,
  IdHTTP, IdSMTP, IdMessage, idGlobal, fsl_websocket, IdText, IdAttachment, IdPOP3, IdMessageParts, IdExplicitTLSClientServerBase,
  IdOpenSSLIOHandlerClient, IdOpenSSLVersion,
  fsl_base, fsl_utilities, fsl_threads, fsl_fpc,
  fsl_http,
  fdb_manager, fdb_dialects,
  fsl_collections, fsl_stream, fsl_json,
  fhir_objects,  fhir_utilities, fhir_factory, fhir_client, fhir_common,
  session, utilities, fhir_parser, fsl_logging;

const
  EXTENSION_PREFETCH = 'http://www.healthintersections.com.au/fhir/StructureDefinition/subscription-prefetch';

Type
  TFHIRSubscriptionWOperation = (subscriptionCreate, subscriptionUpdate, subscriptionDelete);

  TFHIRIdAttachment = class (TIdAttachment)
  private
    FStream : TMemoryStream;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function OpenLoadStream: TStream; override;
    procedure CloseLoadStream; override;
    function  PrepareTempStream: TStream; override;
    procedure FinishTempStream; override;
  end;

  TFHIRSubscriptionTracker = class (TFslObject)
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

  TWebSocketQueueInfo = class (TFslObject)
  private
    FConnected: boolean;
    FQueue: TFslList<TFslBuffer>;
    FEvent: TEvent;
    FPersistent: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TWebSocketQueueInfo; overload;

    property Connected : boolean read FConnected write FConnected;
    property Persistent : boolean read FPersistent write FPersistent;
    property Event : TEvent read FEvent;
    property Queue : TFslList<TFslBuffer> read FQueue;
  end;

  TFHIRSubscriptionTrackerList = class (TFslObjectList)
  private
    function GetTracker(i: integer): TFHIRSubscriptionTracker;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    procedure addOrUpdate(Key : integer; info : String; status : TSubscriptionStatus); overload;
    function getByKey(Key : integer) : TFHIRSubscriptionTracker;
    property TrackerItem[i : integer] : TFHIRSubscriptionTracker read GetTracker; default;
  end;

  TSubscriptionTopic = class (TFslObject)
  private
    FResource: TFHIRResourceV;
    FId: String;
    FUrl: String;
    procedure SetResource(const Value: TFHIRResourceV);
  public
    destructor Destroy; override;
    function link : TSubscriptionTopic;
    property id : String read FId write FId;
    property url : String read FUrl write FUrl;
    property resource : TFHIRResourceV read FResource write SetResource;
  end;

  TFHIRSubscriptionEntry = class (TFslObject)
  private
    FSubscription : TFHIRSubscriptionW;
    FKey : Integer;
    FResourceTypes: TArray<Integer>;
    FId: String;
    procedure SetFHIRSubscriptionW(const Value: TFHIRSubscriptionW);
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Property Key : integer read FKey write FKey;
    Property Id : String read FId write FId;
    Property ResourceTypes : TArray<Integer> read FResourceTypes write FResourceTypes;
    Property Subscription : TFHIRSubscriptionW read FSubscription write SetFHIRSubscriptionW;

    function hasResourceType(key : Integer) : boolean;
  end;

  TFHIRSubscriptionEntryList = class (TFslObjectList)
  private
    function GetEntry(i: integer): TFHIRSubscriptionEntry;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    procedure add(Key : Integer; ResourceTypes : TArray<Integer>; id : String; Subscription : TFHIRSubscriptionW); overload;
    function getByKey(Key : integer) : TFHIRSubscriptionEntry;
    property EntryItem[i : integer] : TFHIRSubscriptionEntry read GetEntry; default;
  end;

  TGetSessionEvent = function (userkey : Integer) : TFhirSession of object;
  TExecuteOperationEvent = procedure(request : TFHIRRequest; response : TFHIRResponse; bWantSession : boolean) of object;
  TExecuteSearchEvent = function (typekey : Integer; compartment : TFHIRCompartmentId; sessionCompartments: TFslList<TFHIRCompartmentId>; params : THTTPParameters; conn : TFDBConnection): String of object;

  TSubscriptionManager = class abstract (TFHIRServerWorker)
  private
    FSubscriptions : TFHIRSubscriptionEntryList;
    FSubscriptionTrackers  : TFHIRSubscriptionTrackerList;
    FLastSubscriptionKey, FLastNotificationQueueKey, FLastWebSocketKey : integer;
    FDatabase: TFDBManager;
    EmptyQueue : boolean;
    FOnExecuteOperation : TExecuteOperationEvent;
    FOnExecuteSearch : TExecuteSearchEvent;
    FBase : String;
    FOnGetSessionEvent: TGetSessionEvent;
    FLastPopCheck : TDateTime;
    FSubscriptionTopics : TFslMap<TSubscriptionTopic>;

    FCloseAll : boolean;
    FSemaphores : TFslMap<TWebSocketQueueInfo>;
    FLang : THTTPLanguages;
//    FLoading: boolean;

    function settings : TFHIRServerSettings;
    function factory : TFHIRFactory;
    function wsWait(id : String) : boolean;
    function wsConnect(id : String; persistent : boolean) : boolean;
    procedure wsDisconnect(id : String);
    procedure wsWakeAll;
    procedure wsWake(id : String);
    function wsPersists(id : String; b : TBytes) : boolean;

    function chooseSMTPPort(direct : boolean): String;
    function chooseSMTPPassword(direct : boolean): String;
    function chooseSMTPHost(direct : boolean): String;
    function chooseSMTPSender(direct : boolean): String;
    function chooseSMTPUsername(direct : boolean): String;

    function determineResourceTypeKeys(subscription : TFHIRSubscriptionW; conn : TFDBConnection) : TArray<integer>; overload;
    function determineResourceTypeKeys(criteria : String; conn : TFDBConnection) : TArray<integer>; overload;
    procedure SeeNewSubscription(key : Integer; id : String; subscription : TFHIRResourceV; session : TFHIRSession; conn : TFDBConnection);
    function ProcessSubscription(conn : TFDBConnection): Boolean;
    function ProcessNotification(conn : TFDBConnection): Boolean;
    procedure createNotification(vkey, skey : integer; created : boolean; conn : TFDBConnection);
    function LoadResourceFromDBByKey(conn : TFDBConnection; key: integer; var userkey : integer) : TFhirResourceV;

    procedure doSendEmail(subst : TFHIRSubscriptionW; resource : TFHIRResourceV; dest : String; direct : boolean);  overload;
    procedure sendDirectResponse(id, address, message: String; ok: boolean);
    procedure processReportDeliveryMessage(id : string; txt : String; details : TStringList);
    procedure processReportDeliveryNotification(id : string; txt : String; details : TStringList);
    procedure processDirectMessage(txt, ct : String; res : TBytesStream);
    procedure processIncomingDirectMessage(msg : TIdMessage);

    procedure sendByRest(id : String; subst : TFHIRSubscriptionW; package : TFHIRResourceV);
    procedure sendByEmail(id : String; subst : TFHIRSubscriptionW; package : TFHIRResourceV);  overload;
    procedure sendBySms(id : String; subst : TFHIRSubscriptionW; package : TFHIRResourceV);
    procedure sendByWebSocket(conn : TFDBConnection; id : String; subst : TFHIRSubscriptionW; package : TFHIRResourceV);
//    procedure processByScript(conn : TFDBConnection; id : String; subst : TFHIRSubscriptionW; package : TFHIRResourceV);

//    procedure saveTags(conn : TFDBConnection; ResourceKey : integer; res : TFHIRResourceV);
    procedure NotifySuccess(userkey, SubscriptionKey : integer);
    procedure NotifyFailure(userkey, SubscriptionKey : integer; message : string);
    procedure DoDropResource(key, vkey, pvkey : Integer; internal : boolean);
    procedure HandleWebSocketBind(id: String; connection: TIdWebSocket);
    procedure HandleWebSocketSubscribe(json : TJsonObject; connection: TIdWebSocket);
    function checkForClose(connection: TIdWebSocket; id : String; worked: boolean): boolean;
    procedure ApplyUpdateToResource(userkey : integer; id : String; resource : TFhirResourceV);
    procedure checkSaveTags(subst : TFHIRSubscriptionW; conn : TFDBConnection; key : integer; res : TFHIRResourceV);
  protected
    FLock : TFslLock;
    function MeetsCriteriaSearch(criteria : String; res : TFHIRResourceV; typekey : Integer; key : integer; conn : TFDBConnection) : boolean;
    function LoadResourceFromDBByVer(conn : TFDBConnection; vkey: integer; var id : String) : TFhirResourceV; overload;
    function LoadResourceFromDBByVer(conn : TFDBConnection; vkey: integer; var id : String; canBeNull : boolean) : TFhirResourceV; overload;

    procedure checkAcceptable(subscription : TFHIRSubscriptionW; session : TFHIRSession); virtual; abstract;
    function makeSubscription(resource : TFHIRResourceV) : TFHIRSubscriptionW; virtual; abstract;
    function preparePackage(userkey : integer; created : boolean; subscription : TFHIRSubscriptionW; resource : TFHIRResourceV) : TFHIRResourceV; virtual; abstract;
    function MeetsCriteria(subscription : TFHIRSubscriptionW; typekey, key, ResourceVersionKey, ResourcePreviousKey : integer; newRes, oldRes : TFHIRResourceV; conn : TFDBConnection) : boolean; virtual; abstract;
    function checkSubscription(subscription: TFHIRResourceV) : TFHIRSubscriptionW; virtual; abstract;
    function checkSubscriptionTopic(subscription: TFHIRResourceV) : TFHIRSubscriptionTopicW; virtual; abstract;
    function loadSubscriptionTopic(res : TFHIRResourceV) : TSubscriptionTopic; virtual; abstract;
    function loadSubscription(res : TFHIRResourceV) : TFHIRSubscriptionW; virtual; abstract;
    function bundleIsTransaction(res : TFHIRResourceV) : boolean; virtual; abstract;
    function processUrlTemplate(url : String; resource : TFhirResourceV) : String; virtual; abstract;
    function determineResourceTypeKeys(topic: TSubscriptionTopic; conn: TFDBConnection): TArray<integer>; overload; virtual; abstract;
    // function getSummaryForChannel(subst : TFHIRSubscriptionW) : String; virtual; abstract;

  public
    constructor Create(ServerContext : TFslObject);
    destructor Destroy; Override;

    procedure loadQueue(conn : TFDBConnection);
    procedure SeeResource(key, vkey, pvkey : Integer; id : String; op : TFHIRSubscriptionWOperation; resource : TFHIRResourceV; conn : TFDBConnection; reload : boolean; session : TFHIRSession);
    procedure DropResource(key, vkey, pvkey : Integer);
    procedure Process; // spend up to 30 seconds working on subscriptions
    procedure ProcessEmails; // on a separate thread to Process
    procedure HandleWebSocket(connection : TIdWebSocket);
    procedure DoneLoading(conn : TFDBConnection);

    procedure sendByEmail(resource : TFHIRResourceV; dest : String; direct : boolean); overload;
    property SubscriptionTopics : TFslMap<TSubscriptionTopic> read FSubscriptionTopics;
    function getSubscriptionTopic(canonical : string): TSubscriptionTopic; virtual;

    Property Database : TFDBManager read FDatabase write FDatabase;

    Property Base : String read FBase write FBase;
    Property OnExecuteOperation : TExecuteOperationEvent read FOnExecuteOperation write FOnExecuteOperation;
    Property OnExecuteSearch : TExecuteSearchEvent read FOnExecuteSearch write FOnExecuteSearch;
    Property OnGetSessionEvent : TGetSessionEvent read FOnGetSessionEvent write FOnGetSessionEvent;
  end;

implementation

uses
  server_context,
  fsl_twilio;

{ TSubscriptionManager }

constructor TSubscriptionManager.Create(ServerContext : TFslObject);
begin
  inherited Create(ServerContext as TFHIRServerContext);
  FLock := TFslLock.Create('Subscriptions');
  FSubscriptions := TFHIRSubscriptionEntryList.Create;
  FSubscriptionTrackers := TFHIRSubscriptionTrackerList.Create;
  FSemaphores := TFslMap<TWebSocketQueueInfo>.Create('subscr.semaphore');
  FCloseAll := false;
  FLastPopCheck := 0;
  FSubscriptionTopics := TFslMap<TSubscriptionTopic>.create('subscr.event');
  FLang := THTTPLanguages.create('en');
end;

destructor TSubscriptionManager.Destroy;
begin
  wsWakeAll;
  FSemaphores.Free;
  FSubscriptionTrackers.Free;
  FSubscriptionTopics.Free;
  FSubscriptions.Free;
  FLock.Free;
  FDatabase.Free;
  inherited;
end;

procedure TSubscriptionManager.ApplyUpdateToResource(userkey : integer; id: String; resource: TFhirResourceV);
var
  request : TFHIRRequest;
  response : TFHIRResponse;
begin
  request := TFHIRRequest.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, roSubscription, TFHIRServerContext(ServerContext).Indexes.Compartments.Link);
  response := TFHIRResponse.Create(TFHIRServerContext(ServerContext).ValidatorContext.link);
  try
    request.internalRequestId := TFHIRServerContext(ServerContext).Globals.nextRequestId;
    request.Id := id;
    if (userkey <> 0) then
      request.Session := OnGetSessionEvent(userkey);
    request.Resource := resource.Link;
    request.ResourceName := resource.fhirType;
    request.CommandType := fcmdUpdate;
    request.LoadParams('');
    OnExecuteOperation(request, response, false);
    // and we ignore the outcome, because what can we do?
  finally
    request.Free;
    response.Free;
  end;
end;


procedure TSubscriptionManager.processEmails;
var
  pop : TIdPOP3;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
  c, i : integer;
begin
//  if FDirectPopHost = '' then
    exit();
  if FLastPopCheck > now - DATETIME_MINUTE_ONE then
    exit();
  try
    pop := TIdPop3.Create(Nil);
    try
      pop.Host := settings.DirectPopHost;
      pop.port := StrToInt(settings.DirectPopPort);
      pop.Username := settings.DirectUsername;
      pop.Password := settings.DirectPassword;
      if settings.SMTPUseTLS then
      begin
        ssl := TIdOpenSSLIOHandlerClient.create;
        pop.IOHandler := ssl;
        pop.UseTLS := utUseExplicitTLS;
        ssl.Destination := settings.DirectPopHost+':'+settings.DirectPopPort;
        ssl.Host := settings.DirectPopHost;
        ssl.MaxLineAction := maException;
        ssl.Port := StrToInt(settings.DirectPopPort);
        ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
        ssl.Options.VerifyServerCertificate := false;
      end;
      pop.Connect();
      try
        c := pop.CheckMessages;
        for i := 0 to c - 1 do
        begin
          msg := TIdMessage.Create(nil);
          try
            pop.Retrieve(i+1, msg);
            processIncomingDirectMessage(msg);
          finally
            msg.Free;
          end;
        end;
        for i := 0 to c - 1 do
          pop.Delete(i+1);
      finally
        pop.Disconnect;
      end;
    finally
      pop.Free;
    end;
  except
    on e : Exception do
      Logging.log('Exception checking email: '+e.message);
  end;
  FLastPopCheck := now;
end;

procedure TSubscriptionManager.DropResource(key, vkey, pvkey: Integer);
var
  evd : TSubscriptionTopic;
begin
  DoDropResource(key, vKey, pvkey, false);
  FLock.Lock('DropResource');
  try
    if FSubscriptionTopics.ContainsKey('key:'+inttostr(key)) then
    begin
      evd := FSubscriptionTopics['key:'+inttostr(key)];
      FSubscriptionTopics.Remove(evd.id);
      FSubscriptionTopics.Remove(evd.url);
      FSubscriptionTopics.Remove('key:'+inttostr(key));
    end;
  finally
    FLock.Unlock;
  end;
end;

function TSubscriptionManager.factory: TFHIRFactory;
begin
  result := TFHIRServerContext(ServerContext).Factory;
end;

function TSubscriptionManager.getSubscriptionTopic(canonical: string): TSubscriptionTopic;
begin
  result := nil;
  FLock.Lock('getSubscriptionTopic');
  try
    if not FSubscriptionTopics.TryGetValue(canonical, result) then
    begin
      if canonical.StartsWith('SubscriptionTopic/') then
        FSubscriptionTopics.TryGetValue(canonical.Substring(16), result);
    end;
  finally
    FLock.Unlock;
  end;

end;

function TSubscriptionManager.checkForClose(connection: TIdWebSocket; id : String; worked : boolean) : boolean;
var
  i, t : integer;
  cmd : TIdWebSocketCommand;
begin
  if worked then
    t := 1
  else
    t := 100;
  result := false;

  i := 0;
  while not wsWait(id) and not result and (i < t) do
  begin
    inc(i);
    if FCloseAll or  not connection.IsOpen then
      result := true
    else
    begin
      cmd := connection.read(false);
      if cmd.op <> wsoNoOp then
      begin
        if cmd.op = wsoClose then
          result := true
        else
          connection.write('Unexpected operation');
      end;
    end;
  end;
end;

procedure TSubscriptionManager.checkSaveTags(subst: TFHIRSubscriptionW; conn: TFDBConnection; key: integer; res: TFHIRResourceV);
begin
  // todo....
end;

procedure TSubscriptionManager.HandleWebSocketSubscribe(json : TJsonObject; connection: TIdWebSocket);
//var
//  parser : TFHIRJsonParser;
//  subscription : TFhirSubscription;
begin
//  parser := TFHIRJsonParser.Create(nil, connection.request.AcceptLanguage);
//  try
//    subscription := parser.ParseFragment(json, 'TFhirSubscription') as TFhirSubscription;
//  finally
//    parser.Free;
//  end;
//  try
//    subscription.id := NewGuidId;
////    SeeNewSubscription(-1, vkey, id, resource as TFhirSubscription, session, conn);
////    register subscription
////    loop until disconnected
////    unregister subscription
////    disconnect
//  finally
//    subscription.free;
//  end;
end;

procedure TSubscriptionManager.HandleWebSocketBind(id : String; connection: TIdWebSocket);
var
  conn : TFDBConnection;
  key : integer;
  cnt : String;
begin
  if (not IsId(id)) then
    raise EFHIRException.create('invalid syntax');
  if not wsConnect(id, true) then
    raise EFHIRException.create('There is already a web socket bound to '+id);
  try
    connection.write('bound '+id);
    repeat
      conn := FDatabase.GetConnection('subscription');
      try
        conn.SQL := 'select top 1 WebSocketsQueueKey, Content from WebSocketsQueue where SubscriptionId = :sid and handled = 0 order by WebSocketsQueueKey asc';
        conn.prepare;
        conn.BindString('sid', id);
        conn.Execute;
        if (conn.FetchNext) then
        begin
          key := conn.ColIntegerByName['WebSocketsQueueKey'];
          cnt := TEncoding.UTF8.GetString(conn.ColBlobByName['Content']);
          if (cnt = '') then
            cnt := 'ping :'+id;
          connection.write(cnt);
          conn.Terminate;
          conn.ExecSQL('delete from WebSocketsQueue where WebSocketsQueueKey = '+inttostr(key));
        end
        else
        begin
          key := 0;
          conn.Terminate;
        end;
        conn.Release;
      except
        on e : Exception do
        begin
          conn.Error(e);
          raise;
        end;
      end;
    until checkForClose(connection, id, key <> 0);
  finally
    wsDisconnect(id);
  end;
end;

procedure TSubscriptionManager.HandleWebSocket(connection: TIdWebSocket);
var
  cmd : TIdWebSocketCommand;
  json : TJsonObject;
begin
  try
    try
      cmd := connection.read(true);
      if cmd.op <> wsoText then
        raise EFHIRException.create('No Bind Statement');
      if cmd.text.StartsWith('bind ') then
        handleWebSocketBind(cmd.text.Substring(5), connection)
      else if not cmd.text.Trim.StartsWith('{') then
        raise EFHIRException.create('invalid syntax - expected bind :id, or a JSON object')
      else
      begin
        json := TJSONParser.Parse(cmd.text);
        try
          if (json.str['type'] = 'bind-subscription') then
            handleWebSocketBind(json.str['id'], connection)
          else if (json.str['type'] = 'create-subscription') then
            handleWebSocketSubscribe(json.obj['subscription'], connection)
          else
            raise EFHIRException.create('invalid syntax - expected type "bind-subscription" or "create-subscription"');
        finally
          json.Free;
        end;
      end;
    except
      on e : Exception do
        connection.write(e.message);
    end;
  finally
    connection.close;
  end;
end;

procedure TSubscriptionManager.DoDropResource(key, vkey, pvkey: Integer; internal : boolean);
var
  i : integer;
  dodelete : boolean;
  conn : TFDBConnection;
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

procedure TSubscriptionManager.DoneLoading(conn : TFDBConnection);
var
  i : integer;
  sub : TFHIRSubscriptionEntry;
  topic : TSubscriptionTopic;
begin
  for i := FSubscriptions.Count - 1 downto 0 do
  begin
    sub := FSubscriptions[i];
    if (length(sub.FResourceTypes) = 1) and (sub.FResourceTypes[0] = -1) then
    begin
      topic := getSubscriptionTopic(sub.FSubscription.topic);
      if topic = nil then
      begin
        // this is an error condition; we're going to remove this subscription
        NotifyFailure(0, sub.Key, 'Topic '+sub.FSubscription.topic+' not found');
        FSubscriptions.DeleteByIndex(i);
      end
      else
      begin
        sub.ResourceTypes := determineResourceTypeKeys(topic, conn);
      end;
    end;
  end;
end;

procedure TSubscriptionManager.SeeNewSubscription(key: Integer; id : String; subscription: TFHIRResourceV; session: TFHIRSession; conn : TFDBConnection);
var
  s : TFHIRSubscriptionW;
begin
  s := checkSubscription(subscription);
  if s <> nil then
  begin
    DoDropResource(Key, 0, 0, true); // delete any previously existing entry for this subscription
    FSubscriptions.add(key, determineResourceTypeKeys(s, conn), id, s);
    FSubscriptionTrackers.addorUpdate(key, s.summary, s.status);
  end;
end;

procedure TSubscriptionManager.SeeResource(key, vkey, pvkey: Integer; id : String; op : TFHIRSubscriptionWOperation; resource: TFHIRResourceV; conn : TFDBConnection; reload: boolean; session : TFHIRSession);
var
  evd : TSubscriptionTopic;
begin
  FLock.Enter('SeeResource');
  try
    if resource.fhirType = 'Subscription' then
      SeeNewSubscription(key, id, resource, session, conn);

    if resource.fhirType = 'SubscriptionTopic' then
    begin
      evd := loadSubscriptionTopic(resource);
      try
        FLock.Lock('SeeResource');
        try
          FSubscriptionTopics.addOrSetValue(evd.url, evd.link);
          FSubscriptionTopics.addOrSetValue(evd.id, evd.link);
          FSubscriptionTopics.addOrSetValue('key:'+inttostr(key), evd.link);
        finally
          FLock.Unlock;
        end;
      finally
        evd.free;
      end;
    end;

    if not reload then // ignore if we are starting up
    begin
      // we evaluate the criteria retrospectively in a different thead, so for now, all we do is add the entry to a queue
      inc(FLastSubscriptionKey);
      if pvkey = 0 then
        conn.ExecSQL('Insert into SubscriptionQueue (SubscriptionQueueKey, ResourceKey, ResourceVersionKey, ResourcePreviousKey, Operation, Entered) values ('+inttostr(FLastSubscriptionKey)+', '+inttostr(key)+', '+inttostr(vkey)+', null, '+inttostr(ord(op)+1)+', '+DBGetDate(conn.Owner.Platform)+')')
      else
        conn.ExecSQL('Insert into SubscriptionQueue (SubscriptionQueueKey, ResourceKey, ResourceVersionKey, ResourcePreviousKey, Operation, Entered) values ('+inttostr(FLastSubscriptionKey)+', '+inttostr(key)+', '+inttostr(vkey)+', '+inttostr(pvkey)+', '+inttostr(ord(op)+1)+', '+DBGetDate(conn.Owner.Platform)+')');
      EmptyQueue := false;
    end;

  finally
    FLock.Leave;
  end;
end;


procedure TSubscriptionManager.doSendEmail(subst : TFHIRSubscriptionW; resource : TFHIRResourceV; dest : String; direct : boolean);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
  comp : TFHIRComposer;
  part: TIdText;
  m : TMemoryStream;
  att : TIdAttachment;
begin
  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := chooseSMTPHost(direct);
    if chooseSMTPPort(direct) <> '' then
      sender.port := StrToInt(chooseSMTPPort(direct));
    sender.Username := chooseSMTPUsername(direct);
    sender.Password := chooseSMTPPassword(direct);
    if settings.SMTPUseTLS then
    begin
      ssl := TIdOpenSSLIOHandlerClient.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := chooseSMTPHost(direct)+':'+chooseSMTPPort(direct);
      ssl.Host := chooseSMTPHost(direct);
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(chooseSMTPPort(direct));
      ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
      ssl.Options.VerifyServerCertificate := false;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      if (subst <> nil) and (length(subst.headers) > 0) then
        msg.Subject := subst.headers[0]
      else
        msg.Subject := 'FHIR Message';
      msg.Recipients.Add.Address := dest;
      msg.From.Text := chooseSMTPSender(direct);
      msg.Body.Clear;
      msg.MsgId := '<'+NewGuidId+'>';
      part := TIdText.Create(msg.MessageParts);
      part.Body.Text := 'This email contains FHIR content as an attachment. Open it with your own personal records program';
      part.ContentType := 'text/plain';
      part.ContentTransfer := '7bit';
      if subst = nil then
        comp := factory.makeComposer(TFHIRServerContext(ServerContext).ValidatorContext.link, ffJson, FLang, OutputStylePretty)
      else if subst.payload <> '' then
        comp := factory.makeComposer(TFHIRServerContext(ServerContext).ValidatorContext.link, mimeTypeToFormat(subst.payload), FLang, OutputStylePretty)
      else
        comp := nil;
      try
        if comp <> nil then
        begin
          m := TMemoryStream.Create;
          try
            comp.Compose(m, resource);
            m.Position := 0;
            att := TFHIRIdAttachment.Create(msg.MessageParts);
            att.LoadFromStream(m);
            att.ContentDisposition := 'Content-Disposition: attachment; filename="bundle'+comp.extension+'"';
            att.ContentType := comp.MimeType+'; charset=UTF-8'
          finally
            m.Free;
          end;
        end
        else
        begin
          part.Body.Text := 'This email informs you that the FHIR content at '+TFHIRServerContext(ServerContext).FormalURLPlain+'/'+resource.fhirType+'/'+resource.id+' has been updated. Retrieve it with your own personal records program';
        end;
      finally
        comp.Free;
      end;
      Logging.log('Send '+msg.MsgId+' to '+dest);
//      msg.SaveToFile('c:\temp\out.msg');
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

procedure TSubscriptionManager.sendByEmail(id : String; subst: TFHIRSubscriptionW; package : TFHIRResourceV);
begin
  doSendEmail(subst, package, subst.endpoint.Replace('mailto:', ''), subst.direct);
end;

procedure TSubscriptionManager.sendByEmail(resource : TFHIRResourceV; dest: String; direct : boolean);
begin
  doSendEmail(nil, resource, dest, direct);
end;

procedure TSubscriptionManager.sendByRest(id : String; subst: TFHIRSubscriptionW; package : TFHIRResourceV);
var
  http : TIdHTTP;
  client : TFhirClientV;
  ssl : TIdOpenSSLIOHandlerClient;
  stream : TMemoryStream;
  s : String;
begin
  if subst.payload = '' then
  begin
    stream := TMemoryStream.Create;
    http := TIdHTTP.create(nil);
    ssl := TIdOpenSSLIOHandlerClient.Create(nil);
    try
      http.IOHandler := ssl;
      for s in subst.headers do
        http.Request.CustomHeaders.Add(s);
      http.Post(subst.endpoint, stream);
    finally
      ssl.Free;
      http.Free;
      stream.Free;
    end;
  end
  else
  begin
    client := factory.makeClient(TFHIRServerContext(ServerContext).ValidatorContext.link, subst.endpoint, mimeTypeToFormat(subst.payload));
    try
      if (package.fhirType = 'Bundle') and bundleIsTransaction(package) then
        client.transactionV(package)
      else
        client.createResourceV(package, id);
    finally
      client.Free;
    end;
  end;
end;

procedure TSubscriptionManager.sendBySms(id: String; subst: TFHIRSubscriptionW; package : TFHIRResourceV);
var
  client : TTwilioClient;
begin
  client := TTwilioClient.Create;
  try
    client.Account := settings.SMSAccount;
    client.Token := settings.SMSToken;
    client.From := settings.SMSFrom;
    if subst.endpoint.StartsWith('tel:') then
      client.dest := subst.endpoint.Substring(4)
    else
      client.dest := subst.endpoint;
    if subst.payload <> '' then
      client.Body := subst.payload
    else
      client.Body := 'A new matching resource has been received for Subscription '+subst.resource.id;
    client.send;
  finally
    client.Free;
  end;
end;


procedure TSubscriptionManager.sendByWebSocket(conn : TFDBConnection; id: String; subst: TFHIRSubscriptionW; package : TFHIRResourceV);
var
  key : integer;
  comp : TFHIRComposer;
  b : TBytes;
begin
  comp := factory.makeComposer(TFHIRServerContext(ServerContext).ValidatorContext.link, mimeTypeToFormat(subst.payload), FLang, OutputStyleNormal);
  try
    if comp <> nil then
      b := TEncoding.UTF8.GetBytes(comp.Compose(package))
    else
     SetLength(b, 0);
  finally
    comp.Free;
  end;

  if wsPersists(subst.resource.id, b) then
  begin
    FLock.Lock;
    try
      inc(FLastWebSocketKey);
      key := FLastWebSocketKey;
    finally
      FLock.Unlock;
    end;

    conn.SQL := 'insert into WebSocketsQueue (WebSocketsQueueKey, SubscriptionId, Handled, Content) values ('+inttostr(key)+', '''+SQLWrapString(subst.resource.id)+''', 0, :c)';
    conn.Prepare;
    if subst.payload = '' then
      conn.BindNull('c')
    else
    begin
      conn.BindBlob('c', b);
    end;
    conn.Execute;
    conn.Terminate;
  end;
  wsWake(subst.resource.id);
end;

procedure TSubscriptionManager.sendDirectResponse(id, address, message: String; ok: boolean);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
  part: TIdText;
  m : TMemoryStream;
  att : TIdAttachment;
  s : String;
  b : TBytes;
begin
  if ok then
    exit; // on advice from Luis Maas
  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := settings.DirectHost;
    if settings.DirectPort <> '' then
      sender.port := StrToInt(settings.DirectPort);
    sender.Username := settings.DirectUsername;
    sender.Password := settings.DirectPassword;
    if settings.SMTPUseTLS then
    begin
      ssl := TIdOpenSSLIOHandlerClient.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := settings.DirectHost+':'+settings.DirectPort;
      ssl.Host := settings.DirectHost;
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(settings.DirectPort);
      ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
      ssl.Options.VerifyServerCertificate := false;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := 'Direct Response';
      msg.Recipients.Add.Address := address;
      msg.From.Text := settings.DirectSender;
      msg.Body.Clear;
      msg.MsgId := NewGuidId;
      msg.InReplyTo := id;
      part := TIdText.Create(msg.MessageParts);
      att := TFHIRIdAttachment.Create(msg.MessageParts);
      part.ContentType := 'text/plain';
      part.ContentTransfer := '7bit';
      att.ContentTransfer := '7bit';
      att.ContentType := 'Content-Disposition: attachment';
      if ok then
      begin
        part.Body.Text := 'The receiving server has accepted your message and will attempt to process it.';
        att.ContentType := 'message/disposition-notification';
        s :=
          'Reporting-UA: '+settings.DirectUsername+'; ('+TFHIRServerContext(ServerContext).Globals.OwnerName+')'+#13#10+
          'Final-Recipient: '+settings.DirectUsername+#13#10+
          'Original-Message-ID: '+id+#13#10+
          'Disposition: automatic-action/MDN-sent-automatically;processed'+#13#10;
      end
      else
      begin
        part.Body.Text := 'Error accepting your message: '+message;
        att.ContentType := 'message/delivery-status';
        att.ContentType := 'Content-Disposition: attachment ';
        s := 'Reporting-MTA: dns;'+settings.DirectUsername+#13#10+
             'X-Original-Message-ID: '+id+#13#10+
             ''+#13#10+
             'Final-Recipient: rfc822'+settings.DirectUsername+#13#10+
             'Action: failed'+#13#10+
             'Diagnostic-Code: '+message+#13#10;
      end;
      b := TEncoding.ANSI.GetBytes(s);
      m := TMemoryStream.Create;
      try
        m.write(b[0], length(b));
        m.position := 0;
        att.LoadFromStream(m);
      finally
        m.Free;
      end;
      Logging.log('Send response for '+id+' as '+msg.MsgId+' to '+address+' as '+BoolToStr(ok)+' ('+message+')');
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

function TSubscriptionManager.settings: TFHIRServerSettings;
begin
  result := TFHIRServerContext(ServerContext).Globals;
end;

procedure TSubscriptionManager.Process;
var
  finish : TDateTime;
  found : boolean;
  conn : TFDBConnection;
begin
  if EmptyQueue then
    exit;
  try
    finish := now + (DATETIME_MINUTE_ONE / 2);
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
      Logging.log('Error handling subscriptions: '+e.Message);
  end;
end;

function partByContentType(parts: TIdMessageParts; ct : String) : TIdMessagePart;
var
  i : integer;
  part : TIdMessagePart;
begin
  result := nil;
  for i := 0 to parts.Count - 1 do
  begin
    part := parts[i];
    if part.ContentType = ct then
      exit(part);
  end;
end;

//procedure TSubscriptionManager.processByScript(conn: TFDBConnection; id: String; subst: TFHIRSubscriptionW; package: TFHIRResourceV);
//begin
//  raise EFHIRTodo.create('TSubscriptionManager.processByScript');
//end;
//
procedure TSubscriptionManager.processDirectMessage(txt, ct: String; res: TBytesStream);
var
  p : TFHIRParser;
begin
  p := factory.makeParser(TFHIRServerContext(ServerContext).ValidatorContext, mimeTypeToFormat(ct), FLang);
  try
    p.source := res;
    p.Parse;
    p.resource.Tags['process'] := 'true';
    TFHIRServerContext(ServerContext).Storage.QueueResource(nil, p.resource);
  finally
    p.Free;
  end;
end;

procedure TSubscriptionManager.processIncomingDirectMessage(msg: TIdMessage);
var
  id, s : String;
  ct : TArray<String>;
  part : TIdMessagePart;
  ss : TStringStream;
  ts : TStringList;
  bs : TBytesStream;
begin
//  msg.SaveToFile('c:\temp\in.msg');
  ct := msg.ContentType.split([' ']);
  if (ct[0] = 'multipart/report;') and (ct[1] = 'report-type=delivery-status') then
  begin
    id := msg.InReplyTo;
    part := partByContentType(msg.MessageParts, 'text/plain');
    if (part <> nil) then
      s := TIdText(part).Body.Text;
    part := partByContentType(msg.MessageParts, 'message/delivery-status');
    if (part <> nil) then
    begin
      ts := TStringList.Create;
      try
        ts.NameValueSeparator := ':';
        ss := TStringStream.Create;
        try
          TIdAttachment(part).SaveToStream(ss);
          ts.Text := ss.DataString;
        finally
          ss.Free;
        end;
        processReportDeliveryMessage(id, s, ts);
      finally
        ts.Free;
      end;
    end
    else
      Logging.log('email from '+msg.Sender.Text+' could not be processed');
  end
  else if (ct[0] = 'multipart/report;') and (ct[1] = 'report-type=disposition-notification') then
  begin
    id := msg.InReplyTo;
    part := partByContentType(msg.MessageParts, 'text/plain');
    if (part <> nil) then
      s := TIdText(part).Body.Text;
    part := partByContentType(msg.MessageParts, 'message/disposition-notification');
    if (part <> nil) then
    begin
      ts := TStringList.Create;
      try
        ts.NameValueSeparator := ':';
        ss := TStringStream.Create;
        try
          TIdAttachment(part).SaveToStream(ss);
          ts.Text := ss.DataString;
        finally
          ss.Free;
        end;
        processReportDeliveryNotification(id, s, ts);
      finally
        ts.Free;
      end;
    end
    else
      Logging.log('email from '+msg.Sender.Text+' could not be processed');
  end
  else if ct[0] = 'multipart/mixed' then // direct message
  begin
    try
      id := msg.MsgId;
      part := partByContentType(msg.MessageParts, 'text/plain');
      if (part <> nil) then
        s := TIdText(part).Body.Text;
      part := partByContentType(msg.MessageParts, 'application/octet-stream');
      if id = '' then
        raise EFHIRException.create('No id found');
      if (part = nil) then
        raise EFHIRException.create('Unable to find direct body');
      bs := TBytesStream.Create;
      try
        TIdAttachment(part).SaveToStream(bs);
        bs.position := 0;
        processDirectMessage(s, part.ContentType, bs);
        sendDirectResponse(id, msg.sender.Address, '', true);
      finally
        bs.Free;
      end;
    except
      on e : Exception do
      begin
        if msg.Sender <> nil then
          sendDirectResponse(id, msg.sender.Address, e.Message, false);
        Logging.log('processing incoming direct message from '+msg.sender.Address+' failed: '+e.Message);
      end;
    end;
  end
  else
    Logging.log('email from '+msg.Sender.Text+' could not be understood');
end;

function TSubscriptionManager.ProcessNotification(conn : TFDBConnection): Boolean;
var
  NotificationnQueueKey, ResourceKey, SubscriptionKey : integer;
  res : TFhirResourceV;
  id : string;
  subst : TFHIRSubscriptionW;
  done, created : boolean;
  tnow, dnow : TDateTime;
  userkey: Integer;
  package : TFHIRResourceV;
begin
  NotificationnQueueKey := 0;
  SubscriptionKey := 0;
  ResourceKey := 0;
  tNow := now;
  created := false;

  conn.SQL := 'Select NotificationQueueKey, ResourceVersionKey, SubscriptionKey, LastTry, ErrorCount, Operation from NotificationQueue where '+
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
      created := conn.ColIntegerByName['Operation'] = 1;
    end;
  finally
    conn.Terminate;
  end;
  if result then
  begin
    try
      subst := loadSubscription(LoadResourceFromDBByKey(conn, SubscriptionKey, userkey));
      try
        res := LoadResourceFromDBByVer(conn, ResourceKey, id);
        try
          package := preparePackage(userkey, created, subst, res);
          try
            case subst.method of
              smRestHook: sendByRest(id, subst, package);
              smEmail: sendByEmail(id, subst, package);
              smSms: sendBySms(id, subst, package);
              smWebsocket: sendByWebSocket(conn, id, subst, package);
//              smChangeScript: processByScript(conn, id, subst, package);
            end;
            checkSaveTags(subst, conn, resourceKey, res);
            conn.ExecSQL('update NotificationQueue set Handled = '+DBGetDate(conn.Owner.Platform)+' where NotificationQueueKey = '+inttostr(NotificationnQueueKey));
          finally
            package.Free;
          end;
        finally
          res.free;
        end;
      finally
        subst.Free;
      end;
      NotifySuccess(userkey, SubscriptionKey);
    except
      on e:exception do
      begin
        conn.ExecSQL('update NotificationQueue set LastTry = '+DBGetDate(conn.Owner.Platform)+', ErrorCount = ErrorCount + 1 where NotificationQueueKey = '+inttostr(NotificationnQueueKey));
        NotifyFailure(userkey, SubscriptionKey, e.message);
      end;
    end;
  end;
end;


procedure TSubscriptionManager.processReportDeliveryMessage(id, txt: String; details: TStringList);
begin
  if details.Values['Action'].Trim = 'failed' then
    Logging.log('Direct Message '+id+' failed: '+details.Values['Diagnostic-Code']+' ('+txt+')');
end;

procedure TSubscriptionManager.processReportDeliveryNotification(id, txt: String; details: TStringList);
begin
  if id = '' then
    id := details.Values['Original-Message-ID'];
  Logging.log('Direct Message '+id+' notice: '+details.Values['Disposition'].Trim);
end;

function TSubscriptionManager.ProcessSubscription(conn: TFDBConnection): Boolean;
var
  SubscriptionQueueKey, ResourceKey, ResourceVersionKey, ResourcePreviousKey, ResourceTypeKey : integer;
  i : integer;
  list : TFHIRSubscriptionEntryList;
  created : boolean;
  id : String;
  oldRes, newRes : TFHIRResourceV;
begin
  SubscriptionQueueKey := 0;
  ResourceKey := 0;
  ResourceVersionKey := 0;
  ResourceTypeKey := 0;
  ResourcePreviousKey := 0;
  created := false;

  conn.SQL := RestrictToNRows(conn.Owner.Platform, 'Select SubscriptionQueueKey, Ids.ResourceKey, SubscriptionQueue.ResourceVersionKey, SubscriptionQueue.ResourcePreviousKey, '+
    'Operation, Ids.ResourceTypeKey from SubscriptionQueue, Ids where '+
    'Handled is null and Ids.ResourceKey = SubscriptionQueue.ResourceKey '+
    'order by SubscriptionQueueKey', 1);
  conn.Prepare;
  try
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      SubscriptionQueueKey := conn.ColIntegerByName['SubscriptionQueueKey'];
      ResourceKey := conn.ColIntegerByName['ResourceKey'];
      ResourceVersionKey := conn.ColIntegerByName['ResourceVersionKey'];
      ResourcePreviousKey := conn.ColIntegerByName['ResourcePreviousKey'];
      ResourceTypeKey := conn.ColIntegerByName['ResourceTypeKey'];
      created := conn.ColIntegerByName['Operation'] = 1;
    end;
  finally
    conn.Terminate;
  end;
  if result then
  begin
    newRes := LoadResourceFromDBByVer(conn, ResourceVersionKey, id, true); // may return null
    try
      oldRes := nil;
      if (ResourcePreviousKey <> 0) then
        oldRes := LoadResourceFromDBByVer(conn, ResourcePreviousKey, id);
      try
        list := TFHIRSubscriptionEntryList.Create;
        try
          // keep the lock short
          FLock.Lock('List Subscriptions');
          try
            for i := 0 to FSubscriptions.Count - 1 do
              list.Add(FSubscriptions[i].FKey, FSubscriptions[i].ResourceTypes, FSubscriptions[i].FId, FSubscriptions[i].FSubscription.Link); // not clone
          finally
            FLock.Leave;
          end;
          conn.StartTransact;
          try
            for i := 0 to list.Count - 1 do
            begin
              if list[i].hasResourceType(ResourceTypeKey) and MeetsCriteria(list[i].Subscription, ResourceTypeKey, ResourceKey, ResourceVersionKey, ResourcePreviousKey, newRes, oldRes, conn) then
              begin
                CreateNotification(ResourceVersionKey, list[i].FKey, created, conn);
              end;
            end;
            conn.ExecSQL('Update SubscriptionQueue set Handled = '+DBGetDate(conn.Owner.Platform)+' where SubscriptionQueueKey = '+inttostr(SubscriptionQueueKey));
            conn.Commit;
          except
            conn.Rollback;
            raise;
          end;
        finally
          list.free;
        end;
      finally
        oldRes.Free;
      end;
    finally
      newRes.Free;
    end;
  end;
end;

//
//procedure TSubscriptionManager.saveTags(conn: TFDBConnection; ResourceKey: integer; res: TFHIRResourceV);
//begin
//{  conn.SQL := 'Update Versions set content = :t where ResourceVersionKey = '+inttostr(ResourceKey);
//  conn.Prepare;
//  try
//    conn.BindBlobFromBytes('t', tags.Json);
//    conn.Execute;
//  finally
//    conn.Terminate;
//  end;
//  }
//end;
//
procedure TSubscriptionManager.loadQueue(conn: TFDBConnection);
begin
  FLastSubscriptionKey := conn.CountSQL('select max(SubscriptionQueueKey) from SubscriptionQueue');
  FLastNotificationQueueKey := conn.CountSQL('select max(NotificationQueueKey) from NotificationQueue');
  FLastWebSocketKey := conn.CountSQL('select max(WebSocketsQueueKey) from WebSocketsQueue');
end;

function TSubscriptionManager.LoadResourceFromDBByVer(conn: TFDBConnection; vkey: integer; var id : String): TFhirResourceV;
begin
  result := LoadResourceFromDBByVer(conn, vkey, id, false);
end;

function TSubscriptionManager.LoadResourceFromDBByVer(conn: TFDBConnection; vkey: integer; var id : String; canBeNull : boolean): TFhirResourceV;
var
  parser : TFHIRParser;
begin
  conn.SQL := 'select ResourceName, Ids.Id, Status, Tags, XmlContent From Versions, Ids, Types where ResourceVersionKey = '+inttostr(vkey)+' and Versions.ResourceKey = IDs.ResourceKey and IDs.ResourceTypeKey = Types.ResourceTypeKey';
  conn.prepare;
  try
    conn.Execute;
    if not conn.FetchNext then
      raise EFHIRException.create('Cannot find resource');
    id := conn.ColStringByName['Id'];
    if conn.ColIntegerByName['Status'] = 2 then
    begin
      // deleted
      if canBeNull then
        exit(nil)
      else
      raise EFHIRException.create('Resource has been deleted');
    end;
    if conn.ColStringByName['ResourceName'] = 'Binary' then
      result := LoadBinaryResource(factory, THTTPLanguages.create('en'), conn.ColBlobByName['XmlContent'])
    else
    begin
      parser := factory.makeParser(TFHIRServerContext(ServerContext).ValidatorContext.link, ffXml, FLang);
      try
        result := parser.parseResource(conn.ColBlobByName['XmlContent']);
      finally
        parser.free;
      end;
    end;
  finally
    conn.terminate;
  end;
end;

function TSubscriptionManager.LoadResourceFromDBByKey(conn: TFDBConnection; key: integer; var userkey : integer): TFhirResourceV;
var
  parser : TFHIRParser;
begin
  conn.SQL := 'select ResourceName, Ids.Id, UserKey, XmlContent From Versions, Ids, Types, Sessions '+
    'where IDs.ResourceKey = '+inttostr(key)+' and Versions.SessionKey = Sessions.SessionKey and '+
    'Versions.ResourceVersionKey = IDs.MostRecent and IDs.ResourceTypeKey = Types.ResourceTypeKey';
  conn.prepare;
  try
    conn.Execute;
    if not conn.FetchNext then
      raise EFHIRException.create('Cannot find resource');
    userKey := conn.ColIntegerByName['UserKey'];
    if conn.ColStringByName['ResourceName'] = 'Binary' then
      result := LoadBinaryResource(factory, THTTPLanguages.create('en'), conn.ColBlobByName['Content'])
    else
    begin
      parser := factory.makeParser(TFHIRServerContext(ServerContext).ValidatorContext.link, ffXml, FLang);
      try
        result := parser.parseResource(conn.ColBlobByName['XmlContent']);
      finally
        parser.free;
      end;
    end;
  finally
    conn.terminate;
  end;
end;

function TSubscriptionManager.determineResourceTypeKeys(subscription : TFHIRSubscriptionW; conn: TFDBConnection) : TArray<integer>;
begin
  if subscription.topic <> '' then
  begin
    setLength(result, 1);
    result[0] := -1; // look this up later
  end
  else
    result := determineResourceTypeKeys(subscription.criteria, conn);
end;

function TSubscriptionManager.determineResourceTypeKeys(criteria : String; conn: TFDBConnection) : TArray<integer>;
var
  t : string;
begin
  if criteria.StartsWith('?') then
    SetLength(result, 0)
  else
  begin
    if criteria.IndexOf('?') = -1 then
      t := criteria
    else
      t := criteria.Substring(0, criteria.IndexOf('?'));
    SetLength(result, 1);
    result[0] := conn.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(t)+'''')
  end;
end;

function TSubscriptionManager.MeetsCriteriaSearch(criteria: String; res : TFHIRResourceV; typekey : Integer; key : integer; conn: TFDBConnection): boolean;
var
  l, r, sql : String;
  p : THTTPParameters;
begin
  StringSplit(criteria, '?', l, r);
  if r = '' then
  begin
    result := l = res.fhirType;
  end
  else
  begin
    p := THTTPParameters.create('_type='+l+'&'+r, true);
    try
      sql := FOnExecuteSearch(typekey, nil, nil, p, conn);
      result := conn.CountSQL('select count(*) from Ids where not MostRecent is null and ResourceKey = '+inttostr(key)+' and '+sql) > 0;
    finally
      p.Free;
    end;
  end;
end;

procedure TSubscriptionManager.NotifySuccess(userkey, SubscriptionKey: integer);
var
  tracker : TFHIRSubscriptionTracker;
  notify : boolean;
  entry : TFHIRSubscriptionEntry;
  subst : TFHIRSubscriptionW;
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
    subst.status := ssActive;
    subst.error := '';
    ApplyUpdateToResource(userkey, id, subst.Resource);
  end;
end;

procedure TSubscriptionManager.NotifyFailure(userkey, SubscriptionKey: integer; message: string);
var
  tracker : TFHIRSubscriptionTracker;
  entry : TFHIRSubscriptionEntry;
  subst : TFhirSubscriptionW;
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
      subst.status := ssError
    else
      subst.status := ssOff;
    subst.error := message;
    ApplyUpdateToResource(userkey, id, subst.Resource);
  end;
end;

procedure TSubscriptionManager.createNotification(vkey, skey: integer; created : boolean; conn : TFDBConnection);
var
  k : integer;
  c : String;
begin
  FLock.Lock('createNotification');
  try
    inc(FLastNotificationQueueKey);
    k := FLastNotificationQueueKey;
  finally
    FLock.Unlock;
  end;
  if created then
    c := '1'
  else
    c := '2';

  conn.ExecSQL('insert into NotificationQUeue (NotificationQueueKey, SubscriptionKey, ResourceVersionKey, Entered, ErrorCount, Operation) values '+
       '('+inttostr(k)+', '+inttostr(skey)+', '+inttostr(vkey)+', '+DBGetDate(conn.Owner.Platform)+', 0, '+c+')');
end;

{ TFHIRSubscriptionEntryList }

procedure TFHIRSubscriptionEntryList.add(Key : Integer; ResourceTypes : TArray<Integer>; Id : String; Subscription: TFhirSubscriptionW);
var
  entry : TFHIRSubscriptionEntry;
begin
  entry := TFHIRSubscriptionEntry.Create;
  try
    entry.Key := key;
    entry.resourceTypes := ResourceTypes;
    entry.Subscription := subscription;
    entry.Id := id;
    add(entry.link);
  finally
    entry.free;
  end;
end;

function TFHIRSubscriptionEntryList.getByKey(Key: integer): TFHIRSubscriptionEntry;
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

function TFHIRSubscriptionEntryList.GetEntry(i: integer): TFHIRSubscriptionEntry;
begin
  result := TFHIRSubscriptionEntry(ObjectByIndex[i]);
end;

function TFHIRSubscriptionEntryList.ItemClass: TFslObjectClass;
begin
  result := TFHIRSubscriptionEntry;
end;

{ TFHIRSubscriptionTrackerList }

procedure TFHIRSubscriptionTrackerList.addOrUpdate(Key: integer; info: String; status : TSubscriptionStatus);
var
  t : TFHIRSubscriptionTracker;
begin
  t := getByKey(key);
  if t = nil then
  begin
    t := TFHIRSubscriptionTracker.Create;
    try
      t.FKey := key;
      t.FInfo := info;
      if status = ssError then
        t.FErrorCount := 1;

      add(t.Link);
    finally
      t.Free;
    end;
  end
  else
    t.UpdateForInfo(info);
end;

function TFHIRSubscriptionTrackerList.getByKey(Key: integer): TFHIRSubscriptionTracker;
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

function TFHIRSubscriptionTrackerList.GetTracker(i: integer): TFHIRSubscriptionTracker;
begin
  result := TFHIRSubscriptionTracker(ObjectByIndex[i]);
end;

function TFHIRSubscriptionTrackerList.ItemClass: TFslObjectClass;
begin
  result := TFHIRSubscriptionTracker;
end;

function TSubscriptionManager.wsConnect(id : String; persistent : boolean) : boolean;
var
  info : TWebSocketQueueInfo;
begin
  info := TWebSocketQueueInfo.create;
  try
    info.Persistent := persistent;
    FLock.Lock;
    try
      result := not FSemaphores.ContainsKey(id);
      if result then
        FSemaphores.Add(id, info.Link);
    finally
      FLock.Unlock;
    end;
  finally
    info.Free;
  end;
end;

procedure TSubscriptionManager.wsDisconnect(id : String);
begin
  FLock.Lock;
  try
    if not FSemaphores.ContainsKey(id) then
      raise EFHIRException.create('WS Queue not found: '+id);
    FSemaphores.Remove(id);
  finally
    FLock.Unlock;
  end;
end;

function TSubscriptionManager.wsWait(id : String) : boolean;
var
  info : TWebSocketQueueInfo;
begin
  FLock.Lock;
  try
    info := FSemaphores[id];
  finally
    FLock.Unlock;
  end;
  // this - using the signal outside the lock - is safe because only the thread waiting here will remove the info from the list
  result := info.Event.WaitFor(100) = wrSignaled;
end;

function TSubscriptionManager.wsPersists(id : String; b : TBytes) : boolean;
var
  info : TWebSocketQueueInfo;
  buf : TFslBuffer;
begin
  FLock.Lock;
  try
    if not FSemaphores.ContainsKey(id) then
      result := true // since it doesn't exist, it must be persistent
    else
    begin
      info := FSemaphores[id];
      result := info.Persistent;
      if not result then
      begin
        buf := TFslBuffer.Create;
        info.FQueue.Add(buf);
        buf.AsBytes := b;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TSubscriptionManager.wsWake(id : String);
begin
  FLock.Lock;
  try
    if FSemaphores.ContainsKey(id) then
      FSemaphores[id].Event.SetEvent;
  finally
    FLock.Unlock;
  end;
end;

procedure TSubscriptionManager.wsWakeAll;
var
  info : TWebSocketQueueInfo;
  ok : boolean;
begin
  FCloseAll := true;
  FLock.Lock;
  try
    for info in FSemaphores.Values do
      info.Event.SetEvent;
  finally
    FLock.Unlock;
  end;
  repeat
    sleep(100);
    FLock.Lock;
    try
      ok := FSemaphores.Count = 0;
    finally
      FLock.Unlock;
    end;
  until ok;
end;

function TSubscriptionManager.chooseSMTPPort(direct : boolean): String;
begin
  if (direct) then
    result := settings.DirectPort
  else
    result:= settings.SMTPPort;
end;

function TSubscriptionManager.chooseSMTPPassword(direct : boolean): String;
begin
  if (direct) then
    result := settings.DirectPassword
  else
    result:= settings.SMTPPassword;
end;

function TSubscriptionManager.chooseSMTPHost(direct : boolean): String;
begin
  if (direct) then
    result := settings.DirectHost
  else
    result:= settings.SMTPHost;
end;

function TSubscriptionManager.chooseSMTPSender(direct : boolean): String;
begin
  if (direct) then
    result := settings.DirectSender
  else
    result:= settings.SMTPSender;
end;

function TSubscriptionManager.chooseSMTPUsername(direct : boolean): String;
begin
  if (direct) then
    result := settings.DirectUsername
  else
    result:= settings.SMTPUsername;
end;


{ TFHIRSubscriptionEntry }

constructor TFHIRSubscriptionEntry.Create;
begin
  inherited;
end;

destructor TFHIRSubscriptionEntry.Destroy;
begin
  FSubscription.Free;
  inherited;
end;

function TFHIRSubscriptionEntry.hasResourceType(key: Integer): boolean;
var
  i : integer;
begin
  if length(FResourceTypes) = 0 then
    result := true
  else
  begin
    result := false;
    for I in FResourceTypes do
      if (i = key) then
        exit(true);
  end;
end;

procedure TFHIRSubscriptionEntry.SeTFHIRSubscriptionW(const Value: TFhirSubscriptionW);
begin
  FSubscription.Free;
  FSubscription := Value;
end;

{ TFHIRSubscriptionTracker }

procedure TFHIRSubscriptionTracker.UpdateForInfo(info: String);
begin
  if FInfo <> info then
  begin
    FInfo := info;
    FErrorCount := 0; // reset error count if channel details change
  end;
end;

{ TWebSocketQueueInfo }

constructor TWebSocketQueueInfo.create;
begin
  inherited;
  FConnected := false;
  FQueue := TFslList<TFslBuffer>.create;
  FEvent := TEvent.Create(nil, True, False, '');
  FEvent.ResetEvent;
end;

destructor TWebSocketQueueInfo.destroy;
begin
  FQueue.Free;
  Fevent.Free;
  inherited;
end;

function TWebSocketQueueInfo.link: TWebSocketQueueInfo;
begin
  result := TWebSocketQueueInfo(inherited link);
end;

{ TFHIRIdAttachment }

procedure TFHIRIdAttachment.CloseLoadStream;
begin
  // nothing
end;

constructor TFHIRIdAttachment.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TFHIRIdAttachment.Destroy;
begin
  FStream.free;
  inherited;
end;

procedure TFHIRIdAttachment.FinishTempStream;
begin
end;

function TFHIRIdAttachment.OpenLoadStream: TStream;
begin
  FStream.position := 0;
  result := FStream;
end;

function TFHIRIdAttachment.PrepareTempStream: TStream;
begin
  FStream.Clear;
  result := FStream;
end;

{ TSubscriptionTopic }

destructor TSubscriptionTopic.Destroy;
begin
  FResource.free;
  inherited;
end;

function TSubscriptionTopic.link: TSubscriptionTopic;
begin
  result := TSubscriptionTopic(inherited link);
end;

procedure TSubscriptionTopic.SetResource(const Value: TFHIRResourceV);
begin
  FResource.Free;
  FResource := value;
end;


end.


