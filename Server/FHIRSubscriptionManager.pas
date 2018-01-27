unit FHIRSubscriptionManager;

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
  DateSupport, StringSupport, GuidSupport, BytesSupport,
  kCritSct, KDBManager, KDBDialects,  ParseMap,
  AdvObjects, AdvObjectLists, AdvGenerics, AdvBuffers, AdvJson,
  IdHTTP, IdSSLOpenSSL, IdSMTP, IdMessage, IdExplicitTLSClientServerBase, idGlobal, IdWebSocket, IdText, IdAttachment, IdPop3, IdMessageParts,
  FHIRBase, FhirResources, FHIRTypes, FHIRConstants, FHIRUtilities, FHIRClient,
  FhirSupport, FHIRIndexManagers, FHIRServerUtilities, FHIRParser, FHIRParserBase, FHIRPath, FHIRContext, FHIRLog, ServerUtilities;

const
  EXTENSION_PREFETCH = 'http://www.healthintersections.com.au/fhir/StructureDefinition/subscription-prefetch';

Type
  TSubscriptionOperation = (subscriptionCreate, subscriptionUpdate, subscriptionDelete);

  TFHIRIdAttachment = class (TIdAttachment)
  private
    FStream : TMemoryStream;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    function OpenLoadStream: TStream; override;
    procedure CloseLoadStream; override;
    function  PrepareTempStream: TStream; override;
    procedure FinishTempStream; override;
  end;

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

  TWebSocketQueueInfo = class (TAdvObject)
  private
    FConnected: boolean;
    FQueue: TAdvList<TAdvBuffer>;
    FEvent: TEvent;
    FPersistent: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TWebSocketQueueInfo; overload;

    property Connected : boolean read FConnected write FConnected;
    property Persistent : boolean read FPersistent write FPersistent;
    property Event : TEvent read FEvent;
    property Queue : TAdvList<TAdvBuffer> read FQueue;
  end;

  TSubscriptionTrackerList = class (TAdvObjectList)
  private
    function GetTracker(i: integer): TSubscriptionTracker;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    procedure addOrUpdate(Key : integer; info : String; status : TFhirSubscriptionStatusEnum); overload;
    function getByKey(Key : integer) : TSubscriptionTracker;
    property TrackerItem[i : integer] : TSubscriptionTracker read GetTracker; default;
  end;

  TSubscriptionEntry = class (TAdvObject)
  private
    FSubscription : TFhirSubscription;
    FKey : Integer;
    FResourceType: Integer;
    FId: String;
    procedure SetSubscription(const Value: TFhirSubscription);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Property Key : integer read FKey write FKey;
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
    procedure add(Key, ResourceType : Integer; id : String; Subscription : TFhirSubscription); overload;
    function getByKey(Key : integer) : TSubscriptionEntry;
    property EntryItem[i : integer] : TSubscriptionEntry read GetEntry; default;
  end;

  TGetSessionEvent = function (userkey : Integer) : TFhirSession of object;
  TExecuteOperationEvent = procedure(request : TFHIRRequest; response : TFHIRResponse; bWantSession : boolean) of object;
  TExecuteSearchEvent = function (typekey : integer; compartment : TFHIRCompartmentId; sessionCompartments: TAdvList<TFHIRCompartmentId>; params : TParseMap; conn : TKDBConnection): String of object;

  TSubscriptionManager = class (TFHIRServerWorker)
  private
    FLock : TCriticalSection;
    FSubscriptions : TSubscriptionEntryList;
    {$IFDEF FHIR4}
    FEventDefinitions : TAdvMap<TFHIREventDefinition>;
    {$ENDIF}
    FSubscriptionTrackers  : TSubscriptionTrackerList;
    FLastSubscriptionKey, FLastNotificationQueueKey, FLastWebSocketKey : integer;
    FDatabase: TKDBManager;
    EmptyQueue : boolean;
    FSMTPPort: String;
    FSMTPPassword: String;
    FSMTPHost: String;
    FSMTPSender: String;
    FSMTPUsername: String;
    FDirectPort: String;
    FDirectPassword: String;
    FDirectHost: String;
    FDirectSender: String;
    FDirectUsername: String;
    FDirectPopHost : String;
    FDirectPopPort : String;
    FOnExecuteOperation : TExecuteOperationEvent;
    FOnExecuteSearch : TExecuteSearchEvent;
    FSMTPUseTLS: boolean;
    FSMSFrom: String;
    FSMSToken: String;
    FSMSAccount: String;
    FBase : String;
    FOnGetSessionEvent: TGetSessionEvent;
    FLastPopCheck : TDateTime;

    FCloseAll : boolean;
    FSemaphores : TAdvMap<TWebSocketQueueInfo>;

    fpp : TFHIRPathParser;
    fpe : TFHIRPathEngine;
    function wsWait(id : String) : boolean;
    function wsConnect(id : String; persistent : boolean) : boolean;
    procedure wsDisconnect(id : String);
    procedure wsWakeAll;
    procedure wsWake(id : String);
    function wsPersists(id : String; b : TBytes) : boolean;

//    procedure go(id : String; content : TAdvBuffer);
//    procedure goAll;
//    procedure done(id : String);

    function chooseSMTPPort(direct : boolean): String;
    function chooseSMTPPassword(direct : boolean): String;
    function chooseSMTPHost(direct : boolean): String;
    function chooseSMTPSender(direct : boolean): String;
    function chooseSMTPUsername(direct : boolean): String;

//    FMessageQueue : TNotification;
    {$IFDEF FHIR4}
    function getEventDefinition(subscription : TFhirSubscription) : TFhirEventDefinition;
    function MeetsCriteriaEvent(evd : TFHIREventDefinition; subscription : TFhirSubscription; typekey, key, ResourceVersionKey, ResourcePreviousKey : integer; conn : TKDBConnection) : boolean;
    {$ENDIF}
    function determineResourceTypeKey(criteria : String; conn : TKDBConnection) : integer;
    procedure checkAcceptable(subscription : TFhirSubscription; session : TFHIRSession);
    procedure SeeNewSubscription(key : Integer; id : String; subscription : TFhirSubscription; session : TFHIRSession; conn : TKDBConnection);
    function ProcessSubscription(conn : TKDBConnection): Boolean;
    function ProcessNotification(conn : TKDBConnection): Boolean;
    function preparePackage(userkey : integer; created : boolean; subscription : TFhirSubscription; resource : TFHIRResource) : TFHIRResource;
    function MeetsCriteria(subscription : TFhirSubscription; typekey, key, ResourceVersionKey, ResourcePreviousKey : integer; conn : TKDBConnection) : boolean;
    function MeetsCriteriaSearch(criteria : String; typekey, key : integer; conn : TKDBConnection) : boolean;
    procedure createNotification(vkey, skey : integer; created : boolean; conn : TKDBConnection);
    function LoadResourceFromDBByKey(conn : TKDBConnection; key: integer; var userkey : integer) : TFhirResource;
    function LoadResourceFromDBByVer(conn : TKDBConnection; vkey: integer; var id : String) : TFhirResource;

    procedure doSendEmail(subst : TFhirSubscription; resource : TFHIRResource; dest : String; direct : boolean);  overload;
    procedure sendDirectResponse(id, address, message: String; ok: boolean);
    procedure processReportDeliveryMessage(id : string; txt : String; details : TStringList);
    procedure processReportDeliveryNotification(id : string; txt : String; details : TStringList);
    procedure processDirectMessage(txt, ct : String; res : TBytesStream);
    procedure processIncomingDirectMessage(msg : TIdMessage);

    procedure sendByRest(id : String; subst : TFhirSubscription; package : TFHIRResource);
    procedure sendByEmail(id : String; subst : TFhirSubscription; package : TFHIRResource);  overload;
    procedure sendBySms(id : String; subst : TFhirSubscription; package : TFHIRResource);
    procedure sendByWebSocket(conn : TKDBConnection; id : String; subst : TFhirSubscription; package : TFHIRResource);
    procedure processByScript(conn : TKDBConnection; id : String; subst : TFhirSubscription; package : TFHIRResource);

    procedure saveTags(conn : TKDBConnection; ResourceKey : integer; res : TFHIRResource);
    procedure NotifySuccess(userkey, SubscriptionKey : integer);
    procedure NotifyFailure(userkey, SubscriptionKey : integer; message : string);
    procedure DoDropResource(key, vkey, pvkey : Integer; internal : boolean);
    function getSummaryForChannel(subst : TFhirSubscription) : String;
    procedure ApplyUpdateToResource(userkey : integer; id : String; resource : TFhirResource);
    procedure HandleWebSocketBind(id: String; connection: TIdWebSocket);
    procedure HandleWebSocketSubscribe(json : TJsonObject; connection: TIdWebSocket);
    function checkForClose(connection: TIdWebSocket; id : String; worked: boolean): boolean;
  public
    Constructor Create(ServerContext : TAdvObject);
    Destructor Destroy; Override;

    procedure loadQueue(conn : TKDBConnection);
    procedure SeeResource(key, vkey, pvkey : Integer; id : String; op : TSubscriptionOperation; resource : TFHIRResource; conn : TKDBConnection; reload : boolean; session : TFHIRSession);
    procedure DropResource(key, vkey, pvkey : Integer);
    procedure Process; // spend up to 30 seconds working on subscriptions
    procedure ProcessEmails; // on a separate thread to Process
    procedure HandleWebSocket(connection : TIdWebSocket);

    procedure sendByEmail(resource : TFHIRResource; dest : String; direct : boolean); overload;

    Property Database : TKDBManager read FDatabase write FDatabase;

    Property Base : String read FBase write FBase;
    Property SMTPHost : String read FSMTPHost write FSMTPHost;
    Property SMTPPort : String read FSMTPPort write FSMTPPort;
    Property SMTPUsername : String read FSMTPUsername write FSMTPUsername;
    Property SMTPPassword : String read FSMTPPassword write FSMTPPassword;
    Property SMTPSender : String read FSMTPSender write FSMTPSender;
    Property SMTPUseTLS : boolean read FSMTPUseTLS write FSMTPUseTLS;
    Property DirectHost : String read FDirectHost write FDirectHost;
    Property DirectPort : String read FDirectPort write FDirectPort;
    Property DirectUsername : String read FDirectUsername write FDirectUsername;
    Property DirectPassword : String read FDirectPassword write FDirectPassword;
    Property DirectSender : String read FDirectSender write FDirectSender;
    Property SMSAccount : String read FSMSAccount write FSMSAccount;
    Property SMSToken : String read FSMSToken write FSMSToken;
    Property SMSFrom : String read FSMSFrom write FSMSFrom;
    property DirectPopHost : String read FDirectPopHost write FDirectPopHost;
    property DirectPopPort : String read FDirectPopPort write FDirectPopPort;
    Property OnExecuteOperation : TExecuteOperationEvent read FOnExecuteOperation write FOnExecuteOperation;
    Property OnExecuteSearch : TExecuteSearchEvent read FOnExecuteSearch write FOnExecuteSearch;
    Property OnGetSessionEvent : TGetSessionEvent read FOnGetSessionEvent write FOnGetSessionEvent;
  end;

implementation

uses
  FHIRServerContext,
  TwilioClient;

{ TSubscriptionManager }

constructor TSubscriptionManager.Create(ServerContext : TAdvObject);
begin
  inherited Create(TFHIRServerContext(ServerContext));
  FLock := TCriticalSection.Create('Subscriptions');
  FSubscriptions := TSubscriptionEntryList.Create;
  FSubscriptionTrackers := TSubscriptionTrackerList.Create;
  FSemaphores := TAdvMap<TWebSocketQueueInfo>.Create;
  fpp := TFHIRPathParser.create;
  fpe := TFHIRPathEngine.Create(TFHIRServerContext(ServerContext).ValidatorContext.Link);
  FCloseAll := false;
  FLastPopCheck := 0;
  {$IFDEF FHIR4}
  FEventDefinitions := TAdvMap<TFHIREventDefinition>.create;
  {$ENDIF}
end;

destructor TSubscriptionManager.Destroy;
begin
  fpe.Free;
  fpp.Free;
  wsWakeAll;
  {$IFDEF FHIR4}
  FEventDefinitions.Free;
  {$ENDIF}
  FSemaphores.Free;
  FSubscriptionTrackers.Free;
  FSubscriptions.Free;
  FLock.Free;
  FDatabase.Free;
  inherited;
end;


// , TFHIRServerContext(ServerContext).ValidatorContext.link, TFHIRServerContext(ServerContext).Indexes.Compartments.Link
procedure TSubscriptionManager.ApplyUpdateToResource(userkey : integer; id: String; resource: TFhirResource);
var
  request : TFHIRRequest;
  response : TFHIRResponse;
begin
  request := TFHIRRequest.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, roSubscription, TFHIRServerContext(ServerContext).Indexes.Compartments.Link);
  response := TFHIRResponse.Create;
  try
    request.Id := id;
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

procedure TSubscriptionManager.checkAcceptable(subscription: TFhirSubscription; session: TFHIRSession);
var
  ts : TStringList;
{$IFDEF FHIR4}
  evd : TFHIREventDefinition;
{$ENDIF}
  function rule(test : boolean; message : String) : boolean;
  begin
    if not test then
      ts.Add(message);
    result := test;
  end;
var
  expr : TFHIRPathExpressionNode;
begin
  ts := TStringList.Create;
  try
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
  //  end;

    // basic setup stuff
    subscription.checkNoModifiers('SubscriptionManager.checkAcceptable', 'subscription');
    subscription.channel.checkNoModifiers('SubscriptionManager.checkAcceptable', 'subscription');
    rule(subscription.channel.type_ <> SubscriptionChannelTypeNull, 'A channel type must be specified');
    rule(not (subscription.channel.type_ in [SubscriptionChannelTypeMessage]), 'The channel type '+CODES_TFhirSubscriptionChannelTypeEnum[subscription.channel.type_]+' is not supported');
    rule((subscription.channel.type_ = SubscriptionChannelTypeWebsocket) or (subscription.channel.endpoint <> ''), 'A channel URL must be specified if not websockets');
    rule((subscription.channel.type_ <> SubscriptionChannelTypeSms) or subscription.channel.endpoint.StartsWith('tel:'), 'When the channel type is "sms", then the URL must start with "tel:"');

    {$IFDEF FHIR4}
    if subscription.hasExtension('http://hl7.org/fhir/subscription/topics') then
    begin
      evd := getEventDefinition(subscription);
      if rule(evd <> nil, 'Topic not understood or found') then
      try
        evd.checkNoModifiers('SubscriptionManager.checkAcceptable', 'topic definition');
        if rule(evd.trigger <> nil, 'Topic has no trigger') then
        begin
          rule(evd.trigger.type_ in [TriggerTypeDataAdded, TriggerTypeDataModified, TriggerTypeDataRemoved], 'Topic has trigger type = '+CODES_TFhirTriggerTypeEnum[evd.trigger.type_]+', which is not supported');
          rule(evd.trigger.name = '', 'Topic has named trigger, which is not supported');
          rule(evd.trigger.timing = nil, 'Topic has event timing on trigger, which is not supported');
          rule((evd.trigger.data = nil) or (evd.trigger.condition = nil), 'Topic has both data and condition on trigger, which is not supported');
          if evd.trigger.data <> nil then
          begin
            rule(evd.trigger.data.type_ <> AllTypesNull, 'DataRequirement type must not be present');
            rule(evd.trigger.data.profileList.IsEmpty, 'DataRequirement profile must be absent');
            if evd.trigger.condition <> nil then
            begin
              rule(evd.trigger.condition.language = 'text\fhirpath', 'Condition language must be FHIRPath');
              rule(evd.trigger.condition.expression <> '', 'Condition FHIRPath must not be blank');
              try
                expr := fpp.parse(evd.trigger.condition.expression);
                evd.trigger.condition.expressionElement.Tag := expr;
                fpe.check(nil, CODES_TFhirAllTypesEnum[evd.trigger.data.type_], CODES_TFhirAllTypesEnum[evd.trigger.data.type_], CODES_TFhirAllTypesEnum[evd.trigger.data.type_], expr, false);
              except
                on e : exception do
                  rule(false, 'Error parsing expression: '+e.Message);
              end;
            end;
          end;
        end;
      finally
        evd.Free;
      end;
    end;
    {$ENDIF}
    if ts.Count > 0 then
      raise Exception.Create(ts.Text);
  finally
    ts.Free;
  end;
end;


procedure TSubscriptionManager.processEmails;
var
  pop : TIdPOP3;
  msg : TIdMessage;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  c, i : integer;
begin
//  if FDirectPopHost = '' then
    exit();
  if FLastPopCheck > now - DATETIME_MINUTE_ONE then
    exit();
  try
    pop := TIdPop3.Create(Nil);
    try
      pop.Host := FDirectPopHost;
      pop.port := StrToInt(FDirectPopPort);
      pop.Username := FDirectUsername;
      pop.Password := FDirectPassword;
      if SMTPUseTLS then
      begin
        ssl := TIdSSLIOHandlerSocketOpenSSL.create;
        pop.IOHandler := ssl;
        pop.UseTLS := utUseExplicitTLS;
        ssl.Destination := FDirectPopHost+':'+FDirectPopPort;
        ssl.Host := FDirectPopHost;
        ssl.MaxLineAction := maException;
        ssl.Port := StrToInt(FDirectPopPort);
        ssl.SSLOptions.Method := sslvTLSv1;
        ssl.SSLOptions.Mode := sslmUnassigned;
        ssl.SSLOptions.VerifyMode := [];
        ssl.SSLOptions.VerifyDepth := 0;
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
      logt('Exception checking email: '+e.message);
  end;
  FLastPopCheck := now;
end;

procedure TSubscriptionManager.DropResource(key, vkey, pvkey: Integer);
{$IFDEF FHIR4}
var
  evd : TFHIREventDefinition;
{$ENDIF}
begin
  DoDropResource(key, vKey, pvkey, false);
  {$IFDEF FHIR4}
  FLock.Lock('DropResource');
  try
    if FEventDefinitions.ContainsKey('key:'+inttostr(key)) then
    begin
      evd := FEventDefinitions['key:'+inttostr(key)];
      FEventDefinitions.Remove(evd.id);
      FEventDefinitions.Remove(evd.url);
      FEventDefinitions.Remove('key:'+inttostr(key));
    end;
  finally
    FLock.Unlock;
  end;
  {$ENDIF}
end;

{$IFDEF FHIR4}
function TSubscriptionManager.getEventDefinition(subscription: TFhirSubscription): TFhirEventDefinition;
var
  ext : TFhirType;
  s : String;
begin
  result := nil;
  ext := subscription.getExtensionValue('http://hl7.org/fhir/subscription/topics');
  if (ext <> nil) and (ext is TFHIRReference) then
  begin
    s := (ext as TFHIRReference).reference;
    FLock.Lock('getEventDefinition');
    try
      if (s.StartsWith('EventDefinition/')) then
      begin
        if FEventDefinitions.TryGetValue(s.Substring(16), result) then
          result.Link;
      end
      else if FEventDefinitions.TryGetValue(s, result) then
        result.Link;
    finally
      FLock.Unlock;
    end;
  end;
end;
  {$ENDIF}

function TSubscriptionManager.getSummaryForChannel(subst: TFhirSubscription): String;
var
  s : TFHIRString;
begin
  result := subst.channel.type_Element.value+#1+subst.channel.endpoint+#1+subst.channel.payload;
  {$IFNDEF FHIR2}
  for s in subst.channel.headerList do
    result := result+#0+s.value;
  {$ELSE}
  result := result+#0+subst.channel.header;
  {$ENDIF}
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

procedure TSubscriptionManager.HandleWebSocketSubscribe(json : TJsonObject; connection: TIdWebSocket);
var
  parser : TFHIRJsonParser;
  subscription : TFhirSubscription;
begin
  parser := TFHIRServerContext(ServerContext).Factory.newJsonParser(connection.request.AcceptLanguage);
  try
    subscription := parser.ParseFragment(json, 'TFhirSubscription') as TFhirSubscription;
  finally
    parser.Free;
  end;
  try
    subscription.id := NewGuidId;
//    SeeNewSubscription(-1, vkey, id, resource as TFhirSubscription, session, conn);
//    register subscription
//    loop until disconnected
//    unregister subscription
//    disconnect
  finally
    subscription.free;
  end;
end;

procedure TSubscriptionManager.HandleWebSocketBind(id : String; connection: TIdWebSocket);
var
  conn : TKDBConnection;
  key : integer;
  cnt : String;
begin
  if (not IsId(id)) then
    raise Exception.Create('invalid syntax');
  if not wsConnect(id, true) then
    raise Exception.Create('There is already a web socket bound to '+id);
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
        raise Exception.Create('No Bind Statement');
      if cmd.text.StartsWith('bind ') then
        handleWebSocketBind(cmd.text.Substring(5), connection)
      else if not cmd.text.Trim.StartsWith('{') then
        raise Exception.Create('invalid syntax - expected bind :id, or a JSON object')
      else
      begin
        json := TJSONParser.Parse(cmd.text);
        try
          if (json.str['type'] = 'bind-subscription') then
            handleWebSocketBind(json.str['id'], connection)
          else if (json.str['type'] = 'create-subscription') then
            handleWebSocketSubscribe(json.obj['subscription'], connection)
          else
            raise Exception.Create('invalid syntax - expected type "bind-subscription" or "create-subscription"');
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

procedure TSubscriptionManager.SeeNewSubscription(key: Integer; id : String; subscription: TFhirSubscription; session: TFHIRSession; conn : TKDBConnection);
begin
  subscription.checkNoImplicitRules('SubscriptionManager.SeeNewSubscription', 'subscription');
  if subscription.status in [SubscriptionStatusActive, SubscriptionStatusError] then
  begin
    CheckAcceptable(subscription, session);
    DoDropResource(Key, 0, 0, true); // delete any previously existing entry for this subscription
    FSubscriptions.add(key, determineResourceTypeKey(subscription.criteria, conn), id, subscription.Link);
    FSubscriptionTrackers.addorUpdate(key, getSummaryForChannel(subscription), subscription.status);
  end;
end;

procedure TSubscriptionManager.SeeResource(key, vkey, pvkey: Integer; id : String; op : TSubscriptionOperation; resource: TFHIRResource; conn : TKDBConnection; reload: boolean; session : TFHIRSession);
{$IFDEF FHIR4}
var
  evd : TFHIREventDefinition;
{$ENDIF}
begin
  FLock.Enter('SeeResource');
  try
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

    if resource is TFhirSubscription then
      SeeNewSubscription(key, id, resource as TFhirSubscription, session, conn);
    {$IFDEF FHIR4}
    if resource is TFHIREventDefinition then
    begin
      evd := resource as TFHIREventDefinition;
      FLock.Lock('getEventDefinition');
      try
        FEventDefinitions.addOrSetValue(evd.url, evd.link);
        FEventDefinitions.addOrSetValue(evd.id, evd.link);
        FEventDefinitions.addOrSetValue('key:'+inttostr(key), evd.link);
      finally
        FLock.Unlock;
      end;
    end;
    {$ENDIF}
  finally
    FLock.Leave;
  end;
end;


procedure TSubscriptionManager.doSendEmail(subst : TFhirSubscription; resource : TFHIRResource; dest : String; direct : boolean);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
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
    if SMTPUseTLS then
    begin
      ssl := TIdSSLIOHandlerSocketOpenSSL.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := chooseSMTPHost(direct)+':'+chooseSMTPPort(direct);
      ssl.Host := chooseSMTPHost(direct);
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(chooseSMTPPort(direct));
      ssl.SSLOptions.Method := sslvTLSv1;
      ssl.SSLOptions.Mode := sslmUnassigned;
      ssl.SSLOptions.VerifyMode := [];
      ssl.SSLOptions.VerifyDepth := 0;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      if (subst <> nil) and ({$IFNDEF FHIR2} subst.channel.headerList.count > 0 {$ELSE} subst.channel.header <> '' {$ENDIF}) then
        msg.Subject := {$IFNDEF FHIR2} subst.channel.headerList[0].value {$ELSE} subst.channel.header {$ENDIF}
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
        comp := MakeComposer(OutputStylePretty, 'en', 'application/json', nil)
      else if subst.channel.payload <> '' then
        comp := MakeComposer(OutputStylePretty, 'en', subst.channel.payload, nil)
      else
        comp := nil;
      try
        if comp <> nil then
        begin
          m := TMemoryStream.Create;
          try
            comp.Compose(m, resource, nil);
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
      logt('Send '+msg.MsgId+' to '+dest);
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

procedure TSubscriptionManager.sendByEmail(id : String; subst: TFhirSubscription; package : TFHIRResource);
begin
  doSendEmail(subst, package, subst.channel.endpoint.Replace('mailto:', ''),
   subst.channel.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') and
    (subst.channel.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true'));
end;

procedure TSubscriptionManager.sendByEmail(resource : TFHIRResource; dest: String; direct : boolean);
begin
  doSendEmail(nil, resource, dest, direct);
end;

procedure TSubscriptionManager.sendByRest(id : String; subst: TFhirSubscription; package : TFHIRResource);
var
  http : TIdHTTP;
  client : TFhirHTTPClient;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  stream : TMemoryStream;
  s : TFHIRString;
begin
  if subst.channel.payload = '' then
  begin
    stream := TMemoryStream.Create;
    http := TIdHTTP.create(nil);
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      {$IFNDEF FHIR2}
      for s in subst.channel.headerList do
        http.Request.CustomHeaders.Add(s.value);
      {$ELSE}
      if subst.channel.header <> '' then
        http.Request.CustomHeaders.Add(subst.channel.header);
      {$ENDIF}
      http.Post(subst.channel.endpoint, stream);
    finally
      ssl.Free;
      http.Free;
      stream.Free;
    end;
  end
  else
  begin
    client := TFhirHTTPClient.create(nil, subst.channel.endpoint, subst.channel.payload.Contains('json'));
    try
      if (package is TFHIRBundle) and (TFHIRBundle(package).type_ = BundleTypeTransaction) then
        client.transaction(TFHIRBundle(package))
      else
        client.createResource(package, id);
    finally
      client.Free;
    end;
  end;
end;

procedure TSubscriptionManager.sendBySms(id: String; subst: TFhirSubscription; package : TFHIRResource);
var
  client : TTwilioClient;
begin
  client := TTwilioClient.Create;
  try
    client.Account := SMSAccount;
    client.Token := SMSToken;
    client.From := SMSFrom;
    if subst.channel.endpoint.StartsWith('tel:') then
      client.dest := subst.channel.endpoint.Substring(4)
    else
      client.dest := subst.channel.endpoint;
    if subst.channel.payload <> '' then
      client.Body := subst.channel.payload
    else
      client.Body := 'A new matching resource has been received for Subscription '+subst.id;
    client.send;
  finally
    client.Free;
  end;
end;


procedure TSubscriptionManager.sendByWebSocket(conn : TKDBConnection; id: String; subst: TFhirSubscription; package : TFHIRResource);
var
  key : integer;
  comp : TFHIRComposer;
  b : TBytes;
begin
  comp := nil;
  if (subst.channel.payload = 'application/xml+fhir') or (subst.channel.payload = 'application/fhir+xml') or (subst.channel.payload = 'application/xml') then
    comp := TFHIRXmlComposer.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, OutputStyleNormal, 'en')
  else if (subst.channel.payload = 'application/json+fhir') or (subst.channel.payload = 'application/fhir+json') or (subst.channel.payload = 'application/json') then
    comp := TFHIRJsonComposer.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, OutputStyleNormal,'en')
  else if subst.channel.payload <> '' then
    raise Exception.Create('unknown payload type '+subst.channel.payload);
  try
    if comp <> nil then
      b := TEncoding.UTF8.GetBytes(comp.Compose(package))
    else
     SetLength(b, 0);
  finally
    comp.Free;
  end;

  if wsPersists(subst.id, b) then
  begin
    FLock.Lock;
    try
      inc(FLastWebSocketKey);
      key := FLastWebSocketKey;
    finally
      FLock.Unlock;
    end;

    conn.SQL := 'insert into WebSocketsQueue (WebSocketsQueueKey, SubscriptionId, Handled, Content) values ('+inttostr(key)+', '''+SQLWrapString(subst.id)+''', 0, :c)';
    conn.Prepare;
    if subst.channel.payload = '' then
      conn.BindNull('c')
    else
    begin
      conn.BindBlob('c', b);
    end;
    conn.Execute;
    conn.Terminate;
  end;
  wsWake(subst.id);
end;

procedure TSubscriptionManager.sendDirectResponse(id, address, message: String; ok: boolean);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
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
    sender.Host := FDirectHost;
    if FDirectPort <> '' then
      sender.port := StrToInt(FDirectPort);
    sender.Username := FDirectUsername;
    sender.Password := FDirectPassword;
    if SMTPUseTLS then
    begin
      ssl := TIdSSLIOHandlerSocketOpenSSL.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := FDirectHost+':'+FDirectPort;
      ssl.Host := FDirectHost;
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(FDirectPort);
      ssl.SSLOptions.Method := sslvTLSv1;
      ssl.SSLOptions.Mode := sslmUnassigned;
      ssl.SSLOptions.VerifyMode := [];
      ssl.SSLOptions.VerifyDepth := 0;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := 'Direct Response';
      msg.Recipients.Add.Address := address;
      msg.From.Text := FDirectSender;
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
          'Reporting-UA: '+FDirectUsername+'; ('+TFHIRServerContext(ServerContext).OwnerName+')'+#13#10+
          'Final-Recipient: '+FDirectUsername+#13#10+
          'Original-Message-ID: '+id+#13#10+
          'Disposition: automatic-action/MDN-sent-automatically;processed'+#13#10;
      end
      else
      begin
        part.Body.Text := 'Error accepting your message: '+message;
        att.ContentType := 'message/delivery-status';
        att.ContentType := 'Content-Disposition: attachment ';
        s := 'Reporting-MTA: dns;'+DirectUsername+#13#10+
             'X-Original-Message-ID: '+id+#13#10+
             ''+#13#10+
             'Final-Recipient: rfc822'+DirectUsername+#13#10+
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
      logt('Send response for '+id+' as '+msg.MsgId+' to '+address+' as '+BoolToStr(ok)+' ('+message+')');
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

procedure TSubscriptionManager.Process;
var
  finish : TDateTime;
  found : boolean;
  conn : TKDBConnection;
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
      logt('Error handling subscriptions: '+e.Message);
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

procedure TSubscriptionManager.processByScript(conn: TKDBConnection; id: String; subst: TFhirSubscription; package: TFHIRResource);
begin
  raise Exception.Create('Not done yet');
end;

procedure TSubscriptionManager.processDirectMessage(txt, ct: String; res: TBytesStream);
var
  p : TFHIRParser;
begin
  p := MakeParser(TFHIRServerContext(ServerContext).ValidatorContext, 'en', ct, res.bytes, xppReject);
  try
    p.source := res;
    p.Parse;
    p.resource.Tags['process'] := 'true';
    TFHIRServerContext(ServerContext).Storage.QueueResource(p.resource);
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
      logt('email from '+msg.Sender.Text+' could not be processed');
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
      logt('email from '+msg.Sender.Text+' could not be processed');
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
        raise Exception.Create('No id found');
      if (part = nil) then
        raise Exception.Create('Unable to find direct body');
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
        logt('processing incoming direct message from '+msg.sender.Address+' failed: '+e.Message);
      end;
    end;
  end
  else
    logt('email from '+msg.Sender.Text+' could not be understood');
end;

function TSubscriptionManager.ProcessNotification(conn : TKDBConnection): Boolean;
var
  NotificationnQueueKey, ResourceKey, SubscriptionKey : integer;
  res : TFhirResource;
  id : string;
  subst : TFhirSubscription;
  done, created : boolean;
  tnow, dnow : TDateTime;
  userkey: Integer;
  package : TFHIRResource;
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
      subst := LoadResourceFromDBByKey(conn, SubscriptionKey, userkey) as TFhirSubscription;
      try
        res := LoadResourceFromDBByVer(conn, ResourceKey, id);
        try
          package := preparePackage(userkey, created, subst, res);
          try
            case subst.channel.type_ of
              SubscriptionChannelTypeRestHook: sendByRest(id, subst, package);
              SubscriptionChannelTypeEmail: sendByEmail(id, subst, package);
              SubscriptionChannelTypeSms: sendBySms(id, subst, package);
              SubscriptionChannelTypeWebsocket: sendByWebSocket(conn, id, subst, package);
              {$IFDEF FHIR4}
              SubscriptionChannelTypeChangeScript: processByScript(conn, id, subst, package);
              {$ENDIF}
            end;

            if (subst.tagList.Count > 0) then
              saveTags(conn, ResourceKey, res);
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
    logt('Direct Message '+id+' failed: '+details.Values['Diagnostic-Code']+' ('+txt+')');
end;

procedure TSubscriptionManager.processReportDeliveryNotification(id, txt: String; details: TStringList);
begin
  if id = '' then
    id := details.Values['Original-Message-ID'];
  logt('Direct Message '+id+' notice: '+details.Values['Disposition'].Trim);
end;

function processUrlTemplate(url : String; resource : TFhirResource) : String;
var
  b, e : integer;
  code, value : String;
  qry : TFHIRPathEngine;
  o : TFHIRSelection;
  results : TFHIRSelectionList;
begin
  while url.Contains('{{') do
  begin
    b := url.IndexOf('{{');
    e := url.IndexOf('}}');
    code := url.Substring(b+2, e-(b+2));
    if (code = 'id') then
      value := Codes_TFHIRResourceType[resource.ResourceType]+'/'+resource.id
    else
    begin
      qry := TFHIRPathEngine.create(nil);
      try
        results := qry.evaluate(nil, resource, code);
        try
          value := '';
          for o in results do
          begin
            if o.value is TFHIRPrimitiveType then
              CommaAdd(value, TFHIRPrimitiveType(o.value).StringValue)
            else
              raise Exception.Create('URL templates can only refer to primitive types (found '+o.ClassName+')');
          end;
          if (value = '') then
            value := '(nil)';
        finally
          results.Free;
        end;
      finally
        qry.Free;
      end;
    end;

    url := url.Substring(0, b)+value+url.Substring(e+2);
  end;
  result := url;
end;

function TSubscriptionManager.ProcessSubscription(conn: TKDBConnection): Boolean;
var
  SubscriptionQueueKey, ResourceKey, ResourceVersionKey, ResourcePreviousKey, ResourceTypeKey : integer;
  i : integer;
  list : TSubscriptionEntryList;
  created : boolean;
begin
  SubscriptionQueueKey := 0;
  ResourceKey := 0;
  ResourceVersionKey := 0;
  ResourceTypeKey := 0;
  ResourcePreviousKey := 0;
  created := false;

  conn.SQL := RestrictToNRows(conn.Owner.Platform, 'Select SubscriptionQueueKey, Ids.ResourceKey, SubscriptionQueue.ResourceVersionKey, SubscriptionQueue.ResourcePreviousKey, Operation, Ids.ResourceTypeKey from SubscriptionQueue, Ids where '+
    'Handled is null and Ids.ResourceKey = SubscriptionQueue.ResourceKey order by SubscriptionQueueKey', 1);
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
    list := TSubscriptionEntryList.Create;
    try
      // keep the lock short
      FLock.Lock('List Subscriptions');
      try
        for i := 0 to FSubscriptions.Count - 1 do
          list.Add(FSubscriptions[i].FKey, FSubscriptions[i].FResourceType, FSubscriptions[i].FId, FSubscriptions[i].FSubscription.Link); // not clone
      finally
        FLock.Leave;
      end;
      conn.StartTransact;
      try
        for i := 0 to list.Count - 1 do
          if ((list[i].FResourceType = 0) or (list[i].FResourceType = ResourceTypeKey) ) and MeetsCriteria(list[i].Subscription, list[i].FResourceType, ResourceKey, ResourceVersionKey, ResourcePreviousKey, conn) then
            CreateNotification(ResourceVersionKey, list[i].FKey, created, conn);
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
  FLastWebSocketKey := conn.CountSQL('select max(WebSocketsQueueKey) from WebSocketsQueue');
end;

function TSubscriptionManager.LoadResourceFromDBByVer(conn: TKDBConnection; vkey: integer; var id : String): TFhirResource;
var
  parser : TFHIRParser;
begin
  result := nil;
  conn.SQL := 'select ResourceName, Ids.Id, Tags, XmlContent From Versions, Ids, Types where ResourceVersionKey = '+inttostr(vkey)+' and Versions.ResourceKey = IDs.ResourceKey and IDs.ResourceTypeKey = Types.ResourceTypeKey';
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
      parser := MakeParser(TFHIRServerContext(ServerContext).ValidatorContext, 'en', ffXml, conn.ColBlobByName['XmlContent'], xppDrop);
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

function TSubscriptionManager.LoadResourceFromDBByKey(conn: TKDBConnection; key: integer; var userkey : integer): TFhirResource;
var
  parser : TFHIRParser;
begin
  result := nil;
  conn.SQL := 'select ResourceName, Ids.Id, UserKey, XmlContent From Versions, Ids, Types, Sessions '+
    'where IDs.ResourceKey = '+inttostr(key)+' and Versions.SessionKey = Sessions.SessionKey and '+
    'Versions.ResourceVersionKey = IDs.MostRecent and IDs.ResourceTypeKey = Types.ResourceTypeKey';
  conn.prepare;
  try
    conn.Execute;
    if not conn.FetchNext then
      raise Exception.Create('Cannot find resource');
    userKey := conn.ColIntegerByName['UserKey'];
    if conn.ColStringByName['ResourceName'] = 'Binary' then
      result := LoadBinaryResource('en', conn.ColBlobByName['Content'])
    else
    begin
      parser := MakeParser(TFHIRServerContext(ServerContext).ValidatorContext, 'en', ffXml, conn.ColBlobByName['XmlContent'], xppDrop);
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
      t := criteria
    else
      t := criteria.Substring(0, criteria.IndexOf('?'));
    result := conn.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(t)+'''')
  end;
end;

function TSubscriptionManager.MeetsCriteria(subscription : TFhirSubscription; typekey, key, ResourceVersionKey, ResourcePreviousKey: integer; conn: TKDBConnection): boolean;
{$IFDEF FHIR4}
var
  evd : TFHIREventDefinition;
{$ENDIF}
begin
  if subscription.criteria = '*' then
    result := true
  else
    result := MeetsCriteriaSearch(subscription.criteria, typekey, key, conn);

  {$IFDEF FHIR4}
  // extension for San Deigo Subscription connectathon
  if result then
  begin
    evd := getEventDefinition(subscription);
    try
      if evd <> nil then
        result := MeetsCriteriaEvent(evd, subscription, typekey, key, ResourceVersionKey, ResourcePreviousKey, conn);
    finally
      evd.Free;
    end;
  end;
  {$ENDIF}
end;

{$IFDEF FHIR4}
function TSubscriptionManager.MeetsCriteriaEvent(evd : TFHIREventDefinition; subscription : TFhirSubscription; typekey, key, ResourceVersionKey, ResourcePreviousKey: integer; conn: TKDBConnection): boolean;
var
  old : TFhirResource;
  new : TFhirResource;
  oldId, newId : string;
begin
  new := LoadResourceFromDBByVer(conn, ResourceVersionKey, newId);
  try
    old := nil;
    if ResourcePreviousKey <> 0 then
    begin
      old := LoadResourceFromDBByVer(conn, ResourcePreviousKey, oldId);
      assert(newId = oldId);
    end;
    try
      // todo: code and date requirements from DataRequirement
      if evd.trigger.condition <> nil then
      begin

      end
      else
        result := false;
    finally
      old.Free;
    end;
  finally
    new.Free;
  end;

//  is the date criteria met?
//  is the code criteria met?
//  is the expression criteria met?
//  result := false; // todo
end;
{$ENDIF}

function TSubscriptionManager.MeetsCriteriaSearch(criteria: String; typekey, key : integer; conn: TKDBConnection): boolean;
var
  l, r, sql : String;
  p : TParseMap;
begin
  StringSplit(criteria, '?', l, r);
  p := TParseMap.create('_type='+l+'&'+r, true);
  try
    sql := FOnExecuteSearch(typekey, nil, nil, p, conn);
    result := conn.CountSQL('select count(*) from Ids where not MostRecent is null and ResourceKey = '+inttostr(key)+' and '+sql) > 0;
  finally
    p.Free;
  end;
end;

procedure TSubscriptionManager.NotifySuccess(userkey, SubscriptionKey: integer);
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
    ApplyUpdateToResource(userkey, id, subst);
  end;
end;

function TSubscriptionManager.preparePackage(userkey : integer; created : boolean; subscription: TFhirSubscription; resource: TFHIRResource): TFHIRResource;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  url : string;
  i : integer;
  ex: TFhirExtension;
  entry : TFhirBundleEntry;
  bundle : TFHIRBundle;
begin
  if subscription.hasExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/subscription-prefetch') then
  begin
    bundle := TFhirBundle.Create(BundleTypeCollection);
    try
      bundle.id := NewGuidId;
      bundle.link_List.AddRelRef('source', AppendForwardSlash(base)+'Subsecription/'+subscription.id);
      request := TFHIRRequest.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, roSubscription, TFHIRServerContext(ServerContext).Indexes.Compartments.Link);
      response := TFHIRResponse.Create;
      try
        bundle.entryList.Append.resource := resource.Link;
        request.Session := OnGetSessionEvent(userkey);
        request.baseUrl := FBase;
        for ex in subscription.extensionList do
          if ex.url = 'http://www.healthintersections.com.au/fhir/StructureDefinition/subscription-prefetch' then
          begin
            entry := bundle.entryList.Append;
            try
              url := (ex.value as TFHIRPrimitiveType).StringValue;
              entry.link_List.AddRelRef('source', url);
              if (url = '{{id}}') then
              begin
                // copy the specified tags on the subscription to the resource.
                for i := 0 to subscription.tagList.Count - 1 do
                  if (subscription.tagList[i].code <> '') and (subscription.tagList[i].system <> '') then
                    if not resource.meta.HasTag(subscription.tagList[i].system, subscription.tagList[i].code) then
                      resource.meta.tagList.AddCoding(subscription.tagList[i].system, subscription.tagList[i].code, subscription.tagList[i].display);
                entry.request := TFhirBundleEntryRequest.Create;
                if created then
                begin
                  entry.request.url := CODES_TFHIRResourceType[resource.ResourceType];
                  entry.request.method := HttpVerbPOST;
                end
                else
                begin
                  entry.request.url := CODES_TFHIRResourceType[resource.ResourceType]+'/'+resource.id;
                  entry.request.method := HttpVerbPUT;
                end;
                entry.resource := resource.link;
              end
              else
              begin
                url := processUrlTemplate((ex.value as TFHIRPrimitiveType).StringValue, resource);
                entry.request := TFhirBundleEntryRequest.Create;
                entry.request.url := url;
                entry.request.method := HttpVerbGET;
                if (url.Contains('?')) then
                  request.CommandType := fcmdSearch
                else
                  request.CommandType := fcmdRead;
                FOnExecuteOperation(request, response, false);
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := inttostr(response.HTTPCode);
                entry.response.location := response.Location;
                entry.response.etag := 'W/'+response.versionId;
                entry.response.lastModified := TDateTimeEx.makeUTC(response.lastModifiedDate);
                entry.resource := response.resource.link;
              end;
            except
              on e : ERestfulException do
              begin
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := inttostr(e.Status);
                entry.resource := BuildOperationOutcome(request.Lang, e);
              end;
              on e : Exception do
              begin
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := '500';
                entry.resource := BuildOperationOutcome(request.Lang, e);
              end;
            end;
          end;
      finally
        request.Free;
        response.Free;
      end;
      result := bundle.Link;
    finally
      bundle.free;
    end;
  end
  else
    result := resource.Link;
end;

procedure TSubscriptionManager.NotifyFailure(userkey, SubscriptionKey: integer; message: string);
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
    ApplyUpdateToResource(userkey, id, subst);
  end;
end;

procedure TSubscriptionManager.createNotification(vkey, skey: integer; created : boolean; conn : TKDBConnection);
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

{ TSubscriptionEntryList }

procedure TSubscriptionEntryList.add(Key, ResourceType: Integer; Id : String; Subscription: TFhirSubscription);
var
  entry : TSubscriptionEntry;
begin
  entry := TSubscriptionEntry.Create;
  try
    entry.Key := key;
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

procedure TSubscriptionTrackerList.addOrUpdate(Key: integer; info: String; status : TFhirSubscriptionStatusEnum);
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
      raise Exception.Create('WS Queue not found: '+id);
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
  buf : TAdvBuffer;
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
        buf := TAdvBuffer.Create;
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
    result := DirectPort
  else
    result:= SMTPPort;
end;

function TSubscriptionManager.chooseSMTPPassword(direct : boolean): String;
begin
  if (direct) then
    result := DirectPassword
  else
    result:= SMTPPassword;
end;

function TSubscriptionManager.chooseSMTPHost(direct : boolean): String;
begin
  if (direct) then
    result := DirectHost
  else
    result:= SMTPHost;
end;

function TSubscriptionManager.chooseSMTPSender(direct : boolean): String;
begin
  if (direct) then
    result := DirectSender
  else
    result:= SMTPSender;
end;

function TSubscriptionManager.chooseSMTPUsername(direct : boolean): String;
begin
  if (direct) then
    result := DirectUsername
  else
    result:= SMTPUsername;
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

{ TWebSocketQueueInfo }

constructor TWebSocketQueueInfo.create;
begin
  inherited;
  FConnected := false;
  FQueue := TAdvList<TAdvBuffer>.create;
  FEvent := TEvent.Create;
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

end.


