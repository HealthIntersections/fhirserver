unit subscriptions_r4;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities,
  fdb_manager,
  fsl_http,
  fhir_objects, fhir_common,  fhir_pathengine,
  fhir5_enums, fhir5_types, fhir5_resources, fhir5_utilities, // first...
  fhir4_types, fhir4_resources, fhir4_utilities, fhir4_constants, fhir4_pathengine, fhir4_pathnode, fhir4_common, fhir4_context,
  subscriptions, session, server_context;

Type
  TFHIRPathSubscriptionContext = class (TFslObject)
  private
    FPrevious : TFHIRObject;
    FCurrent : TFhirObject;
    procedure SetCurrent(const Value: TFHIRObject);
    procedure SetPrevious(const Value: TFHIRObject);
  public
    constructor Create(previous : TFHIRObject; current : TFhirObject);
    destructor Destroy; override;

    property Previous : TFHIRObject read FPrevious write SetPrevious;
    property Current : TFHIRObject read FCurrent write SetCurrent;
  end;

  TFHIRPathSubscriptionExtensions = class (TFHIRPathEngineExtension)
  public
    function resolveConstant(context : TFHIRPathExecutionContext; s : String; var obj : TFHIRObject) : boolean; override;
    function isValidFunction(name : String) : boolean; override;
    function functionApplies(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; name : String): boolean; override;
    function execute(context : TFHIRPathExecutionContext; focus: TFHIRObject; name : String; params : TFslList<TFHIRPathExpressionNodeV>; engine : TFHIRPathEngineV): TFHIRSelectionList; override;
  end;

  TSubscriptionManagerR4 = class (TSubscriptionManager)
  private
    fpp : TFHIRPathParser;
    fpe : TFHIRPathEngine;

    function MeetsTopicResourceType(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String): boolean;
    function MeetsTopicMethodCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
    function MeetsTopicQueryCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
    function MeetsTopicFhirPathCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
    function meetsQueryCriteria(query, resourceType : String; resource : TFHIRResource) : boolean;
    function meetsQueryCriteriaParameter(name, value, resourceType : String; resource : TFHIRResource) : boolean;
    function MeetsCriteriaTopic(topic : TFHIRSubscriptionTopic; subscription : TFhirSubscription; typekey : Integer; key, ResourceVersionKey, ResourcePreviousKey: integer; newRes, oldRes : TFHIRResource; conn: TFDBConnection): boolean;
    function inList(v : String; list : TFHIRSelectionList) : boolean;
  protected
    procedure checkAcceptable(sub : TFHIRSubscriptionW; session : TFHIRSession); override;
    function makeSubscription(resource : TFHIRResourceV) : TFHIRSubscriptionW; override;
    function preparePackage(userkey : integer; created : boolean; sub : TFHIRSubscriptionW; res : TFHIRResourceV) : TFHIRResourceV; override;
    function MeetsCriteria(sub : TFHIRSubscriptionW; typekey, key, ResourceVersionKey, ResourcePreviousKey : integer; newRes, oldRes : TFHIRResourceV; conn : TFDBConnection) : boolean; override;
    function checkSubscription(sub: TFHIRResourceV) : TFHIRSubscriptionW; override;
    function checkSubscriptionTopic(sub: TFHIRResourceV) : TFHIRSubscriptionTopicW; override;
    function loadSubscriptionTopic(res : TFHIRResourceV) : TSubscriptionTopic; override;
    function loadSubscription(res : TFHIRResourceV) : TFHIRSubscriptionW; override;
    function bundleIsTransaction(res : TFHIRResourceV) : boolean; override;
    function processUrlTemplate(url : String; res : TFhirResourceV) : String; override;
    function determineResourceTypeKeys(topic: TSubscriptionTopic; conn: TFDBConnection): TArray<integer>; override;
  public
    constructor Create(ServerContext : TFslObject);
    destructor Destroy; Override;
    function getSubscriptionTopic(canonical : string): TSubscriptionTopic; overload; override;
  end;

implementation

constructor TSubscriptionManagerR4.Create(ServerContext: TFslObject);
begin
  inherited Create(ServerContext);
  fpp := TFHIRPathParser.Create;
  fpe := TFHIRPathEngine.Create(TFHIRServerContext(ServerContext).ValidatorContext.link as TFHIRWorkerContext, nil);
  fpe.registerExtension(TFHIRPathSubscriptionExtensions.create());
end;

destructor TSubscriptionManagerR4.Destroy;
begin
  fpp.Free;
  fpe.Free;
  inherited;
end;

function TSubscriptionManagerR4.getSubscriptionTopic(canonical : string): TSubscriptionTopic;
var
  r5 : TFHIRServerContext;
  topic : TSubscriptionTopic;
begin
  r5 := TFHIRServerContext(ServerContext).OnGetNamedContext(self, 'r5');
  if (r5 <> nil) then
  begin
    topic := r5.SubscriptionManager.getSubscriptionTopic(canonical);
    if (topic <> nil) then
    begin
      exit(topic);
    end;
  end;
  result := nil;
end;

function TSubscriptionManagerR4.MeetsTopicResourceType(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String): boolean;
var
  enum : fhir5_types.TFhirEnum;
begin
  if topic.resourceTypeList.Count = 0 then
    result := true
  else
  begin
    result := false;
    for enum in topic.resourceTypeList do
      if (enum.value = resourceType) then
        exit(true);
  end;
end;

function TSubscriptionManagerR4.MeetsTopicMethodCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
begin
  if topic.methodCriteria = [] then
    result := true
  else
  begin
    if (newRes = nil) and (oldRes <> nil) then
      result := InteractionTriggerDelete in topic.methodCriteria
    else if (newRes <> nil) and (oldRes <> nil) then
      result := InteractionTriggerUpdate in topic.methodCriteria
    else // if (newRes <> nil) and (oldRes = nil) then
      result := InteractionTriggerCreate in topic.methodCriteria;
  end;
end;

function TSubscriptionManagerR4.MeetsTopicQueryCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
var
  prev, curr : boolean;
begin
  if (topic.queryCriteria = nil) then
    result := true
  else
  begin
    if topic.queryCriteria.previous = '' then
      prev := true
    else
      prev := meetsQueryCriteria(topic.queryCriteria.previous, resourceType, oldRes);

    if topic.queryCriteria.current = '' then
      curr := true
    else
      curr := meetsQueryCriteria(topic.queryCriteria.current, resourceType, newRes);

    if topic.queryCriteria.requireBoth then
      result := prev and curr
    else
      result := prev or curr;
  end;
end;

function TSubscriptionManagerR4.MeetsTopicFhirPathCriteria(topic : TFHIRSubscriptionTopicResourceTrigger; resourceType : String; newRes, oldRes : TFHIRResource): boolean;
var
  ctxt : TFHIRPathSubscriptionContext;
  expr : fhir5_types.TFHIRString;
begin
  ctxt := TFHIRPathSubscriptionContext.Create(oldres.Link, newRes.Link);
  try
    for expr in topic.fhirPathCriteriaList do
    begin
      if not fpe.evaluateToBoolean(ctxt, nil, nil, expr.value) then
        exit(false);
    end;
    result := true;
  finally
    ctxt.Free;
  end;
end;

function TSubscriptionManagerR4.MeetsCriteriaTopic(topic : TFHIRSubscriptionTopic; subscription : TFhirSubscription; typekey : Integer; key, ResourceVersionKey, ResourcePreviousKey: integer; newRes, oldRes : TFHIRResource; conn: TFDBConnection): boolean;
var
  rt : String;
begin
  if (newRes <> nil) then
    rt := newRes.fhirType
  else
    rt := oldRes.fhirType;

  result := MeetsTopicResourceType(topic.resourceTrigger, rt) and
     MeetsTopicMethodCriteria(topic.resourceTrigger, rt, newRes, oldRes) and
     MeetsTopicQueryCriteria(topic.resourceTrigger, rt, newRes, oldRes) and
     MeetsTopicFhirPathCriteria(topic.resourceTrigger, rt, newRes, oldRes);
end;

function TSubscriptionManagerR4.meetsQueryCriteria(query, resourceType: String; resource: TFHIRResource): boolean;
var
  params : THTTPParameters;
  i : integer;
begin
  params := THTTPParameters.Create(query);
  try
    for i := 0 to params.Count - 1 do
      if not meetsQueryCriteriaParameter(params.Name[i], params.Value[params.Name[i]], resourceType, resource) then
        exit(false);
    result := true;
  finally
    params.Free;
  end;
end;

function TSubscriptionManagerR4.inList(v : String; list : TFHIRSelectionList) : boolean;
var
  s : String;
  item : TFHIRSelection;
begin
  result := false;
  for item in list do
  begin
    s := fpe.convertToString(item.value);
    if (s = v) then
      exit(true);
  end;
end;

function TSubscriptionManagerR4.meetsQueryCriteriaParameter(name, value, resourceType: String; resource: TFHIRResource): boolean;
var
  n, m, expr : String;
  sp : TFHIRSearchParameter;
  sl : TFHIRSelectionList;
begin
  if name.Contains(':') then
  begin
    StringSplit(name, ':', n, m);
  end
  else
  begin
    n := name;
    m := '';
  end;

  sp := (TFHIRServerContext(ServerContext).ValidatorContext as TFHIRWorkerContext).getSearchParameter(resourceType, n) as TFhirSearchParameter;
  if (sp = nil) then
    exit(false);
  expr := sp.expression;
  if (resource = nil) then
    sl := TFhirSelectionList.create
  else
    sl := fpe.evaluate(resource, resource, expr);
  try
    if (m = '') then
      exit(inList(value, sl))
    else if (m = 'not') then
      exit(not inList(value, sl))
    else
      exit(false);
  finally
    sl.Free;
  end;
  result := false;
end;

function TSubscriptionManagerR4.checkSubscription(sub : TFHIRResourceV): TFHIRSubscriptionW;
var
  subscription: TFHIRSubscription;
begin
  subscription := sub as TFHIRSubscription;
  subscription.checkNoImplicitRules('SubscriptionManager.SeeNewSubscription', 'subscription');
  if subscription.status in [SubscriptionStatusActive, SubscriptionStatusError] then
    result := TFHIRSubscription4.Create(subscription.Link)
  else
    result := nil;
end;

function TSubscriptionManagerR4.checkSubscriptionTopic(sub: TFHIRResourceV): TFHIRSubscriptionTopicW;
begin
  result := nil;
end;

function TSubscriptionManagerR4.loadSubscriptionTopic(res: TFHIRResourceV): TSubscriptionTopic;
var
  r : TFhirSubscriptionTopic;
begin
  r := res as TFhirSubscriptionTopic;
  result := TSubscriptionTopic.Create;
  try
    result.id := r.id;
    result.url := r.url;
    result.resource := r.Link;
    result.link;
  finally
    result.Free;
  end;
end;

function TSubscriptionManagerR4.loadSubscription(res: TFHIRResourceV): TFHIRSubscriptionW;
begin
  if res is TFhirSubscription then
    result := TFHIRSubscription4.Create(res.link)
  else if res.fhirType = 'Subscription' then
    raise EFHIRUnsupportedVersion.Create(fhirVersionRelease2, 'Creating Event Definition')
  else
    raise EFslException.Create('Wrong resource type '+res.fhirType+' looking for Subscription');
end;

function TSubscriptionManagerR4.makeSubscription(resource: TFHIRResourceV): TFHIRSubscriptionW;
begin
  result := TFHIRSubscription4.Create(TFhirSubscription.create);
end;

function TSubscriptionManagerR4.bundleIsTransaction(res: TFHIRResourceV): boolean;
begin
  if res is TFhirBundle then
    result := TFhirBundle(res).type_ = BundleTypeTransaction
  else
    result := false;
end;

procedure TSubscriptionManagerR4.checkAcceptable(sub: TFhirSubscriptionW; session: TFHIRSession);
var
  ts : TStringList;
//  evd : TFHIRSubscriptionTopic;
  function rule(test : boolean; message : String) : boolean;
  begin
    if not test then
      ts.Add(message);
    result := test;
  end;
var
  subscription: TFhirSubscription;
//  expr : TFHIRPathExpressionNode;
begin
  subscription := sub.Resource as TFhirSubscription;
  ts := TStringList.Create;
  try
    // permissions. Anyone who is authenticated can set up a subscription
  //  if (session <> nil) and (session.Name <> 'server') and (session.Name <> 'service') then // session is nil if we are reloading. Service is always allowed to create a subscription
  //  begin
  //    if session.Email = '' then
  //      raise EFHIRException.create('This server does not accept subscription request unless an email address is the client login is associated with an email address');
  //    ok := false;
  //    for i := 0 to subscription.contactList.Count - 1 do
  //      ok := ok or ((subscription.contactList[i].systemST = ContactSystemEmail) and (subscription.contactList[i].valueST = session.Email));
  //    if not ok then
  //      raise EFHIRException.create('The subscription must explicitly list the logged in user email ('+session.Email+') as a contact');
  //  end;

    // basic setup stuff
    subscription.checkNoModifiers('SubscriptionManager.checkAcceptable', 'subscription');
    subscription.channel.checkNoModifiers('SubscriptionManager.checkAcceptable', 'subscription');
    rule(subscription.channel.type_ <> SubscriptionChannelTypeNull, 'A channel type must be specified');
    rule(not (subscription.channel.type_ in [SubscriptionChannelTypeMessage]), 'The channel type '+CODES_TFhirSubscriptionChannelTypeEnum[subscription.channel.type_]+' is not supported');
    rule((subscription.channel.type_ = SubscriptionChannelTypeWebsocket) or (subscription.channel.endpoint <> ''), 'A channel URL must be specified if not websockets');
    rule((subscription.channel.type_ <> SubscriptionChannelTypeSms) or subscription.channel.endpoint.StartsWith('tel:'), 'When the channel type is "sms", then the URL must start with "tel:"');

//    if subscription.hasExtension('http://hl7.org/fhir/subscription/topics') then
//    begin
//      evd := getSubscriptionTopicResource(subscription);
//      if rule(evd <> nil, 'Topic not understood or found') then
//      try
//        evd.checkNoModifiers('SubscriptionManager.checkAcceptable', 'topic definition');
////        if rule(evd.triggerList.Count = 1, 'Topic has no trigger (or more than one)') then
////        begin
////// todo...          rule(evd.triggerList[0].type_ in [TriggerTypeDataAdded, TriggerTypeDataModified, TriggerTypeDataRemoved], 'Topic has trigger type = '+CODES_TFhirTriggerTypeEnum[evd.triggerList[0].type_]+', which is not supported');
////          rule(evd.triggerList[0].name = '', 'Topic has named trigger, which is not supported');
////          rule(evd.triggerList[0].timing = nil, 'Topic has event timing on trigger, which is not supported');
////          rule((evd.triggerList[0].dataList.count <> 0) or (evd.triggerList[0].condition = nil), 'Topic has both data and condition on trigger, which is not supported');
////          if evd.triggerList[0].dataList.count > 0 then
////          begin
////            rule(evd.triggerList[0].dataList[0].type_ <> AllTypesNull, 'DataRequirement type must not be present');
////            rule(evd.triggerList[0].dataList[0].profileList.IsEmpty, 'DataRequirement profile must be absent');
////            if evd.triggerList[0].condition <> nil then
////            begin
////              rule(evd.triggerList[0].condition.language = 'text\fhir2_pathengine', 'Condition language must be fhir2_pathengine');
////              rule(evd.triggerList[0].condition.expression <> '', 'Condition fhir2_pathengine must not be blank');
////              try
////                expr := fpp.parse(evd.triggerList[0].condition.expression);
////                evd.triggerList[0].condition.expressionElement.Tag := expr;
////                fpe.check(nil, CODES_TFhirAllTypesEnum[evd.triggerList[0].dataList[0].type_], CODES_TFhirAllTypesEnum[evd.triggerList[0].dataList[0].type_], CODES_TFhirAllTypesEnum[evd.triggerList[0].dataList[0].type_], expr, false);
////              except
////                on e : exception do
////                  rule(false, 'Error parsing expression: '+e.Message);
////              end;
////            end;
////          end;
////        end;
//      finally
//        evd.Free;
//      end;
//    end;

    if ts.Count > 0 then
      raise EFHIRException.create(ts.Text);
  finally
    ts.Free;
  end;
end;

function TSubscriptionManagerR4.preparePackage(userkey : integer; created : boolean; sub: TFhirSubscriptionW; res: TFHIRResourceV): TFHIRResourceV;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  url : string;
  ex: TFhirExtension;
  entry : TFhirBundleEntry;
  bundle : TFHIRBundle;
  subscription: TFhirSubscription;
  resource: TFHIRResource;
begin
  subscription := sub.Resource as TFhirSubscription;
  resource := res as TFhirResource;

  if subscription.hasExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/subscription-prefetch') then
  begin
    bundle := TFhirBundle.Create(BundleTypeCollection);
    try
      bundle.id := NewGuidId;
      bundle.link_List.AddRelRef('source', AppendForwardSlash(base)+'Subsecription/'+subscription.id);
      request := TFHIRRequest.Create(TFHIRServerContext(ServerContext).ValidatorContext.link, roSubscription, TFHIRServerContext(ServerContext).Indexes.Compartments.Link);
      response := TFHIRResponse.Create(TFHIRServerContext(ServerContext).ValidatorContext.link);
      try
        bundle.entryList.Append.resource := resource.Link;
        request.Session := OnGetSessionEvent(userkey);
        request.baseUrl := Base;
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
                OnExecuteOperation(request, response, false);
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := inttostr(response.HTTPCode);
                entry.response.location := response.Location;
                entry.response.etag := 'W/'+response.versionId;
                entry.response.lastModified := TFslDateTime.makeUTC(response.lastModifiedDate);
                entry.resource := response.resource.link as TFhirResource;
              end;
            except
              on e : ERestfulException do
              begin
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := inttostr(e.Status);
                entry.resource := BuildOperationOutcome4(request.Lang, e);
              end;
              on e : Exception do
              begin
                entry.response := TFhirBundleEntryResponse.Create;
                entry.response.status := '500';
                entry.resource := BuildOperationOutcome4(request.Lang, e);
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

function TSubscriptionManagerR4.processUrlTemplate(url : String; res : TFhirResourceV) : String;
var
  b, e : integer;
  code, value : String;
  qry : TFHIRPathEngine;
  o : TFHIRSelection;
  results : TFHIRSelectionList;
  resource : TFhirResource;
begin
  resource := res as TFhirResource;

  while url.Contains('{{') do
  begin
    b := url.IndexOf('{{');
    e := url.IndexOf('}}');
    code := url.Substring(b+2, e-(b+2));
    if (code = 'id') then
      value := Codes_TFHIRResourceType[resource.ResourceType]+'/'+resource.id
    else
    begin
      qry := TFHIRPathEngine.create(nil, nil);
      try
        results := qry.evaluate(nil, resource, code);
        try
          value := '';
          for o in results do
          begin
            if o.value is TFHIRPrimitiveType then
              CommaAdd(value, TFHIRPrimitiveType(o.value).StringValue)
            else
              raise EFHIRException.create('URL templates can only refer to primitive types (found '+o.ClassName+')');
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

function TSubscriptionManagerR4.MeetsCriteria(sub : TFhirSubscriptionW; typekey, key, ResourceVersionKey, ResourcePreviousKey: integer; newRes, oldRes : TFHIRResourceV; conn: TFDBConnection): boolean;
var
  subscription : TFhirSubscription;
  t : TSubscriptionTopic;
  topic : TFHIRSubscriptionTopic;
  ext : String;
begin
  subscription := sub.Resource as TFhirSubscription;

  // R5 subscriptions backport
  ext := subscription.getExtensionString('http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-topic-canonical');
  if (ext <> '') then
  begin
    topic := nil;
    t := getSubscriptionTopic(ext);
    if (t <> nil) then
      topic := t.resource as TFhirSubscriptionTopic;
    if topic = nil then
      exit(false)
    else if not MeetsCriteriaTopic(topic, subscription, typekey, key, ResourceVersionKey, ResourcePreviousKey, NewRes as TFhirResource, oldRes as TFhirResource, conn) then
      exit(false);
  end;

  if subscription.criteria = '*' then
    result := true
  else if newRes = nil then
    result := false
  else
    result := MeetsCriteriaSearch(subscription.criteria, newRes, typekey, key, conn);
end;

function TSubscriptionManagerR4.determineResourceTypeKeys(topic: TSubscriptionTopic; conn: TFDBConnection): TArray<integer>;
var
  st : TFHIRSubscriptionTopic;
  i : integer;
begin
  st := topic.resource as TFHIRSubscriptionTopic;
  SetLength(result, st.resourceTrigger.resourceType.Count);
  for i := 0 to st.resourceTrigger.resourceType.Count - 1 do
    result[i] := conn.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(st.resourceTrigger.resourceType[i].value)+'''')
end;


{ TFHIRPathSubscriptionContext }

constructor TFHIRPathSubscriptionContext.Create(previous, current: TFhirObject);
begin
  inherited Create;
  FCurrent := current;
  FPrevious := previous;
end;

destructor TFHIRPathSubscriptionContext.Destroy;
begin
  FCurrent.Free;
  FPrevious.Free;
  inherited;
end;

procedure TFHIRPathSubscriptionContext.SetCurrent(const Value: TFHIRObject);
begin
  FCurrent.Free;
  FCurrent := Value;
end;

procedure TFHIRPathSubscriptionContext.SetPrevious(const Value: TFHIRObject);
begin
  FPrevious.Free;
  FPrevious := Value;
end;

{ TFHIRPathSubscriptionExtensions }

function TFHIRPathSubscriptionExtensions.isValidFunction(name: String): boolean;
begin
  result := false;
end;

function TFHIRPathSubscriptionExtensions.functionApplies(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; name: String): boolean;
begin
  result := false;
end;

function TFHIRPathSubscriptionExtensions.execute(context: TFHIRPathExecutionContext; focus: TFHIRObject; name: String; params: TFslList<TFHIRPathExpressionNodeV>; engine: TFHIRPathEngineV): TFHIRSelectionList;
begin
  result := nil;
end;

function TFHIRPathSubscriptionExtensions.resolveConstant(context: TFHIRPathExecutionContext; s: String; var obj: TFHIRObject): boolean;
var
  ctxt : TFHIRPathSubscriptionContext;
begin
  ctxt := context.appInfo as TFHIRPathSubscriptionContext;
  result := true;
  if s = '%previous' then
    obj := ctxt.FPrevious.Link
  else if s = '%current' then
    obj := ctxt.FCurrent.Link
  else
    result := false;
end;

end.


