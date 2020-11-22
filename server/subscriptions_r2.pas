unit subscriptions_r2;

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
  fhir_objects, fhir_common, 
  subscriptions, session, server_context;

Type
  TSubscriptionManagerR2 = class (TSubscriptionManager)
  private
  protected
    procedure checkAcceptable(sub : TFHIRSubscriptionW; session : TFHIRSession); override;
    function makeSubscription(resource : TFHIRResourceV) : TFHIRSubscriptionW; override;
    function preparePackage(userkey : integer; created : boolean; sub : TFHIRSubscriptionW; res : TFHIRResourceV) : TFHIRResourceV; override;
    function MeetsCriteria(sub : TFHIRSubscriptionW; typekey : Integer; key, ResourceVersionKey, ResourcePreviousKey : integer; newRes, oldRes : TFHIRResourceV; conn : TFDBConnection) : boolean; override;
    function checkSubscription(sub: TFHIRResourceV) : TFHIRSubscriptionW; override;
    function checkSubscriptionTopic(sub: TFHIRResourceV) : TFHIRSubscriptionTopicW; override;
    function loadSubscriptionTopic(res : TFHIRResourceV) : TSubscriptionTopic; override;
    function loadSubscription(res : TFHIRResourceV) : TFHIRSubscriptionW; override;
    function bundleIsTransaction(res : TFHIRResourceV) : boolean; override;
    function processUrlTemplate(url : String; res : TFhirResourceV) : String; override;
    function determineResourceTypeKeys(topic: TSubscriptionTopic; conn: TFDBConnection): TArray<integer>; override;
  end;

implementation

uses
  fhir2_types, fhir2_utilities, fhir2_constants, fhir2_pathengine, fhir2_pathnode, fhir2_common,
  fhir2_resources_base, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_canonical, fhir2_resources_other;

function TSubscriptionManagerR2.checkSubscription(sub : TFHIRResourceV): TFHIRSubscriptionW;
var
  subscription: TFHIRSubscription;
begin
  subscription := sub as TFHIRSubscription;
  subscription.checkNoImplicitRules('SubscriptionManager.SeeNewSubscription', 'subscription');
  if subscription.status in [SubscriptionStatusActive, SubscriptionStatusError] then
    result := TFHIRSubscription2.Create(subscription.Link)
  else
    result := nil;
end;

function TSubscriptionManagerR2.checkSubscriptionTopic(sub: TFHIRResourceV): TFHIRSubscriptionTopicW;
begin
  result := nil;
end;

function TSubscriptionManagerR2.determineResourceTypeKeys(topic: TSubscriptionTopic; conn: TFDBConnection): TArray<integer>;
begin
  SetLength(result, 0);
end;

function TSubscriptionManagerR2.loadSubscriptionTopic(res: TFHIRResourceV): TSubscriptionTopic;
begin
  raise EFHIRUnsupportedVersion.Create(fhirVersionRelease2, 'Creating Event Definition');
end;

function TSubscriptionManagerR2.loadSubscription(res: TFHIRResourceV): TFHIRSubscriptionW;
begin
  if res is TFhirSubscription then
    result := TFHIRSubscription2.Create(res.link)
  else if res.fhirType = 'Subscription' then
    raise EFHIRUnsupportedVersion.Create(fhirVersionRelease2, 'Creating Event Definition')
  else
    raise EFslException.Create('Wrong resource type '+res.fhirType+' looking for Subscription');
end;

function TSubscriptionManagerR2.makeSubscription(resource: TFHIRResourceV): TFHIRSubscriptionW;
begin
  result := TFHIRSubscription2.Create(TFhirSubscription.create);
end;

function TSubscriptionManagerR2.bundleIsTransaction(res: TFHIRResourceV): boolean;
begin
  if res is TFhirBundle then
    result := TFhirBundle(res).type_ = BundleTypeTransaction
  else
    result := false;
end;

procedure TSubscriptionManagerR2.checkAcceptable(sub: TFhirSubscriptionW; session: TFHIRSession);
var
  ts : TStringList;
  function rule(test : boolean; message : String) : boolean;
  begin
    if not test then
      ts.Add(message);
    result := test;
  end;
var
  subscription: TFhirSubscription;
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

    if ts.Count > 0 then
      raise EFHIRException.create(ts.Text);
  finally
    ts.Free;
  end;
end;

function TSubscriptionManagerR2.preparePackage(userkey : integer; created : boolean; sub: TFhirSubscriptionW; res: TFHIRResourceV): TFHIRResourceV;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  url : string;
  i : integer;
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

function TSubscriptionManagerR2.processUrlTemplate(url : String; res : TFhirResourceV) : String;
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

function TSubscriptionManagerR2.MeetsCriteria(sub : TFhirSubscriptionW; typekey : Integer; key, ResourceVersionKey, ResourcePreviousKey: integer; newRes, oldRes : TFHIRResourceV; conn: TFDBConnection): boolean;
var
  subscription : TFhirSubscription;
begin
  subscription := sub.Resource as TFhirSubscription;
  if subscription.criteria = '*' then
    result := true
  else if newRes = nil then
    result := false
  else
    result := MeetsCriteriaSearch(subscription.criteria, newRes, typekey, key, conn);
end;

end.


