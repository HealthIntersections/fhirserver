unit cds_hooks_server;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes, Generics.Collections,
  IdHTTPServer, IdContext, IdCustomHTTPServer,
  fsl_base, fsl_json, fsl_http,
  fhir_objects, fhir_client,
  session,
  fhir_cdshooks, utilities, server_context;

type
  TCDSHooksProcessor = class (TFslObject)
  private
    Frequest: TCDSHookRequest;
    Fresponse: TCDSHookResponse;
    FClient: TFHIRClientV;
    procedure SetClient(const Value: TFHIRClientV);
    procedure Setrequest(const Value: TCDSHookRequest);
    procedure Setresponse(const Value: TCDSHookResponse);
  public
    destructor Destroy; override;
    function Link : TCDSHooksProcessor; overload;

    property request : TCDSHookRequest read Frequest write Setrequest;
    property response : TCDSHookResponse read Fresponse write Setresponse;
    property Client : TFHIRClientV read FClient write SetClient;

    function execute : boolean; virtual;

    function addCard(summary, detail, indicator, sourceLabel, sourceUrl : String) : TCDSHookCard;
  end;

  TCDSHooksProcessorClass = class of TCDSHooksProcessor;

  TCDSHooksService = class (TFslObject)
  private
    Procedure HandleRequest(base : String; server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo); overload;
  protected
    FEngines : TList<TCDSHooksProcessorClass>;
    Procedure require(test : boolean; msg : String);
    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; overload; virtual; abstract;
    function ProcessRequestEngines(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest; response : TCDSHookResponse) : boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function hook : string; virtual; abstract; // see the hook catalog (http://cds-hooks.org/#hook-catalog)
    function name : String; virtual; abstract;
    function description : String; virtual; abstract;
    function id : String; virtual; // must be unique across this server
    procedure registerPreFetch(json : TJsonObject); virtual;

  end;

  TCDSHooksServer = class (TFHIRServerWorker)
  private
    FServices : TFslMap<TCDSHooksService>;
    function GetActive: boolean;
    Procedure HandleServiceList(response: TIdHTTPResponseInfo);
    function getBase(secure : boolean; basePath : String; request : TIdHTTPRequestInfo) : string;
  public
    constructor Create(ServerContext : TFslObject);
    destructor Destroy; override;

    procedure RegisterService(service : TCDSHooksService);

    Procedure HandleRequest(secure : boolean; basePath : String; session : TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    property Active : boolean read GetActive;
  end;

implementation

{ TCDSHooksServer }

constructor TCDSHooksServer.Create(ServerContext : TFslObject);
begin
  inherited Create(ServerContext);
  FServices := TFslMap<TCDSHooksService>.create('cds.hooks.event');
end;

destructor TCDSHooksServer.Destroy;
begin
  FServices.Free;
  inherited;
end;

function TCDSHooksServer.GetActive: boolean;
begin
  result := FServices.Count > 0;
end;

function TCDSHooksServer.getBase(secure : boolean; basePath : String; request: TIdHTTPRequestInfo): string;
begin
  if secure then
    result := 'https://'+request.Host+basePath
  else
    result := 'http://'+request.Host+basePath
end;

procedure TCDSHooksServer.HandleRequest(secure: boolean; basePath : String; session: TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  response.CustomHeaders.Add('Access-Control-Allow-Credentials: true');
  response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
  response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
  if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
    response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
  if request.Document = basePath+'/cds-services' then
    handleServiceList(response)
  else if FServices.ContainsKey(request.Document.Substring(basePath.Length + 14)) then
    FServices[request.Document.Substring(basePath.Length + 14)].handleRequest(getBase(secure, basePath, request), TFHIRServerContext(ServerContext), secure, session, context, request, response)
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
  end;
end;

procedure TCDSHooksServer.HandleServiceList(response: TIdHTTPResponseInfo);
var
  json, jsvc : TJsonObject;
  services : TJsonArray;
  svc : TCDSHooksService;
  id : String;
begin
  json := TJsonObject.Create;
  try
    services := json.forceArr['services'];
    for id in FServices.SortedKeys do
    begin
      svc := FServices[id];
      jsvc := services.addObject;
      jsvc.str['hook'] := svc.hook;
      jsvc.str['name'] := svc.name;
      jsvc.str['description'] := svc.description;
      jsvc.str['id'] := svc.id;
      svc.registerPreFetch(jsvc);
    end;
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentType := 'application/json';
    response.ContentText := TJSONWriter.writeObjectStr(json,true);
  finally
    json.Free;
  end;
end;

procedure TCDSHooksServer.RegisterService(service: TCDSHooksService);
begin
  FServices.Add(service.id, service);
end;

{ TCDSHooksService }

constructor TCDSHooksService.Create;
begin
  inherited;
  FEngines := TList<TCDSHooksProcessorClass>.create;
end;


procedure TCDSHooksService.HandleRequest(base : String; server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  jrequest : TJsonObject;
  req : TCDSHookRequest;
  resp : TCDSHookResponse;
  card : TCDSHookCard;
begin
  require(request.CommandType = hcPOST, 'Request to cds-hooks service must be a POST');
  require(request.ContentType = 'application/json', 'Request to cds-hooks service must be a POST of application/json');
  require((request.PostStream <> nil) and (request.PostStream.Size > 0), 'Request to cds-hooks service must include a body');

  try
    jrequest := TJSONParser.Parse(request.PostStream);
    try
      req := TCDSHookRequest.Create(jrequest);
      try
        req.lang := THTTPLanguages.Create(request.AcceptLanguage);
        req.baseURL := base;
        resp := HandleRequest(server, secure, session, context, req);
        try
          response.ResponseNo := 200;
          response.ResponseText := 'OK';
          response.ContentType := 'application/json';
          response.ContentText := resp.asJson;
        finally
          resp.free;
        end;
      finally
        req.free;
      end;
    finally
      jrequest.Free;
    end;
  except
    on e : Exception do
    begin
      resp := TCDSHookResponse.Create;
      try
        card := resp.addCard;
        card.indicator := 'error';
        card.summary := e.Message;
        card.sourceLabel := server.Globals.OwnerName;
        card.sourceURL := request.Document;
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentType := 'application/json';
        response.ContentText := resp.asJson;
      finally
        resp.Free;
      end;
    end;
  end;
end;

destructor TCDSHooksService.Destroy;
begin
  FEngines.Free;
  inherited;
end;

function TCDSHooksService.id: String;
begin
  result := hook;
end;

function TCDSHooksService.ProcessRequestEngines(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest; response: TCDSHookResponse): boolean;
var
  client : TFHIRClientV;
  t : TCDSHooksProcessorClass;
  p : TCDSHooksProcessor;
begin
  result := false;

  if FEngines.Count = 0 then
    exit(false);
  client := server.Storage.createClient(THTTPLanguages.create('en'), server, server.ValidatorContext, session);
  try
    for t in FEngines do
    begin
      p := t.Create;
      try
        p.request := request.Link;
        p.response := response.Link;
        p.Client := client.link;
        if p.execute then
          exit(true);
      finally
        p.Free;
      end;
    end;
    server.Storage.Yield(client, nil);
  except
    on e : Exception do
    begin
      server.Storage.Yield(client, e);
      raise;
    end;
  end;
end;

procedure TCDSHooksService.registerPreFetch(json: TJsonObject);
begin

end;

Procedure TCDSHooksService.require(test: boolean; msg: String);
begin
  if not test then
    raise EFHIRException.create(msg);
end;


{ TCDSHooksProcessor }

function TCDSHooksProcessor.addCard(summary, detail, indicator, sourceLabel, sourceUrl : String): TCDSHookCard;
begin
  result := TCDSHookCard.Create;
  try
    result.summary := summary;
    result.detail := detail;
    result.indicator := indicator;
    result.sourceLabel := sourceLabel;
    result.sourceURL := sourceUrl;

    response.cards.Add(result.Link);
  finally
    result.free;
  end;
end;

destructor TCDSHooksProcessor.Destroy;
begin
  Frequest.Free;
  Fresponse.Free;
  FClient.Free;

  inherited;
end;

function TCDSHooksProcessor.execute: boolean;
begin
  result := false;
end;

function TCDSHooksProcessor.Link: TCDSHooksProcessor;
begin
  result := TCDSHooksProcessor(inherited Link);
end;

procedure TCDSHooksProcessor.SetClient(const Value: TFHIRClientV);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TCDSHooksProcessor.Setrequest(const Value: TCDSHookRequest);
begin
  Frequest.Free;
  Frequest := Value;
end;

procedure TCDSHooksProcessor.Setresponse(const Value: TCDSHookResponse);
begin
  Fresponse.Free;
  Fresponse := Value;
end;

end.

