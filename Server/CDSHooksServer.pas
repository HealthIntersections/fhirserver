unit CDSHooksServer;

interface

uses
  SysUtils, Classes,
  IdHTTPServer, IdContext, IdCustomHTTPServer,
  AdvObjects, AdvGenerics, AdvJson,
  FHIRSupport,
  CDSHooksUtilities, ServerUtilities, FHIRServerContext;

type
  TCDSHooksService = class (TAdvObject)
  private
    Procedure HandleRequest(base : String; server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo); overload;
  protected
    Procedure require(test : boolean; msg : String);
    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; overload; virtual;
  public
    function hook : string; virtual; // see the hook catalog (http://cds-hooks.org/#hook-catalog)
    function name : String; virtual;
    function description : String; virtual;
    function id : String; virtual; // must be unique across this server
    procedure registerPreFetch(json : TJsonObject); virtual;
  end;

  TCDSHooksServer = class (TFHIRServerWorker)
  private
    FServices : TAdvMap<TCDSHooksService>;
    function GetActive: boolean;
    Procedure HandleServiceList(response: TIdHTTPResponseInfo);
    function getBase(secure : boolean; basePath : String; request : TIdHTTPRequestInfo) : string;
  public
    Constructor Create(ServerContext : TAdvObject);
    Destructor Destroy; override;

    procedure RegisterService(service : TCDSHooksService);

    Procedure HandleRequest(secure : boolean; basePath : String; session : TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    property Active : boolean read GetActive;
  end;

implementation

{ TCDSHooksServer }

constructor TCDSHooksServer.Create(ServerContext : TAdvObject);
begin
  inherited Create(ServerContext);
  FServices := TAdvMap<TCDSHooksService>.create;
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

function TCDSHooksService.description: String;
begin
  raise Exception.Create('Must override description() in '+className);
end;

procedure TCDSHooksService.HandleRequest(base : String; server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  jrequest : TJsonObject;
  req : TCDSHookRequest;
  resp : TCDSHookResponse;
begin
  require(request.CommandType = hcPOST, 'Request to cds-hooks service must be a POST');
  require(request.ContentType = 'application/json', 'Request to cds-hooks service must be a POST');
  require((request.PostStream <> nil) and (request.PostStream.Size > 0), 'Request to cds-hooks service must include a body');

  try
    jrequest := TJSONParser.Parse(request.PostStream);
    try
      req := TCDSHookRequest.Create(jrequest);
      try
        req.lang := request.AcceptLanguage;
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
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      response.ContentType := 'test/plain';
      response.ContentText := e.Message;
    end;
  end;
end;

function TCDSHooksService.HandleRequest(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
begin
  raise Exception.Create('Must override HandleRequest in '+className);
end;

function TCDSHooksService.hook: string;
begin
  raise Exception.Create('Must override hook() in '+className);
end;

function TCDSHooksService.id: String;
begin
  result := hook;
end;

function TCDSHooksService.name: String;
begin
  raise Exception.Create('Must override name() in '+className);
end;

procedure TCDSHooksService.registerPreFetch(json: TJsonObject);
begin

end;

Procedure TCDSHooksService.require(test: boolean; msg: String);
begin
  if not test then
    raise Exception.Create(msg);
end;


end.
