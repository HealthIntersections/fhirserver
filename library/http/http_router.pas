unit http_router;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, RegExpr,
  http_support, http_server;

type
  // Forward declarations
  THttpRouter = class;
  
  // Route parameters extracted from path
  TRouteParams = class
  private
    FParams: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Add(const Name, Value: string);
    function Get(const Name: string): string; overload;
    function Get(const Name: string; const Default: string): string; overload;
    function GetAsInteger(const Name: string; const Default: Integer = 0): Integer;
    function Has(const Name: string): Boolean;
    procedure Clear;
    function Count: Integer;
    
    // For debugging
    function AsString: string;
  end;
  
  // Enhanced request handler with route parameters
  TRouteHandler = procedure(Request: THttpRequest; Response: THttpResponse; Params: TRouteParams) of object;
  TErrorHandler = procedure(Request: THttpRequest; Response: THttpResponse; const Error: Exception) of object;
  
  // Middleware handler (can modify request/response or stop processing)
  TMiddlewareHandler = procedure(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean) of object;
  
  // Route definition
  TRouteDefinition = class
  private
    FMethod: THttpMethod;
    FPattern: string;
    FRegex: TRegExpr;
    FParamNames: TStringList;
    FHandler: TRouteHandler;
    FMiddleware: TList<TMiddlewareHandler>;
    
    procedure BuildRegexFromPattern;
    function ExtractParams(const Path: string; Params: TRouteParams): Boolean;
  public
    constructor Create(AMethod: THttpMethod; const APattern: string; AHandler: TRouteHandler);
    destructor Destroy; override;
    
    function Matches(Method: THttpMethod; const Path: string): Boolean;
    function Execute(Request: THttpRequest; Response: THttpResponse; const Path: string): Boolean;
    
    // Middleware support
    procedure AddMiddleware(Middleware: TMiddlewareHandler);
    
    property Method: THttpMethod read FMethod;
    property Pattern: string read FPattern;
  end;
  
  // Main router class
  THttpRouter = class
  private
    FRoutes: TObjectList<TRouteDefinition>;
    FGlobalMiddleware: TList<TMiddlewareHandler>;
    FNotFoundHandler: TRouteHandler;
    FErrorHandler: TErrorHandler;
    
    procedure DefaultNotFoundHandler(Request: THttpRequest; Response: THttpResponse; Params: TRouteParams);
    procedure DefaultErrorHandler(Request: THttpRequest; Response: THttpResponse; const Error: Exception);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Route registration methods (Express.js style)
    function Get(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Post(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Put(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Delete(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Head(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Options(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    
    // Generic route registration
    function Route(Method: THttpMethod; const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    
    // Middleware registration
    procedure Use(Middleware: TMiddlewareHandler); overload;
    procedure Use(const Pattern: string; Middleware: TMiddlewareHandler); overload;
    
    // Error and 404 handlers
    procedure SetNotFoundHandler(Handler: TRouteHandler);
    procedure SetErrorHandler(Handler: TErrorHandler);
    
    // Main request handler (called by HTTP server)
    procedure HandleRequest(Request: THttpRequest; Response: THttpResponse);
    
    // Helper methods
    function RouteCount: Integer;
    procedure ListRoutes(Routes: TStringList);
  end;
  
  // Convenience class that combines router with server
  THttpApp = class
  private
    FServer: TLightHttpServer;
    FRouter: THttpRouter;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Server control
    procedure Listen(Port: Word);
    procedure Stop;
    
    // Route registration (delegates to router)
    function Get(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Post(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Put(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Delete(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Head(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    function Options(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
    
    // Middleware
    procedure Use(Middleware: TMiddlewareHandler); overload;
    procedure Use(const Pattern: string; Middleware: TMiddlewareHandler); overload;
    
    // Error handlers
    procedure SetNotFoundHandler(Handler: TRouteHandler);
    procedure SetErrorHandler(Handler: TErrorHandler);
    
    // Properties
    property Server: TLightHttpServer read FServer;
    property Router: THttpRouter read FRouter;
  end;

// Common middleware functions
procedure CorsMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);
procedure LoggingMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);
procedure JsonParserMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);

implementation

uses
  DateUtils;

{ TRouteParams }

constructor TRouteParams.Create;
begin
  inherited Create;
  FParams := TDictionary<string, string>.Create;
end;

destructor TRouteParams.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TRouteParams.Add(const Name, Value: string);
begin
  FParams.AddOrSetValue(Name, Value);
end;

function TRouteParams.Get(const Name: string): string;
begin
  if not FParams.TryGetValue(Name, Result) then
    Result := '';
end;

function TRouteParams.Get(const Name: string; const Default: string): string;
begin
  if not FParams.TryGetValue(Name, Result) then
    Result := Default;
end;

function TRouteParams.GetAsInteger(const Name: string; const Default: Integer): Integer;
var
  Value: string;
begin
  if FParams.TryGetValue(Name, Value) then
    Result := StrToIntDef(Value, Default)
  else
    Result := Default;
end;

function TRouteParams.Has(const Name: string): Boolean;
begin
  Result := FParams.ContainsKey(Name);
end;

procedure TRouteParams.Clear;
begin
  FParams.Clear;
end;

function TRouteParams.Count: Integer;
begin
  Result := FParams.Count;
end;

function TRouteParams.AsString: string;
var
  Pair: TPair<string, string>;
begin
  Result := '';
  for Pair in FParams do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Pair.Key + '=' + Pair.Value;
  end;
end;

{ TRouteDefinition }

constructor TRouteDefinition.Create(AMethod: THttpMethod; const APattern: string; AHandler: TRouteHandler);
begin
  inherited Create;
  FMethod := AMethod;
  FPattern := APattern;
  FHandler := AHandler;
  FParamNames := TStringList.Create;
  FMiddleware := TList<TMiddlewareHandler>.Create;
  FRegex := TRegExpr.Create;
  BuildRegexFromPattern;
end;

destructor TRouteDefinition.Destroy;
begin
  FParamNames.Free;
  FMiddleware.Free;
  FRegex.Free;
  inherited Destroy;
end;

procedure TRouteDefinition.BuildRegexFromPattern;
var
  RegexPattern: string;
  i: Integer;
  InParam: Boolean;
  ParamName: string;
  c: Char;
begin
  // Convert Express-style pattern like "/users/:id/posts/:postId" 
  // to regex like "^/users/([^/]+)/posts/([^/]+)$"
  
  FParamNames.Clear;
  RegexPattern := '';
  InParam := False;
  ParamName := '';
  
  for i := 1 to Length(FPattern) do
  begin
    c := FPattern[i];
    
    if c = ':' then
    begin
      InParam := True;
      ParamName := '';
      RegexPattern := RegexPattern + '([^/]+)'; // Match anything except /
    end
    else if InParam and (c in ['/', '?', '&', '#']) then
    begin
      // End of parameter name
      FParamNames.Add(ParamName);
      InParam := False;
      RegexPattern := RegexPattern + c;
    end
    else if InParam then
    begin
      ParamName := ParamName + c;
    end
    else if c in ['.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '\'] then
    begin
      // Escape regex special characters
      RegexPattern := RegexPattern + '\' + c;
    end
    else
    begin
      RegexPattern := RegexPattern + c;
    end;
  end;
  
  // Handle parameter at end of pattern
  if InParam then
    FParamNames.Add(ParamName);
  
  // Anchor the pattern
  RegexPattern := '^' + RegexPattern + '$';
  
  FRegex.Expression := RegexPattern;
  FRegex.ModifierI := True; // Case insensitive
  FRegex.Compile;
end;

function TRouteDefinition.Matches(Method: THttpMethod; const Path: string): Boolean;
begin
  Result := (FMethod = Method) and FRegex.Exec(Path);
end;

function TRouteDefinition.ExtractParams(const Path: string; Params: TRouteParams): Boolean;
var
  i: Integer;
begin
  Result := False;
  Params.Clear;
  
  if not FRegex.Exec(Path) then Exit;
  
  // Extract parameter values from captured groups
  for i := 0 to FParamNames.Count - 1 do
  begin
    if i + 1 <= FRegex.SubExprMatchCount then
      Params.Add(FParamNames[i], FRegex.Match[i + 1]);
  end;
  
  Result := True;
end;

function TRouteDefinition.Execute(Request: THttpRequest; Response: THttpResponse; const Path: string): Boolean;
var
  Params: TRouteParams;
  Middleware: TMiddlewareHandler;
  Continue: Boolean;
begin
  Result := False;
  
  Params := TRouteParams.Create;
  try
    if not ExtractParams(Path, Params) then Exit;
    
    // Execute middleware chain
    Continue := True;
    for Middleware in FMiddleware do
    begin
      Middleware(Request, Response, Continue);
      if not Continue then Exit;
    end;
    
    // Execute main handler
    if Assigned(FHandler) then
    begin
      FHandler(Request, Response, Params);
      Result := True;
    end;
    
  finally
    Params.Free;
  end;
end;

procedure TRouteDefinition.AddMiddleware(Middleware: TMiddlewareHandler);
begin
  FMiddleware.Add(Middleware);
end;

{ THttpRouter }

constructor THttpRouter.Create;
begin
  inherited Create;
  FRoutes := TObjectList<TRouteDefinition>.Create(True);
  FGlobalMiddleware := TList<TMiddlewareHandler>.Create;
  FNotFoundHandler := DefaultNotFoundHandler;
  FErrorHandler := DefaultErrorHandler;
end;

destructor THttpRouter.Destroy;
begin
  FRoutes.Free;
  FGlobalMiddleware.Free;
  inherited Destroy;
end;

function THttpRouter.Get(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmGET, Pattern, Handler);
end;

function THttpRouter.Post(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmPOST, Pattern, Handler);
end;

function THttpRouter.Put(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmPUT, Pattern, Handler);
end;

function THttpRouter.Delete(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmDELETE, Pattern, Handler);
end;

function THttpRouter.Head(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmHEAD, Pattern, Handler);
end;

function THttpRouter.Options(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := Route(hmOPTIONS, Pattern, Handler);
end;

function THttpRouter.Route(Method: THttpMethod; const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := TRouteDefinition.Create(Method, Pattern, Handler);
  FRoutes.Add(Result);
end;

procedure THttpRouter.Use(Middleware: TMiddlewareHandler);
begin
  FGlobalMiddleware.Add(Middleware);
end;

procedure THttpRouter.Use(const Pattern: string; Middleware: TMiddlewareHandler);
var
  Route: TRouteDefinition;
begin
  // Find routes matching pattern and add middleware
  for Route in FRoutes do
  begin
    if Route.Pattern.StartsWith(Pattern) then
      Route.AddMiddleware(Middleware);
  end;
end;

procedure THttpRouter.SetNotFoundHandler(Handler: TRouteHandler);
begin
  FNotFoundHandler := Handler;
end;

procedure THttpRouter.SetErrorHandler(Handler: TErrorHandler);
begin
  FErrorHandler := Handler;
end;

procedure THttpRouter.HandleRequest(Request: THttpRequest; Response: THttpResponse);
var
  Route: TRouteDefinition;
  Middleware: TMiddlewareHandler;
  Continue: Boolean;
  Params: TRouteParams;
  Handled: Boolean;
begin
  Handled := False;
  
  try
    // Execute global middleware
    Continue := True;
    for Middleware in FGlobalMiddleware do
    begin
      Middleware(Request, Response, Continue);
      if not Continue then Exit;
    end;
    
    // Find matching route
    for Route in FRoutes do
    begin
      if Route.Matches(Request.Method, Request.Path) then
      begin
        if Route.Execute(Request, Response, Request.Path) then
        begin
          Handled := True;
          Break;
        end;
      end;
    end;
    
    // Call 404 handler if no route matched
    if not Handled then
    begin
      Params := TRouteParams.Create;
      try
        FNotFoundHandler(Request, Response, Params);
      finally
        Params.Free;
      end;
    end;
    
  except
    on E: Exception do
      FErrorHandler(Request, Response, E);
  end;
end;

procedure THttpRouter.DefaultNotFoundHandler(Request: THttpRequest; Response: THttpResponse; Params: TRouteParams);
begin
  Response.SetStatus(404, 'Not Found');
  Response.SetJSONContent(Format('{"error": "Not Found", "path": "%s", "method": "%s"}', 
    [Request.Path, MethodToString(Request.Method)]));
end;

procedure THttpRouter.DefaultErrorHandler(Request: THttpRequest; Response: THttpResponse; const Error: Exception);
begin
  Response.SetStatus(500, 'Internal Server Error');
  Response.SetJSONContent(Format('{"error": "Internal Server Error", "message": "%s"}', [Error.Message]));
end;

function THttpRouter.RouteCount: Integer;
begin
  Result := FRoutes.Count;
end;

procedure THttpRouter.ListRoutes(Routes: TStringList);
var
  Route: TRouteDefinition;
begin
  Routes.Clear;
  for Route in FRoutes do
    Routes.Add(Format('%s %s', [MethodToString(Route.Method), Route.Pattern]));
end;

{ THttpApp }

constructor THttpApp.Create;
begin
  inherited Create;
  FServer := CreateHttpServer;
  FRouter := THttpRouter.Create;
  FServer.SetRequestHandler(FRouter.HandleRequest);
end;

destructor THttpApp.Destroy;
begin
  Stop;
  FRouter.Free;
  FServer.Free;
  inherited Destroy;
end;

procedure THttpApp.Listen(Port: Word);
begin
  FServer.Port := Port;
  FServer.Start;
end;

procedure THttpApp.Stop;
begin
  FServer.Stop;
end;

function THttpApp.Get(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Get(Pattern, Handler);
end;

function THttpApp.Post(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Post(Pattern, Handler);
end;

function THttpApp.Put(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Put(Pattern, Handler);
end;

function THttpApp.Delete(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Delete(Pattern, Handler);
end;

function THttpApp.Head(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Head(Pattern, Handler);
end;

function THttpApp.Options(const Pattern: string; Handler: TRouteHandler): TRouteDefinition;
begin
  Result := FRouter.Options(Pattern, Handler);
end;

procedure THttpApp.Use(Middleware: TMiddlewareHandler);
begin
  FRouter.Use(Middleware);
end;

procedure THttpApp.Use(const Pattern: string; Middleware: TMiddlewareHandler);
begin
  FRouter.Use(Pattern, Middleware);
end;

procedure THttpApp.SetNotFoundHandler(Handler: TRouteHandler);
begin
  FRouter.SetNotFoundHandler(Handler);
end;

procedure THttpApp.SetErrorHandler(Handler: TErrorHandler);
begin
  FRouter.SetErrorHandler(Handler);
end;

{ Common Middleware }

procedure CorsMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);
begin
  Response.SetHeader('Access-Control-Allow-Origin', '*');
  Response.SetHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  Response.SetHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  
  // Handle preflight requests
  if Request.Method = hmOPTIONS then
  begin
    Response.SetStatus(200, 'OK');
    Response.SetTextContent('');
    Continue := False; // Stop processing
  end
  else
    Continue := True;
end;

procedure LoggingMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);
begin
  WriteLn(Format('[%s] %s %s - %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    MethodToString(Request.Method),
    Request.Path,
    Request.ClientIP
  ]));
  Continue := True;
end;

procedure JsonParserMiddleware(Request: THttpRequest; Response: THttpResponse; var Continue: Boolean);
begin
  // Add a parsed JSON property to request if content-type is application/json
  // This would require extending THttpRequest with a ParsedJSON property
  // For now, just ensure content-type is set properly
  if Request.GetContentType.StartsWith('application/json') then
  begin
    // JSON parsing would go here
    // Could add parsed JSON to request properties
  end;
  Continue := True;
end;

end.
