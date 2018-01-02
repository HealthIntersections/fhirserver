unit ServerJavascriptHost;

interface

uses
  SysUtils, Classes,
  kCritSct,
  AdvObjects, AdvGenerics,
  FHIRBase, FHIRResources, FHIRSupport,
  FHIRJavascript;

Type
  TJsEventScript = class (TAdvObject)
  private
    FId : String;
    FScript : string;
    FRoutine : String;
    FCommands : TFHIRCommandTypeSet;
    FResources : TStringList;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function link : TJsEventScript; overload;

    property id : String read FId write FId;
    property script : String read FScript write FScript;
    property routine : String read FRoutine write FRoutine;
    property Commands : TFHIRCommandTypeSet read FCommands write FCommands;
    property Resources : TStringList read FResources;
  end;

  TJsEventScriptRegistry = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FScripts : TAdvMap<TJsEventScript>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure registerScript(script : TJsEventScript);
    procedure unregisterScript(id : String);
    procedure getScripts(list : TAdvList<TJsEventScript>);
  end;

Type
  // we create one of these for evrey thread, but it will only actually create a javscript engine when it needs to.
  // then, we retain it as long as we can
  TJsHost = class (TAdvObject)
  private

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure previewRequest(request : TFHIRRequest);
    procedure checkChanges(before, after : TFHIRResource);
  end;

threadvar
  GJsHost : TJsHost;

implementation

{ TServerJavascriptHost }

procedure TJsHost.checkChanges(before, after: TFHIRResource);
begin

end;

constructor TJsHost.Create;
begin
  inherited;

end;

destructor TJsHost.Destroy;
begin

  inherited;
end;

procedure TJsHost.previewRequest(request: TFHIRRequest);
begin

end;

{ TJsEventScriptRegistry }

constructor TJsEventScriptRegistry.Create;
begin
  inherited;
  FLock := TCriticalSection.create;
  FScripts := TAdvMap<TJsEventScript>.create;
end;

destructor TJsEventScriptRegistry.Destroy;
begin
  FLock.Free;
  FScripts.Free;
  inherited;
end;

procedure TJsEventScriptRegistry.getScripts(list : TAdvList<TJsEventScript>);
var
  e : TJsEventScript;
begin
  FLock.Lock;
  try
    for e in FScripts.Values do
      list.Add(e.Link);
  finally
    FLock.Unlock;
  end;
end;

procedure TJsEventScriptRegistry.registerScript(script: TJsEventScript);
begin
  FLock.Lock;
  try
    if FScripts.ContainsKey(script.id) then
      raise Exception.Create('Duplicate Script: '+script.id);
    FScripts.Add(script.id, script.link);
  finally
    FLock.Unlock;
  end;
end;

procedure TJsEventScriptRegistry.unregisterScript(id: String);
begin
  FLock.Lock;
  try
    if not FScripts.ContainsKey(id) then
      raise Exception.Create('unknown Script: '+id);
    FScripts.Remove(id);
  finally
    FLock.Unlock;
  end;
end;

{ TJsEventScript }

constructor TJsEventScript.Create;
begin
  inherited;
  FResources := TStringList.Create;
end;

destructor TJsEventScript.Destroy;
begin
  FResources.Free;
  inherited;
end;

function TJsEventScript.link: TJsEventScript;
begin
  result := TJsEventScript(inherited Link);
end;

end.
