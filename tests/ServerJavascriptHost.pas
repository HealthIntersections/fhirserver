unit ServerJavascriptHost;

interface

uses
  SysUtils, Classes,
  kCritSct, Javascript,
  AdvObjects, AdvGenerics,
  FHIRBase, FHIRTypes, FHIRResources, FHIRSupport, FHIRClient,
  FHIRJavascript;

Const
  ROUTINE_NAMES : array[TFhirTriggerTypeEnum] of String = ('xx', 'xx', 'xx', 'dataChanged', 'dataAdded', 'dataModified', 'dataRemoved', 'dataAccessed', 'xx');
  SUPPORTED_TRIGGER_TYPES = [TriggerTypeDataChanged,  TriggerTypeDataAdded,  TriggerTypeDataModified, TriggerTypeDataRemoved, TriggerTypeDataAccessed];

Type
  TJsEventScript = class (TAdvObject)
  private
    FId : String;
    FScript : string;
    FCommand : TFhirTriggerTypeEnum;
    FResources : TStringList;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function link : TJsEventScript; overload;

    property id : String read FId write FId;
    property script : String read FScript write FScript;
    property Commands : TFhirTriggerTypeEnum read FCommand write FCommand;
    property Resources : TStringList read FResources;
  end;

  TJsEventScriptRegistry = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FScripts : TAdvMap<TJsEventScript>;
//    procedure registerScript(script : TJsEventScript);
//    procedure unregisterScript(id : String);
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TJsEventScriptRegistry; overload;

    procedure getScripts(list: TAdvList<TJsEventScript>);
    procedure getApplicableScripts(event : TFhirTriggerTypeEnum; resource : String; list : TAdvList<TJsEventScript>);

    procedure checkResource(event : TFhirEventDefinition);
    procedure seeResource(event : TFhirEventDefinition);
  end;

  TJsGetFHIRResource = reference to function : TFHIRResource;
  TJsGetFHIRClient = reference to function : TFHIRClient;

  // we create one of these for evrey thread, but it will only actually create a javscript engine when it needs to.
  // then, we retain it as long as we can
  TJsHost = class (TAdvObject)
  private
    FRegistry: TJsEventScriptRegistry;
    FEngine : TFHIRJavascript;
    procedure SetRegistry(const Value: TJsEventScriptRegistry);
    procedure checkHasEngine;
    procedure previewRequest(session: TFHIRSession; request: TFHIRRequest);
  public
    constructor Create; override;
    destructor Destroy; override;

    property registry : TJsEventScriptRegistry read FRegistry write SetRegistry;

//    procedure previewRequest(session : TFHIRSession; request : TFHIRRequest);
    procedure checkChanges(event: TFhirTriggerTypeEnum; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResource);
  end;

threadvar
  GJsHost : TJsHost;

implementation

{ TServerJavascriptHost }

procedure TJsHost.checkChanges(event: TFhirTriggerTypeEnum; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResource);
var
  scripts : TAdvList<TJsEventScript>;
  script : TJsEventScript;
  rn : String;
  s, b, a, c : TJsValue;
begin
  scripts := TAdvList<TJsEventScript>.create;
  try
    if before <> nil then
      rn := before.fhirType
    else
      rn := after.fhirType;

    FRegistry.getApplicableScripts(event, rn, scripts);
    if (scripts.count > 0) then
    begin
      checkHasEngine;
      s := FEngine.wrap(session.Link, 'Session', true);
      b := FEngine.wrap(before.Link, rn, true);
      a := FEngine.wrap(after.Link, rn, true);
      c := FEngine.wrap(client.link, 'FHIRClient', true);
      FEngine.addGlobal('fhir', c);
      for script in scripts do
        FEngine.execute(script.FScript, 'event-'+script.id, ROUTINE_NAMES[script.FCommand], [s, b,a]);
    end;
  finally
    scripts.Free;
  end;
end;

procedure TJsHost.checkHasEngine;
begin
  if FEngine = nil then
    FEngine := TFHIRJavascript.Create;
end;

constructor TJsHost.Create;
begin
  inherited;

end;

destructor TJsHost.Destroy;
begin
  FEngine.Free;
  FRegistry.Free;
  inherited;
end;

procedure TJsHost.previewRequest(session : TFHIRSession; request: TFHIRRequest);
begin

end;

procedure TJsHost.SetRegistry(const Value: TJsEventScriptRegistry);
begin
  FRegistry.Free;
  FRegistry := Value;
end;

{ TJsEventScriptRegistry }

procedure TJsEventScriptRegistry.checkResource(event: TFhirEventDefinition);
var
  tag : boolean;
  c : TFHIRCoding;
begin
  tag := false;
  for c in event.meta.tagList do
    tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

  if tag then
  begin
    if event.trigger.condition.language <> 'application/javascript' then
      raise Exception.Create('Unknown error message');
    if not (event.trigger.type_ in SUPPORTED_TRIGGER_TYPES) then
      raise Exception.Create('Unsupported Trigger type');
    if not event.trigger.condition.expression.Contains('function '+ROUTINE_NAMES[event.trigger.type_]+'(') then
      raise Exception.Create('Unable to find function '+ROUTINE_NAMES[event.trigger.type_]);
  end;
end;

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

function matches(actual, specified : TFhirTriggerTypeEnum) : boolean; overload;
begin
  result := (actual = specified) or ((actual in [TriggerTypeDataAdded, TriggerTypeDataModified, TriggerTypeDataRemoved]) and (specified = TriggerTypeDataChanged));
end;

function matches(actual : String; specified : TStringList) : boolean; overload;
begin
  if actual = '' then
    result := specified.Count = 0
  else
    result := (specified.Count = 0) or (specified.IndexOf(actual) > -1);
end;

procedure TJsEventScriptRegistry.getApplicableScripts(event: TFhirTriggerTypeEnum; resource: String; list: TAdvList<TJsEventScript>);
var
  e : TJsEventScript;
begin
  FLock.Lock;
  try
    for e in FScripts.Values do
    begin
      if matches(event, e.FCommand) and matches(resource, e.FResources) then
        list.Add(e.link);
    end;
  finally
    FLock.Unlock;
  end;
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

function TJsEventScriptRegistry.link: TJsEventScriptRegistry;
begin
  result := TJsEventScriptRegistry(inherited Link);
end;

//procedure TJsEventScriptRegistry.registerScript(script: TJsEventScript);
//begin
//  FLock.Lock;
//  try
//    if FScripts.ContainsKey(script.id) then
//      raise Exception.Create('Duplicate Script: '+script.id);
//    FScripts.Add(script.id, script.link);
//  finally
//    FLock.Unlock;
//  end;
//end;

procedure TJsEventScriptRegistry.seeResource(event: TFhirEventDefinition);
var
  tag : boolean;
  c : TFHIRCoding;
  ev : TJsEventScript;
begin
  tag := false;
  for c in event.meta.tagList do
    tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

  if tag then
  begin
    if event.trigger.condition.language <> 'application/javascript' then
      raise Exception.Create('Unknown error message');
    if not (event.trigger.type_ in SUPPORTED_TRIGGER_TYPES) then
      raise Exception.Create('Unsupported Trigger type');
    if not event.trigger.condition.expression.Contains('function '+ROUTINE_NAMES[event.trigger.type_]+'(') then
      raise Exception.Create('Unable to find function '+ROUTINE_NAMES[event.trigger.type_]);
  end;

  FLock.Lock;
  try
    if tag then
    begin

      ev := TJsEventScript.Create;
      try
        ev.FId := event.id;
        ev.FScript := event.trigger.condition.expression;
        ev.FCommand := event.trigger.type_;
        if (event.trigger.data <> nil) and (event.trigger.data.type_ <> AllTypesNull) then
          ev.FResources.Add(CODES_TFhirAllTypesEnum[event.trigger.data.type_]);
        FScripts.AddOrSetValue(event.id, ev.link);
      finally
        ev.Free;
      end;
    end
    else if FScripts.ContainsKey(event.id) then
      FScripts.Remove(event.id);
  finally
    FLock.Unlock;
  end;
end;

//procedure TJsEventScriptRegistry.unregisterScript(id: String);
//begin
//  FLock.Lock;
//  try
//    if not FScripts.ContainsKey(id) then
//      raise Exception.Create('unknown Script: '+id);
//    FScripts.Remove(id);
//  finally
//    FLock.Unlock;
//  end;
//end;

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
