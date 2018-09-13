unit FHIR.Server.Javascript;

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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

interface

uses
  SysUtils, Classes,
  FHIR.Support.Threads, FHIR.Javascript, FHIR.Support.Utilities, FHIR.Support.Base,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Client.Base, FHIR.Base.Common,
  FHIR.Javascript.Base,
  FHIR.Server.Session;

Const
  ROUTINE_NAMES : array[TTriggerType] of String = ('xx', 'xx', 'xx', 'dataChanged', 'dataAdded', 'dataModified', 'dataRemoved', 'dataAccessed', 'xx');
  SUPPORTED_TRIGGER_TYPES = [ttDataChanged, ttDataAdded,  ttDataModified, ttDataRemoved, ttDataAccessed];

Type
  TEventScriptLanguage = (langJavascript, langFHIRPath);

  TEventScript = class (TFslObject)
  private
    FId : String;
    FCommand : TTriggerType;
    FResources : TStringList;
    FLang : TEventScriptLanguage;
    FScript : string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TEventScript; overload;

    property id : String read FId write FId;
    property script : String read FScript write FScript;
    property Commands : TTriggerType read FCommand write FCommand;
    property Resources : TStringList read FResources;
  end;

  TEventScriptRegistry = class (TFslObject)
  private
    FLock : TFslLock;
    FFactory : TFHIRFactory;
    FScripts : TFslMap<TEventScript>;
    function matches(actual, specified : TTriggerType) : boolean; overload;
    function matches(actual : String; specified : TStringList) : boolean; overload;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;

    function link : TEventScriptRegistry; overload;

    procedure getScripts(list: TFslList<TEventScript>);
    procedure getApplicableScripts(event : TTriggerType; resource : String; list : TFslList<TEventScript>);

    procedure checkResource(event : TFhirEventDefinitionW);
    procedure seeResource(event : TFhirEventDefinitionW);
  end;

  TJsGetFHIRResource = reference to function : TFHIRResourceV;
  TJsGetFHIRClient = reference to function : TFHIRClientV;

  // we create one of these for evrey thread, but it will only actually create a javscript engine when it needs to.
  // then, we retain it as long as we can
  TJsHost = class (TFslObject)
  private
    FChakraPath : String;
    FRegistry: TEventScriptRegistry;
    FEngines : array[TFHIRVersion] of TFHIRJavascript;
    procedure SetRegistry(const Value: TEventScriptRegistry);
    function checkHasEngine(version : TFHIRVersion) : TFHIRJavascript;
  public
    constructor Create(chakraPath : String);
    destructor Destroy; override;

    procedure registerVersion(worker : TFHIRWorkerContextWithFactory; reg : TRegisterFHIRTypes);

    property registry : TEventScriptRegistry read FRegistry write SetRegistry;

    procedure previewRequest(session : TFHIRSession; request : TFHIRRequest);
    procedure checkChanges(event: TTriggerType; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResourceV);
  end;

  TRegisterJavascriptEvent = procedure (sender : TObject; js : TJsHost) of Object;

threadvar
  GJsHost : TJsHost;

implementation


{ TJsHost }

procedure TJsHost.checkChanges(event: TTriggerType; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResourceV);
var
  scripts : TFslList<TEventScript>;
  script : TEventScript;
  rn : String;
  s, b, a, c : TJsValue;
  engine : TFHIRJavascript;
begin
  if FRegistry = nil then
    exit;

  scripts := TFslList<TEventScript>.create;
  try
    if before <> nil then
      rn := before.fhirType
    else
      rn := after.fhirType;

    FRegistry.getApplicableScripts(event, rn, scripts);
    if (scripts.count > 0) then
    begin
      engine := checkHasEngine(before.fhirObjectVersion);
      engine.readOnly := true;
      s := engine.wrap(session.Link, 'Session', true);
      b := engine.wrap(before.Link, rn, true);
      a := engine.wrap(after.Link, rn, true);
      c := engine.wrap(client.link, 'FHIR.Version.Client', true);
      engine.addGlobal('fhir', c);
      for script in scripts do
        engine.execute(script.FScript, 'event-'+script.id, ROUTINE_NAMES[script.FCommand], [s, b, a]);
    end;
  finally
    scripts.Free;
  end;
end;

function TJsHost.checkHasEngine(version : TFHIRVersion) : TFHIRJavascript;
begin
  if FEngines[version] = nil then
    raise EJavascriptApplication.Create('Javascript is not supported on this server for version '+CODES_TFHIRVersion[version]);
  result := FEngines[version];
end;

constructor TJsHost.Create(chakraPath : String);
var
  v : TFHIRVersion;
begin
  inherited create;
  FChakraPath := chakraPath;
  for v in FHIR_ALL_VERSIONS do
    FEngines[v] := nil;
end;

destructor TJsHost.Destroy;
var
  v : TFHIRVersion;
begin
  for v in FHIR_ALL_VERSIONS do
    FEngines[v].Free;
  FRegistry.Free;
  inherited;
end;


procedure TJsHost.registerVersion(worker : TFHIRWorkerContextWithFactory; reg : TRegisterFHIRTypes);
begin
  if (FChakraPath <> '') then
    FEngines[worker.version] := TFHIRJavascript.Create(FChakraPath, worker.link, reg)
  else
    worker.free;
end;

procedure TJsHost.previewRequest(session : TFHIRSession; request: TFHIRRequest);
var
  scripts : TFslList<TEventScript>;
  script : TEventScript;
  s, r : TJsValue;
  engine : TFHIRJavascript;
begin
  scripts := TFslList<TEventScript>.create;
  try
    if FRegistry <> nil then
      FRegistry.getApplicableScripts(ttDataAccessed, '', scripts);
    if (scripts.count > 0) then
    begin
      engine := checkHasEngine(request.Version);
      s := engine.wrap(session.Link, 'Session', true);
      r := engine.wrap(request.Link, 'Request', true);
      for script in scripts do
        engine.execute(script.FScript, 'event-'+script.id, ROUTINE_NAMES[script.FCommand], [s, r]);
    end;
  finally
    scripts.Free;
  end;
end;

procedure TJsHost.SetRegistry(const Value: TEventScriptRegistry);
begin
  FRegistry.Free;
  FRegistry := Value;
end;

{ TEventScriptRegistry }

constructor TEventScriptRegistry.Create;
begin
  inherited Create;
  FLock := TFslLock.create;
  FScripts := TFslMap<TEventScript>.create;
  FFactory := factory;
end;

destructor TEventScriptRegistry.Destroy;
begin
  FFactory.Free;
  FLock.Free;
  FScripts.Free;
  inherited;
end;

function TEventScriptRegistry.matches(actual, specified : TTriggerType) : boolean;
begin
  result := (actual = specified) or ((FFactory.version = fhirVersionRelease4) and (actual in [ttDataAdded, ttDataModified, ttDataRemoved]) and (specified = ttDataChanged));
end;

function TEventScriptRegistry.matches(actual : String; specified : TStringList) : boolean;
begin
  if actual = '' then
    result := specified.Count = 0
  else
    result := (specified.Count = 0) or (specified.IndexOf(actual) > -1);
end;

procedure TEventScriptRegistry.getApplicableScripts(event: TTriggerType; resource: String; list: TFslList<TEventScript>);
var
  e : TEventScript;
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

procedure TEventScriptRegistry.getScripts(list : TFslList<TEventScript>);
var
  e : TEventScript;
begin
  FLock.Lock;
  try
    for e in FScripts.Values do
      list.Add(e.Link);
  finally
    FLock.Unlock;
  end;
end;

function TEventScriptRegistry.link: TEventScriptRegistry;
begin
  result := TEventScriptRegistry(inherited Link);
end;

procedure TEventScriptRegistry.checkResource(event: TFhirEventDefinitionW);
var
  tag : boolean;
  c : TFHIRCodingW;
  m : TFhirMetaW;
begin
  m := FFactory.wrapMeta(event.Resource);
  try
    tag := false;
    for c in m.tags.forEnum do
      tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

    if tag then
    begin
      if not StringArrayExistsSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.language) then
        raise EJavascriptSource.create('Unknown script language');
      if not (event.triggerType in SUPPORTED_TRIGGER_TYPES) then
        raise EJavascriptSource.create('Unsupported Trigger type');
      if not event.expression.Contains('function '+ROUTINE_NAMES[event.triggerType]+'(') then
        raise EJavascriptSource.create('Unable to find function '+ROUTINE_NAMES[event.triggerType]);
    end;
  finally
    m.free;
  end;
end;

procedure TEventScriptRegistry.seeResource(event: TFhirEventDefinitionW);
var
  tag : boolean;
  c : TFHIRCodingW;
  ev : TEventScript;
  m : TFhirMetaW;
begin
  try
    m := FFactory.wrapMeta(event.Resource);
    try
      tag := false;
      for c in m.tags.forEnum do
        tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

      if tag then
      begin
        if not StringArrayExistsSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.language) then
          raise EJavascriptSource.create('Unknown script language');
        if not (event.triggerType in SUPPORTED_TRIGGER_TYPES) then
          raise EJavascriptSource.create('Unsupported Trigger type');
        if not event.expression.Contains('function '+ROUTINE_NAMES[event.triggerType]+'(') then
          raise EJavascriptSource.create('Unable to find function '+ROUTINE_NAMES[event.triggerType]);
      end;

      FLock.Lock;
      try
        if tag then
        begin
          ev := TEventScript.Create;
          try
            ev.FId := event.id;
            ev.FScript := event.expression;
            ev.FCommand := event.triggerType;
            case StringArrayIndexOfSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.language)  of
              0: ev.FLang := langJavascript;
              1: ev.FLang := langFHIRPath;
    //          2: ev.FLang := langCQL;
            end;
            if (event.dataType <> '') then
              ev.FResources.Add(event.dataType);
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
    finally
      m.free;
    end;
  finally
    event.free;
  end;
end;


{ TJsEventScript }

constructor TEventScript.Create;
begin
  inherited;
  FResources := TStringList.Create;
end;

destructor TEventScript.Destroy;
begin
  FResources.Free;
  inherited;
end;

function TEventScript.link: TEventScript;
begin
  result := TEventScript(inherited Link);
end;

end.
