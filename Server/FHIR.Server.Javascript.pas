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
  FHIR.Base.Objects, FHIR.Base.Factory,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Server.Session, FHIR.Version.Client,
  FHIR.Javascript.Base;

{$IFDEF FHIR2}
Type
  TFhirTriggerTypeEnum = (TriggerTypeNull, TriggerTypeNamedEvent, TriggerTypePeriodic, TriggerTypeDataAdded, TriggerTypeDataModified, TriggerTypeDataRemoved, TriggerTypeDataAccessed, TriggerTypeDataAccessEnded);
{$ENDIF}

Const
  ROUTINE_NAMES : array[TFhirTriggerTypeEnum] of String = ('xx', 'xx', 'xx', {$IFDEF FHIR4}'dataChanged', {$ENDIF} 'dataAdded', 'dataModified', 'dataRemoved', 'dataAccessed', 'xx');
  SUPPORTED_TRIGGER_TYPES = [{$IFDEF FHIR4}TriggerTypeDataChanged, {$ENDIF}TriggerTypeDataAdded,  TriggerTypeDataModified, TriggerTypeDataRemoved, TriggerTypeDataAccessed];

Type
  TEventScriptLanguage = (langJavascript, langFHIRPath);

  TEventScript = class (TFslObject)
  private
    FId : String;
    FCommand : TFhirTriggerTypeEnum;
    FResources : TStringList;
    FLang : TEventScriptLanguage;
    FScript : string;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function link : TEventScript; overload;

    property id : String read FId write FId;
    property script : String read FScript write FScript;
    property Commands : TFhirTriggerTypeEnum read FCommand write FCommand;
    property Resources : TStringList read FResources;
  end;

  TEventScriptRegistry = class (TFslObject)
  private
    FLock : TFslLock;
    FScripts : TFslMap<TEventScript>;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TEventScriptRegistry; overload;

    procedure getScripts(list: TFslList<TEventScript>);
    procedure getApplicableScripts(event : TFhirTriggerTypeEnum; resource : String; list : TFslList<TEventScript>);

{$IFDEF FHIR4}
    procedure checkResource(event : TFhirEventDefinition);
    procedure seeResource(event : TFhirEventDefinition);
{$ENDIF}
  end;

  TJsGetFHIRResource = reference to function : TFHIRResource;
  TJsGetFHIRClient = reference to function : TFHIRClient;

  // we create one of these for evrey thread, but it will only actually create a javscript engine when it needs to.
  // then, we retain it as long as we can
  TJsHost = class (TFslObject)
  private
    FRegistry: TEventScriptRegistry;
    FEngine : TFHIRJavascript;
    procedure SetRegistry(const Value: TEventScriptRegistry);
    procedure checkHasEngine;
  public
    constructor Create(chakraPath : String; factory : TFHIRFactory);
    destructor Destroy; override;

    property registry : TEventScriptRegistry read FRegistry write SetRegistry;

    procedure previewRequest(session : TFHIRSession; request : TFHIRRequest);
    procedure checkChanges(event: TFhirTriggerTypeEnum; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResource);
  end;

threadvar
  GJsHost : TJsHost;

implementation

{ TJsHost }

procedure TJsHost.checkChanges(event: TFhirTriggerTypeEnum; session : TFHIRSession; client : TJsGetFHIRClient; before : TJsGetFHIRResource; after : TFHIRResource);
var
  scripts : TFslList<TEventScript>;
  script : TEventScript;
  rn : String;
  s, b, a, c : TJsValue;
begin
  scripts := TFslList<TEventScript>.create;
  try
    if before <> nil then
      rn := before.fhirType
    else
      rn := after.fhirType;

    FRegistry.getApplicableScripts(event, rn, scripts);
    if (scripts.count > 0) then
    begin
      checkHasEngine;
      FEngine.readOnly := true;
      s := FEngine.wrap(session.Link, 'Session', true);
      b := FEngine.wrap(before.Link, rn, true);
      a := FEngine.wrap(after.Link, rn, true);
      c := FEngine.wrap(client.link, 'FHIR.Version.Client', true);
      FEngine.addGlobal('fhir', c);
      for script in scripts do
        FEngine.execute(script.FScript, 'event-'+script.id, ROUTINE_NAMES[script.FCommand], [s, b, a]);
    end;
  finally
    scripts.Free;
  end;
end;

procedure TJsHost.checkHasEngine;
begin
  if FEngine = nil then
    raise EJavascriptApplication.Create('Javascript is not supported on this server');
end;

constructor TJsHost.Create(chakraPath : String; factory : TFHIRFactory);
begin
  inherited create;
  if (chakraPath <> '') then
    FEngine := TFHIRJavascript.Create(chakraPath, factory.link);
end;

destructor TJsHost.Destroy;
begin
  FEngine.Free;
  FRegistry.Free;
  inherited;
end;

procedure TJsHost.previewRequest(session : TFHIRSession; request: TFHIRRequest);
var
  scripts : TFslList<TEventScript>;
  script : TEventScript;
  rn : String;
  s, r : TJsValue;
begin
  scripts := TFslList<TEventScript>.create;
  try
    FRegistry.getApplicableScripts(TriggerTypeDataAccessed, '', scripts);
    if (scripts.count > 0) then
    begin
      checkHasEngine;
      s := FEngine.wrap(session.Link, 'Session', true);
      r := FEngine.wrap(request.Link, 'Request', true);
      for script in scripts do
        FEngine.execute(script.FScript, 'event-'+script.id, ROUTINE_NAMES[script.FCommand], [s, r]);
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
  inherited;
  FLock := TFslLock.create;
  FScripts := TFslMap<TEventScript>.create;
end;

destructor TEventScriptRegistry.Destroy;
begin
  FLock.Free;
  FScripts.Free;
  inherited;
end;

function matches(actual, specified : TFhirTriggerTypeEnum) : boolean; overload;
begin
  result := (actual = specified) {$IFDEF FHIR4}or ((actual in [TriggerTypeDataAdded, TriggerTypeDataModified, TriggerTypeDataRemoved]) and (specified = TriggerTypeDataChanged)) {$ENDIF};
end;

function matches(actual : String; specified : TStringList) : boolean; overload;
begin
  if actual = '' then
    result := specified.Count = 0
  else
    result := (specified.Count = 0) or (specified.IndexOf(actual) > -1);
end;

procedure TEventScriptRegistry.getApplicableScripts(event: TFhirTriggerTypeEnum; resource: String; list: TFslList<TEventScript>);
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

{$IFDEF FHIR4}
procedure TEventScriptRegistry.checkResource(event: TFhirEventDefinition);
var
  tag : boolean;
  c : TFHIRCoding;
begin
  tag := false;
  for c in event.meta.tagList do
    tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

  if tag then
  begin
    if not StringArrayExistsSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.trigger.condition.language) then
      raise EJavascriptSource.create('Unknown script language');
    if not (event.trigger.type_ in SUPPORTED_TRIGGER_TYPES) then
      raise EJavascriptSource.create('Unsupported Trigger type');
    if not event.trigger.condition.expression.Contains('function '+ROUTINE_NAMES[event.trigger.type_]+'(') then
      raise EJavascriptSource.create('Unable to find function '+ROUTINE_NAMES[event.trigger.type_]);
  end;
end;

procedure TEventScriptRegistry.seeResource(event: TFhirEventDefinition);
var
  tag : boolean;
  c : TFHIRCoding;
  ev : TEventScript;
begin
  tag := false;
  for c in event.meta.tagList do
    tag := tag or ((c.system = 'http://www.healthintersections.com.au') and (c.code = 'active'));

  if tag then
  begin
    if not StringArrayExistsSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.trigger.condition.language) then
      raise EJavascriptSource.create('Unknown script language');
    if not (event.trigger.type_ in SUPPORTED_TRIGGER_TYPES) then
      raise EJavascriptSource.create('Unsupported Trigger type');
    if not event.trigger.condition.expression.Contains('function '+ROUTINE_NAMES[event.trigger.type_]+'(') then
      raise EJavascriptSource.create('Unable to find function '+ROUTINE_NAMES[event.trigger.type_]);
  end;

  FLock.Lock;
  try
    if tag then
    begin
      ev := TEventScript.Create;
      try
        ev.FId := event.id;
        ev.FScript := event.trigger.condition.expression;
        ev.FCommand := event.trigger.type_;
        case StringArrayIndexOfSensitive(['application/javascript', 'text/FHIR.Version.PathEngine'{, 'text/cql'}], event.trigger.condition.language)  of
          0: ev.FLang := langJavascript;
          1: ev.FLang := langFHIRPath;
//          2: ev.FLang := langCQL;
        end;
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
{$ENDIF}


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
