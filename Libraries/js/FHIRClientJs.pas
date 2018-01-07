unit FHIRClientJs;

interface

uses
  SysUtils, Classes,
  Javascript,
  FHIRResources, FHIRClient, FHIRUtilities;

procedure registerFHIRClient(js : TJavascript);

implementation

function CreateFHIRClientJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject;
begin
  result := TFhirHTTPClient.Create(nil, js.asString(params[0]), js.asBoolean(params[1]));
end;

function FHIRClientAddressJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirClient(this).address);
end;

function FHIRClientCapabilitiesJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  if (length(parameters) > 0) then
    result := js.wrap(TFhirClient(this).conformance(js.asBoolean(parameters[0])), 'CapabilityStatement', true)
  else
    result := js.wrap(TFhirClient(this).conformance(false), 'CapabilityStatement', true);
end;

function FHIRClientTransactionJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  bnd : TFhirBundle;
  def : TJavascriptClassDefinition;
  owns : boolean;
begin
  bnd := js.getWrapped<TFhirBundle>(parameters[0]).link;
  try
    if bnd = nil then
    begin
      def := js.getDefinedClass('Bundle');
      bnd := def.factory(js, def, parameters, owns) as TFhirBundle;
    end;
    result := js.wrap(TFhirClient(this).transaction(bnd), 'Bundle', true);
  finally
    bnd.Free;
  end;
end;

function FHIRClientCreateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResource;
  def : TJavascriptClassDefinition;
  owns : boolean;
  id : String;
begin
  res := js.getWrapped<TFhirResource>(parameters[0]).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirBundle;
    end;
    o := TFhirClient(this).createResource(res, id);
    try
      result := js.wrap(o.Link, o.fhirType, true);
    finally
      o.Free;
    end;
  finally
    res.Free;
  end;
end;

function FHIRClientReadJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  a : TFhirResourceType;
  id : String;
  res : TFhirResource;
begin
  a := ResourceTypeByName(js.asString(parameters[0]));
  id := js.asString(parameters[1]);
  res := TFhirClient(this).readResource(a, id);
  result := js.wrap(res, res.fhirType, true);
end;

function FHIRClientUpdateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResource;
  def : TJavascriptClassDefinition;
  owns : boolean;
begin
  res := js.getWrapped<TFhirResource>(parameters[0]).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirBundle;
    end;
    o := TFhirClient(this).updateResource(res);
    try
      result := js.wrap(o.Link, o.fhirType, true);
    finally
      o.Free;
    end;
  finally
    res.Free;
  end;
end;

function FHIRClientDeleteJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  a : TFhirResourceType;
  id : String;
begin
  a := ResourceTypeByName(js.asString(parameters[0]));
  id := js.asString(parameters[1]);
  TFhirClient(this).deleteResource(a, id);
  result := JS_INVALID_REFERENCE;
end;

function FHIRClientSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  ts : TStringList;
  a : TFhirResourceType;
  s : String;
  bnd : TFHIRBundle;
begin
  a := ResourceTypeByName(js.asString(parameters[0]));
  ts := TStringList.create;
  try
    if js.getType(parameters[1]) = JsString then
      bnd := TFhirClient(this).search(a, true, js.asString(parameters[1]))
    else
    begin
      js.iterateProperties(parameters[1],
        procedure (name : String; value : TJsValue)
        begin
           if js.getType(value) = JsArray then
             js.iterateArray(value, procedure (i : integer; v : TJsValue)
               begin
                 ts.AddPair(name, js.asString(v));
               end)
           else
             ts.AddPair(name, js.asString(value));
        end);
      bnd := TFhirClient(this).search(a, true, js.asString(parameters[1]))
    end;
    result := js.wrap(bnd, 'Bundle', true);
  finally
    ts.free;
  end;
end;


function FHIRClientSearchAllJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  ts : TStringList;
  s : String;
  bnd : TFHIRBundle;
begin
  ts := TStringList.create;
  try
    if js.getType(parameters[0]) = JsString then
      bnd := TFhirClient(this).search(true, js.asString(parameters[1]))
    else
    begin
      js.iterateProperties(parameters[0],
        procedure (name : String; value : TJsValue)
        begin
           if js.getType(value) = JsArray then
             js.iterateArray(value, procedure (i : integer; v : TJsValue)
               begin
                 ts.AddPair(name, js.asString(v));
               end)
           else
             ts.AddPair(name, js.asString(value));
        end);
      bnd := TFhirClient(this).search(true, js.asString(parameters[1]))
    end;
    result := js.wrap(TFhirClient(this).transaction(bnd), 'Bundle', true);
  finally
    ts.free;
  end;
end;

function FHIRClientOperationJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResource;
  def : TJavascriptClassDefinition;
  a : TFhirResourceType;
  n : String;
  owns : boolean;
begin
  a := ResourceTypeByName(js.asString(parameters[0]));
  n := js.asString(parameters[1]);
  res := js.getWrapped<TFhirResource>(parameters[0]).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirBundle;
    end;
    o := TFhirClient(this).operation(a, n, res as TFhirParameters);
    try
      result := js.wrap(o, o.fhirType, true);
    finally
      o.Free;
    end;
  finally
    res.Free;
  end;
end;

procedure registerFHIRClient(js : TJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FHIRClient', nil, 'FHIRClient', CreateFHIRClientJs);
  def.defineRoutine('address', nil, FHIRClientAddressJs);
  def.defineRoutine('capabilities', nil, FHIRClientCapabilitiesJs);
  def.defineRoutine('transaction', nil, FHIRClientTransactionJs);
  def.defineRoutine('create', nil, FHIRClientCreateJs);
  def.defineRoutine('read', nil, FHIRClientReadJs);
  def.defineRoutine('update', nil, FHIRClientUpdateJs);
  def.defineRoutine('delete', nil, FHIRClientDeleteJs);
  def.defineRoutine('search', nil, FHIRClientSearchJs);
  def.defineRoutine('searchAll', nil, FHIRClientSearchAllJs);
  def.defineRoutine('operation', nil, FHIRClientOperationJs);
//  def.defineRoutine('historyType', nil, FHIRClientHistoryTypeJs);
//  def.defineRoutine('historyResource', nil, FHIRClientHistoryResourceJs);
//  def.defineRoutine('historySystem', nil, FHIRClientHistorySystemJs);
end;

end.
