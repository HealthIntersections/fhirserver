unit FHIRClientJs;

interface

uses
  SysUtils, Classes,
  Javascript,
  AdvObjects,
  FHIRResources, FHIRClient, FHIRUtilities;

type
  TFHIRClientJSHelper = class (TAdvObject)
  private
    function CreateFHIRClientJs(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; var owns: boolean): TObject;
    function FHIRClientAddressJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientCapabilitiesJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientCreateJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientDeleteJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientOperationJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientReadJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientSearchAllJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientSearchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientTransactionJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRClientUpdateJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
  public
    class procedure registerFHIRClient(js : TJavascript);
  end;


implementation

function TFHIRClientJSHelper.CreateFHIRClientJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject;
begin
  result := TFhirHTTPClient.Create(nil, js.asString(params[0]), js.asBoolean(params[1]));
end;

function TFHIRClientJSHelper.FHIRClientAddressJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirClient(this).address);
end;

function TFHIRClientJSHelper.FHIRClientCapabilitiesJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  if (length(parameters) > 0) then
    result := js.wrap(TFhirClient(this).conformance(js.asBoolean(parameters[0])), 'CapabilityStatement', true)
  else
    result := js.wrap(TFhirClient(this).conformance(false), 'CapabilityStatement', true);
end;

function TFHIRClientJSHelper.FHIRClientTransactionJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

function TFHIRClientJSHelper.FHIRClientCreateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

function TFHIRClientJSHelper.FHIRClientReadJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

function TFHIRClientJSHelper.FHIRClientUpdateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

function TFHIRClientJSHelper.FHIRClientDeleteJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  a : TFhirResourceType;
  id : String;
begin
  a := ResourceTypeByName(js.asString(parameters[0]));
  id := js.asString(parameters[1]);
  TFhirClient(this).deleteResource(a, id);
  result := JS_INVALID_REFERENCE;
end;

function TFHIRClientJSHelper.FHIRClientSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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


function TFHIRClientJSHelper.FHIRClientSearchAllJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

function TFHIRClientJSHelper.FHIRClientOperationJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
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

class procedure TFHIRClientJSHelper.registerFHIRClient(js : TJavascript);
var
  this : TFHIRClientJSHelper;
  def : TJavascriptClassDefinition;
begin
  this := TFHIRClientJSHelper.Create;
  js.ownObject(this);
  def := js.defineClass('FHIRClient', nil, 'FHIRClient', this.CreateFHIRClientJs);
  def.defineRoutine('address', nil, this.FHIRClientAddressJs);
  def.defineRoutine('capabilities', nil, this.FHIRClientCapabilitiesJs);
  def.defineRoutine('transaction', nil, this.FHIRClientTransactionJs);
  def.defineRoutine('create', nil, this.FHIRClientCreateJs);
  def.defineRoutine('read', nil, this.FHIRClientReadJs);
  def.defineRoutine('update', nil, this.FHIRClientUpdateJs);
  def.defineRoutine('delete', nil, this.FHIRClientDeleteJs);
  def.defineRoutine('search', nil, this.FHIRClientSearchJs);
  def.defineRoutine('searchAll', nil, this.FHIRClientSearchAllJs);
  def.defineRoutine('operation', nil, this.FHIRClientOperationJs);
end;

end.
