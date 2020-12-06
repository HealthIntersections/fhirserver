unit fhir_js_client;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_javascript,  
  fhir_objects, fhir_factory, fhir_client;

type
  TFHIRClientJSHelper = class (TFslObject)
  private
    FWorker : TFHIRWorkerContextWithFactory;
    function CreateFHIRClientJs(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
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
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(worker : TFHIRWorkerContextWithFactory);
    destructor Destroy; override;

    class procedure registerFHIRClient(js : TJavascript; worker : TFHIRWorkerContextWithFactory);
  end;


implementation

constructor TFHIRClientJSHelper.Create(worker: TFHIRWorkerContextWithFactory);
begin
  inherited Create;
  FWorker := worker;
end;

destructor TFHIRClientJSHelper.Destroy;
begin
  FWorker.Free;
  inherited;
end;

function TFHIRClientJSHelper.CreateFHIRClientJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean) : TObject;
begin
  owns := false;
  if js.asBoolean(params[1]) then
    result := FWorker.Factory.makeClient(FWorker.link, js.asString(params[0]), ffJson)
  else
    result := FWorker.Factory.makeClient(FWorker.link, js.asString(params[0]), ffXml);
end;

function TFHIRClientJSHelper.FHIRClientAddressJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirClientV(this).address);
end;

function TFHIRClientJSHelper.FHIRClientCapabilitiesJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  if (length(parameters) > 0) then
    result := js.wrap(TFhirClientV(this).conformanceV(js.asBoolean(parameters[0])), 'CapabilityStatement', true)
  else
    result := js.wrap(TFhirClientV(this).conformanceV(false), 'CapabilityStatement', true);
end;

function TFHIRClientJSHelper.FHIRClientTransactionJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  bnd : TFhirResourceV;
  def : TJavascriptClassDefinition;
  owns : boolean;
begin
  bnd := js.getWrappedObj(parameters[0]) as TFhirResourceV;
  try
    bnd.link;
    if bnd = nil then
    begin
      def := js.getDefinedClass('Bundle');
      bnd := def.factory(js, def, parameters, owns) as TFhirResourceV;
    end;
    result := js.wrap(TFhirClientV(this).transactionV(bnd), 'Bundle', true);
  finally
    bnd.Free;
  end;
end;

function TFHIRClientJSHelper.FHIRClientCreateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResourceV;
  def : TJavascriptClassDefinition;
  owns : boolean;
  id : String;
begin
  res := (js.getWrappedObj(parameters[0]) as TFhirResourceV).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirResourceV;
    end;
    o := TFhirClientV(this).createResourceV(res, id);
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
  id : String;
  res : TFhirResourceV;
begin
  id := js.asString(parameters[1]);
  res := TFhirClientV(this).readResourceV(js.asString(parameters[0]), id);
  result := js.wrap(res, res.fhirType, true);
end;

function TFHIRClientJSHelper.FHIRClientUpdateJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResourceV;
  def : TJavascriptClassDefinition;
  owns : boolean;
begin
  res := (js.getWrappedObj(parameters[0]) as TFhirResourceV).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirResourceV;
    end;
    o := TFhirClientV(this).updateResourceV(res);
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
  id : String;
begin
  id := js.asString(parameters[1]);
  TFhirClientV(this).deleteResourceV(js.asString(parameters[0]), id);
  result := JS_INVALID_REFERENCE;
end;

type
  TCallBackContext1 = record
    js : TJavascript;
    ts : TStringList;
    name : String;
  end;
  PCallBackContext1 = ^TCallBackContext1;

procedure arryIter1(js : TJavascript; context : pointer; i : integer; v : TJsValue);
var
  p : PCallBackContext1;
begin
  p := context;
  p.ts.Add(p.name+'='+p.js.asString(v));
end;

procedure propIter1(js : TJavascript; context : pointer; name : String; value : TJsValue);
var
  p : PCallBackContext1;
begin
  p := context;
  if p.js.getType(value) = JsArray then
  begin
    p.name := name;
    p.js.iterateArray(value, arryIter1, p);
  end
  else
    p.ts.Add(name+'='+p.js.asString(value));
end;

function TFHIRClientJSHelper.FHIRClientSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  ts : TStringList;
  bnd : TFhirResourceV;
  cb1 : TCallBackContext1;
begin
  ts := TStringList.create;
  try
    if js.getType(parameters[1]) = JsString then
      bnd := TFhirClientV(this).searchV(js.asString(parameters[0]), true, js.asString(parameters[1]))
    else
    begin
      cb1.js := js;
      cb1.ts := ts;
      js.iterateProperties(parameters[1], propIter1, @cb1);
      bnd := TFhirClientV(this).searchV(js.asString(parameters[0]), true, js.asString(parameters[1]))
    end;
    result := js.wrap(bnd, 'Bundle', true);
  finally
    ts.free;
  end;
end;


function TFHIRClientJSHelper.FHIRClientSearchAllJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  ts : TStringList;
  bnd : TFhirResourceV;
  cb1 : TCallBackContext1;
begin
  ts := TStringList.create;
  try
    if js.getType(parameters[0]) = JsString then
      bnd := TFhirClientV(this).searchV(true, js.asString(parameters[1]))
    else
    begin
      cb1.js := js;
      cb1.ts := ts;
      js.iterateProperties(parameters[0], propIter1, @cb1);
      bnd := TFhirClientV(this).searchV(true, js.asString(parameters[1]))
    end;
    result := js.wrap(TFhirClientV(this).transactionV(bnd), 'Bundle', true);
  finally
    ts.free;
  end;
end;

function TFHIRClientJSHelper.FHIRClientOperationJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  res, o : TFhirResourceV;
  def : TJavascriptClassDefinition;
  n : String;
  owns : boolean;
begin
  n := js.asString(parameters[1]);
  res := js.getWrapped<TFhirResourceV>(parameters[0]).link;
  try
    if res = nil then
    begin
      def := js.getDefinedClass('Resource');
      res := def.factory(js, def, parameters, owns) as TFhirResourceV;
    end;
    o := TFhirClientV(this).operationV(js.asString(parameters[0]), n, res);
    try
      result := js.wrap(o, o.fhirType, true);
    finally
      o.Free;
    end;
  finally
    res.Free;
  end;
end;

function TFHIRClientJSHelper.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FWorker.sizeInBytes);
end;

class procedure TFHIRClientJSHelper.registerFHIRClient(js : TJavascript; worker : TFHIRWorkerContextWithFactory);
var
  this : TFHIRClientJSHelper;
  def : TJavascriptClassDefinition;
begin
  this := TFHIRClientJSHelper.Create(worker.link);
  js.ownObject(this);
  def := js.defineClass('FHIR.Version.Client', nil, 'FHIR.Version.Client', this.CreateFHIRClientJs);
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
