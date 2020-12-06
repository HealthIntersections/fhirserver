unit fhir_javascript;

{
  Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  fsl_base, fsl_utilities, fsl_javascript,
  fhir_objects, fhir_factory;

type
  TFHIRJavascriptDefinedElement = class (TFslObject)
  private
    FDefiningType : String;
    FName : String;
    FFHIRType : String;
    function FHIRTypeNV : String;
  protected
    function sizeInBytesV : cardinal; override;
  end;

  TFHIRJavascript = class;
  TRegisterFHIRTypes = procedure (js : TFHIRJavascript);
  TFHIRJavascript = class (TFslJavascript)
  private
    FDefinedElements : TFslMap<TFHIRJavascriptDefinedElement>;
    FFactories : TFslMap<TFHIRFactory>;
    function createObj(name : string) : TFHIRObject;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function acquire : TFHIRJavascript; overload;

    procedure registerFactory(reg : TRegisterFHIRTypes; version : TFHIRVersion; fact : TFHIRFactory);
    function factory(v : TFHIRVersion) : TFHIRFactory;

    function wrap(o : TObject; owns : boolean; immutable : boolean = false) : JsValueRef; overload; override;

    procedure registerElement(classDef : TJavascriptClassDefinition; definingType, name, fhirType : String; getter : TJsGetterFunction; setter : TJsSetterProcedure);

    function FHIRFactoryJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean) : TObject;
    function getFHIRStringProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRStringProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRBooleanProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRBooleanProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRDateTimeProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRDateTimeProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRBinaryProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRBinaryProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRIntegerProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRIntegerProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRDecimalProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRDecimalProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRObjectProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRObjectProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
    function getFHIRArrayProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
    procedure setFHIRArrayProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
  end;

  TFHIRListManagerResolver = class (TFslListManagerResolver)
  private
    FJs: TJavascript;
  public
    constructor Create(js :TJavascript);
    function resolve(source : TObject) : TJavascriptClassDefinition; override;
    function category(source : TObject) : TFslJavascriptTypeCategory; override;
    function primitive(source : TObject) : String; override;
  end;


implementation

uses
  fhir_js_client{,
  FHIR.Server.EventJs};

{ TFHIRJavascript }

class function TFHIRJavascript.acquire: TFHIRJavascript;
begin
  result := TFHIRJavascript(inherited acquire(TFHIRJavascript));
end;

constructor TFHIRJavascript.Create;
begin
  inherited create;
  FDefinedElements := TFslMap<TFHIRJavascriptDefinedElement>.create('Defined Elements');
  FFactories := TFslMap<TFHIRFactory>.create('Javascript factories');
//  TFHIRClientJSHelper.registerFHIRClient(self, worker);
//  TFHIRServerJsHelper.registerFHIRServerEvent(self);
end;

destructor TFHIRJavascript.Destroy;
begin
  FDefinedElements.free;
  FFactories.Free;
  inherited;
end;

const
  VER_DIGIT : Array [TFHIRVersion] of String = ('', '1', '2', '3', '4', '5');

function TFHIRJavascript.createObj(name: string): TFHIRObject;
var
  v : TFHIRVersion;
begin
  for v in SUPPORTED_VERSIONS do
    if name.EndsWith(VER_DIGIT[v]) then
      exit(factory(v).makeByName(name.Substring(0, name.Length-1)));
  result := factory(fhirVersionUnknown).makeByName(name);
end;

function TFHIRJavascript.factory(v: TFHIRVersion): TFHIRFactory;
begin
  result := FFactories[CODES_TFHIRVersion[v]];
  if result = nil then
    raise EJavascriptApplication.Create('No factory registered for FHIR '+CODES_TFHIRVersion[v]);
end;

procedure TFHIRJavascript.registerElement(classDef : TJavascriptClassDefinition; definingType, name, fhirType : String; getter: TJsGetterFunction; setter: TJsSetterProcedure);
var
  def : TFHIRJavascriptDefinedElement;
begin
  if not FDefinedElements.TryGetValue(definingType+'.'+name, def) then
  begin
    def :=  TFHIRJavascriptDefinedElement.create;
    def.FDefiningType := definingType;
    def.FName := name;
    def.FFHIRType := fhirType;
    FDefinedElements.Add(definingType+'.'+name, def);
  end;
  classDef.defineProperty(name, def, getter, setter);
end;

procedure TFHIRJavascript.registerFactory(reg: TRegisterFHIRTypes; version : TFHIRVersion; fact: TFHIRFactory);
begin
  reg(Self);
  FFactories.AddOrSetValue(CODES_TFHIRVersion[version], fact);
end;

function TFHIRJavascript.FHIRFactoryJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean) : TObject;
var
  obj : TFHIRObject;
  prop : TJavascriptRegisteredProperty;
  v : TJsValue;
begin
  obj := TFHIRJavascript(js).createObj(classDef.Name);
  try
    if length(params) > 0 then
    begin
      if js.getType(params[0]) = JsObject then
      begin
        for prop in classdef.Properties do
        begin
          v := js.getProperty(params[0], prop.name);
          if not (js.getType(v) in [JsUndefined, jsNull]) then
            prop.setter(js, prop, obj, v);
        end;
      end;
    end;
    result := obj.link;
    owns := true;
  finally
    obj.Free;
  end;
end;

function TFHIRJavascript.getFHIRStringProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else
      result := js.wrap(p.Values[0].primitiveValue)
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRStringProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
  res : TFHIRObject;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).CreateObj(def.FFHIRType);
      try
        res.setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

function TFHIRJavascript.wrap(o: TObject; owns: boolean; immutable : boolean = false): JsValueRef;
begin
  if o is TFHIRObject then
    result := wrap(o, (o as TFHIRObject).JSType, owns, immutable)
  else
    result := inherited wrap(o, owns, immutable);
end;

function TFHIRJavascript.getFHIRArrayProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.jsInstance = js.instanceId) then
      result := p.Values.jsHandle
    else
    begin
      result := js.makeManagedArray(TFslObjectListManager.Create(p.Values.Link, js.getDefinedClass(def.FFHIRType)));
      p.Values.jsHandle := result;
      p.Values.jsInstance := js.InstanceId;
    end;
  finally
    p.Free;
  end;
end;

type
  TCallBackRecord2 = record
    js : TJavascript;
    cdef : TJavascriptClassDefinition;
    params : TJsValues;
    owns : boolean;
    p : TFHIRProperty;
  end;
  PCallBackRecord2 = ^TCallBackRecord2;

procedure iterArray2(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  p : PCallBackRecord2;
  o : TFHIRObject;
begin
  p := context;
  o := p.js.getWrapped<TFHIRObject>(v).Link;
  try
    if (o = nil) then
    begin
      p.params[0] := v;
      o := p.cdef.factory(p.js, p.cdef, p.params, p.owns) as TFHIRObject;
    end;
    p.p.values.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRArrayProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  cb1 : TCallBackRecord2;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  cb1.cdef := js.getDefinedClass(def.FFHIRType);
  cb1.p := obj.getPropertyValue(propDef.Name);
  if (cb1.p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    cb1.p.Values.Clear;
    cb1.p.Values.jsInstance := 0;
    setLength(cb1.params, 1);
    js.iterateArray(value, iterArray2, @cb1);
  finally
    cb1.p.Free;
  end;
end;

function TFHIRJavascript.getFHIRObjectProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or ((p.Values[0].JSType <> def.FFHIRType) and (def.FHIRTypeNV <> 'Resource')) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values[0].jsInstance = js.instanceId) then
      result := p.Values[0].jsHandle
    else
    begin
      result := js.wrap(p.Values[0].Link, p.Values[0].JSType, true);
      p.Values[0].jsHandle := result;
      p.Values[0].jsInstance := js.InstanceId;
    end;
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRObjectProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EJavascriptHost.create('not implemented yet');
end;

function TFHIRJavascript.getFHIRBinaryProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a binary but it had mutiple values')
    else
      result := js.wrap(p.Values[0].primitiveValue)
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRBinaryProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
  res : TFHIRObject;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
    begin
      res := factory(obj.fhirObjectVersion).makeBase64Binary(js.asString(value));
      try
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

function TFHIRJavascript.getFHIRBooleanProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a boolean but it had mutiple values')
    else
      result := js.wrap(StrToBool(p.Values[0].primitiveValue))
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRBooleanProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
     if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
    begin
      obj.setProperty(def.FName, factory(obj.fhirObjectVersion).makeBoolean(js.asBoolean(value)));
    end
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

function TFHIRJavascript.getFHIRDateTimeProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a datetime but it had mutiple values')
    else
      result := js.wrap(p.Values[0].primitiveValue);
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRDateTimeProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
  res : TFHIRObject;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).factory(obj.fhirObjectVersion).makeByName(def.FHIRTypeNV);
      try
        res.setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end
    end
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

function TFHIRJavascript.getFHIRIntegerProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as an integer but it had mutiple values')
    else
      result := js.wrap(StrToInt(p.Values[0].primitiveValue))
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRIntegerProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
  res : TFHIRObject;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as an integer but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).createObj(def.FFHIRType);
      try
        res.setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

function TFHIRJavascript.getFHIRDecimalProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a decimal but it had mutiple values')
    else
      result := js.wrap(p.Values[0].primitiveValue)
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRDecimalProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
     if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].JSType <> def.FFHIRType) then
      obj.setProperty(def.FName, factory(obj.fhirObjectVersion).makeDecimal(js.asString(value)))
    else
      p.Values[0].setProperty('value', factory(obj.fhirObjectVersion).makeString(js.asString(value)));
  finally
    p.Free;
  end;
end;

{ TFHIRListManagerResolver }

function TFHIRListManagerResolver.category(source: TObject): TFslJavascriptTypeCategory;
begin
  if not (source is TFHIRObject) then
    result := jtcObject
  else if not TFHIRObject(source).isPrimitive then
    result := jtcObject
  else if TFHIRObject(source).fhirType = 'boolean' then
    result := jtcBoolean
  else if StringArrayExistsSensitive(['integer', 'positiveInt', 'unsignedInt'], TFHIRObject(source).fhirType) then
    result := jtcInteger
  else
    result := jtcString;
end;

constructor TFHIRListManagerResolver.Create(js: TJavascript);
begin
  inherited create;
  FJs := js;
end;

function TFHIRListManagerResolver.primitive(source: TObject): String;
begin
  result := TFHIRObject(source).primitiveValue;
end;

function TFHIRListManagerResolver.resolve(source: TObject): TJavascriptClassDefinition;
begin
  if not (source is TFHIRObject) then
    raise EJavascriptApplication.Create('Javascript: Incorrect class "'+source.className+'" - must be a TFHIRObject');
  result := FJs.getDefinedClass(TFHIRObject(source).fhirType);
  if result = nil then
    raise EJavascriptException.Create('Javascript: Undefived class "'+TFHIRObject(source).fhirType+'"');
end;

{ TFHIRJavascriptDefinedElement }

function TFHIRJavascriptDefinedElement.FHIRTypeNV: String;
begin
  result := FFHIRType.Substring(0, FFHIRType.Length-1);
end;

function TFHIRJavascriptDefinedElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDefiningType.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FFHIRType.length * sizeof(char)) + 12);
end;

end.

