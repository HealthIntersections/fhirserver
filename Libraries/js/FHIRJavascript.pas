unit FHIRJavascript;

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

interface

uses
  SysUtils, Classes,
  StringSupport, DateSupport,
  AdvObjects, AdvGenerics,
  Javascript, AdvJavascript,
  FHIRBase, FHIRTypes, FHIRResources, FHIRClient;

type
  TFHIRJavascriptDefinedElement = class (TAdvObject)
  private
    FDefiningType : String;
    FName : String;
    FFHIRType : String;
  end;

  TFHIRJavascript = class (TAdvJavascript)
  private
    FDefinedElements : TAdvMap<TFHIRJavascriptDefinedElement>;
    FFactory : TFhirResourceFactory;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure registerElement(classDef : TJavascriptClassDefinition; definingType, name, fhirType : String; getter : TJsGetterFunction; setter : TJsSetterProcedure);

    function FHIRFactoryJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject;
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

implementation

uses
  FHIRJavascriptReg,
  FHIRClientJs,
  ServerEventJs;

{ TFHIRJavascript }

constructor TFHIRJavascript.Create;
begin
  inherited;
  FDefinedElements := TAdvMap<TFHIRJavascriptDefinedElement>.create;
  FFactory := TFhirResourceFactory.Create;
  registerFHIRTypes(self);
  TFHIRClientJSHelper.registerFHIRClient(self);
  TFHIRServerJsHelper.registerFHIRServerEvent(self);
end;

destructor TFHIRJavascript.Destroy;
begin
  FDefinedElements.free;
  FFactory.Free;
  inherited;
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


function TFHIRJavascript.FHIRFactoryJs(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject;
var
  obj : TFHIRObject;
  prop : TJavascriptRegisteredProperty;
  v : TJsValue;
begin
  obj := TFHIRJavascript(js).FFactory.makeByName(classDef.Name);
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
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
  res : TFHIRPrimitiveType;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).FFactory.makeByName(def.FFHIRType) as TFHIRPrimitiveType;
      try
        res.StringValue := js.asString(value);
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
  finally
    p.Free;
  end;
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
      result := js.makeManagedArray(TAdvObjectListManager.Create(p.Values.Link, js.getDefinedClass(def.FFHIRType)));
      p.Values.jsHandle := result;
      p.Values.jsInstance := js.InstanceId;
    end;
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRArrayProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  cdef : TJavascriptClassDefinition;
  p : TFHIRProperty;
  o : TFHIRObject;
  owns : boolean;
//  def : TJavascriptClassDefinition;
  params : TJsValues;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  cdef := js.getDefinedClass(def.FFHIRType);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    p.Values.Clear;
    p.Values.jsInstance := 0;
    setLength(params, 1);
    js.iterateArray(value,
      procedure (i : integer; v : JsValueRef)
      begin
        o := js.getWrapped<TFHIRObject>(v).Link;
        try
          if (o = nil) then
          begin
            params[0] := v;
            o := cdef.factory(js, cdef, params, owns) as TFHIRObject;
          end;
          p.values.Add(o.Link);
        finally
          o.Free;
        end;
      end);
  finally
    p.Free;
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
    if (p.Values.Count = 0) or ((p.Values[0].fhirType <> def.FFHIRType) and (def.FFHIRType <> 'Resource')) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values[0].jsInstance = js.instanceId) then
      result := p.Values[0].jsHandle
    else
    begin
      result := js.wrap(p.Values[0].Link, p.Values[0].fhirType, true);
      p.Values[0].jsHandle := result;
      p.Values[0].jsInstance := js.InstanceId;
    end;
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRObjectProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('not implemented yet');
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
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
  res : TFHIRPrimitiveType;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFhirBase64Binary.Create;
      try
        res.StringValue := js.asString(value);
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
      result := js.getNull
    else if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a boolean but it had mutiple values')
    else
      result := js.wrap((p.Values[0] as TFHIRBoolean).Value)
  finally
    p.Free;
  end;
end;

procedure TFHIRJavascript.setFHIRBooleanProp(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TFHIRObject;
  def : TFHIRJavascriptDefinedElement;
  p : TFHIRProperty;
  res : TFHIRBoolean;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
     if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFHIRBoolean.create(js.asBoolean(value));
      try
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
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
  res : TFHIRPrimitiveType;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).FFactory.makeByName(def.FFHIRType) as TFHIRPrimitiveType;
      try
        res.StringValue := js.asString(value);
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
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
  res : TFHIRPrimitiveType;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
    if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as an integer but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFHIRJavascript(propDef.Javascript).FFactory.makeByName(def.FFHIRType) as TFHIRPrimitiveType;
      try
        res.StringValue := js.asString(value);
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
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
    if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
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
  res : TFHIRPrimitiveType;
begin
  obj := this as TFHIRObject;
  def := TFHIRJavascriptDefinedElement(propDef.context);
  p := obj.getPropertyValue(propDef.Name);
  if (p = nil) then
    raise EJavascriptHost.Create('Attempt to access illegal property '+propDef.Name);
  try
     if p.Values.Count > 1 then
      raise EJavascriptHost.Create('Attempt to access '+propDef.Name+' as a string but it had mutiple values')
    else if (p.Values.Count = 0) or (p.Values[0].fhirType <> def.FFHIRType) then
    begin
      res := TFhirDecimal.create(js.asString(value));
      try
        obj.setProperty(def.FName, res.Link);
      finally
        res.Free;
      end;
    end
    else
      (p.Values[0] as TFHIRPrimitiveType).StringValue := js.asString(value);
  finally
    p.Free;
  end;
end;

end.

