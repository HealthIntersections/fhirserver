unit FHIR.Support.Javascript;

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

{
  Subclasses the Javascript library so it knows about FHIR.Support.Base library reference counting
}

interface

uses
  SysUtils, Classes, Generics.Collections, Soap.EncdDecd, System.NetEncoding,
  FHIR.Javascript,
  FHIR.Support.Base, FHIR.Support.Collections;

type
  TFslJavascript = class (TJavascript)
  protected
    procedure freeObject(obj : TObject); override;
  end;

  TFslObjectListManager = class (TJavascriptArrayManager)
  private
    FList : TFslObjectList;
    FClassDefinition : TJavascriptClassDefinition;
  public
    constructor Create(list : TFslObjectList; def : TJavascriptClassDefinition);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

  TFslListManager<T: TFslObject> = class (TJavascriptArrayManager)
  private
    FList : TFslList<T>;
    FClassDefinition : TJavascriptClassDefinition;
  public
    constructor Create(list : TFslList<T>; def : TJavascriptClassDefinition);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

function base64AsString(value: TBytes): String;
function stringAsBase64(value: String): TBytes;


implementation

function base64AsString(value: TBytes): String;
begin
  result := String(EncodeBase64(@value[0], length(value))).replace(#13#10, '');
end;

function stringAsBase64(value: String): TBytes;
begin
  result := DecodeBase64(AnsiString(value));
end;

{ TFslJavascript }

procedure TFslJavascript.freeObject(obj: TObject);
begin
  if obj is TFslObject then
    TFslObject(obj).Free
  else
    obj.Free;
end;

{ TFslListManager<T> }

constructor TFslListManager<T>.create(list: TFslList<T>; def : TJavascriptClassDefinition);
begin
  inherited Create;
  FList := list;
  FClassDefinition := def;
end;

destructor TFslListManager<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFslListManager<T>.count: integer;
begin
  result := FList.Count;
end;

function TFslListManager<T>.item(i: integer): JsValueRef;
begin
  result := FJavascript.wrap(FList[i].Link, FClassDefinition, true);
end;

function TFslListManager<T>.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  o : T;
  owns : boolean;
  pl : TJsValues;
  p : TJsValue;
begin
  setLength(pl, 1);
  for p in params do
  begin
    o := FJavascript.getWrapped<T>(p);
    if o = nil then
    begin
      pl[0] := p;
      o := FClassDefinition.Factory(FJavascript, FClassDefinition, pl, owns) as T;
    end;
    Flist.add(o.Link);
  end;
  result := FJavascript.wrap(FList.Count);
end;

{ TFslObjectListManager }

constructor TFslObjectListManager.create(list: TFslObjectList; def : TJavascriptClassDefinition);
begin
  inherited Create;
  FList := list;
  FClassDefinition := def;
end;

destructor TFslObjectListManager.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFslObjectListManager.count: integer;
begin
  result := FList.Count;
end;

function TFslObjectListManager.item(i: integer): JsValueRef;
begin
  result := FJavascript.wrap(FList[i].Link, FClassDefinition, true);
end;

function TFslObjectListManager.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  p : TJsValue;
  o : TFslObject;
  pl : TJsValues;
  owns : boolean;
begin
  setLength(pl, 1);
  for p in params do
  begin
    o := FJavascript.getWrapped<TFslObject>(p).Link;
    if o = nil then
    begin
      pl[0] := p;
      o := FClassDefinition.Factory(FJavascript, FClassDefinition, pl, owns) as TFslObject;
    end;
    try
      Flist.add(o.Link);
    finally
      o.Free;
    end;
  end;
  result := FJavascript.wrap(FList.Count);
end;

end.
