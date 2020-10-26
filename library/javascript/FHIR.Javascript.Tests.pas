unit FHIR.Javascript.Tests;
{
These tests are based on the overview found
at https://github.com/Microsoft/ChakraCore/wiki/JavaScript-Runtime-%28JSRT%29-Overview

Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  SysUtils, Classes, Generics.Collections,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} TestFramework, {$ENDIF} FHIR.Support.Testing,
  FHIR.Support.Base, FHIR.Base.Objects, FHIR.Javascript;

Type
  TJavascriptTests = Class (TFslTestCase)
  Private
    FLog : TStringList;
    FRaise : boolean;
    js : TJavascript;

    procedure JSLog(sender : TJavascript; message : String);
    procedure defineTestTypes(js: TJavascript);
    function AddOne(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function MakePropObjFromData(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; var owns: boolean): TObject;
    function PropArray1GetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function PropArray2GetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropArray1SetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure PropArray2SetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function PropComplexArrayGetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropComplexArraySetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function PropObjGetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropObjSetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure execConsole;
    procedure execException;
  public
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    Procedure TestHelloWorld;
    Procedure TestConsoleLog;
    Procedure TestException;
    Procedure TestAppException;
    Procedure TestAddOne;
    Procedure TestProperty;
    Procedure TestType;
    Procedure TestArray;
    Procedure TestArrayManaged;
    Procedure TestArrayPush;
    Procedure TestArrayComplex;
    Procedure TestArrayComplexAnonymous;
    Procedure TestArrayComplexPush;
  End;

procedure registerTests;

implementation

Type
  TIntObj = class
  private
    value : integer;
  public
    procedure addOne;
  end;

procedure TIntObj.addOne;
begin
  inc(value);
end;

function TJavascriptTests.AddOne(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  obj : TIntObj;
begin
  obj := this as TIntObj;
  obj.addOne;
  result := JS_INVALID_REFERENCE;
end;

type
  TPropObj = class
  private
    FValue : String;
  public
    constructor Create(v : String);
    destructor Destroy; override;
    property value : String read FValue write FValue;
  end;

{ TPropObj }

constructor TPropObj.create(v: String);
begin
  inherited Create;
  FValue := v;
end;

destructor TPropObj.destroy;
begin
  inherited;
end;

function TJavascriptTests.PropObjGetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TPropObj;
begin
  obj := this as TPropObj;
  result := js.wrap(obj.value);
end;

procedure TJavascriptTests.PropObjSetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TPropObj;
begin
  obj := this as TPropObj;
  obj.value := js.asString(value);
end;

function TJavascriptTests.MakePropObjFromData(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject;
var
  p : TJsValue;
begin
  p := params[0];
  if js.getType(p) = JsObject then
    result := TPropObj.create(js.asString(js.getProperty(p, 'value')))
  else
    result := TPropObj.create('test-'+js.asString(p));
  owns := true;
end;

type
  TArrayObj = class
  private
    FValue : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property value : TStringList read FValue;
  end;

type
  TArrayObj2 = class
  private
    FValue : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property value : TStringList read FValue;
  end;

{ TArrayObj }

constructor TArrayObj.Create;
begin
  inherited;
  FValue := TStringList.create;
end;

destructor TArrayObj.Destroy;
begin
  FValue.Free;
  inherited;
end;

{ TArrayObj2 }

constructor TArrayObj2.Create;
begin
  inherited;
  FValue := TStringList.create;
end;

destructor TArrayObj2.Destroy;
begin
  FValue.Free;
  inherited;
end;

Function TJavascriptTests.PropArray1GetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  result := js.makeArray(obj.value.Count,
      function (context : pointer; index : integer) : JsValueRef
      begin
        result := js.wrap(obj.value[index]);
      end);
end;

function TJavascriptTests.PropArray2GetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj2;
begin
  obj := this as TArrayObj2;
  result := js.makeManagedArray(TStringListManager.create(obj.FValue));
end;

procedure TJavascriptTests.PropArray1SetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  obj.value.Clear;
  js.iterateArray(value,
      procedure (context : pointer; i : integer; v : JsValueRef)
      begin
        obj.value.Add(js.asString(v));
      end, nil);
end;

procedure TJavascriptTests.PropArray2SetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TArrayObj2;
begin
  obj := this as TArrayObj2;
  obj.value.Clear;
  js.iterateArray(value,
      procedure (context : pointer; i : integer; v : JsValueRef)
      begin
        obj.value.Add(js.asString(v));
      end, nil);
end;


type
  TComplexArrayObj = class
  private
    FValue : TObjectList<TPropObj>;
  public
    constructor Create;
    destructor Destroy; override;
    property value : TObjectList<TPropObj> read FValue;
  end;

{ TComplexArrayObj }

constructor TComplexArrayObj.Create;
begin
  inherited;
  FValue := TObjectList<TPropObj>.create;
  // this is important - you won't be able to manage this when javascript starts assigned arrays around
  FValue.OwnsObjects := false;
end;

destructor TComplexArrayObj.Destroy;
var
  p : TPropObj;
begin
  for p in value do
    p.Free;
  FValue.Free;
  inherited;
end;

function TJavascriptTests.PropComplexArrayGetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TComplexArrayObj;
begin
  obj := this as TComplexArrayObj;
  result := js.makemanagedArray(TObjectListManager<TPropObj>.create(obj.FValue, js.getDefinedClass('TPropObj')));
end;

procedure TJavascriptTests.PropComplexArraySetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TComplexArrayObj;
  o : TPropObj;
  owns : boolean;
  def : TJavascriptClassDefinition;
  params : TJsValues;
begin
  obj := this as TComplexArrayObj;
  obj.value.Clear;
  def := js.getDefinedClass('TPropObj');
  js.iterateArray(value,
    procedure (context : pointer; i : integer; v : JsValueRef)
    begin
      o := js.getWrapped<TPropObj>(v);
      if (o = nil) then
      begin
        setLength(params, 1);
        params[0] := v;
        o := MakePropObjFromData(js, def, params, owns) as TPropObj;
      end;
      obj.value.Add(o);
      js.unOwn(v);
    end, nil);
end;


{ TJavascriptTests }

procedure TJavascriptTests.Setup;
begin
  FRaise := false;
  FLog := TStringList.create;
  js := TJavascript.acquire();
  js.OnLog := JSLog;
  defineTestTypes(js);
end;

procedure TJavascriptTests.TearDown;
begin
  js.yield;
  FLog.Free;
end;

procedure TJavascriptTests.defineTestTypes(js : TJavascript);
var
  def : TJavascriptClassDefinition;
begin
  if not js.namespaceDefined('tests') then
  begin
    def := js.defineClass('TIntObj', nil);
    def.defineRoutine('addOne', nil, AddOne);
    def := js.defineClass('TPropObj', nil, 'MyObject', MakePropObjFromData);
    def.defineProperty('value', nil, PropObjGetValue, PropObjSetValue);
    def := js.defineClass('TArrayObj', nil);
    def.defineProperty('value', nil, PropArray1GetValue, PropArray1SetValue);
    def := js.defineClass('TArrayObj2', nil);
    def.defineProperty('value', nil, PropArray2GetValue, PropArray2SetValue);
    def := js.defineClass('TComplexArrayObj', nil);
    def.defineProperty('value', nil, PropComplexArrayGetValue, PropComplexArraySetValue);
    js.defineNamespace('tests');
  end;
end;

procedure TJavascriptTests.JSLog(sender: TJavascript; message: String);
begin
  FLog.Add(message);
  if FRaise then
  begin
    FRaise := false;
    raise ETestCase.create('Internal error');
  end;
end;


procedure TJavascriptTests.TestHelloWorld;
begin
  assertTrue(js.asString(js.execute('(()=>{return ''Hello world!'';})()', 'test.js')) = 'Hello world!');
end;

procedure TJavascriptTests.TestConsoleLog;
begin
  FLog.Clear;
  js.OnLog := JSLog;
  js.execute('console.log("Hello world");', 'test.js');
  assertTrue(FLog.Text = 'Hello world'+#13#10);
end;

procedure TJavascriptTests.execException;
begin
  js.execute('(()=>{throw "test exception";})()', 'test.js');
end;
procedure TJavascriptTests.TestException;
begin
  assertWillRaise(execException, EChakraCoreScript, 'test exception');
end;

procedure TJavascriptTests.execConsole;
begin
  js.execute('console.log("Hello world");', 'test.js');
end;

procedure TJavascriptTests.TestAppException;
begin
  FRaise := true;
  assertWillRaise(execConsole, EChakraCoreScript, 'Internal error');
end;

procedure TJavascriptTests.TestAddOne;
var
  i : TIntObj;
  o : JsValueRef;
  c : integer;
begin
  i := TIntObj.Create;
  try
    i.value := 1;
    o := js.wrap(i, 'TIntObj', false);
    js.execute('function func1(o) {'+#13#10+' o.addOne();'+#13#10+' } ', 'test.js', 'func1', [o]);
    c := 0;
    js.iterateProperties(o, procedure (context : pointer; name : String; v : TJsValue)
      begin
        inc(c);
      end, nil);
    assertTrue(c > 0);
    assertTrue(i.value = 2);
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestProperty;
var
  i : TPropObj;
  o : JsValueRef;
begin
  i := TPropObj.Create('test');
  try
    o := js.wrap(i, i.ClassName, false);
    js.execute('function funcX(o) {'+#13#10+' if (o.value == ''test'') o.value = ''test1'';'+#13#10+' } ', 'test.js', 'funcX', [o]);
    assertTrue(i.value = 'test1');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestType;
begin
  js.execute('function funcX() {'+#13#10+
             ' var o = new MyObject(''text'');'+#13#10+
             ' console.log(o.value);'+#13#10+
             '} ', 'test.js', 'funcX', []);
  assertTrue(FLog.Text = 'test-text'+#13#10);
end;

procedure TJavascriptTests.TestArray;
var
  i : TArrayObj;
  o : JsValueRef;
begin
  i := TArrayObj.Create;
  try
    i.value.CommaText := 'test,test1';
    o := js.wrap(i, i.ClassName, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0]);'+#13#10+
               ' console.log(o.value[1]);'+#13#10+
               ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
               '  o.value = o.value.concat([''test2'']);'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayManaged;
var
  i : TArrayObj;
  o : JsValueRef;
begin
  i := TArrayObj.Create;
  try
    i.value.CommaText := 'test,test1';
    o := js.wrap(i, i.ClassName, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0]);'+#13#10+
               ' console.log(o.value[1]);'+#13#10+
               ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
               '  o.value = o.value.concat([''test2'']);'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayPush;
var
  i : TArrayObj2;
  o : JsValueRef;
begin
  i := TArrayObj2.Create;
  try
    i.value.CommaText := 'test,test1';
    o := js.wrap(i, i.ClassName, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0]);'+#13#10+
               ' console.log(o.value[1]);'+#13#10+
               ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
               '  o.value.push(''test2'');'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;


procedure TJavascriptTests.TestArrayComplex;
var
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    o := js.wrap(i, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0].value);'+#13#10+
               ' o.value = o.value.concat([new MyObject(''v2'')]);'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.Count = 2);
    assertTrue(i.value[0].value = 'v1');
    assertTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayComplexAnonymous;
var
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    o := js.wrap(i, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0].value);'+#13#10+
               ' o.value = o.value.concat([{ "value" : "test-v2"} ]);'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.Count = 2);
    assertTrue(i.value[0].value = 'v1');
    assertTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayComplexPush;
var
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    o := js.wrap(i, false);
    js.execute('function funcX(o) {'+#13#10+
               ' console.log(o.value.length);'+#13#10+
               ' console.log(o.value[0].value);'+#13#10+
               ' o.value.push({ "value" : "test-v2"} );'+#13#10+
               '} ', 'test.js', 'funcX', [o]);
    assertTrue(i.value.Count = 2);
    assertTrue(i.value[0].value = 'v1');
    assertTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('Javascript', TJavascriptTests.suite);
end;

end.
