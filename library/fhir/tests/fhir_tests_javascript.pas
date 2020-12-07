unit fhir_tests_javascript;
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

{$I fhir.inc}

interface


uses
  SysUtils, Classes, Generics.Collections,
  fsl_testing,
  fsl_base, fhir_objects, fsl_javascript, fhir_factory, fhir_common,
  fhir_javascript;

Type
  TJavascriptTests = Class (TFslTestCase)
  Private
    FLog : TStringList;
    FRaise : boolean;
    FJs : TJavascript;

    procedure JSLog(sender : TJavascript; message : String);
    procedure defineTestTypes;
    function AddOne(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function MakePropObjFromData(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
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
    procedure TearDown; override;
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

function TJavascriptTests.MakePropObjFromData(js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean) : TObject;
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

function array1ObjectProvider(js : TJavascript; context : pointer; index : integer) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := TArrayObj(context);
  result := js.wrap(obj.value[index]);
end;

function array1Maker(js : TJavascript; context : pointer; index : integer) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := TArrayObj(context);
  result := js.wrap(obj.value[index]);
end;

Function TJavascriptTests.PropArray1GetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  result := js.makeArray(obj.value.Count, array1Maker, obj);
end;

function TJavascriptTests.PropArray2GetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj2;
begin
  obj := this as TArrayObj2;
  result := js.makeManagedArray(TStringListManager.create(obj.FValue));
end;

procedure Array1Consumer(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  obj : TArrayObj;
begin
  obj := TArrayObj(context);
  obj.value.Add(js.asString(v));
end;

procedure TJavascriptTests.PropArray1SetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  obj.value.Clear;
  js.iterateArray(value, Array1Consumer, obj);
end;

procedure Array2Consumer(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  obj : TArrayObj2;
begin
  obj := TArrayObj2(context);
  obj.value.Add(js.asString(v));
end;

procedure TJavascriptTests.PropArray2SetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TArrayObj2;
begin
  obj := this as TArrayObj2;
  obj.value.Clear;
  js.iterateArray(value, Array2Consumer, obj);
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

type
  TarrayCConsumerContext = record
    this : TJavascriptTests;
    obj : TComplexArrayObj;
    def : TJavascriptClassDefinition;
  end;
  ParrayCConsumerContext = ^TarrayCConsumerContext;

procedure arrayCConsumer(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  ctxt : ParrayCConsumerContext;
  params : TJsValues;
  o : TPropObj;
  owns : boolean;
begin
  ctxt := context;
  o := js.getWrapped<TPropObj>(v);
  if (o = nil) then
  begin
    setLength(params, 1);
    params[0] := v;
    o := ctxt.this.MakePropObjFromData(js, ctxt.def, params, owns) as TPropObj;
  end;
  ctxt.obj.value.Add(o);
  js.unOwn(v);
end;

procedure TJavascriptTests.PropComplexArraySetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  ctxt : TarrayCConsumerContext;
begin
  ctxt.this := self;
  ctxt.obj := this as TComplexArrayObj;
  ctxt.obj.value.Clear;
  ctxt.def := js.getDefinedClass('TPropObj');
  js.iterateArray(value, arrayCConsumer, @ctxt);
end;


{ TJavascriptTests }

procedure TJavascriptTests.Setup;
begin
  FRaise := false;
  FLog := TStringList.create;
  FJs := TJavascript.acquire();
  FJs.OnLog := JSLog;
  defineTestTypes;
end;

procedure TJavascriptTests.TearDown;
begin
  FJs.yield;
  FLog.Free;
end;

procedure TJavascriptTests.defineTestTypes;
var
  def : TJavascriptClassDefinition;
begin
  if not FJs.namespaceDefined('tests') then
  begin
    def := FJs.defineClass('TIntObj', nil);
    def.defineRoutine('addOne', nil, AddOne);
    def := FJs.defineClass('TPropObj', nil, 'MyObject', MakePropObjFromData);
    def.defineProperty('value', nil, PropObjGetValue, PropObjSetValue);
    def := FJs.defineClass('TArrayObj', nil);
    def.defineProperty('value', nil, PropArray1GetValue, PropArray1SetValue);
    def := FJs.defineClass('TArrayObj2', nil);
    def.defineProperty('value', nil, PropArray2GetValue, PropArray2SetValue);
    def := FJs.defineClass('TComplexArrayObj', nil);
    def.defineProperty('value', nil, PropComplexArrayGetValue, PropComplexArraySetValue);
    FJs.defineNamespace('tests');
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
  assertTrue(FJs.asString(FJs.execute('(()=>{return ''Hello world!'';})()', 'test.js')) = 'Hello world!');
end;

procedure TJavascriptTests.TestConsoleLog;
begin
  FLog.Clear;
  FJs.execute('console.log("Hello world");', 'test.js');
  assertTrue(FLog.Text = 'Hello world'+#13#10);
end;

procedure TJavascriptTests.execException;
begin
  FJs.execute('(()=>{throw "test exception";})()', 'test.js');
end;
procedure TJavascriptTests.TestException;
begin
  assertWillRaise(execException, EChakraCoreScript, 'test exception');
end;

procedure TJavascriptTests.execConsole;
begin
  FJs.execute('console.log("Hello world");', 'test.js');
end;

procedure TJavascriptTests.TestAppException;
begin
  FRaise := true;
  assertWillRaise(execConsole, EChakraCoreScript, 'Internal error');
end;

procedure propIterCounter(js : TJavascript; context : pointer; name : String; v : TJsValue);
begin
  inc(integer(context^));
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
    o := FJs.wrap(i, 'TIntObj', false);
    FJs.execute('function func1(o) {'+#13#10+' o.addOne();'+#13#10+' } ', 'test.js', 'func1', [o]);
    c := 0;
    FJs.iterateProperties(o, propIterCounter, @c);
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
    o := FJs.wrap(i, i.ClassName, false);
    FJs.execute('function funcX(o) {'+#13#10+' if (o.value == ''test'') o.value = ''test1'';'+#13#10+' } ', 'test.js', 'funcX', [o]);
    assertTrue(i.value = 'test1');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestType;
begin
  FJs.execute('function funcX() {'+#13#10+
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
    o := FJs.wrap(i, i.ClassName, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
    o := FJs.wrap(i, i.ClassName, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
    o := FJs.wrap(i, i.ClassName, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
    o := FJs.wrap(i, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
    o := FJs.wrap(i, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
    o := FJs.wrap(i, false);
    FJs.execute('function funcX(o) {'+#13#10+
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
