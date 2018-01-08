unit JavascriptTests;
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
  Javascript,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TJavascriptTests = Class (TObject)
  Private
    FLog : TStringList;
    FRaise : boolean;
    procedure JSLog(sender : TJavascript; message : String);
    procedure defineTestTypes(js: TJavascript; manArrayValue: boolean);
    function AddOne(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function MakePropObjFromData(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; var owns: boolean): TObject;
    function PropArrayGetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function PropArrayGetValueManaged(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropArraySetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function PropComplexArrayGetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropComplexArraySetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function PropObjGetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure PropObjSetValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
  Published
    [SetUp]    Procedure Setup;
    [TearDown] Procedure TearDown;
    [TestCase] Procedure TestHelloWorld;
    [TestCase] Procedure TestConsoleLog;
    [TestCase] Procedure TestException;
    [TestCase] Procedure TestAppException;
    [TestCase] Procedure TestAddOne;
    [TestCase] Procedure TestProperty;
    [TestCase] Procedure TestType;
    [TestCase] Procedure TestArray;
    [TestCase] Procedure TestArrayManaged;
    [TestCase] Procedure TestArrayPush;
    [TestCase] Procedure TestArrayComplex;
    [TestCase] Procedure TestArrayComplexAnonymous;
    [TestCase] Procedure TestArrayComplexPush;
  End;


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
    Destructor Destroy; override;
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

function TJavascriptTests.PropArrayGetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  result := js.makeArray(obj.value.Count,
      function (index : integer) : JsValueRef
      begin
        result := js.wrap(obj.value[index]);
      end);
end;

function TJavascriptTests.PropArrayGetValueManaged(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  result := js.makeManagedArray(TStringListManager.create(obj.FValue));
end;

procedure TJavascriptTests.PropArraySetValue(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  obj : TArrayObj;
begin
  obj := this as TArrayObj;
  obj.value.Clear;
  js.iterateArray(value,
      procedure (i : integer; v : JsValueRef)
      begin
        obj.value.Add(js.asString(v));
      end);
end;

type
  TComplexArrayObj = class
  private
    FValue : TObjectList<TPropObj>;
  public
    constructor Create;
    Destructor Destroy; override;
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
    procedure (i : integer; v : JsValueRef)
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
    end);
end;


{ TJavascriptTests }

procedure TJavascriptTests.Setup;
begin
  FRaise := false;
  FLog := TStringList.create;
end;

procedure TJavascriptTests.TearDown;
begin
  FLog.Free;
end;

procedure TJavascriptTests.defineTestTypes(js : TJavascript; manArrayValue : boolean);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TIntObj', nil);
  def.defineRoutine('addOne', nil, AddOne);
  def := js.defineClass('TPropObj', nil, 'MyObject', MakePropObjFromData);
  def.defineProperty('value', nil, PropObjGetValue, PropObjSetValue);
  def := js.defineClass('TArrayObj', nil);
  if manArrayValue then
    def.defineProperty('value', nil, PropArrayGetValueManaged, PropArraySetValue)
  else
    def.defineProperty('value', nil, PropArrayGetValue, PropArraySetValue);
  def := js.defineClass('TComplexArrayObj', nil);
  def.defineProperty('value', nil, PropComplexArrayGetValue, PropComplexArraySetValue);
end;

procedure TJavascriptTests.JSLog(sender: TJavascript; message: String);
begin
  FLog.Add(message);
  if FRaise then
  begin
    FRaise := false;
    raise Exception.Create('Internal error');
  end;
end;


procedure TJavascriptTests.TestHelloWorld;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    Assert.IsTrue(js.asString(js.execute('(()=>{return ''Hello world!'';})()', 'test.js')) = 'Hello world!');
  finally
    js.Free;
  end;
end;

procedure TJavascriptTests.TestConsoleLog;
var
  js : TJavascript;
begin
  FLog.Clear;
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    js.execute('console.log("Hello world");', 'test.js');
  finally
    js.Free;
  end;
  Assert.IsTrue(FLog.Text = 'Hello world'+#13#10);
end;

procedure TJavascriptTests.TestException;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    try
      js.execute('(()=>{throw "test exception";})()', 'test.js');
      Assert.isTrue(false, 'exception expected');
    except
      on e : exception do
      begin
        Assert.isTrue(e is EJavascriptScript, 'exception had wrong class "'+e.ClassName+'" instead of "EJavascriptScript"');
        Assert.isTrue(e.message = 'test exception', 'exception had wrong text "'+e.Message+'" instead of "test exception"');
      end;
    end;
  finally
    js.Free;
  end;
end;

procedure TJavascriptTests.TestAppException;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    FRaise := true;
    try
      js.execute('console.log("Hello world");', 'test.js');
      Assert.isTrue(false, 'exception expected');
    except
      on e : exception do
      begin
        Assert.isTrue(e.message = 'Internal error', 'exception had wrong text "'+e.Message+'" instead of "Internal error"');
        Assert.isTrue(e is EJavascriptApplication, 'exception had wrong class "'+e.ClassName+'" instead of "EJavascriptApplication"');
      end;
    end;
  finally
    js.Free;
  end;
end;

procedure TJavascriptTests.TestAddOne;
var
  js : TJavascript;
  i : TIntObj;
  o : JsValueRef;
  c : integer;
begin
  i := TIntObj.Create;
  try
    i.value := 1;
    js := TJavascript.Create;
    try
      defineTestTypes(js, false);
      o := js.wrap(i, 'TIntObj', false);
      js.execute('function func1(o) {'+#13#10+' o.addOne();'+#13#10+' } ', 'test.js', 'func1', [o]);
      c := 0;
      js.iterateProperties(o, procedure (name : String; v : TJsValue)
        begin
          inc(c);
        end);
      Assert.IsTrue(c > 0);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value = 2);
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestProperty;
var
  js : TJavascript;
  i : TPropObj;
  o : JsValueRef;
begin
  i := TPropObj.Create('test');
  try
    js := TJavascript.Create;
    try
      defineTestTypes(js, false);
      o := js.wrap(i, i.ClassName, false);
      js.execute('function funcX(o) {'+#13#10+' if (o.value == ''test'') o.value = ''test1'';'+#13#10+' } ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value = 'test1');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestType;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    defineTestTypes(js, false);
    js.execute('function funcX() {'+#13#10+
               ' var o = new MyObject(''text'');'+#13#10+
               ' console.log(o.value);'+#13#10+
               '} ', 'test.js', 'funcX', []);
    Assert.IsTrue(FLog.Text = 'test-text'+#13#10);
  finally
    js.Free;
  end;
end;

procedure TJavascriptTests.TestArray;
var
  js : TJavascript;
  i : TArrayObj;
  o : JsValueRef;
begin
  i := TArrayObj.Create;
  try
    i.value.CommaText := 'test,test1';
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, false);
      o := js.wrap(i, i.ClassName, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0]);'+#13#10+
                 ' console.log(o.value[1]);'+#13#10+
                 ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
                 '  o.value = o.value.concat([''test2'']);'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayManaged;
var
  js : TJavascript;
  i : TArrayObj;
  o : JsValueRef;
begin
  i := TArrayObj.Create;
  try
    i.value.CommaText := 'test,test1';
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, false);
      o := js.wrap(i, i.ClassName, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0]);'+#13#10+
                 ' console.log(o.value[1]);'+#13#10+
                 ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
                 '  o.value = o.value.concat([''test2'']);'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayPush;
var
  js : TJavascript;
  i : TArrayObj;
  o : JsValueRef;
begin
  i := TArrayObj.Create;
  try
    i.value.CommaText := 'test,test1';
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, true);
      o := js.wrap(i, i.ClassName, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0]);'+#13#10+
                 ' console.log(o.value[1]);'+#13#10+
                 ' if (o.value[0] == ''test'' && o.value[1] == ''test1'')'+#13#10+
                 '  o.value.push(''test2'');'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.CommaText = 'test,test1,test2');
  finally
    i.Free;
  end;
end;


procedure TJavascriptTests.TestArrayComplex;
var
  js : TJavascript;
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, false);
      o := js.wrap(i, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0].value);'+#13#10+
                 ' o.value = o.value.concat([new MyObject(''v2'')]);'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.Count = 2);
    Assert.IsTrue(i.value[0].value = 'v1');
    Assert.IsTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayComplexAnonymous;
var
  js : TJavascript;
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, false);
      o := js.wrap(i, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0].value);'+#13#10+
                 ' o.value = o.value.concat([{ "value" : "test-v2"} ]);'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.Count = 2);
    Assert.IsTrue(i.value[0].value = 'v1');
    Assert.IsTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

procedure TJavascriptTests.TestArrayComplexPush;
var
  js : TJavascript;
  i : TComplexArrayObj;
  o : JsValueRef;
begin
  i := TComplexArrayObj.Create;
  try
    i.value.add(TPropObj.Create('v1'));
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      defineTestTypes(js, false);
      o := js.wrap(i, false);
      js.execute('function funcX(o) {'+#13#10+
                 ' console.log(o.value.length);'+#13#10+
                 ' console.log(o.value[0].value);'+#13#10+
                 ' o.value.push({ "value" : "test-v2"} );'+#13#10+
                 '} ', 'test.js', 'funcX', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value.Count = 2);
    Assert.IsTrue(i.value[0].value = 'v1');
    Assert.IsTrue(i.value[1].value = 'test-v2');
  finally
    i.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJavascriptTests);
end.
