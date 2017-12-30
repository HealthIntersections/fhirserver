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
  ChakraCommon,
  Javascript,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TJavascriptTests = Class (TObject)
  Private
    FLog : TStringList;
    FRaise : boolean;
    procedure JSLog(sender : TJavascript; message : String);
  Published
    [SetUp]    Procedure Setup;
    [TearDown] Procedure TearDown;
    [TestCase] Procedure TestHelloWorld;
    [TestCase] Procedure TestConsoleLog;
    [TestCase] Procedure TestException;
    [TestCase] Procedure TestAppException;
    [TestCase] Procedure TestAddOne;
    [TestCase] Procedure TestProperty;
    [TestCase] Procedure TestArray;
    [TestCase] Procedure TestArrayManaged;
    [TestCase] Procedure TestArrayPush;
    [TestCase] Procedure TestArrayComplex;
    [TestCase] Procedure TestArrayComplexAnonymous;
    [TestCase] Procedure TestArrayComplexPush;
    [TestCase] Procedure TestType;
  End;


implementation

procedure defineTestInt(sender : TJavascript; obj : JsValueRef); forward;
procedure defineTestProp(sender : TJavascript; obj : JsValueRef); forward;
procedure defineTestArray(sender : TJavascript; obj : JsValueRef); forward;
procedure defineTestArrayManaged(sender : TJavascript; obj : JsValueRef); forward;
procedure defineTestComplexArray(sender : TJavascript; obj : JsValueRef); forward;

Type
  TIntObj = class
  private
    value : integer;
  end;

  TPropObj = class
  private
    FValue : String;
  public
    constructor Create(v : String);
    destructor Destroy; override;
    property value : String read FValue write FValue;
  end;

  TArrayObj = class
  private
    FValue : TStringList;
  public
    constructor Create;
    Destructor Destroy; override;
    property value : TStringList read FValue;
  end;

  TComplexArrayObj = class
  private
    FValue : TObjectList<TPropObj>;
  public
    constructor Create;
    Destructor Destroy; override;
    property value : TObjectList<TPropObj> read FValue;
  end;

function MakePropObjFromData(js : TJavascript; obj : JsValueRef) : TPropObj;
begin
  result := TPropObj.create(js.asString(js.getProperty(obj, 'value')));
end;

function AddOne(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TIntObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TIntObj>(p[0]);
    inc(obj.value);
    result := JS_INVALID_REFERENCE;
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropObjGetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TPropObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TPropObj>(p[0]);
    result := js.wrap(obj.value);
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropObjSetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TPropObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TPropObj>(p[0]);
    obj.value := js.asString(p[1]);
    result := JS_INVALID_REFERENCE;
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropArrayGetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TArrayObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TArrayObj>(p[0]);
    result := js.makeArray(obj.value.Count,
      function (index : integer) : JsValueRef
      begin
        result := js.wrap(obj.value[index]);
      end);
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropArrayGetValueManaged(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TArrayObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TArrayObj>(p[0]);
    result := js.makeManagedArray(TStringListManager.create(obj.FValue));
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropArraySetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TArrayObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TArrayObj>(p[0]);
    obj.value.Clear;
    js.iterateArray(p[1],
      procedure (i : integer; v : JsValueRef)
      begin
        obj.value.Add(js.asString(v));
      end);
    result := JS_INVALID_REFERENCE;
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropComplexArrayGetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TComplexArrayObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TComplexArrayObj>(p[0]);
    result := js.makemanagedArray(TObjectListManager<TPropObj>.create(obj.FValue, defineTestProp, MakePropObjFromData));
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function PropComplexArraySetValue(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TComplexArrayObj;
  p : PJsValueRefArray absolute arguments;
  o : TPropObj;
begin
  js := TJavascript(callbackState);
  try
    obj := js.getWrapped<TComplexArrayObj>(p[0]);
    obj.value.Clear;
    js.iterateArray(p[1],
      procedure (i : integer; v : JsValueRef)
      begin
        o := js.getWrapped<TPropObj>(v);
        if (o = nil) then
          o := MakePropObjFromData(js, v);
        obj.value.Add(o);
        js.unOwn(v);
      end);
    result := JS_INVALID_REFERENCE;
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function CreateMyObject(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TPropObj;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    obj := TPropObj.Create('test-'+js.asString(p[1]));
    result := js.wrap(obj, defineTestProp, true);
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

procedure defineTestInt(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'addOne', AddOne);
end;

procedure defineTestProp(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'value', PropObjGetValue, PropObjSetValue);
end;

procedure defineTestArray(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'value', PropArrayGetValue, PropArraySetValue);
end;

procedure defineTestArrayManaged(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'value', PropArrayGetValueManaged, PropArraySetValue);
end;

procedure defineTestComplexArray(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'value', PropComplexArrayGetValue, PropComplexArraySetValue);
end;


{ TJavascriptTests }

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

procedure TJavascriptTests.JSLog(sender: TJavascript; message: String);
begin
  FLog.Add(message);
  if FRaise then
    raise Exception.Create('Internal error');
end;

procedure TJavascriptTests.Setup;
begin
  FRaise := false;
  FLog := TStringList.create;
end;

procedure TJavascriptTests.TearDown;
begin
  FLog.Free;
end;

procedure TJavascriptTests.TestAddOne;
var
  js : TJavascript;
  i : TIntObj;
  o : JsValueRef;
begin
  i := TIntObj.Create;
  try
    i.value := 1;
    js := TJavascript.Create;
    try
      o := js.wrap(i, defineTestInt, false);
      js.execute('function func1(o) {'+#13#10+' o.addOne();'+#13#10+' } ', 'test.js', 'func1', [o]);
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value = 2);
  finally
    i.Free;
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
      o := js.wrap(i, defineTestProp, false);
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
    js.defineType('MyObject', CreateMyObject);
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
      o := js.wrap(i, defineTestArray, false);
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
      o := js.wrap(i, defineTestArrayManaged, false);
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
      o := js.wrap(i, defineTestArrayManaged, false);
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
      js.defineType('MyObject', CreateMyObject);
      o := js.wrap(i, defineTestComplexArray, false);
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
      js.defineType('MyObject', CreateMyObject);
      o := js.wrap(i, defineTestComplexArray, false);
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
      js.defineType('MyObject', CreateMyObject);
      o := js.wrap(i, defineTestComplexArray, false);
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

initialization
  TDUnitX.RegisterTestFixture(TJavascriptTests);
end.
