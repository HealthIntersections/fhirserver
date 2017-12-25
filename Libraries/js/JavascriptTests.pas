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
  Classes,
  ChakraCommon,
  Javascript,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TJavascriptTests = Class (TObject)
  Private
    FLog : TStringList;
    procedure defineTestInt(sender : TJavascript; obj : JsValueRef);
    procedure JSLog(sender : TJavascript; message : String);
  Published
    [TestCase] Procedure TestHelloWorld;
    [TestCase] Procedure TestConsoleLog;
    [TestCase] Procedure TestAddOne;
  End;


implementation

Type
  TIntObj = class
  private
    value : integer;
  end;

function AddOne(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  obj : TIntObj;
begin
  js := TJavascript(callbackState);
  obj := js.getWrapped<TIntObj>(arguments[0]);
  inc(obj.value);
  result := JS_INVALID_REFERENCE;
end;

procedure TJavascriptTests.TestHelloWorld;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.OnLog := JSLog;
    Assert.IsTrue(js.toString(js.execute('(()=>{return ''Hello world!'';})()')) = 'Hello world!');
  finally
    js.Free;
  end;
end;

procedure TJavascriptTests.TestConsoleLog;
var
  js : TJavascript;
begin
  FLog := TStringList.create;
  try
    js := TJavascript.Create;
    try
      js.OnLog := JSLog;
      js.execute('console.log("Hello world");');
    finally
      js.Free;
    end;
    Assert.IsTrue(FLog.Text = 'Hello world'+#13#10);
  finally
    FLog.Free;
  end;
end;

procedure TJavascriptTests.defineTestInt(sender : TJavascript; obj : JsValueRef);
begin
  sender.defineProperty(obj, 'addOne', AddOne);
end;

procedure TJavascriptTests.JSLog(sender: TJavascript; message: String);
begin
  FLog.Add(message);
end;

procedure TJavascriptTests.TestAddOne;
var
  js : TJavascript;
  i : TIntObj;
  o : JsValueRef;
  p : JsPropertyIdRef;
begin
  i := TIntObj.Create;
  try
    i.value := 1;
    js := TJavascript.Create;
    try
      o := js.wrapObject(i, defineTestInt);
      js.execute('function func1() {'+#13#10+'// o.addOne();'+#13#10+' } ', 'func', []); // [o]
    finally
      js.Free;
    end;
    Assert.IsTrue(i.value = 2);
  finally
    i.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJavascriptTests);
end.
