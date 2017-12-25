unit Javascript;

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
  SysUtils,
  Generics.Collections,
  ChakraCommon;

type
  // facade for ChakraCommon:
  JsValueRef = ChakraCommon.JsValueRef;
  PJsValueRef = ChakraCommon.PJsValueRef;
  JsPropertyIdRef = ChakraCommon.JsPropertyIdRef;

const
  JS_INVALID_REFERENCE = nil;

type
  TJavascript = class;
  EJavascript = class (Exception);

  TJavascriptConsoleLogEvent = procedure (sender : TJavascript; message : String) of object;
  TJavascriptDefineObjectEvent = procedure (sender : TJavascript; obj : JsValueRef) of object;

  // only one of these per thread
  TJavascript = class // (TAdvObject)
  private
    FRuntime : JsRuntimeHandle;
    FCounter : Longword;
    FOnLog : TJavascriptConsoleLogEvent;
    procedure init;
    procedure fin;
    procedure registerConsoleLog;
  public
    constructor Create; // override;
    destructor Destroy; override;

    function execute(script : String) : JsValueRef; overload;
    function execute(script : String; funcName : String; params : array of JsValueRef) : JsValueRef; overload;
    procedure jsCheck(code : JsErrorCode);
    function toString(val: JsValueRef): String;

    procedure defineProperty(obj : JsValueRef; pId : JsPropertyIdRef; func : JsNativeFunction); overload;
    procedure defineProperty(obj : JsValueRef; name : AnsiString; func : JsNativeFunction); overload;
    function registerPropertyId(name : AnsiString) : JsRef;
    function wrapObject(o : TObject; definer : TJavascriptDefineObjectEvent) : JsValueRef;
    function getWrapped<T : class>(obj : JsValueRef) : T;

    property OnLog : TJavascriptConsoleLogEvent read FOnLog write FOnLog;
  end;


implementation

threadvar
  gjs : TJavascript;

function LogCB(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  i : integer;
  s : String;
begin
  s := '';
  for i := 1 to argumentCount - 1 do
  begin
    if (i > 1) then
      s := s + ' ';
    s := s + gjs.toString(arguments[i]);
  end;
  gjs.OnLog(gjs, s);
  result := JS_INVALID_REFERENCE;
end;


{ TJavascript }

constructor TJavascript.Create;
begin
  inherited;

  if gjs <> nil then
    raise Exception.Create('There is already a javascript runtime on this thread');
  gjs := self;
  FCounter := 0;
  init;
  registerConsoleLog;
end;

procedure TJavascript.defineProperty(obj: JsValueRef; name: AnsiString; func: JsNativeFunction);
begin
  defineProperty(obj, registerPropertyId(name), func);
end;

procedure TJavascript.defineProperty(obj: JsValueRef; pId: JsPropertyIdRef; func: JsNativeFunction);
var
  f : JsValueRef;
begin
  jsCheck(JsCreateFunction(func, self, f));
  jsCheck(JsSetProperty(obj, pId, f, true));
end;

destructor TJavascript.Destroy;
begin
  fin;
  gjs := nil;
  inherited;
end;


function TJavascript.execute(script, funcName: String; params: array of JsValueRef): JsValueRef;
var
  p : PChar;
  s : JsValueRef;
  undefined, global, func : JsValueRef;
  pl : PJsValueRef;
  i : integer;
//JsValueRef func, funcPropId, global, undefined, result;
begin
  // parse the script
  p := @script[1];
  inc(FCounter);
  jsCheck(JsParseScript(p, @FCounter, '', s));

  // look up the name on the global object
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsGetUndefinedValue(undefined));
  jsCheck(JsGetProperty(global, registerPropertyId(funcName), func));

  // execute it
  GetMem(pl, (length(params) + 1) * SizeOf(JsValueRef));
  try
    pl[0] := undefined;
    for i := 0 to length(params) - 1 do
      pl[i+1] := params[i];
    jsCheck(JsCallFunction(func, pl, 1 + length(params), result));
  finally
    Freemem(pl);
  end;
end;

function TJavascript.execute(script: String): JsValueRef;
var
  p : PChar;
begin
  p := @script[1];
  inc(FCounter);
  jsCheck(JsRunScript(p, @FCounter, '', result));
end;

procedure TJavascript.fin;
begin
  // Dispose runtime
  jsCheck(JsSetCurrentContext(JS_INVALID_REFERENCE));
  jsCheck(JsDisposeRuntime(FRuntime));
end;

function TJavascript.getWrapped<T>(obj: JsValueRef): T;
var
  data : Pointer;
begin
  jsCheck(JsGetExternalData(obj, data));
  result := T(data);
end;

procedure TJavascript.init;
var
  context : JsContextRef;
begin
  // Create a runtime
  jsCheck(JsCreateRuntime(JsRuntimeAttributeNone, nil, FRuntime));

  // Create an execution context.
  jsCheck(JsCreateContext(FRuntime, context));

  // Now set the current execution context.
  jsCheck(JsSetCurrentContext(context));
end;

procedure TJavascript.jsCheck(code: JsErrorCode);
var
  s : String;
  exception : JsValueRef;
begin
  if code > JsNoError then
  begin
    s := 'No Exception Information Available';
    if JsGetAndClearException(exception) = JsNoError then
      s := toString(exception);
    case code of
      JsErrorCategoryUsage : raise EJavascript.create('JsErrorCategoryUsage: '+s);
      JsErrorInvalidArgument : raise EJavascript.create('JsErrorInvalidArgument: '+s);
      JsErrorNullArgument : raise EJavascript.create('JsErrorNullArgument: '+s);
      JsErrorNoCurrentContext : raise EJavascript.create('JsErrorNoCurrentContext: '+s);
      JsErrorInExceptionState : raise EJavascript.create('JsErrorInExceptionState: '+s);
      JsErrorNotImplemented : raise EJavascript.create('JsErrorNotImplemented: '+s);
      JsErrorWrongThread : raise EJavascript.create('JsErrorWrongThread: '+s);
      JsErrorRuntimeInUse : raise EJavascript.create('JsErrorRuntimeInUse: '+s);
      JsErrorBadSerializedScript : raise EJavascript.create('JsErrorBadSerializedScript: '+s);
      JsErrorInDisabledState : raise EJavascript.create('JsErrorInDisabledState: '+s);
      JsErrorCannotDisableExecution : raise EJavascript.create('JsErrorCannotDisableExecution: '+s);
      JsErrorHeapEnumInProgress : raise EJavascript.create('JsErrorHeapEnumInProgress: '+s);
      JsErrorArgumentNotObject : raise EJavascript.create('JsErrorArgumentNotObject: '+s);
      JsErrorInProfileCallback : raise EJavascript.create('JsErrorInProfileCallback: '+s);
      JsErrorInThreadServiceCallback : raise EJavascript.create('JsErrorInThreadServiceCallback: '+s);
      JsErrorCannotSerializeDebugScript : raise EJavascript.create('JsErrorCannotSerializeDebugScript: '+s);
      JsErrorAlreadyDebuggingContext : raise EJavascript.create('JsErrorAlreadyDebuggingContext: '+s);
      JsErrorAlreadyProfilingContext : raise EJavascript.create('JsErrorAlreadyProfilingContext: '+s);
      JsErrorIdleNotEnabled : raise EJavascript.create('JsErrorIdleNotEnabled: '+s);
      JsCannotSetProjectionEnqueueCallback : raise EJavascript.create('JsCannotSetProjectionEnqueueCallback: '+s);
      JsErrorCannotStartProjection : raise EJavascript.create('JsErrorCannotStartProjection: '+s);
      JsErrorInObjectBeforeCollectCallback : raise EJavascript.create('JsErrorInObjectBeforeCollectCallback: '+s);
      JsErrorObjectNotInspectable : raise EJavascript.create('JsErrorObjectNotInspectable: '+s);
      JsErrorPropertyNotSymbol : raise EJavascript.create('JsErrorPropertyNotSymbol: '+s);
      JsErrorPropertyNotString : raise EJavascript.create('JsErrorPropertyNotString: '+s);
      JsErrorInvalidContext : raise EJavascript.create('JsErrorInvalidContext: '+s);
      JsInvalidModuleHostInfoKind : raise EJavascript.create('JsInvalidModuleHostInfoKind: '+s);
      JsErrorModuleParsed : raise EJavascript.create('JsErrorModuleParsed: '+s);
      JsErrorModuleEvaluated : raise EJavascript.create('JsErrorModuleEvaluated: '+s);
      JsErrorCategoryEngine : raise EJavascript.create('JsErrorCategoryEngine: '+s);
      JsErrorOutOfMemory : raise EJavascript.create('JsErrorOutOfMemory: '+s);
      JsErrorBadFPUState : raise EJavascript.create('JsErrorBadFPUState: '+s);
      JsErrorCategoryScript : raise EJavascript.create('JsErrorCategoryScript: '+s);
      JsErrorScriptException : raise EJavascript.create('JsErrorScriptException: '+s);
      JsErrorScriptCompile : raise EJavascript.create('JsErrorScriptCompile: '+s);
      JsErrorScriptTerminated : raise EJavascript.create('JsErrorScriptTerminated: '+s);
      JsErrorScriptEvalDisabled : raise EJavascript.create('JsErrorScriptEvalDisabled: '+s);
      JsErrorCategoryFatal : raise EJavascript.create('JsErrorCategoryFatal: '+s);
      JsErrorFatal : raise EJavascript.create('JsErrorFatal: '+s);
      JsErrorWrongRuntime : raise EJavascript.create('JsErrorWrongRuntime: '+s);
      JsErrorCategoryDiagError : raise EJavascript.create('JsErrorCategoryDiagError: '+s);
      JsErrorDiagAlreadyInDebugMode : raise EJavascript.create('JsErrorDiagAlreadyInDebugMode: '+s);
      JsErrorDiagNotInDebugMode : raise EJavascript.create('JsErrorDiagNotInDebugMode: '+s);
      JsErrorDiagNotAtBreak : raise EJavascript.create('JsErrorDiagNotAtBreak: '+s);
      JsErrorDiagInvalidHandle : raise EJavascript.create('JsErrorDiagInvalidHandle: '+s);
      JsErrorDiagObjectNotFound : raise EJavascript.create('JsErrorDiagObjectNotFound: '+s);
      JsErrorDiagUnableToPerformAction : raise EJavascript.create('JsErrorDiagUnableToPerformAction: '+s);
    else
      raise EJavascript.Create('An Error');
    end;
  end;
end;


procedure TJavascript.registerConsoleLog;
var
  console, logFunc, global : JsValueRef;
  consolePropId, logPropId : JsPropertyIdRef;
  consoleString : PAnsiChar;
begin
  logPropId := registerPropertyId('log');
  jsCheck(JsCreateObject(console));
  jsCheck(JsCreateFunction(LogCB, self, logFunc));
  consoleString := 'console';
  jsCheck(JsSetProperty(console, logPropId, logFunc, true));
  // set console as property of global object
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsCreatePropertyId(consoleString, strlen(consoleString), consolePropId));
  jsCheck(JsSetProperty(global, consolePropId, console, true));
end;


function TJavascript.registerPropertyId(name: AnsiString): JsRef;
var
  p : PAnsiChar;
begin
  p := @name[1];
  jsCheck(JsCreatePropertyId(p, length(name), &result));
end;

function TJavascript.toString(val: JsValueRef) : String;
var
  p : PChar;
  str : JsValueRef;
  l : Cardinal;
begin
  // Convert your script result to String in JavaScript; redundant if your script returns a String
  jsCheck(JsConvertValueToString(val, str));
  // Project script result back to C++.
  jsCheck(JsStringToPointer(str, p, l));
  result := p;
end;

function TJavascript.wrapObject(o: TObject; definer: TJavascriptDefineObjectEvent): JsValueRef;
begin
  JsCheck(JsCreateExternalObject(o, nil, result));
end;

end.

