unit Javascript;

{
A unit that allows you to execute Javascript inside your application,
with access to functionality as defined by you.

The underlying engine is Microsoft Chakra. For downloads, see
https://github.com/Microsoft/ChakraCore/releases

For examples of use, see JavaScriptTests.pas


Most of the functionality is made by registering callbacks with the
chakra. All call backs have the same structure:

function [name](callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  p : PJsValueRefArray absolute arguments;
begin
  js := TJavascript(callbackState);
  try
    .... body ...
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

callee - handle to the source function that called this function. (use for logging name of origin)?
isConstructCall - whether this was called with a "new" e.g. new Object() - up to you to make a difference or not
arguments - handle to arguments to the procedure
  note: var p allows easy access to arguments other than the first one e.g. p[1]
argumentCount - how many ag
callbackState - a reference to the Javascript engine

You must handle exceptions this way: do not let them propagate back to the Javascript Host

To fully expose an object to the Javascript Host, you need the following:

- a define routine that defines the properties it has
- a factory routine that can construct the type (possibly taking parameters)
- a second factory routine that can construct the type when presented with an anonymous class
- getter and setter routines for all the properties
- wrapper routines for all the functions

Todo:
 - figure out debugging
}
{

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
  SysUtils, Classes, AnsiStrings,
  Generics.Collections,
  ChakraCommon;

type
  // facade for ChakraCommon:
  JsValueRef = ChakraCommon.JsValueRef;
  PJsValueRef = ChakraCommon.PJsValueRef;
  JsPropertyIdRef = ChakraCommon.JsPropertyIdRef;
  JsValueType = ChakraCommon.JsValueType;
  PJsValueRefArray = ChakraCommon.PJsValueRefArray;

const
  JS_INVALID_REFERENCE = nil;
  JsUndefined = ChakraCommon.JsUndefined;
  JsNull = ChakraCommon.JsNull;
  JsNumber = ChakraCommon.JsNumber;
  JsString = ChakraCommon.JsString;
  JsBoolean = ChakraCommon.JsBoolean;
  JsObject = ChakraCommon.JsObject;
  JsFunction = ChakraCommon.JsFunction;
  JsError = ChakraCommon.JsError;
  JsArray = ChakraCommon.JsArray;
  JsSymbol = ChakraCommon.JsSymbol;
  JsArrayBuffer =  ChakraCommon.JsArrayBuffer ;
  JsTypedArray =  ChakraCommon.JsTypedArray ;
  JsDataView = ChakraCommon.JsDataView;

type
  TJavascript = class;
  EJavascriptBase = class (Exception);
  EJavascriptScript = class (EJavascriptBase); // error thrown by script
  EJavascriptSource = class (EJavascriptBase); // error compiling
  EJavascriptHost = class (EJavascriptBase);   // error from hosting infrastructure
  EJavascriptApplication = class (EJavascriptBase);    // error running application functionality

  TJavascriptConsoleLogEvent = procedure (sender : TJavascript; message : String) of object;
  TJavascriptDefineObjectProc = procedure (sender : TJavascript; obj : JsValueRef);
  TJavascriptObjectFactoryProc<T : class> = function (sender : TJavascript; obj : JsValueRef) : T;


  TJavascriptArrayValueProvider = reference to function (index : integer) : JsValueRef;
  TJavascriptArrayValueConsumer = reference to procedure (index : integer; value : JsValueRef);

  {
  javascript API to implement:
  concat()	Joins two or more arrays, and returns a copy of the joined arrays
copyWithin()	Copies array elements within the array, to and from specified positions
every()	Checks if every element in an array pass a test
fill()	Fill the elements in an array with a static value
filter()	Creates a new array with every element in an array that pass a test
find()	Returns the value of the first element in an array that pass a test
findIndex()	Returns the index of the first element in an array that pass a test
forEach()	Calls a function for each array element
indexOf()	Search the array for an element and returns its position
isArray()	Checks whether an object is an array
join()	Joins all elements of an array into a string
lastIndexOf()	Search the array for an element, starting at the end, and returns its position
map()	Creates a new array with the result of calling a function for each array element
pop()	Removes the last element of an array, and returns that element
// push()	Adds new elements to the end of an array, and returns the new length
reduce()	Reduce the values of an array to a single value (going left-to-right)
reduceRight()	Reduce the values of an array to a single value (going right-to-left)
reverse()	Reverses the order of the elements in an array
shift()	Removes the first element of an array, and returns that element
slice()	Selects a part of an array, and returns the new array
some()	Checks if any of the elements in an array pass a test
sort()	Sorts the elements of an array
splice()	Adds/Removes elements from an array
toString()	Converts an array to a string, and returns the result
unshift()	Adds new elements to the beginning of an array, and returns the new length
valueOf()	Returns the primitive value of an array

  }
  TJavascriptArrayManager = {abstract} class
  protected
    FManager : TJavascript;
  public
    // populating the array in the first place
    function count : integer; virtual;
    function item(i : integer) : JsValueRef; virtual;

    // operations from the script
    function push(params : PJsValueRefArray; paramCount : integer) : JsValueRef; virtual;
  end;

  // only one of these per thread
  TJavascript = class // (TAdvObject)
  private
    FRuntime : JsRuntimeHandle;
    FContext : JsContextRef;
    FApplicationError : JsValueRef;
    FOnLog : TJavascriptConsoleLogEvent;

    FPIdGetter : JsPropertyIdRef;
    FPIdSetter : JsPropertyIdRef;

    procedure init;
    procedure fin;
    procedure registerConsoleLog;
    procedure definePropertyById(obj : JsValueRef; pId : JsPropertyIdRef; read : JsNativeFunction); overload;
    procedure definePropertyById(obj : JsValueRef; pId : JsPropertyIdRef; read, write : JsNativeFunction); overload;
    procedure jsCheck(code : JsErrorCode);
    function getPropertyId(name : AnsiString) : JsRef;
  protected
    procedure freeObject(obj : TObject); virtual;
  public
    constructor Create; // override;
    destructor Destroy; override;

    {
      Execute a script.

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
    }
    function execute(script : String; scriptName: AnsiString) : JsValueRef; overload;

    {
      Execute a named function in the script. Note that global functions in the script are executed before the named function

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
      funcName - the javascript function to run
      params - params to pass to the function (must match the named parameters on the function).
         Prepare these using wrap() functions or makeArray
    }
    function execute(script : String; scriptName, funcName: AnsiString; params : array of JsValueRef) : JsValueRef; overload;

    {
      Convert whatever javascript variable is in val to a string representation
    }
    function asString(val: JsValueRef): String;

    {
      Define a type, along with a constructor. This will allow the javascript function
      to do:

        var o = new [Name]();

      note: name is customarily capitalized.

      Your constructor - see above for signature - will be called, so you can create the
      correct underlying object, and initialise it. (something like this:

        obj := TMyUnderlyingObject.Create;
        // whatever initializtion is appropriate
        result := js.wrap(obj, defineTestProp, true);
      )

      note: actually, there's no real class model here, and the 'new' is optional.
      you can have as many constructors as you like for a single underlying class
    }
    procedure defineType(name : AnsiString; factory : JsNativeFunction); overload;

    {
      given an object, define a property value on it - a function that will be called
      with parameters when a javascript function invokes it.

      Your method - see above for signature - will be called, so you can do whatever.

      note: properties can be functions or procedures. Procedures are just functions
      that return JS_INVALID_REFERENCE
    }
    procedure defineProperty(obj : JsValueRef; name : AnsiString; read : JsNativeFunction); overload;

    {
      given an object, define a property value on it that has both read and write (e.g. like a delphi property).

      Your methods - see above for signature - will be called, so you can do whatever.

      note: getter must return something; setter returns JS_INVALID_REFERENCE
    }
    procedure defineProperty(obj : JsValueRef; name : AnsiString; read, write : JsNativeFunction); overload;


    // the rest of the routines are used for converting between delphi native types and
    // javascript native types. They'll be used in the native call back routines referred to above
    {
      find out what type of variable this is
    }
    function getType(v : JsValueRef) : JsValueType;

    {
      Convert a delphi string to a javascript string
    }
    function wrap(s : String) : JsValueRef; overload;
    {
      Convert a delphi integer to a javascript integer
    }
    function wrap(i : integer) : JsValueRef; overload;

    {
      Generate a Javascript object that wraps a native delphi object
        o : the object to wrap
        definer: a procedure that defines properties for the object - using defineProperty above
        owns : true if the object is owned by the javascript engine and should be cleaned up when done
    }
    function wrap(o : TObject; definer : TJavascriptDefineObjectProc; owns : boolean) : JsValueRef; overload;
    // todo: add a wrap variant that uses RTTI

    {
      given a javscript object. get the underlying delphi object
    }
    function getWrapped<T : class>(obj : JsValueRef) : T;

    {
      given a javascript object, get an property value from it (remember to check for undefined)
    }
    function getProperty(obj : JsValueRef; name : AnsiString) : JsValueRef;

    {
      if the wrapper was generated with owns := true (see wrap()), then
      stop owning the object this wraps
    }
    procedure unOwn(v : JsValueRef);

    {
       Create an array with [count] objects, and then iterate [count] objects calling value Provider
       to populate the array

       note: this is called everytime the array is accessed. it can make a difference to performance
       to cache the result and use it until the array changes

       It's important to understand that this creates a javascript managed array - the
       script can change the array, but unless the modified array is assigned to something,
       the changes will disappear. To get a managed array, use the next method
    }
    function makeArray(count : integer; valueProvider : TJavascriptArrayValueProvider) : JsValueRef;

    {
       Create a managed array. The manager is provided by the host application,
       and populates the array, and is notified of changes to the array.

       The manager will be owned by the Javascript Host environment, and will be
       freed when the array is garbage collected.
    }
    function makeManagedArray(manager : TJavascriptArrayManager) : JsValueRef;
    {
      given a javascript array, find out how many entries are in it
    }
    function arrayLength(arr : JsValueRef) : integer;
    {
      iterate the array calling valueConsumer once for each item in it
    }
    procedure iterateArray(arr : JsValueRef; valueConsumer : TJavascriptArrayValueConsumer);

    {
      Javascript callbaxks cannot let an exception propagate back into the JS run time.
      instead, catch all at the last line, and call this
    }
    function handleException(e : Exception) : JsValueRef;

    {
      hook any calls to console.log for debugging purposes
    }
    property OnLog : TJavascriptConsoleLogEvent read FOnLog write FOnLog;

  end;

  TStringListManager = class (TJavascriptArrayManager)
  private
    FList : TStringList;
  public
    constructor create(list : TStringList);

    // populating the array in the first place
    function count : integer; override;
    function item(i : integer) : JsValueRef; override;

    // operations from the script
    function push(params : PJsValueRefArray; paramCount : integer) : JsValueRef; override;
  end;

  TObjectListManager<T: class> = class (TJavascriptArrayManager)
  private
    FList : TObjectList<T>;
    FDefiner : TJavascriptDefineObjectProc;
    FFactory : TJavascriptObjectFactoryProc<T>;
  public
    constructor create(list : TObjectList<T>; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<T>);

    // populating the array in the first place
    function count : integer; override;
    function item(i : integer) : JsValueRef; override;

    // operations from the script
    function push(params : PJsValueRefArray; paramCount : integer) : JsValueRef; override;
  end;


implementation

threadvar
  gjs : TJavascript;

function DoPush(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  js : TJavascript;
  p : PJsValueRefArray absolute arguments;
  o : JsValueRef;
  manager : TJavascriptArrayManager;
  mref : pointer;
begin
  js := TJavascript(callbackState);
  try
    o := js.getProperty(p[0], '__manager');
    js.jsCheck(JsGetExternalData(o, mref));
    manager := TJavascriptArrayManager(mref);
    result := manager.push(p, argumentCount);
  except
    on e:exception do
      result := js.handleException(e);
  end;
end;

function LogCB(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  i : integer;
  s : String;
  p : PJsValueRefArray absolute arguments;
begin
  try
    s := '';
    for i := 1 to argumentCount - 1 do
    begin
      if (i > 1) then
        s := s + ' ';
      s := s + gjs.asString(p[i]);
    end;
    gjs.OnLog(gjs, s);
    result := JS_INVALID_REFERENCE;
  except
    on e:exception do
      result := gjs.handleException(e);
  end;
end;

procedure FreeCallBack(ref: JsRef; callbackState: Pointer); stdcall;
var
  js : TJavascript;
begin
  js := TJavascript(callbackState);
  js.freeObject(js.getWrapped<TObject>(ref));
end;


{ TJavascript }

function TJavascript.arrayLength(arr: JsValueRef): integer;
var
  v : JsValueRef;
begin
  jsCheck(JsGetProperty(arr, getPropertyId('length'), v));
  jsCheck(JsNumberToInt(v, result));
end;

constructor TJavascript.Create;
begin
  inherited;

  if gjs <> nil then
    raise Exception.Create('There is already a javascript runtime on this thread');
  gjs := self;
  init;
  registerConsoleLog;
end;

procedure TJavascript.defineProperty(obj: JsValueRef; name: AnsiString; read: JsNativeFunction);
begin
  definePropertyById(obj, getPropertyId(name), read);
end;

procedure TJavascript.defineProperty(obj: JsValueRef; name: AnsiString; read, write: JsNativeFunction);
begin
  definePropertyById(obj, getPropertyId(name), read, write);
end;

procedure TJavascript.definePropertyById(obj: JsValueRef; pId: JsPropertyIdRef; read, write: JsNativeFunction);
var
  fr, fw : JsValueRef;
  d : JsValueRef;
  ok : boolean;
begin
  jsCheck(JsCreateFunction(read, self, fr));
  jsCheck(JsCreateFunction(write, self, fw));
  jsCheck(JsCreateObject(d));
  jsCheck(JsSetProperty(d, FPIdGetter, fr, true));
  jsCheck(JsSetProperty(d, FPIdSetter, fw, true));
  jsCheck(JsDefineProperty(obj, pId, d, ok));
  assert(ok);
end;

procedure TJavascript.defineType(name: AnsiString; factory: JsNativeFunction);
var
  f, global : JsValueRef;
begin
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsCreateFunction(factory, self, f));
  jsCheck(JsSetProperty(global, getPropertyId(name), f, true));
end;

procedure TJavascript.definePropertyById(obj: JsValueRef; pId: JsPropertyIdRef; read: JsNativeFunction);
var
  f : JsValueRef;
begin
  jsCheck(JsCreateFunction(read, self, f));
  jsCheck(JsSetProperty(obj, pId, f, true));
end;

destructor TJavascript.Destroy;
begin
  fin;
  gjs := nil;
  inherited;
end;


function TJavascript.execute(script : String; scriptName, funcName: AnsiString; params: array of JsValueRef): JsValueRef;
var
  global, func, scriptJ, scriptNameJ, res : JsValueRef;
  pl : PJsValueRefArray;
  i : integer;
  vType : JsValueType;
begin
  // parse + initialise the script
  jsCheck(JsCreateString(PAnsiChar(scriptName), Length(scriptName), scriptNameJ));
  jsCheck(JsCreateExternalArrayBuffer(PChar(script), Length(script) * SizeOf(WideChar), nil, nil, scriptJ));
  jsCheck(JsRun(scriptJ, FContext, scriptNameJ, JsParseScriptAttributeArrayBufferIsUtf16Encoded, res));

  // look up the name on the global object
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsGetValueType(global, vType));
  Assert(vType = JsObject, 'Could not find global object');

  jsCheck(JsGetProperty(global, getPropertyId(funcName), func));
  jsCheck(JsGetValueType(func, vType));
  if (vType <> JsFunction) then
    raise EJavascriptSource.Create('Could not find the function "'+string(funcName)+'" in the script "'+string(scriptName)+'"');

  // execute it
  GetMem(pl, (length(params) + 1) * SizeOf(JsValueRef));
  try
    pl[0] := global;
    for i := 0 to length(params) - 1 do
      pl[i+1] := params[i];
    jsCheck(JsCallFunction(func, pointer(pl), 1 + length(params), result));
  finally
    Freemem(pl);
  end;
end;

function TJavascript.execute(script: String; scriptName : AnsiString): JsValueRef;
begin
  jsCheck(JsRunScript(PChar(script), FContext, '', result));
end;

procedure TJavascript.fin;
begin
  // Dispose runtime
  jsCheck(JsSetCurrentContext(JS_INVALID_REFERENCE));
  jsCheck(JsDisposeRuntime(FRuntime));
end;

procedure TJavascript.freeObject(obj: TObject);
begin
  obj.Free;
end;

function TJavascript.getWrapped<T>(obj: JsValueRef): T;
var
  data : Pointer;
  ok : boolean;
begin
  jsCheck(JsHasExternalData(obj, ok));
  if not ok then
    exit(nil);

  jsCheck(JsGetExternalData(obj, data));
  result := T(data);
end;

function TJavascript.handleException(e: Exception) : JsValueRef;
begin
  FApplicationError := wrap(e.Message);
  jsCheck(JsSetException(FApplicationError));
  result := JS_INVALID_REFERENCE;
end;

procedure TJavascript.init;
begin
  // Create a runtime
  jsCheck(JsCreateRuntime(JsRuntimeAttributeNone, nil, FRuntime));

  // Create an execution context.
  jsCheck(JsCreateContext(FRuntime, FContext));

  // Now set the current execution context.
  jsCheck(JsSetCurrentContext(FContext));

  FPIdGetter := getPropertyId('get');
  FPIdSetter := getPropertyId('set');
end;

procedure TJavascript.iterateArray(arr: JsValueRef; valueConsumer: TJavascriptArrayValueConsumer);
var
  i : integer;
  vi, v : JsValueRef;
begin
  for i := 0 to arrayLength(arr) - 1 do
  begin
    vi := wrap(i);
    jsCheck(JsGetIndexedProperty(arr, vi, v));
    valueConsumer(i, v);
  end;
end;

procedure TJavascript.jsCheck(code: JsErrorCode);
var
  s : String;
  exception : JsValueRef;
  app : Boolean;
begin
  if code > JsNoError then
  begin
    app := false;
    s := '';
    if JsGetAndClearException(exception) = JsNoError then
    begin
      app := FApplicationError = exception;
      s := ':' +asString(exception);
    end;
    FApplicationError := nil;
    case code of
      JsErrorCategoryUsage : raise EJavascriptHost.create('JsErrorCategoryUsage'+s);
      JsErrorInvalidArgument : raise EJavascriptHost.create('JsErrorInvalidArgument'+s);
      JsErrorNullArgument : raise EJavascriptHost.create('JsErrorNullArgument'+s);
      JsErrorNoCurrentContext : raise EJavascriptHost.create('JsErrorNoCurrentContext'+s);
      JsErrorInExceptionState : raise EJavascriptHost.create('JsErrorInExceptionState'+s);
      JsErrorNotImplemented : raise EJavascriptHost.create('JsErrorNotImplemented'+s);
      JsErrorWrongThread : raise EJavascriptHost.create('JsErrorWrongThread'+s);
      JsErrorRuntimeInUse : raise EJavascriptHost.create('JsErrorRuntimeInUse'+s);
      JsErrorBadSerializedScript : raise EJavascriptHost.create('JsErrorBadSerializedScript'+s);
      JsErrorInDisabledState : raise EJavascriptHost.create('JsErrorInDisabledState'+s);
      JsErrorCannotDisableExecution : raise EJavascriptHost.create('JsErrorCannotDisableExecution'+s);
      JsErrorHeapEnumInProgress : raise EJavascriptHost.create('JsErrorHeapEnumInProgress'+s);
      JsErrorArgumentNotObject : raise EJavascriptHost.create('JsErrorArgumentNotObject'+s);
      JsErrorInProfileCallback : raise EJavascriptHost.create('JsErrorInProfileCallback'+s);
      JsErrorInThreadServiceCallback : raise EJavascriptHost.create('JsErrorInThreadServiceCallback'+s);
      JsErrorCannotSerializeDebugScript : raise EJavascriptHost.create('JsErrorCannotSerializeDebugScript'+s);
      JsErrorAlreadyDebuggingContext : raise EJavascriptHost.create('JsErrorAlreadyDebuggingContext'+s);
      JsErrorAlreadyProfilingContext : raise EJavascriptHost.create('JsErrorAlreadyProfilingContext'+s);
      JsErrorIdleNotEnabled : raise EJavascriptHost.create('JsErrorIdleNotEnabled'+s);
      JsCannotSetProjectionEnqueueCallback : raise EJavascriptHost.create('JsCannotSetProjectionEnqueueCallback'+s);
      JsErrorCannotStartProjection : raise EJavascriptHost.create('JsErrorCannotStartProjection'+s);
      JsErrorInObjectBeforeCollectCallback : raise EJavascriptHost.create('JsErrorInObjectBeforeCollectCallback'+s);
      JsErrorObjectNotInspectable : raise EJavascriptHost.create('JsErrorObjectNotInspectable'+s);
      JsErrorPropertyNotSymbol : raise EJavascriptHost.create('JsErrorPropertyNotSymbol'+s);
      JsErrorPropertyNotString : raise EJavascriptHost.create('JsErrorPropertyNotString'+s);
      JsErrorInvalidContext : raise EJavascriptHost.create('JsErrorInvalidContext'+s);
      JsInvalidModuleHostInfoKind : raise EJavascriptHost.create('JsInvalidModuleHostInfoKind'+s);
      JsErrorModuleParsed : raise EJavascriptHost.create('JsErrorModuleParsed'+s);
      JsErrorModuleEvaluated : raise EJavascriptHost.create('JsErrorModuleEvaluated'+s);
      JsErrorCategoryEngine : raise EJavascriptHost.create('JsErrorCategoryEngine'+s);
      JsErrorOutOfMemory : raise EJavascriptHost.create('JsErrorOutOfMemory'+s);
      JsErrorBadFPUState : raise EJavascriptHost.create('JsErrorBadFPUState'+s);
      JsErrorCategoryScript : raise EJavascriptHost.create('JsErrorCategoryScript'+s);
      JsErrorScriptException :
        if app then
          raise EJavascriptApplication.create(s.Substring(1))
        else
          raise EJavascriptScript.create(s.Substring(1));
      JsErrorScriptCompile : raise EJavascriptHost.create('JsErrorScriptCompile'+s);
      JsErrorScriptTerminated : raise EJavascriptHost.create('JsErrorScriptTerminated'+s);
      JsErrorScriptEvalDisabled : raise EJavascriptHost.create('JsErrorScriptEvalDisabled'+s);
      JsErrorCategoryFatal : raise EJavascriptHost.create('JsErrorCategoryFatal'+s);
      JsErrorFatal : raise EJavascriptHost.create('JsErrorFatal'+s);
      JsErrorWrongRuntime : raise EJavascriptHost.create('JsErrorWrongRuntime'+s);
      JsErrorCategoryDiagError : raise EJavascriptHost.create('JsErrorCategoryDiagError'+s);
      JsErrorDiagAlreadyInDebugMode : raise EJavascriptHost.create('JsErrorDiagAlreadyInDebugMode'+s);
      JsErrorDiagNotInDebugMode : raise EJavascriptHost.create('JsErrorDiagNotInDebugMode'+s);
      JsErrorDiagNotAtBreak : raise EJavascriptHost.create('JsErrorDiagNotAtBreak'+s);
      JsErrorDiagInvalidHandle : raise EJavascriptHost.create('JsErrorDiagInvalidHandle'+s);
      JsErrorDiagObjectNotFound : raise EJavascriptHost.create('JsErrorDiagObjectNotFound'+s);
      JsErrorDiagUnableToPerformAction : raise EJavascriptHost.create('JsErrorDiagUnableToPerformAction'+s);
    else
      raise EJavascriptHost.Create('Unknown ?? Error'+s);
    end;
  end;
end;


function TJavascript.makeArray(count: integer; valueProvider: TJavascriptArrayValueProvider): JsValueRef;
var
  i : integer;
  vi, v : JsValueRef;
begin
  JsCheck(JsCreateArray(count, result));
  for i := 0 to count - 1 do
  begin
    v := valueProvider(i);
    vi := wrap(i);
    JsCheck(JsSetIndexedProperty(result, vi, v));
  end;
end;

function TJavascript.makeManagedArray(manager: TJavascriptArrayManager): JsValueRef;
var
  i : integer;
  o : JsValueRef;
begin
  manager.FManager := self;

  JsCheck(JsCreateArray(manager.count, result));

  // we can't associate external data with the array directly, but we
  // can create an object, put it on the array as a property, and hang it off that
  JsCheck(JsCreateExternalObject(manager, nil, o));
  jsCheck(JsSetObjectBeforeCollectCallback(o, self, FreeCallBack));
  jsCheck(JsSetProperty(result, getPropertyId('__manager'), o, false));

  // override all the standard Javascript methods
  defineProperty(result, 'push', doPush);

  // now populate the array
  for i := 0 to manager.count - 1 do
    JsCheck(JsSetIndexedProperty(result, wrap(i), manager.item(i)));
end;

procedure TJavascript.registerConsoleLog;
var
  console, logFunc, global : JsValueRef;
  consolePropId, logPropId : JsPropertyIdRef;
  consoleString : PAnsiChar;
begin
  logPropId := getPropertyId('log');
  jsCheck(JsCreateObject(console));
  jsCheck(JsCreateFunction(LogCB, self, logFunc));
  consoleString := 'console';
  jsCheck(JsSetProperty(console, logPropId, logFunc, true));
  // set console as property of global object
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsCreatePropertyId(consoleString, AnsiStrings.strlen(consoleString), consolePropId));
  jsCheck(JsSetProperty(global, consolePropId, console, true));
end;


function TJavascript.getProperty(obj: JsValueRef; name: AnsiString): JsValueRef;
begin
  jsCheck(JsGetProperty(obj, getPropertyId(name), result));
end;

function TJavascript.getPropertyId(name: AnsiString): JsRef;
begin
  jsCheck(JsCreatePropertyId(PAnsiChar(name), length(name), result));
end;

function TJavascript.getType(v: JsValueRef): JsValueType;
begin
  jsCheck(JsGetValueType(v, result));
end;

function TJavascript.asString(val: JsValueRef) : String;
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

procedure TJavascript.unOwn(v: JsValueRef);
begin
  jsCheck(JsSetObjectBeforeCollectCallback(v, self, nil));
end;

function TJavascript.wrap(i: integer): JsValueRef;
begin
  jsCheck(JsIntToNumber(i, result));
end;

function TJavascript.wrap(s: String): JsValueRef;
begin
  jsCheck(JsPointerToString(pchar(s), length(s), result));
end;

function TJavascript.wrap(o: TObject; definer: TJavascriptDefineObjectProc; owns : boolean): JsValueRef;
begin
  JsCheck(JsCreateExternalObject(o, nil, result));
  definer(self, result);
  if owns then
    jsCheck(JsSetObjectBeforeCollectCallback(result, self, FreeCallBack));
end;

{ TJavascriptArrayManager }

function TJavascriptArrayManager.count: integer;
begin
  raise Exception.Create('Need to override '+className+'.count');
end;

function TJavascriptArrayManager.item(i: integer): JsValueRef;
begin
  raise Exception.Create('Need to override '+className+'.item');
end;

function TJavascriptArrayManager.push(params : PJsValueRefArray; paramCount : integer): JsValueRef;
begin
  raise Exception.Create('Need to override '+className+'.push');
end;

{ TStringListManager }

constructor TStringListManager.create(list: TStringList);
begin
  inherited create;
  FList := list;
end;

function TStringListManager.count: integer;
begin
  result := FList.Count;
end;

function TStringListManager.item(i: integer): JsValueRef;
begin
  result := FManager.wrap(FList[i]);
end;

function TStringListManager.push(params : PJsValueRefArray; paramCount : integer): JsValueRef;
var
  i : integer;
begin
  for i := 1 to paramCount - 1 do
    Flist.add(FManager.asString(params[i]));
  result := FManager.wrap(FList.Count);
end;

{ TObjectListManager<T> }

constructor TObjectListManager<T>.create(list: TObjectList<T>; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<T>);
begin
  inherited Create;
  FList := list;
  FDefiner := definer;
  FFactory := factory;
end;

function TObjectListManager<T>.count: integer;
begin
  result := FList.Count;
end;

function TObjectListManager<T>.item(i: integer): JsValueRef;
begin
  result := FManager.wrap(FList[i], FDefiner, false);
end;

function TObjectListManager<T>.push(params: PJsValueRefArray; paramCount: integer): JsValueRef;
var
  i : integer;
  o : T;
begin
  for i := 1 to paramCount - 1 do
  begin
    o := FManager.getWrapped<T>(params[i]);
    if o = nil then
      o := FFactory(FManager, params[i]);
    Flist.add(o);
  end;
  result := FManager.wrap(FList.Count);
end;

end.

