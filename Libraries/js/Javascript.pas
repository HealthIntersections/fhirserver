unit Javascript;
{
A unit that allows you to execute Javascript inside your application,
with access to functionality as defined by you.

The underlying engine is Microsoft Chakra. You need the Chakra DLL.
For downloads, see https://github.com/Microsoft/ChakraCore/releases

For examples of use, see JavaScriptTests.pas

Most of the functionality is made by registering classes, along with properties
To fully expose an object to the Javascript Host, you need the following:
- a define routine that defines the properties it has (see defineClass/defineProperty and variants)
- a factory routine that can construct the type (possibly taking parameters - and always taking a single object, an anonymous class)
- getter and setter routines for all the properties
- wrapper routines for any other relevant functions

Todo:
 - figure out debugging
  - things to unit test: nulll values, boolean conversion
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
  Windows,
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
  bool = Boolean;

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
  TJavascriptClassDefinition = class;
  TJavascriptRegisteredProperty = class;

  EJavascriptBase = class (Exception);
  EJavascriptScript = class (EJavascriptBase); // error thrown by script
  EJavascriptSource = class (EJavascriptBase); // error compiling
  EJavascriptHost = class (EJavascriptBase);   // error from hosting infrastructure
  EJavascriptApplication = class (EJavascriptBase);    // error running application functionality

  TJavascriptConsoleLogEvent = procedure (sender : TJavascript; message : String) of object;
  TJavascriptObjectFactoryProc<T : class> = function (sender : TJavascript; obj : JsValueRef) : T of object;

  TJavascriptArrayValueProvider = reference to function (index : integer) : JsValueRef;
  TJavascriptArrayValueConsumer = reference to procedure (index : integer; value : JsValueRef);
  TJavascriptPropertyConsumer = reference to procedure (name : String; value : JsValueRef);

  TJsValue = JsValueRef;
  TJsValues = array of JsValueRef;

  // callback functions registered with the engine

  // one parameter set that should always be supported is a single anonymous object, which is being auto-converted to the right class
  TJsFactoryFunction = function (js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; var owns : boolean) : TObject of object;
  TJsFunction = function (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef of object;
  TJsGetterFunction = function (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef of object;
  TJsSetterProcedure = procedure (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue) of object;

    {
  javascript API to implement on manager:
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
    FJavascript : TJavascript;
  public
    // populating the array in the first place
    function count : integer; virtual;
    function item(i : integer) : JsValueRef; virtual;

    // operations from the script
    function push(this : TJsValue; params : TJsValues) : TJsValue; virtual;
  end;

  TJavascriptRegisteredProperty = class
  private
    FJavascript : TJavascript;
    FClassDef : TJavascriptClassDefinition;
    FName : String;
    FContext : pointer;
    FRoutine : TJsFunction;
    FGetter : TJsGetterFunction;
    FSetter : TJsSetterProcedure;

    // performance caching
    FPropId : JsPropertyIdRef;
    FRoutineJS : TJsValue;
    FGetterJS : TJsValue;
    FSetterJS : TJsValue;
  public
    property name : String read FName;
    property context : Pointer read FContext;
    property javascript : TJavascript read FJavascript;
    property classDef : TJavascriptClassDefinition read FClassDef;
    property setter : TJsSetterProcedure read FSetter;
  end;

  TJavascriptClassDefinition = class
  private
    FJavascript : TJavascript;
    FName : String;
    FFactoryName : String;
    FContext : pointer;
    FFactory : TJsFactoryFunction;
    FProperties : TObjectList<TJavascriptRegisteredProperty>;
  public
    constructor create;
    destructor Destroy; override;
    property name : String read FName;
    property factoryName : String read FFactoryName;
    property context : Pointer read FContext;
    property factory : TJsFactoryFunction read FFactory;
    property javascript : TJavascript read FJavascript;
    property properties : TObjectList<TJavascriptRegisteredProperty> read FProperties;
    procedure defineRoutine(name : String; context : Pointer; routine : TJsFunction);
    procedure defineProperty(name : String; context : Pointer; getter : TJsGetterFunction; setter : TJsSetterProcedure);
  end;

  // only one of these per thread
  TJavascript = class // (TAdvObject)
  private
    FRuntime : JsRuntimeHandle;
    FInstanceId : cardinal;
    FContext : JsContextRef;
    FApplicationError : JsValueRef;
    FOnLog : TJavascriptConsoleLogEvent;
    FDefinedClasses : TDictionary<String,TJavascriptClassDefinition>;
    FOwnedObjects : TObjectList<TObject>;
    FReadOnly: boolean;

    FPIdGetter : JsPropertyIdRef;
    FPIdSetter : JsPropertyIdRef;

    procedure init;
    procedure fin;
    procedure registerConsoleLog;
    procedure jsCheck(code : JsErrorCode);
    function getPropertyId(name : AnsiString) : JsRef;
    function doLog(js: TJavascript; context: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
  protected
    procedure freeObject(obj : TObject); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property InstanceId : cardinal read FInstanceId;

    // reset - clear all associated run-time memory - *including* owned objects, but leave definitions in place
    procedure reset;

    // take ownership of any object the host application wants to associate with the JS context (usually definition helpers)
    procedure ownObject(obj : TObject);

    // 1st, class registration
    {
      Define a class that the Javascript library will work with.

      name is the name of the class from the pascal code perspective. Customarily this is the class name
      context carries any additional defining information desired
    }
    function defineClass(name : String; context : Pointer) : TJavascriptClassDefinition; overload;
    {
      Define a class that the Javascript library will work with.

      name is the name of the class from the pascal code perspective. Customarily this is the class name
      context carries any additional defining information desired

      Also, define a factory for the class. This will allow the javascript code to do:

        var o = new [Name]();

      note: the factory name is customarily capitalized. It need not be the same as the class name, but usually is.
      note: actually, there's no real class model here, and the 'new' is optional.
      you can have as many constructors as you like for a single underlying class
    }
    function defineClass(name : String; context : Pointer; factoryName : String; factory : TJsFactoryFunction) : TJavascriptClassDefinition; overload;
    function hasDefinedClass(name : String) : boolean;
    function getDefinedClass(name : String) : TJavascriptClassDefinition;

    procedure addGlobal(name : AnsiString; obj : TJsValue);

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
    function execute(script : String; scriptName, funcName: AnsiString; params : TJsValues) : JsValueRef; overload;

    {
      Convert whatever javascript variable is in val to a string representation
    }
    function asString(val: JsValueRef): String;

    {
      Convert whatever javascript variable is in val to a boolean
    }
    function asBoolean(val: JsValueRef): boolean;


//    {
//      given an object, define a property value on it - a function that will be called
//      with parameters when a javascript function invokes it.
//
//      Your method - see above for signature - will be called, so you can do whatever.
//
//      note: properties can be functions or procedures. Procedures are just functions
//      that return JS_INVALID_REFERENCE
//    }
//    procedure defineProperty(obj : JsValueRef; name : AnsiString; read : JsNativeFunction); overload;
//
//    {
//      given an object, define a property value on it that has both read and write (e.g. like a delphi property).
//
//      Your methods - see above for signature - will be called, so you can do whatever.
//
//      note: getter must return something; setter returns JS_INVALID_REFERENCE
//    }
//    procedure defineProperty(obj : JsValueRef; name : AnsiString; read, write : JsNativeFunction); overload;
//

    // the rest of the routines are used for converting between delphi native types and
    // javascript native types. They'll be used in the native call back routines referred to above
    {
      find out what type of variable this is
    }
    function getType(v : JsValueRef) : JsValueType;

    {
      // get the value for javascript null
    }
    function getNull : JsValueRef;
    {
      Convert a delphi string to a javascript string
    }
    function wrap(s : String) : JsValueRef; overload;
    {
      Convert a delphi integer to a javascript integer
    }
    function wrap(i : integer) : JsValueRef; overload;
    {
      Convert a delphi boolean to a javascript integer
    }
    function wrap(b : boolean) : JsValueRef; overload;

    {
      Generate a Javascript object that wraps a native delphi object
        o : the object to wrap
        definer: a procedure that defines properties for the object - using defineProperty above
        owns : true if the object is owned by the javascript engine and should be cleaned up when done
    }
    function wrap(o : TObject; className : String; owns : boolean) : JsValueRef; overload;
    function wrap(o : TObject; classDef : TJavascriptClassDefinition; owns : boolean) : JsValueRef; overload;
    function wrap(o : TObject; owns : boolean) : JsValueRef; overload;

    // todo: add a wrap variant that uses RTTI , and one that uses IDispatch

    {
      given a javscript object. get the underlying delphi object
    }
    function getWrapped<T : class>(obj : JsValueRef) : T;

    {
      given a javascript object, get an property value from it (remember to check for undefined)
    }
    function getProperty(obj : JsValueRef; name : AnsiString) : JsValueRef;

    {
      iterate through all the properties on an object
    }
    procedure iterateProperties(value : JsValueRef; proc : TJavascriptPropertyConsumer);

    {
      given a javascript object, see whether it has a property value which is not undefined
    }
    function hasProperty(obj : JsValueRef; name : AnsiString) : boolean;

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

    {
      Blocks any set property calls
    }
    property readOnly : boolean read FReadOnly write FReadOnly;
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
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

  TObjectListManager<T: class> = class (TJavascriptArrayManager)
  private
    FList : TObjectList<T>;
    FObjectDefinition : TJavascriptClassDefinition;
  public
    constructor create(list : TObjectList<T>; objectDefinition : TJavascriptClassDefinition);

    // populating the array in the first place
    function count : integer; override;
    function item(i : integer) : JsValueRef; override;

    // operations from the script
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;


implementation

procedure FreeCallBack(ref: JsRef; callbackState: Pointer); stdcall;
var
  js : TJavascript;
begin
  js := TJavascript(callbackState);
  js.freeObject(js.getWrapped<TObject>(ref));
end;

function GetterCallback(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  prop : TJavascriptRegisteredProperty;
  p : PJsValueRefArray absolute arguments;
begin
  prop := TJavascriptRegisteredProperty(callbackState);
  try
    result := prop.FGetter(prop.FJavascript, prop, prop.FJavascript.getWrapped<TObject>(p[0]));
  except
    on e : Exception do
      result := prop.FJavascript.handleException(e);
  end;
end;

function SetterCallback(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  prop : TJavascriptRegisteredProperty;
  p : PJsValueRefArray absolute arguments;
begin
  prop := TJavascriptRegisteredProperty(callbackState);
  try
    if prop.FJavascript.readOnly then
      raise EJavascriptScript.Create('Unable to set the value - the engine is in read-Only mode');

    prop.FSetter(prop.FJavascript, prop, prop.FJavascript.getWrapped<TObject>(p[0]), p[1]);
    result := JS_INVALID_REFERENCE;
  except
    on e : Exception do
      result := prop.FJavascript.handleException(e);
  end;
end;

function RoutineCallback(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  prop : TJavascriptRegisteredProperty;
  pl : TJsValues;
  i : integer;
  p : PJsValueRefArray absolute arguments;
begin
  prop := TJavascriptRegisteredProperty(callbackState);
  try
    setLength(pl, argumentCount - 1);
    for i := 0 to argumentCount - 2 do
      pl[i] := p[i+1];
    result := prop.FRoutine(prop.FJavascript, prop, prop.FJavascript.getWrapped<TObject>(p[0]), pl);
  except
    on e : Exception do
      result := prop.FJavascript.handleException(e);
  end;
end;

function factoryCallback(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  def : TJavascriptClassDefinition;
  pl : TJsValues;
  i : integer;
  p : PJsValueRefArray absolute arguments;
  obj : TObject;
  owns : boolean;
begin
  def := TJavascriptClassDefinition(callbackState);
  try
    setLength(pl, argumentCount - 1);
    for i := 0 to argumentCount - 2 do
      pl[i] := p[i+1];
    obj := def.FFactory(def.FJavascript, def, pl, owns);
    result := def.FJavascript.wrap(obj, def, owns);
  except
    on e : Exception do
      result := def.FJavascript.handleException(e);
  end;
end;

function DoPush(callee: JsValueRef; isConstructCall: bool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; stdcall;
var
  p : PJsValueRefArray absolute arguments;
  o : JsValueRef;
  manager : TJavascriptArrayManager;
  pl : TJsValues;
  i : integer;
begin
  manager := TJavascriptArrayManager(callbackState);
  try
    setLength(pl, argumentCount - 1);
    for i := 0 to argumentCount - 2 do
      pl[i] := p[i+1];
    result := manager.push(p[0], pl);
  except
    on e:exception do
      result := manager.FJavascript.handleException(e);
  end;
end;


//doLog
function TJavascript.doLog(js : TJavascript; context : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  p : TJsValue;
  s : String;
begin
  s := '';
  for p in parameters do
  begin
    if s <> '' then
      s := s + ' ';
    s := s + js.asString(p);
  end;
  js.OnLog(js, s);
  result := JS_INVALID_REFERENCE
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

function TJavascriptArrayManager.push(this : TJsValue; params : TJsValues) : TJsValue;
begin
  raise Exception.Create('Need to override '+className+'.push');
end;

{ TJavascriptClassDefinition }

constructor TJavascriptClassDefinition.create;
begin
  inherited Create;
  FProperties := TObjectList<TJavascriptRegisteredProperty>.create;
  FProperties.OwnsObjects := true;
end;

destructor TJavascriptClassDefinition.Destroy;
begin
  FProperties.free;
  inherited;
end;

procedure TJavascriptClassDefinition.defineProperty(name: String; context: Pointer; getter: TJsGetterFunction; setter: TJsSetterProcedure);
var
  p : TJavascriptRegisteredProperty;
begin
  p := TJavascriptRegisteredProperty.Create;
  FProperties.Add(p);
  p.FJavascript := FJavascript;
  p.FClassDef := Self;
  p.FName := name;
  p.FContext := context;
  p.FGetter := getter;
  p.FSetter := setter;
end;

procedure TJavascriptClassDefinition.defineRoutine(name: String; context: Pointer; routine: TJsFunction);
var
  p : TJavascriptRegisteredProperty;
begin
  p := TJavascriptRegisteredProperty.Create;
  FProperties.Add(p);
  p.FJavascript := FJavascript;
  p.FClassDef := Self;
  p.FName := name;
  p.FContext := context;
  p.FRoutine := routine;
end;



{ TJavascript }

threadvar
  gjs : TJavascript;

constructor TJavascript.Create;
begin
  inherited;

  if gjs <> nil then
    raise Exception.Create('There is already a javascript runtime on this thread');
  gjs := self;
  FDefinedClasses := TDictionary<String,TJavascriptClassDefinition>.create;
  FOwnedObjects := TObjectList<TObject>.create;
  registerConsoleLog;
  init;
end;

destructor TJavascript.Destroy;
var
  def : TJavascriptClassDefinition;
begin
  fin;
  FOwnedObjects.Free;
  for def in FDefinedClasses.Values do
      def.Free;
  FDefinedClasses.Free;
  gjs := nil;
  inherited;
end;

var
  GRuntimeCounter : integer = 0;

procedure TJavascript.init;
var
  def : TJavascriptClassDefinition;
  global, f : TJsValue;
begin
  FInstanceId := InterlockedIncrement(GRuntimeCounter);

  // Create a runtime
  jsCheck(JsCreateRuntime(JsRuntimeAttributeNone, nil, FRuntime));

  // Create an execution context.
  jsCheck(JsCreateContext(FRuntime, FContext));

  // Now set the current execution context.
  jsCheck(JsSetCurrentContext(FContext));

  FPIdGetter := getPropertyId('get');
  FPIdSetter := getPropertyId('set');

  for def in FDefinedClasses.Values do
  begin
    if (def.FFactoryName <> '') then
    begin
      jsCheck(JsGetGlobalObject(global));
      jsCheck(JsCreateFunction(factoryCallback, def, f));
      jsCheck(JsSetProperty(global, getPropertyId(def.FName), f, true));
    end;
  end;
  addGlobal('console', wrap(TObject.create, 'Console', true));
end;

procedure TJavascript.fin;
begin
  // Dispose runtime
  jsCheck(JsSetCurrentContext(JS_INVALID_REFERENCE));
  jsCheck(JsDisposeRuntime(FRuntime));
end;

procedure TJavascript.registerConsoleLog;
var
  def : TJavascriptClassDefinition;
begin
  def := defineClass('Console', nil);
  def.defineRoutine('log', nil, doLog);
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

function TJavascript.handleException(e: Exception) : JsValueRef;
begin
  FApplicationError := wrap(e.Message);
  jsCheck(JsSetException(FApplicationError));
  result := JS_INVALID_REFERENCE;
end;

procedure TJavascript.freeObject(obj: TObject);
begin
  obj.Free;
end;

procedure TJavascript.ownObject(obj : TObject);
begin
  FOwnedObjects.add(obj);
end;

procedure TJavascript.reset;
var
  c : TJavascriptClassDefinition;
  p : TJavascriptRegisteredProperty;
begin
  fin;
  FOwnedObjects.Clear;
  for c in FDefinedClasses.Values do
    for p in c.FProperties do
    begin
      p.FPropId := nil;
      p.FRoutineJS := nil;
      p.FGetterJS := nil;
      p.FSetterJS := nil;
    end;

  init;
end;

function TJavascript.getPropertyId(name: AnsiString): JsRef;
begin
  jsCheck(JsCreatePropertyId(PAnsiChar(name), length(name), result));
end;


function TJavascript.defineClass(name : String; context : Pointer; factoryName : String; factory : TJsFactoryFunction) : TJavascriptClassDefinition;
var
  global, f : TJsValue;
begin
  if hasDefinedClass(name) then
    raise Exception.Create('Attempt to redefine '+name);

  result := TJavascriptClassDefinition.create;
  FDefinedClasses.add(name, result);
  result.FJavascript := self;
  result.FName := name;
  result.FContext := context;
  result.FFactoryName := factoryName;
  result.FFactory := factory;
  if FRuntime <> nil then
  begin
    jsCheck(JsGetGlobalObject(global));
    jsCheck(JsCreateFunction(factoryCallback, result, f));
    jsCheck(JsSetProperty(global, getPropertyId(factoryName), f, true));
  end;
end;

function TJavascript.defineClass(name : String; context : Pointer) : TJavascriptClassDefinition;
begin
  if hasDefinedClass(name) then
    raise Exception.Create('Attempt to redefine '+name);

  result := TJavascriptClassDefinition.create;
  FDefinedClasses.add(name, result);
  result.FJavascript := self;
  result.FName := name;
  result.FContext := context;
end;

function TJavascript.hasDefinedClass(name : String) : boolean;
begin
  result := FDefinedClasses.ContainsKey(name);
end;

function TJavascript.getDefinedClass(name : String) : TJavascriptClassDefinition;
begin
  result := FDefinedClasses[name];
end;

function TJavascript.getNull: JsValueRef;
begin
  jsCheck(JsGetNullValue(result));
end;

procedure TJavascript.addGlobal(name : AnsiString; obj : TJsValue);
var
  global : JsValueRef;
begin
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsSetProperty(global, getPropertyId(name), obj, true));
end;

function TJavascript.execute(script: String; scriptName : AnsiString): JsValueRef;
begin
  jsCheck(JsRunScript(PChar(script), FContext, '', result));
end;

function TJavascript.execute(script : String; scriptName, funcName: AnsiString; params: TJsValues): JsValueRef;
var
  global, func, scriptJ, scriptNameJ, res : JsValueRef;
  pl : PJsValueRefArray;
  i : integer;
  vType : int; //JsValueType, but see note on JsGetValueType
begin
  // parse + initialise the script
  jsCheck(JsCreateString(PAnsiChar(scriptName), Length(scriptName), scriptNameJ));
  jsCheck(JsCreateExternalArrayBuffer(PChar(script), Length(script) * SizeOf(WideChar), nil, nil, scriptJ));
  jsCheck(JsRun(scriptJ, FContext, scriptNameJ, JsParseScriptAttributeArrayBufferIsUtf16Encoded, res));

  // look up the name on the global object
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsGetValueType(global, vType));
  Assert(JsValueType(vType) = JsObject, 'Could not find global object');

  jsCheck(JsGetProperty(global, getPropertyId(funcName), func));
  jsCheck(JsGetValueType(func, vType));
  if (JsValueType(vType) <> JsFunction) then
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


function TJavascript.wrap(s: String): JsValueRef;
begin
  if s = '' then
    result := getNull
  else
    jsCheck(JsPointerToString(pchar(s), length(s), result));
end;

function TJavascript.wrap(i: integer): JsValueRef;
begin
  jsCheck(JsIntToNumber(i, result));
end;

function TJavascript.wrap(b: boolean): JsValueRef;
begin
  jsCheck(JsBoolToBoolean(b, result));
end;

function TJavascript.wrap(o : TObject; className : String; owns : boolean) : JsValueRef;
var
  def : TJavascriptClassDefinition;
begin
  if o = nil then
    result := getNull
  else if FDefinedClasses.TryGetValue(className, def) then
    result := wrap(o, def, owns)
  else
  begin
    if owns then
      freeObject(o);
    raise EJavascriptHost.Create('Attempt to use unknown class "'+className+'"');
  end;
end;

function TJavascript.wrap(o : TObject; classDef : TJavascriptClassDefinition; owns : boolean) : JsValueRef;
var
  p : TJavascriptRegisteredProperty;
  d : JsValueRef;
  ok : boolean;
begin
  JsCheck(JsCreateExternalObject(o, nil, result));
  if owns then
    jsCheck(JsSetObjectBeforeCollectCallback(result, self, FreeCallBack));
  for p in classDef.FProperties do
  begin
    if p.FPropId = nil then
      p.FPropId := getPropertyId(p.Name);
    if assigned(p.FSetter) then
    begin
      if p.FGetterJS = nil then
        jsCheck(JsCreateFunction(GetterCallback, p, p.FGetterJS));
      if p.FSetterJS = nil then
        jsCheck(JsCreateFunction(SetterCallback, p, p.FSetterJS));
      jsCheck(JsCreateObject(d));
      jsCheck(JsSetProperty(d, FPIdGetter, p.FGetterJS, true));
      jsCheck(JsSetProperty(d, FPIdSetter, p.FSetterJS, true));
      jsCheck(JsDefineProperty(result, p.FPropId, d, ok));
      assert(ok);
    end
    else
    begin
      if p.FRoutineJS = nil then
        jsCheck(JsCreateFunction(RoutineCallback, p, p.FRoutineJS));
      jsCheck(JsSetProperty(result, p.FPropId, p.FRoutineJS, true));
    end;
  end;
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

function TJavascript.asBoolean(val: JsValueRef): boolean;
var
  p : PChar;
  str : JsValueRef;
  l : Cardinal;
  v : integer;
begin
  case getType(val) of
    JsUndefined : result := false;
    JsNull : result := false;
    JsNumber :
      begin
      JsNumberToInt(val, v);
      result := v <> 0;
      end;
    JsString : result := asString(val) = 'true';
    JsBoolean : jsCheck(JsBooleanToBool(val, result));
    JsObject : result := true;
    JsFunction : result := false; // should evaluate it??
    JsError : result := false;
    JsArray : result := false;
    JsSymbol : result := false;
    JsArrayBuffer  : result := false;
    JsTypedArray  : result := false;
    JsDataView : result := false;
  end;
end;

function TJavascript.getType(v: JsValueRef): JsValueType;
var
  i : int;
begin
  jsCheck(JsGetValueType(v, i));
  result := JsValueType(i);
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

function TJavascript.getProperty(obj: JsValueRef; name: AnsiString): JsValueRef;
begin
  jsCheck(JsGetProperty(obj, getPropertyId(name), result));
end;

function TJavascript.hasProperty(obj: JsValueRef; name: AnsiString): boolean;
var
  o : JsValueRef;
begin
  jsCheck(JsGetProperty(obj, getPropertyId(name), o));
  result := not (getType(o) in [JsUndefined, JsNull]);
end;

procedure TJavascript.unOwn(v: JsValueRef);
begin
  jsCheck(JsSetObjectBeforeCollectCallback(v, self, nil));
end;

function TJavascript.wrap(o: TObject; owns: boolean): JsValueRef;
begin
  result := wrap(o, o.ClassName, owns);
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
  procedure defineProperty(name : AnsiString; read : JsNativeFunction);
  var
    f : TJsValue;
  begin
    jsCheck(JsCreateFunction(read, manager, f));
    jsCheck(JsSetProperty(result, getPropertyId(name), f, true));
  end;
begin
  manager.FJavascript := self;
  JsCheck(JsCreateArray(manager.count, result));

  // we can't associate external data with the array directly, but we
  // can create an object, put it on the array as a property, and hang it off that
  JsCheck(JsCreateExternalObject(manager, nil, o));
  jsCheck(JsSetObjectBeforeCollectCallback(o, self, FreeCallBack));
  jsCheck(JsSetProperty(result, getPropertyId('__manager'), o, false));

  // override all the standard Javascript methods
  defineProperty('push', doPush);

  // todo: all the rest

  // now populate the array
  for i := 0 to manager.count - 1 do
    JsCheck(JsSetIndexedProperty(result, wrap(i), manager.item(i)));
end;

function TJavascript.arrayLength(arr: JsValueRef): integer;
var
  v : JsValueRef;
begin
  jsCheck(JsGetProperty(arr, getPropertyId('length'), v));
  jsCheck(JsNumberToInt(v, result));
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

procedure TJavascript.iterateProperties(value: JsValueRef; proc: TJavascriptPropertyConsumer);
var
  arr : JsValueRef;
  i : integer;
  vi, v : JsValueRef;
  n : String;
begin
  jsCheck(JsGetOwnPropertyNames(value, arr));
  for i := 0 to arrayLength(arr) - 1 do
  begin
    vi := wrap(i);
    jsCheck(JsGetIndexedProperty(arr, vi, v));
    n := asString(v);
    proc(n, getProperty(value, n));
  end;
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
  result := FJavascript.wrap(FList[i]);
end;

function TStringListManager.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  p : TJsValue;
begin
  for p in params do
    Flist.add(FJavascript.asString(p));
  result := FJavascript.wrap(FList.Count);
end;

{ TObjectListManager<T> }

constructor TObjectListManager<T>.create(list: TObjectList<T>; objectDefinition : TJavascriptClassDefinition);
begin
  inherited Create;
  FList := list;
  FObjectDefinition := objectDefinition;;
end;

function TObjectListManager<T>.count: integer;
begin
  result := FList.Count;
end;

function TObjectListManager<T>.item(i: integer): JsValueRef;
begin
  result := FJavascript.wrap(FList[i], FObjectDefinition, false);
end;

function TObjectListManager<T>.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  p : TJsValue;
  o : T;
  pl : TJsValues;
  owns : boolean;
begin
  setLength(pl, 1);
  for p in params do
  begin
    o := FJavascript.getWrapped<T>(p);
    if o = nil then
    begin
      pl[0] := p;
      o := FObjectDefinition.FFactory(FJavascript, FObjectDefinition, pl, owns) as T;
    end;
    Flist.add(o);
  end;
  result := FJavascript.wrap(FList.Count);
end;

end.

