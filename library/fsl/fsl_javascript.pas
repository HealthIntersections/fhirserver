unit fsl_javascript;

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

{$I fhir.inc}

{
  Subclasses the Javascript library so it knows about fsl_base library reference counting
}

interface

uses
  {$IFDEF WINDOWS} WINDOWS, {$ENDIF}
  SysUtils, Classes, {$IFNDEF FPC} AnsiStrings, {$ENDIF}
  Generics.Collections, System.NetEncoding,
  ChakraCoreUtils, ChakraCore, ChakraCommon,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections;

type
  // facade for ChakraCommon:
  JsValueRef = ChakraCommon.JsValueRef;
  PJsValueRef = ChakraCommon.PJsValueRef;
  JsPropertyIdRef = ChakraCommon.JsPropertyIdRef;
  JsValueType = ChakraCommon.JsValueType;
  JsValueRefArray = array [0..10 {arbitrary size}] of JsValueRef;
  PJsValueRefArray = ^JsValueRefArray;
  bool = Boolean;
  EChakraCoreScript = ChakraCoreUtils.EChakraCoreScript;
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

  TJavascriptConsoleLogEvent = procedure (sender : TJavascript; message : String) of object;
  TJavascriptObjectFactoryProc<T : class> = function (sender : TJavascript; obj : JsValueRef) : T of object;

  TJavascriptArrayValueProvider = {$IFNDEF FPC} reference to {$ENDIF} function (js : TJavascript; context : pointer; index : integer) : JsValueRef;
  TJavascriptArrayValueConsumer = {$IFNDEF FPC} reference to {$ENDIF} procedure (js : TJavascript; context : pointer; index : integer; value : JsValueRef);
  TJavascriptPropertyConsumer = {$IFNDEF FPC} reference to {$ENDIF} procedure (js : TJavascript; context : pointer; name : String; value : JsValueRef);

  TJsValue = JsValueRef;
  TJsValues = array of JsValueRef;

  // callback functions registered with the engine

  // one parameter set that should always be supported is a single anonymous object, which is being auto-converted to the right class
  TJsFactoryFunction = function (js : TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean) : TObject of object;
  TJsFunction = function (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef of object;
  TJsGetterFunction = function (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef of object;
  TJsSetterProcedure = procedure (js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue) of object;

    {
  javascript API to implement on manager:
  concat()  Joins two or more arrays, and returns a copy of the joined arrays
copyWithin()  Copies array elements within the array, to and from specified positions
every()  Checks if every element in an array pass a test
fill()  Fill the elements in an array with a static value
filter()  Creates a new array with every element in an array that pass a test
find()  Returns the value of the first element in an array that pass a test
findIndex()  Returns the index of the first element in an array that pass a test
forEach()  Calls a function for each array element
indexOf()  Search the array for an element and returns its position
isArray()  Checks whether an object is an array
join()  Joins all elements of an array into a string
lastIndexOf()  Search the array for an element, starting at the end, and returns its position
map()  Creates a new array with the result of calling a function for each array element
pop()  Removes the last element of an array, and returns that element
// push()  Adds new elements to the end of an array, and returns the new length
reduce()  Reduce the values of an array to a single value (going left-to-right)
reduceRight()  Reduce the values of an array to a single value (going right-to-left)
reverse()  Reverses the order of the elements in an array
shift()  Removes the first element of an array, and returns that element
slice()  Selects a part of an array, and returns the new array
some()  Checks if any of the elements in an array pass a test
sort()  Sorts the elements of an array
splice()  Adds/Removes elements from an array
toString()  Converts an array to a string, and returns the result
unshift()  Adds new elements to the beginning of an array, and returns the new length
valueOf()  Returns the primitive value of an array

  }
  TJavascriptArrayManager = class abstract
  protected
    FJavascript : TJavascript;
  public
    // populating the array in the first place
    function count : integer; virtual; abstract;
    function item(i : integer) : JsValueRef; virtual; abstract;

    // operations from the script
    function push(this : TJsValue; params : TJsValues) : TJsValue; virtual; abstract;
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
    constructor Create;
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

  TJavascriptClass = class of TJavascript;
  // only one of these per thread
  TJavascript = class
  private
    FUseCount : integer;
    FRuntime : JsRuntimeHandle;
    FInstanceId : cardinal;
    FContext : JsHandle;
    FApplicationError : JsValueRef;
    FOnLog : TJavascriptConsoleLogEvent;
    FNamespaces : TStringList;
    FDefinedClasses : TDictionary<String,TJavascriptClassDefinition>;
    FOwnedObjects : TObjectList<TObject>;
    FImmutableObjects : boolean;
    FStrict : boolean;

    FPIdGetter : JsPropertyIdRef;
    FPIdSetter : JsPropertyIdRef;
    FDebugging: boolean;

    procedure init;
    procedure fin;
    procedure registerConsoleLog;
    procedure jsCheck(code : JsErrorCode);
    {$IFNDEF FPC}
    function getPropertyId(name : AnsiString) : JsPropertyIdRef; overload;
    {$ENDIF}
    function getPropertyId(name : String) : JsPropertyIdRef; overload;
    function getPropertyValue(obj : JsValueRef; name : String) : JsValueRef;
    function doLog(js: TJavascript; context: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
  protected
    procedure freeObject(obj : TObject); virtual;
    function checkSetupDefinitions(definitions : String) : string; virtual;

    class function acquire(clss : TJavascriptClass) : TJavascript; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property InstanceId : cardinal read FInstanceId;
    property Debugging : boolean read FDebugging write FDebugging;
    property ObjectsImmutable : boolean read FImmutableObjects write FImmutableObjects;
    property Strict : boolean read FStrict write FStrict;

    class function acquire : TJavascript; overload;
    procedure yield;

    // reset - clear all associated run-time memory - *including* owned objects, but leave definitions in place
    procedure reset;

    // take ownership of any object the host application wants to associate with the JS context (usually definition helpers)
    procedure ownObject(obj : TObject);

    // this is purely advisory, to help track whether a set of definitions has already been performed;
    procedure defineNamespace(name : String);
    function namespaceDefined(name : String) : boolean;

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
    function defineClass(name : String; context : Pointer; factory : TJsFactoryFunction) : TJavascriptClassDefinition; overload;
    function hasDefinedClass(name : String) : boolean;
    function getDefinedClass(name : String) : TJavascriptClassDefinition;

    procedure addGlobal(name : AnsiString; obj : TJsValue);

    {
      Compile a script.

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
    }
    function compile(script: String; scriptName: String) : JsValueRef; overload;

    {
      Execute a script.

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
    }
    function execute(script: String; scriptName: String) : JsValueRef; overload;

    {
      Execute a named function in the script. Note that global functions in the script are executed before the named function

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
      funcName - the javascript function to run
      params - params to pass to the function (must match the named parameters on the function).
         Prepare these using wrap() functions or makeArray
    }
    function execute(script: String; scriptName, funcName: String; params : Array of TJsValue) : JsValueRef; overload;

    {
      Execute a named function in the script. Note that global functions in the script are executed before the named function

      script - the contents of the script
      scriptName - an arbitrary name for the sript that is bing executed (when debugging)
      funcName - the javascript function to run
      params - params to pass to the function (must match the named parameters on the function).
         Prepare these using wrap() functions or makeArray
    }
    function executeObj(script: String; scriptName, funcName: String; params : Array of TFslObject) : JsValueRef; overload;

    {
      Convert whatever javascript variable is in val to a string representation
    }
    function asString(val: JsValueRef): String;

    {
      Convert whatever javascript variable is in val to a string representation
    }
    function asInteger(val: JsValueRef): integer;

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
    function wrap(o : TObject; className : String; owns : boolean; immutable : boolean = false) : JsValueRef; overload; virtual;
    function wrap(o : TObject; classDef : TJavascriptClassDefinition; owns : boolean; immutable : boolean = false) : JsValueRef; overload; virtual;
    function wrap(o : TObject; owns : boolean; immutable : boolean = false) : JsValueRef; overload; virtual;

    // todo: add a wrap variant that uses RTTI , and one that uses IDispatch

    {
      given a javscript object. get the underlying delphi object
    }
    function getWrapped<T : class>(obj : JsValueRef) : T;
    function getWrappedObj(obj : JsValueRef) : TObject;

    {
      given a javascript object, get an property value from it (remember to check for undefined)
    }
    function getProperty(obj : JsValueRef; name : String) : JsValueRef;

    {
      iterate through all the properties on an object
    }
    procedure iterateProperties(value : JsValueRef; proc : TJavascriptPropertyConsumer; context : pointer);

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
    function makeArray(count : integer; valueProvider : TJavascriptArrayValueProvider; context : pointer) : JsValueRef;

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
    procedure iterateArray(arr : JsValueRef; valueConsumer : TJavascriptArrayValueConsumer; context : Pointer);

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
    constructor Create(list : TStringList);

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
    constructor Create(list : TObjectList<T>; objectDefinition : TJavascriptClassDefinition);

    // populating the array in the first place
    function count : integer; override;
    function item(i : integer) : JsValueRef; override;

    // operations from the script
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;



type
  TFslJavascript = class (TJavascript)
  protected
    procedure freeObject(obj : TObject); override;
  public
    function acquire : TFslJavascript; overload;
  end;

  TFslJavascriptTypeCategory = (jtcObject, jtcString, jtcInteger, jtcBoolean);

  TFslListManagerResolver = class abstract (TFslObject)
  public
    function category(source : TObject) : TFslJavascriptTypeCategory; virtual; abstract;
    function primitive(source : TObject) : String; virtual; abstract;
    function resolve(source : TObject) : TJavascriptClassDefinition; virtual; abstract;
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
    FClassResolver : TFslListManagerResolver;
  public
    constructor Create(list : TFslList<T>; def : TJavascriptClassDefinition); overload;
    constructor create(list: TFslList<T>; classResolver : TFslListManagerResolver); overload;
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

function base64AsString(value: TBytes): String;
function stringAsBase64(value: String): TBytes;


implementation


procedure FreeCallBack(ref: JsRef; callbackState: Pointer); {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
var
  js : TJavascript;
begin
  js := TJavascript(callbackState);
  js.freeObject(js.getWrapped<TObject>(ref));
end;

function GetterCallback(callee: JsValueRef; isConstructCall: byteBool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
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

function SetterCallback(callee: JsValueRef; isConstructCall: byteBool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
var
  prop : TJavascriptRegisteredProperty;
  p : PJsValueRefArray absolute arguments;
begin
  prop := TJavascriptRegisteredProperty(callbackState);
  try
    prop.FSetter(prop.FJavascript, prop, prop.FJavascript.getWrapped<TObject>(p[0]), p[1]);
    result := JS_INVALID_REFERENCE;
  except
    on e : Exception do
      result := prop.FJavascript.handleException(e);
  end;
end;

function RoutineCallback(callee: JsValueRef; isConstructCall: byteBool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
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

function factoryCallback(callee: JsValueRef; isConstructCall: byteBool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
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

function DoPush(callee: JsValueRef; isConstructCall: byteBool; arguments: PJsValueRef; argumentCount: Word; callbackState: Pointer): JsValueRef; {$IFDEF WINDOWS}stdcall;{$else}cdecl;{$endif}
var
  p : PJsValueRefArray absolute arguments;
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

function TJavascript.executeObj(script, scriptName, funcName: String; params: array of TFslObject): JsValueRef;
var
  p : array of TJsValue;
  i : integer;
begin
  setlength(p, length(params));
  for i := 0 to length(params) do
    p[i] := wrap(params[i], false);
  result := execute(script, scriptName, funcName, p);
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
  inherited create;

  if gjs <> nil then
    raise EJavascriptApplication.create('There is already a javascript runtime on this thread');
  gjs := self;
  FDefinedClasses := TDictionary<String,TJavascriptClassDefinition>.create;
  FOwnedObjects := TObjectList<TObject>.create;
  FNamespaces := TStringList.create;
  registerConsoleLog;
  FUseCount := 1; // this use, whatever it is.
  init;
end;

function TJavascript.defineClass(name: String; context: Pointer; factory: TJsFactoryFunction): TJavascriptClassDefinition;
begin
  result := defineClass(name, context, name, factory);
end;

procedure TJavascript.defineNamespace(name: String);
begin
  if FNamespaces.IndexOf(name) > -1 then
    raise EJavascriptApplication.Create('Namespace '+name+' is already registered');
  FNamespaces.Add(name);
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
  FNamespaces.Free;
  gjs := nil;
  inherited;
end;

var
  GRuntimeCounter : integer = 0;

procedure TJavascript.init;
var
  def : TJavascriptClassDefinition;
  global : TJsValue;
  f : JsValueRef;
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
  line : String;
  lineRef : JsValueRef;
  eClass : EFslExceptionClass;
begin
  line := '';
  if code > JsNoError then
  begin
    app := false;
    s := '';
    if JsGetAndClearException(exception) = JsNoError then
    begin
      app := FApplicationError = exception;
      s := ': ' +asString(exception);
      if not app then
      begin
        lineRef := getPropertyValue(exception, 'line');
        if (lineRef <> nil) then
          line := asString(lineRef);
      end;
    end;
    if app then
      eClass := EJavascriptApplication
    else
      eClass := EJavascriptHost;
    FApplicationError := nil;
    case code of
      JsErrorCategoryUsage : raise eClass.create('JsErrorCategoryUsage'+s);
      JsErrorInvalidArgument : raise eClass.create('JsErrorInvalidArgument'+s);
      JsErrorNullArgument : raise eClass.create('JsErrorNullArgument'+s);
      JsErrorNoCurrentContext : raise eClass.create('JsErrorNoCurrentContext'+s);
      JsErrorInExceptionState : raise eClass.create('JsErrorInExceptionState'+s);
      JsErrorNotImplemented : raise eClass.create('JsErrorNotImplemented'+s);
      JsErrorWrongThread : raise eClass.create('JsErrorWrongThread'+s);
      JsErrorRuntimeInUse : raise eClass.create('JsErrorRuntimeInUse'+s);
      JsErrorBadSerializedScript : raise eClass.create('JsErrorBadSerializedScript'+s);
      JsErrorInDisabledState : raise eClass.create('JsErrorInDisabledState'+s);
      JsErrorCannotDisableExecution : raise eClass.create('JsErrorCannotDisableExecution'+s);
      JsErrorHeapEnumInProgress : raise eClass.create('JsErrorHeapEnumInProgress'+s);
      JsErrorArgumentNotObject : raise eClass.create('JsErrorArgumentNotObject'+s);
      JsErrorInProfileCallback : raise eClass.create('JsErrorInProfileCallback'+s);
      JsErrorInThreadServiceCallback : raise eClass.create('JsErrorInThreadServiceCallback'+s);
      JsErrorCannotSerializeDebugScript : raise eClass.create('JsErrorCannotSerializeDebugScript'+s);
      JsErrorAlreadyDebuggingContext : raise eClass.create('JsErrorAlreadyDebuggingContext'+s);
      JsErrorAlreadyProfilingContext : raise eClass.create('JsErrorAlreadyProfilingContext'+s);
      JsErrorIdleNotEnabled : raise eClass.create('JsErrorIdleNotEnabled'+s);
      JsCannotSetProjectionEnqueueCallback : raise eClass.create('JsCannotSetProjectionEnqueueCallback'+s);
      JsErrorCannotStartProjection : raise eClass.create('JsErrorCannotStartProjection'+s);
      JsErrorInObjectBeforeCollectCallback : raise eClass.create('JsErrorInObjectBeforeCollectCallback'+s);
      JsErrorObjectNotInspectable : raise eClass.create('JsErrorObjectNotInspectable'+s);
      JsErrorPropertyNotSymbol : raise eClass.create('JsErrorPropertyNotSymbol'+s);
      JsErrorPropertyNotString : raise eClass.create('JsErrorPropertyNotString'+s);
      JsErrorInvalidContext : raise eClass.create('JsErrorInvalidContext'+s);
      JsInvalidModuleHostInfoKind : raise eClass.create('JsInvalidModuleHostInfoKind'+s);
      JsErrorModuleParsed : raise eClass.create('JsErrorModuleParsed'+s);
      JsErrorModuleEvaluated : raise eClass.create('JsErrorModuleEvaluated'+s);
      JsErrorCategoryEngine : raise eClass.create('JsErrorCategoryEngine'+s);
      JsErrorOutOfMemory : raise eClass.create('JsErrorOutOfMemory'+s);
      JsErrorBadFPUState : raise eClass.create('JsErrorBadFPUState'+s);
      JsErrorCategoryScript : raise eClass.create('JsErrorCategoryScript'+s);
      JsErrorScriptException : raise eClass.create(s.Substring(1).trim);
      JsErrorScriptCompile :
        if StringIsInteger32(line) then
          raise EParserException.create(s.Substring(1).trim, TSourceLocation.Create(StrToInt(line), 1))
        else
          raise eClass.create('JsErrorScriptCompile'+s);
      JsErrorScriptTerminated : raise eClass.create('JsErrorScriptTerminated'+s);
      JsErrorScriptEvalDisabled : raise eClass.create('JsErrorScriptEvalDisabled'+s);
      JsErrorCategoryFatal : raise eClass.create('JsErrorCategoryFatal'+s);
      JsErrorFatal : raise eClass.create('JsErrorFatal'+s);
      JsErrorWrongRuntime : raise eClass.create('JsErrorWrongRuntime'+s);
      JsErrorCategoryDiagError : raise eClass.create('JsErrorCategoryDiagError'+s);
      JsErrorDiagAlreadyInDebugMode : raise eClass.create('JsErrorDiagAlreadyInDebugMode'+s);
      JsErrorDiagNotInDebugMode : raise eClass.create('JsErrorDiagNotInDebugMode'+s);
      JsErrorDiagNotAtBreak : raise eClass.create('JsErrorDiagNotAtBreak'+s);
      JsErrorDiagInvalidHandle : raise eClass.create('JsErrorDiagInvalidHandle'+s);
      JsErrorDiagObjectNotFound : raise eClass.create('JsErrorDiagObjectNotFound'+s);
      JsErrorDiagUnableToPerformAction : raise eClass.create('JsErrorDiagUnableToPerformAction'+s);
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

function TJavascript.defineClass(name : String; context : Pointer; factoryName : String; factory : TJsFactoryFunction) : TJavascriptClassDefinition;
var
  global, f : TJsValue;
begin
  if hasDefinedClass(name) then
    raise EJavascriptApplication.create('Attempt to redefine '+name);

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
    raise EJavascriptApplication.create('Attempt to redefine '+name);

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

class function TJavascript.acquire(clss: TJavascriptClass): TJavascript;
begin
  if gjs = nil then
    result := clss.Create
  else if gjs is clss then
  begin
    result := gjs;
    inc(result.FUseCount);
  end
  else
    raise EJavascriptApplication.Create('Attempt to acquire a javascript engine of the wrong class (exists: '+gjs.className+', attempt to use '+clss.ClassName+')');
end;

class function TJavascript.acquire: TJavascript;
begin
  result := acquire(TJavascript);
end;

procedure TJavascript.addGlobal(name : AnsiString; obj : TJsValue);
var
  global : JsValueRef;
begin
  jsCheck(JsGetGlobalObject(global));
  jsCheck(JsSetProperty(global, getPropertyId(name), obj, true));
end;

function TJavascript.execute(script: String; scriptName : String): JsValueRef;
begin
  result := ChakraCoreUtils.JsRunScript(script, scriptName);
end;

function TJavascript.execute(script : String; scriptName, funcName: String; params: Array of TJsValue): JsValueRef;
var
  global, func, scriptJ, scriptNameJ, res : JsValueRef;
  pl : PJsValueRefArray;
  i : integer;
  sn : AnsiString;
  scriptW :WideString;
  vType : JsValueType;
begin
  sn := ansiString(scriptName);
  scriptW := script;
  if FStrict then
    script := '"use strict"; '+script;

  // parse + initialise the script
  jsCheck(JsCreateString(PAnsiChar(sn), Length(scriptName), scriptNameJ));
  jsCheck(JsCreateExternalArrayBuffer(PWideChar(scriptW), Length(scriptW) * SizeOf(WideChar), nil, nil, scriptJ));
  jsCheck(JsRun(scriptJ, 0, scriptNameJ, [JsParseScriptAttributeArrayBufferIsUtf16Encoded], res));

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
    jsCheck(JsCallFunction(func, pointer(pl), 1 + length(params), @result));
  finally
    Freemem(pl);
  end;
end;

function TJavascript.wrap(s: String): JsValueRef;
var
  sw : WideString;
begin
  if s = '' then
    result := getNull
  else
  begin
    sw := s;
    jsCheck(JsPointerToString(PWidechar(sw), length(sw), result));
  end;
end;

function TJavascript.wrap(i: integer): JsValueRef;
begin
  jsCheck(JsIntToNumber(i, result));
end;

function TJavascript.wrap(b: boolean): JsValueRef;
begin
  jsCheck(JsBoolToBoolean(b, result));
end;

function TJavascript.wrap(o : TObject; className : String; owns : boolean; immutable : boolean = false) : JsValueRef;
var
  def : TJavascriptClassDefinition;
begin
  if o = nil then
    result := getNull
  else if FDefinedClasses.TryGetValue(className, def) then
    result := wrap(o, def, owns, immutable)
  else
  begin
    if owns then
      freeObject(o);
    raise EJavascriptHost.Create('Attempt to use unknown class "'+className+'"');
  end;
end;

function TJavascript.wrap(o : TObject; classDef : TJavascriptClassDefinition; owns : boolean; immutable : boolean = false) : JsValueRef;
var
  p : TJavascriptRegisteredProperty;
  d : JsValueRef;
  ok : bytebool;
  args : PJsValueRefArray;
  global, obj, func, res : JsValueRef;
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
      if not FImmutableObjects then
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
  if immutable or FImmutableObjects then
  begin
    jsCheck(JsGetGlobalObject(global));
    jsCheck(jsGetProperty(global, getPropertyId('Object'), obj));
    jsCheck(JsGetProperty(obj, getPropertyId('freeze'), func));
    GetMem(args, 2 * SizeOf(JsValueRef));
    try
      args[0] := global;
      args[1] := result;
      jsCheck(JsCallFunction(func, pointer(args), 2, @res));
    finally
      Freemem(args);
    end;
  end;
end;

function TJavascript.asString(val: JsValueRef) : String;
var
  p : PWideChar;
  str : JsValueRef;
  l : NativeUInt;
begin
  // Convert your script result to String in JavaScript; redundant if your script returns a String
  jsCheck(JsConvertValueToString(val, str));
  // Project script result back to C++.
  jsCheck(JsStringToPointer(str, p, l));
  result := p;
end;

function TJavascript.checkSetupDefinitions(definitions : String): string;
begin
  // nothing
  result := '';
end;

function TJavascript.compile(script, scriptName: String): JsValueRef;
const
  ParseScriptAttributes: array[Boolean] of JsParseScriptAttributes = ([], [JsParseScriptAttributeLibraryCode]);
var
  res : JsErrorCode;
  ss, sn : JsValueRef;
begin
  ss := StringToJsString(script);
  sn := StringToJsString(scriptName);
  res := JsParse(ss, 0, sn, ParseScriptAttributes[false], result);
  jsCheck(res);
end;

function TJavascript.asBoolean(val: JsValueRef): boolean;
var
  v : integer;
  b : ByteBool;
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
    JsBoolean :
      begin
      jsCheck(JsBooleanToBool(val, b));
      result := b;
      end;
    JsObject : result := true;
    JsFunction : result := false; // should evaluate it??
    JsError : result := false;
    JsArray : result := false;
    JsSymbol : result := false;
    JsArrayBuffer  : result := false;
    JsTypedArray  : result := false;
    JsDataView : result := false;
  else
    result := false;
  end;
end;


function TJavascript.asInteger(val: JsValueRef): integer;
begin
  result := StrToInt(asString(val));
end;

function TJavascript.getType(v: JsValueRef): JsValueType;
begin
  jsCheck(JsGetValueType(v, result));
end;

function TJavascript.getWrapped<T>(obj: JsValueRef): T;
var
  data : Pointer;
  ok : byteBool;
begin
  jsCheck(JsHasExternalData(obj, ok));
  if not ok then
    exit(T(nil));

  jsCheck(JsGetExternalData(obj, data));
  result := T(data);
end;

function TJavascript.getWrappedObj(obj: JsValueRef): TObject;
var
  data : Pointer;
  ok : byteBool;
begin
  jsCheck(JsHasExternalData(obj, ok));
  if not ok then
    exit(nil);

  jsCheck(JsGetExternalData(obj, data));
  result := TObject(data);
end;

function TJavascript.getProperty(obj: JsValueRef; name: String): JsValueRef;
begin
  jsCheck(JsGetProperty(obj, getPropertyId(name), result));
end;

function TJavascript.getPropertyId(name: String): JsPropertyIdRef;
{$IFNDEF FPC}
begin
  result := getPropertyId(AnsiString(name));
end;

function TJavascript.getPropertyId(name: AnsiString): JsPropertyIdRef;
{$ENDIF}
begin
  jsCheck(JsCreatePropertyId(PAnsiChar(name), length(name), result));
end;

function TJavascript.getPropertyValue(obj: JsValueRef; name: String): JsValueRef;
var
  id : JsPropertyIdRef;
  b : ByteBool;
  res : JsErrorCode;
begin
  id := getPropertyId(name);
  res := JsHasProperty(obj, id, b);
  if res = JsErrorArgumentNotObject then
    result := nil
  else if b then
    jsCheck(JsGetProperty(obj, id, result))
  else
    result := nil;
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

function TJavascript.wrap(o: TObject; owns: boolean; immutable : boolean = false): JsValueRef;
begin
  result := wrap(o, o.ClassName, owns, immutable);
end;

procedure TJavascript.yield;
begin
  dec(FUseCount);
  if FUseCount = 0 then
    free;
end;

function TJavascript.makeArray(count: integer; valueProvider: TJavascriptArrayValueProvider; context : pointer): JsValueRef;
var
  i : integer;
  vi, v : JsValueRef;
begin
  JsCheck(JsCreateArray(count, result));
  for i := 0 to count - 1 do
  begin
    v := valueProvider(self, context, i);
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

function TJavascript.namespaceDefined(name: String): boolean;
begin
  result := FNamespaces.IndexOf(name) > -1;
end;

function TJavascript.arrayLength(arr: JsValueRef): integer;
var
  v : JsValueRef;
begin
  jsCheck(JsGetProperty(arr, getPropertyId('length'), v));
  jsCheck(JsNumberToInt(v, result));
end;

procedure TJavascript.iterateArray(arr: JsValueRef; valueConsumer: TJavascriptArrayValueConsumer; context : Pointer);
var
  i : integer;
  vi, v : JsValueRef;
begin
  for i := 0 to arrayLength(arr) - 1 do
  begin
    vi := wrap(i);
    jsCheck(JsGetIndexedProperty(arr, vi, v));
    valueConsumer(self, context, i, v);
  end;
end;

procedure TJavascript.iterateProperties(value: JsValueRef; proc: TJavascriptPropertyConsumer; context : pointer);
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
    proc(self, context, n, getProperty(value, n));
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
    if o = T(nil) then
    begin
      pl[0] := p;
      o := FObjectDefinition.FFactory(FJavascript, FObjectDefinition, pl, owns) as T;
    end;
    Flist.add(o);
  end;
  result := FJavascript.wrap(FList.Count);
end;

function base64AsString(value: TBytes): String;
var
  s : String;
begin
  s := String(EncodeBase64(value));
  result := s.replace(#13#10, '');
end;

function stringAsBase64(value: String): TBytes;
begin
  result := DecodeBase64(value);
end;

{ TFslJavascript }

function TFslJavascript.acquire: TFslJavascript;
begin
  result := TFslJavascript(inherited acquire(TFslJavascript));
end;

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

constructor TFslListManager<T>.create(list: TFslList<T>; classResolver : TFslListManagerResolver);
begin
  inherited Create;
  FList := list;
  FClassResolver := classResolver;
end;

destructor TFslListManager<T>.Destroy;
begin
  FList.Free;
  FClassResolver.Free;
  inherited;
end;

function TFslListManager<T>.count: integer;
begin
  result := FList.Count;
end;

function TFslListManager<T>.item(i: integer): JsValueRef;
begin
  if FClassDefinition = nil then
    case FClassResolver.category(FList[i]) of
      jtcObject : result := FJavascript.wrap(FList[i].Link, FClassResolver.resolve(FList[i]), true);
      jtcString : result := FJavascript.wrap(FClassResolver.primitive(FList[i]));
      jtcInteger : result := FJavascript.wrap(StrToInt(FClassResolver.primitive(FList[i])));
      jtcBoolean : result := FJavascript.wrap(StrToBool(FClassResolver.primitive(FList[i])));
    end
  else
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
    if o = T(nil) then
    begin
      if FClassDefinition = nil then
        raise EJavascriptHost.Create('Push with nil object is only possible for lists with known types'); // not sure when this would be triggered
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
