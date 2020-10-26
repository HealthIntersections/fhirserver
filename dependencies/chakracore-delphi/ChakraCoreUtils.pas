(*

MIT License

Copyright (c) 2018 Ondrej Kelle

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

unit ChakraCoreUtils;

{$include common.inc}

interface

uses
  Classes, SysUtils,
{$ifdef HAS_WIDESTRUTILS}
  WideStrUtils,
{$endif}
  Compat, ChakraCommon, ChakraCore;

type
  EChakraCore = class(Exception)
  private
    ErrorCode: JsErrorCode;
    Error: JsValueRef;
  public
    constructor Create(AErrorCode: JsErrorCode; AError: JsValueRef = nil);
  end;
  EChakraCoreEngine = class(EChakraCore);
  EChakraCoreScript = class(EChakraCore)
  private
    Line: Integer;
    Column: Integer;
    Source: UnicodeString;
    ScriptURL: UnicodeString;
    FJSEngineError : string;
  public
    constructor Create(AErrorCode: JsErrorCode);
    property JSEngineError : String read FJSEngineError;
  end;

  EChakraCoreFatal = class(EChakraCore);
  EChakraCoreDiag = class(EChakraCore);

  TErrorType = (etGenericError, etRangeError, etReferenceError, etSyntaxError, etTypeError, etUriError);

  TJsEnumArrayFunc = function(Value: JsValueRef; Index: Integer; ElementValue: JsValueRef; Data: Pointer): Boolean;
  TJsEnumPropertyFunc = function(Value: JsValueRef; const PropName: UnicodeString; PropValue: JsValueRef;
    Data: Pointer): Boolean;

  PJsValueRefArray = ^TJsValueRefArray;
  TJsValueRefArray = array[0..255] of JsValueRef;
  JsValueRefArray = array of JsValueRef;

procedure ChakraCoreCheck(ErrorCode: JsErrorCode);
procedure RaiseChakraCoreError(ErrorCode: JsErrorCode);
procedure RaiseError(Error: JsValueRef);

function BooleanToJsBoolean(Value: Boolean): JsValueRef;
function DoubleToJsNumber(Value: Double): JsValueRef;
function IntToJsNumber(Value: Integer): JsValueRef;
function JsFalseValue: JsValueRef;
function JsNullValue: JsValueRef;
function JsUndefinedValue: JsValueRef;
function JsTrueValue: JsValueRef;
function StringToJsString(const S: UTF8String): JsValueRef; overload;
function StringToJsString(const S: UnicodeString): JsValueRef; overload;

function JsEscapeString(const S: UTF8String): UTF8String; overload;
function JsEscapeString(const S: UnicodeString): UnicodeString; overload;

function JsCreateArray(Length: Integer): JsValueRef;
function JsArrayLength(Value: JsValueRef): Integer;
function JsArrayGetElement(Value: JsValueRef; Index: Integer): JsValueRef;
procedure JsArraySetElement(Value: JsValueRef; Index: Integer; ElementValue: JsValueRef);
function StringsToJsArray(const Strings: array of UnicodeString): JsValueRef; overload;
function StringsToJsArray(const Strings: array of UTF8String): JsValueRef; overload;

function JsBooleanToBoolean(Value: JsValueRef): Boolean;
function JsNumberToDouble(Value: JsValueRef): Double;
function JsNumberToInt(Value: JsValueRef): Integer;
function JsStringToUnicodeString(Value: JsValueRef): UnicodeString;
function JsStringToUTF8String(Value: JsValueRef): UTF8String;
procedure JsEnumArray(Value: JsValueRef; EnumFunc: TJsEnumArrayFunc; Data: Pointer = nil);
procedure JsEnumProperties(Value: JsValueRef; EnumFunc: TJsEnumPropertyFunc; Data: Pointer = nil);

function JsInspect(Value: JsValueRef): UnicodeString;
function JsInspectArray(Value: JsValueRef): UnicodeString;
function JsInspectObject(Value: JsValueRef): UnicodeString;

function JsContextOf(Instance: JsValueRef): JsContextRef;
function JsGlobal: JsValueRef;
function JsGetPrototype(Instance: JsValueRef): JsValueRef;
function JsInstanceOf(Instance: JsValueRef; const ConstructorName: UnicodeString; ThisArg: JsValueRef = nil): Boolean;
  overload;
function JsInstanceOf(Instance, AConstructor: JsValueRef): Boolean; overload;
function JsNew(const ConstructorName: UTF8String; const Args: array of JsValueRef;
  ThisArg: JsValueRef = nil): JsValueRef; overload;
function JsNew(const ConstructorName: UnicodeString; const Args: array of JsValueRef;
  ThisArg: JsValueRef = nil): JsValueRef; overload;
function JsNew(AConstructor, ACallee: JsValueRef; const Args: array of JsValueRef): JsValueRef; overload;

function JsValueAsJsBoolean(Value: JsValueRef): JsValueRef;
function JsValueAsJsNumber(Value: JsValueRef): JsValueRef;
function JsValueAsJsObject(Value: JsValueRef): JsValueRef;
function JsValueAsJsString(Value: JsValueRef): JsValueRef;

function JsEqual(Value1, Value2: JsValueRef; AStrict: Boolean = False): Boolean;
function JsGetErrorMessage(ErrorCode: JsErrorCode): PResStringRec;

function JsCreateObject(APrototype: JsValueRef = nil): JsValueRef;
function JsCreateSymbol(const Description: UnicodeString = ''): JsValueRef;
function JsCreateNativeTypedArray(AType: JsTypedArrayType; ALength: Integer): JsValueRef;

function JsGetExternalData(Value: JsValueRef): Pointer;
procedure JsSetExternalData(Value: JsValueRef; Data: Pointer);
function JsGetValueType(Value: JsValueRef): JsValueType;
function JsGetTypedArrayType(Value: JsValueRef): JsTypedArrayType;

function JsCallFunction(Func: JsValueRef; Args: PJsValueRef; ArgCount: Word): JsValueRef; overload;
function JsCallFunction(Func: JsValueRef; const Args: array of JsValueRef; ThisArg: JsValueRef = nil): JsValueRef;
  overload;
function JsCallFunction(const FunctionName: UTF8String; const Args: array of JsValueRef;
  ThisArg: JsValueRef = nil): JsValueRef; overload;
function JsCallFunction(const FunctionName: UnicodeString; const Args: array of JsValueRef;
  ThisArg: JsValueRef = nil): JsValueRef; overload;

function JsGetProperty(Value, Prop: JsValueRef): JsValueRef; overload;
function JsGetProperty(Value: JsValueRef; PropId: JsPropertyIdRef): JsValueRef; overload;
function JsGetProperty(Value: JsValueRef; const PropName: UTF8String): JsValueRef; overload;
function JsGetProperty(Value: JsValueRef; const PropName: UnicodeString): JsValueRef; overload;
function JsTryGetProperty(Value, Prop: JsValueRef; out PropValue: JsValueRef): Boolean; overload;
function JsTryGetProperty(Value: JsValueRef; const PropName: UTF8String; out PropValue: JsValueRef): Boolean; overload;
function JsTryGetProperty(Value: JsValueRef; const PropName: UnicodeString; out PropValue: JsValueRef): Boolean;
  overload;
function JsHasException: Boolean;
function JsHasExternalData(Value: JsValueRef): Boolean;
function JsHasProperty(Value, Prop: JsValueRef): Boolean; overload;
function JsHasProperty(Value: JsValueRef; const PropId: JsPropertyIdRef): Boolean; overload;
function JsHasProperty(Value: JsValueRef; const PropName: UTF8String): Boolean; overload;
function JsHasProperty(Value: JsValueRef; const PropName: UnicodeString): Boolean; overload;
function JsCreatePropertyDescriptor(Configurable, Enumerable: Boolean; GetAccessor, SetAccessor: JsValueRef;
  UseStrictRules: Boolean = True): JsValueRef; overload;
function JsDefineProperty(PropId: JsPropertyIdRef; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef = nil; UseStrictRules: Boolean = True): Boolean; overload;
function JsDefineProperty(const PropName: UTF8String; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef = nil; UseStrictRules: Boolean = True): Boolean; overload;
function JsDefineProperty(const PropName: UnicodeString; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef = nil; UseStrictRules: Boolean = True): Boolean; overload;

function JsRunScript(const Script, Name: UTF8String; SourceContext: NativeUInt = 0; IsLibraryCode: Boolean = False): JsValueRef; overload;
function JsRunScript(const Script, Name: UnicodeString; SourceContext: NativeUInt = 0; IsLibraryCode: Boolean = False): JsValueRef; overload;
function JsCreateFunction(Callback: JsNativeFunction; CallbackState: Pointer; const Name: UTF8String = ''): JsValueRef;
  overload;
function JsCreateFunction(Callback: JsNativeFunction; CallbackState: Pointer;
  const Name: UnicodeString = ''): JsValueRef; overload;
procedure JsCreatePromise(out Promise, ResolveFunc, RejectFunc: JsValueRef);
function JsSetCallback(Instance: JsValueRef; const CallbackName: UTF8String; Callback: JsNativeFunction;
  CallbackState: Pointer; UseStrictRules: Boolean = True): JsValueRef; overload;
function JsSetCallback(Instance: JsValueRef; const CallbackName: UnicodeString; Callback: JsNativeFunction;
  CallbackState: Pointer; UseStrictRules: Boolean = True): JsValueRef; overload;
procedure JsSetProperty(Instance, Prop, Value: JsValueRef; UseStrictRules: Boolean = True); overload;
procedure JsSetProperty(Instance: JsValueRef; PropId: JsPropertyIdRef; Value: JsValueRef;
  UseStrictRules: Boolean = True); overload;
procedure JsSetProperty(Instance: JsValueRef; const PropName: UTF8String; Value: JsValueRef;
  UseStrictRules: Boolean = True); overload;
procedure JsSetProperty(Instance: JsValueRef; const PropName: UnicodeString; Value: JsValueRef;
  UseStrictRules: Boolean = True); overload;
function JsCreateError(const AMessage: UTF8String; ErrorType: TErrorType = etGenericError): JsValueRef; overload;
function JsCreateError(const AMessage: UnicodeString; ErrorType: TErrorType = etGenericError): JsValueRef; overload;
procedure JsThrowError(const AMessage: UTF8String; ErrorType: TErrorType = etGenericError); overload;
procedure JsThrowError(const AMessage: UnicodeString; ErrorType: TErrorType = etGenericError); overload;
procedure JsThrowError(Error: JsValueRef); overload;

function JsGetCurrentContext: JsContextRef;
function JsGetCurrentRuntime: JsRuntimeHandle;
function JsIsDebugging: Boolean;

type
  TJsInspectExceptionHandler = function (Value: JsValueRef; E: Exception): UnicodeString;

var
  JsInspectExceptionHandler: TJsInspectExceptionHandler = nil;

implementation

uses
  ChakraDebug, ChakraCoreClasses;

resourcestring
  // Category of errors that relates to incorrect usage of the API itself
  SJsErrorInvalidArgument = 'An argument to a hosting API was invalid';
  SJsErrorNullArgument = 'An argument to a hosting API was null in a context where null is not allowed';
  SJsErrorNoCurrentContext = 'The hosting API requires that a context be current, but there is no current context';
  SJsErrorInExceptionState = 'The engine is in an exception state and no APIs can be called until the exception is cleared';
  SJsErrorNotImplemented = 'A hosting API is not yet implemented';
  SJsErrorWrongThread = 'A hosting API was called on the wrong thread';
  SJsErrorRuntimeInUse = 'A runtime that is still in use cannot be disposed';
  SJsErrorBadSerializedScript = 'A bad serialized script was used, or the serialized script was serialized by a different version of the Chakra engine';
  SJsErrorInDisabledState = 'The runtime is in a disabled state';
  SJsErrorCannotDisableExecution = 'Runtime does not support reliable script interruption';
  SJsErrorHeapEnumInProgress = 'A heap enumeration is currently underway in the script context';
  SJsErrorArgumentNotObject = 'A hosting API that operates on object values was called with a non-object value';
  SJsErrorInProfileCallback = 'A script context is in the middle of a profile callback';
  SJsErrorInThreadServiceCallback = 'A thread service callback is currently underway';
  SJsErrorCannotSerializeDebugScript = 'Scripts cannot be serialized in debug contexts';
  SJsErrorAlreadyDebuggingContext = 'The context cannot be put into a debug state because it is already in a debug state';
  SJsErrorAlreadyProfilingContext = 'The context cannot start profiling because it is already profiling';
  SJsErrorIdleNotEnabled = 'Idle notification given when the host did not enable idle processing';
  SJsCannotSetProjectionEnqueueCallback = 'The context did not accept the enqueue callback';
  SJsErrorCannotStartProjection = 'Failed to start projection';
  SJsErrorInObjectBeforeCollectCallback = 'The operation is not supported in an object before collect callback';
  SJsErrorObjectNotInspectable = 'Object cannot be unwrapped to IInspectable pointer';
  SJsErrorPropertyNotSymbol = 'A hosting API that operates on symbol property ids but was called with a non-symbol property id';
  SJsErrorPropertyNotString = 'A hosting API that operates on string property ids but was called with a non-string property id';
  SJsErrorInvalidContext = 'Module evaulation is called in wrong context';
  SJsInvalidModuleHostInfoKind = 'Invalid module host info kind';
  SJsErrorModuleParsed = 'Module was parsed already when JsParseModuleSource is called';
  SJsErrorModuleEvaluated = 'Module was evaluated already when JsModuleEvaluation is called';
  SJsNoWeakRefRequired = 'Argument passed to JsCreateWeakReference is a primitive that is not managed by the GC. No weak reference is required, the value will never be collected.';
  SJsErrorPromisePending = 'The Promise object is still in the pending state.';
  SJsErrorModuleNotEvaluated = 'Module was not yet evaluated when JsGetModuleNamespace was called.';

  // Category of errors that relates to errors occurring within the engine itself
  SJsErrorOutOfMemory = 'The Chakra engine has run out of memory';
  SJsErrorBadFPUState = 'The Chakra engine failed to set the Floating Point Unit state';

  // Category of errors that relates to errors in a script
  SJsErrorScriptException = 'A JavaScript exception occurred while running a script';
  SJsErrorScriptCompile = 'JavaScript failed to compile';
  SJsErrorScriptTerminated = 'A script was terminated due to a request to suspend a runtime';
  SJsErrorScriptEvalDisabled = 'A script was terminated because it tried to use eval or function and eval was disabled';

  // Category of errors that are fatal and signify failure of the engine
  SJsErrorFatal = 'A fatal error in the engine has occurred';
  SJsErrorWrongRuntime = 'A hosting API was called with object created on different javascript runtime';

  // Category of errors that are related to failures during diagnostic operations
  SJsErrorDiagAlreadyInDebugMode = 'The object for which the debugging API was called was not found';
  SJsErrorDiagNotInDebugMode = 'The debugging API can only be called when VM is in debug mode';
  SJsErrorDiagNotAtBreak = 'The debugging API can only be called when VM is at a break';
  SJsErrorDiagInvalidHandle = 'Debugging API was called with an invalid handle';
  SJsErrorDiagObjectNotFound = 'The object for which the debugging API was called was not found';
  SJsErrorDiagUnableToPerformAction = 'VM was unable to perfom the request action';

type
  JsErrorConstructor = function(_message: JsValueRef; out error: JsValueRef): JsErrorCode;
    {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

{ EChakra public }

constructor EChakraCore.Create(AErrorCode: JsErrorCode; AError: JsValueRef);
begin
  inherited CreateRes(JsGetErrorMessage(AErrorCode));
  ErrorCode := AErrorCode;
  Error := AError;
  if not Assigned(Error) then
    JsGetAndClearException(Error);
end;

{ EChakraCoreScript public }

constructor EChakraCoreScript.Create(AErrorCode: JsErrorCode);
var
  PropValue, MetaData: JsValueRef;
  SException: UnicodeString;
begin
  SException := '';
  if JsGetAndClearExceptionWithMetadata(MetaData) = JsNoError then
  begin
    Line := JsNumberToInt(JsGetProperty(MetaData, 'line'));
    Column := JsNumberToInt(JsGetProperty(MetaData, 'column'));
    Source := JsStringToUnicodeString(JsGetProperty(MetaData, 'source'));
    ScriptURL := JsStringToUnicodeString(JsGetProperty(MetaData, 'url'));
    Error := JsGetProperty(MetaData, 'exception');
  end
  else if JsGetAndClearException(Error) = JsNoError then
  begin
    if JsTryGetProperty(Error, 'line', PropValue) and (JsGetValueType(PropValue) = JsNumber) then
      Line := JsNumberToInt(PropValue);
    if JsTryGetProperty(Error, 'column', PropValue) and (JsGetValueType(PropValue) = JsNumber) then
      Column := JsNumberToInt(PropValue);
    if JsTryGetProperty(Error, 'source', PropValue) and (JsGetValueType(PropValue) = JsString) then
      Source := JsStringToUnicodeString(PropValue);
    if JsTryGetProperty(Error, 'url', PropValue) and (JsGetValueType(PropValue) = JsString) then
      ScriptURL := JsStringToUnicodeString(PropValue);
  end;
  inherited Create(AErrorCode, Error);
  if Assigned(Error) then
    SException := JsStringToUnicodeString(Error);

  FJSEngineError := Message;
  if SException <> '' then
  begin
{$ifdef UNICODE}
    Message := SException;
{$else}
    Message := UTF8Encode(SException);
{$endif}
  end;
end;

function BooleanToJsBoolean(Value: Boolean): JsValueRef;
begin
  ChakraCoreCheck(JsBoolToBoolean(Value, Result));
end;

procedure ChakraCoreCheck(ErrorCode: JsErrorCode);
begin
  if ErrorCode <> JsNoError then
    RaiseChakraCoreError(ErrorCode);
end;

procedure RaiseChakraCoreError(ErrorCode: JsErrorCode);
begin
  case JsErrorCode(Ord(ErrorCode) and $F0000) of
    // JsErrorCategoryUsage: ;
    JsErrorCategoryEngine:
      raise EChakraCoreEngine.Create(ErrorCode);
    JsErrorCategoryScript:
      raise EChakraCoreScript.Create(ErrorCode);
    JsErrorCategoryFatal:
      raise EChakraCoreFatal.Create(ErrorCode);
    JsErrorCategoryDiagError:
      raise EChakraCoreDiag.Create(ErrorCode);
    else
      raise EChakraCore.Create(ErrorCode);
  end;
end;

procedure RaiseError(Error: JsValueRef);
begin
  if not Assigned(Error) then
    Exit;

{$ifdef UNICODE}
  raise Exception.Create(JsInspect(Error));
{$else}
  raise Exception.Create(UTF8Encode(JsInspect(Error)));
{$endif}
end;

function DoubleToJsNumber(Value: Double): JsValueRef;
begin
  ChakraCoreCheck(JsDoubleToNumber(Value, Result));
end;

function IntToJsNumber(Value: Integer): JsValueRef;
begin
  ChakraCoreCheck(JsIntToNumber(Value, Result));
end;

function JsFalseValue: JsValueRef;
begin
  ChakraCoreCheck(JsGetFalseValue(Result));
end;

function JsNullValue: JsValueRef;
begin
  ChakraCoreCheck(JsGetNullValue(Result));
end;

function JsUndefinedValue: JsValueRef;
begin
  ChakraCoreCheck(JsGetUndefinedValue(Result));
end;

function JsTrueValue: JsValueRef;
begin
  ChakraCoreCheck(JsGetTrueValue(Result));
end;

function StringToJsString(const S: UTF8String): JsValueRef;
begin
  ChakraCoreCheck(JsCreateString(PAnsiChar(S), Length(S), Result));
end;

function StringToJsString(const S: UnicodeString): JsValueRef;
begin
  ChakraCoreCheck(JsCreateStringUtf16(PUnicodeChar(S), Length(S), Result));
end;

function JsEscapeString(const S: UTF8String): UTF8String;
begin
  Result := UTF8Encode(JsEscapeString(UTF8ToString(S)));
end;

// TODO see https://mathiasbynens.be/notes/javascript-escapes
function JsEscapeString(const S: UnicodeString): UnicodeString;
begin
  Result := S;
  Result := WideStringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := WideStringReplace(Result, #8, '\b', [rfReplaceAll]);
  Result := WideStringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := WideStringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := WideStringReplace(Result, #11, '\v', [rfReplaceAll]);
  Result := WideStringReplace(Result, #12, '\f', [rfReplaceAll]);
  Result := WideStringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := WideStringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := WideStringReplace(Result, '''', '\''', [rfReplaceAll]);
end;

function JsCreateArray(Length: Integer): JsValueRef;
begin
  ChakraCoreCheck(ChakraCommon.JsCreateArray(Length, Result));
end;

function JsArrayLength(Value: JsValueRef): Integer;
begin
  Result := JsNumberToInt(JsGetProperty(Value, 'length'));
end;

function JsArrayGetElement(Value: JsValueRef; Index: Integer): JsValueRef;
begin
  ChakraCoreCheck(JsGetIndexedProperty(Value, IntToJsNumber(Index), Result));
end;

procedure JsArraySetElement(Value: JsValueRef; Index: Integer; ElementValue: JsValueRef);
begin
  ChakraCoreCheck(JsSetIndexedProperty(Value, IntToJsNumber(Index), ElementValue));
end;

function StringsToJsArray(const Strings: array of UnicodeString): JsValueRef;
var
  L, I: Integer;
begin
  L := Length(Strings);
  Result := JsCreateArray(L);
  for I := 0 to L - 1 do
    JsArraySetElement(Result, I, StringToJsString(Strings[I]));
end;

function StringsToJsArray(const Strings: array of UTF8String): JsValueRef;
var
  L, I: Integer;
begin
  L := Length(Strings);
  Result := JsCreateArray(L);
  for I := 0 to L - 1 do
    JsArraySetElement(Result, I, StringToJsString(Strings[I]));
end;

function JsBooleanToBoolean(Value: JsValueRef): Boolean;
var
  B: ByteBool;
begin
  ChakraCoreCheck(JsBooleanToBool(Value, B));
  Result := B;
end;

function JsNumberToDouble(Value: JsValueRef): Double;
begin
  Result := -1;
  ChakraCoreCheck(ChakraCommon.JsNumberToDouble(Value, Result));
end;

function JsNumberToInt(Value: JsValueRef): Integer;
begin
  Result := -1;
  ChakraCoreCheck(ChakraCommon.JsNumberToInt(Value, Result));
end;

function JsStringToUnicodeString(Value: JsValueRef): UnicodeString;
var
  StringValue: JsValueRef;
  StringLength: size_t;
begin
  Result := '';
  StringValue := JsValueAsJsString(Value);
  StringLength := 0;
  ChakraCoreCheck(JsCopyStringUtf16(StringValue, 0, -1, nil, @StringLength));
  if StringLength > 0 then
  begin
    SetLength(Result, StringLength);
    ChakraCoreCheck(JsCopyStringUtf16(StringValue, 0, StringLength, PUnicodeChar(Result), nil));
  end;
end;

function JsStringToUTF8String(Value: JsValueRef): UTF8String;
var
  StringValue: JsValueRef;
  StringLength: size_t;
begin
  Result := '';
  StringValue := JsValueAsJsString(Value);
  StringLength := 0;
  ChakraCoreCheck(JsCopyString(StringValue, nil, 0, @StringLength));
  if StringLength > 0 then
  begin
    SetLength(Result, StringLength);
    ChakraCoreCheck(JsCopyString(StringValue, PAnsiChar(Result), StringLength, nil));
  end;
end;

procedure JsEnumArray(Value: JsValueRef; EnumFunc: TJsEnumArrayFunc; Data: Pointer);
var
  I: Integer;
  ElementValue: JsValueRef;
begin
  if JsGetValueType(Value) <> JsArray then
    Exit;

  for I := 0 to JsArrayLength(Value) - 1 do
  begin
    ElementValue := JsArrayGetElement(Value, I);
    if EnumFunc(Value, I, ElementValue, Data) then
      Break;
  end;
end;

type
  PListPropData = ^TListPropData;
  TListPropData = record
    Instance: JsValueRef;
    EnumFunc: TJsEnumPropertyFunc;
    Data: Pointer;
  end;

function _EnumPropertyFunc(Value: JsValueRef; Index: Integer; ElementValue: JsValueRef; Data: Pointer): Boolean;
var
  ListPropData: PListPropData absolute Data;
  PropName: UnicodeString;
begin
  Result := False;
  PropName := JsStringToUnicodeString(ElementValue);
  ListPropData^.EnumFunc(ListPropData^.Instance, PropName, JsGetProperty(ListPropData^.Instance, PropName),
    ListPropData^.Data);
end;

procedure JsEnumProperties(Value: JsValueRef; EnumFunc: TJsEnumPropertyFunc; Data: Pointer);
var
  PropNames: JsValueRef;
  ListPropData: TListPropData;
begin
  ChakraCoreCheck(JsGetOwnPropertyNames(Value, PropNames));
  if JsGetValuetype(PropNames) = JsArray then
  begin
    ListPropData.Instance := Value;
    ListPropData.EnumFunc := EnumFunc;
    ListPropData.Data := Data;
    JsEnumArray(PropNames, _EnumPropertyFunc, @ListPropData);
  end;
end;

function JsInspect(Value: JsValueRef): UnicodeString;
begin
  Result := '';
  try
    case JsGetValueType(Value) of
      JsObject:
        Result := '{' + JsInspectObject(Value) + '}';
      JsArray:
        Result := '[' + JsInspectArray(Value) + ']';
      JsArrayBuffer:
        Result := '{' + JsInspectObject(Value) + '}';
      JsTypedArray:
        Result := '{' + JsInspectObject(Value) + '}';
      JsDataView:
        Result := '{' + JsInspectObject(Value) + '}';
      else
        Result := '"' + JsEscapeString(JsStringToUnicodeString(JsValueAsJsString(Value))) + '"';
    end;
  except
    on E: Exception do
    begin
      if not Assigned(JsInspectExceptionHandler) then
        raise;
      Result := Result + '"' + JsEscapeString(JsInspectExceptionHandler(Value, E)) + '"';
    end;
  end;
end;

function _JsInspectArrayElement(Value: JsValueRef; Index: Integer; ElementValue: JsValueRef; Data: Pointer): Boolean;
var
  S: PUnicodeString absolute Data;
begin
  Result := False;
  if Index > 0 then
    S^ := S^ + ',';
  S^ := S^ + JsInspect(ElementValue);
end;

function JsInspectArray(Value: JsValueRef): UnicodeString;
begin
  Result := '';
  JsEnumArray(Value, _JsInspectArrayElement, @Result);
end;

function _JsInspectObjectProperty(Instance: JsValueRef; const PropName: UnicodeString; PropValue: JsValueRef; Data: Pointer): Boolean;
var
  S: PUnicodeString absolute data;
begin
  Result := False;
  if S^ <> '' then
    S^ := S^ + ',';
  S^ := S^ + '"' + PropName + '":' + JsInspect(PropValue);
end;

function JsInspectObject(Value: JsValueRef): UnicodeString;
begin
  Result := '';
  JsEnumProperties(Value, _JsInspectObjectProperty, @Result);
end;

function JsContextOf(Instance: JsValueRef): JsContextRef;
begin
  ChakraCoreCheck(JsGetContextOfObject(Instance, Result));
end;

function JsGlobal: JsValueRef;
begin
  ChakraCoreCheck(JsGetGlobalObject(Result));
end;

function JsGetPrototype(Instance: JsValueRef): JsValueRef;
begin
  ChakraCoreCheck(ChakraCommon.JsGetPrototype(Instance, Result));
end;

function JsInstanceOf(Instance: JsValueRef; const ConstructorName: UnicodeString; ThisArg: JsValueRef): Boolean;
begin
  if not Assigned(ThisArg) then
    ThisArg := JsGlobal;
  Result := JsInstanceOf(Instance, JsGetProperty(ThisArg, ConstructorName));
end;

function JsInstanceOf(Instance, AConstructor: JsValueRef): Boolean;
var
  B: ByteBool;
begin
  ChakraCoreCheck(ChakraCommon.JsInstanceOf(Instance, AConstructor, B));
  Result := B;
end;

function JsNew(const ConstructorName: UTF8String; const Args: array of JsValueRef; ThisArg: JsValueRef): JsValueRef;
var
  AConstructor: JsValueRef;
begin
  if not Assigned(ThisArg) then
    ThisArg := JsGlobal;
  AConstructor := JsGetProperty(ThisArg, ConstructorName);
  Result := JsNew(AConstructor, AConstructor, Args);
end;

function JsNew(const ConstructorName: UnicodeString; const Args: array of JsValueRef; ThisArg: JsValueRef): JsValueRef;
var
  AConstructor: JsValueRef;
begin
  if not Assigned(ThisArg) then
    ThisArg := JsGlobal;
  AConstructor := JsGetProperty(ThisArg, ConstructorName);
  Result := JsNew(AConstructor, AConstructor, Args);
end;

function JsNew(AConstructor, ACallee: JsValueRef; const Args: array of JsValueRef): JsValueRef;
var
  NewArgs: JsValueRefArray;
  L: Integer;
begin
  // pass callee + args
  L := Length(Args);
  SetLength(NewArgs, L + 1);
  NewArgs[0] := ACallee;
  Move(Args[0], NewArgs[1], L * SizeOf(JsValueRef));
  ChakraCoreCheck(ChakraCommon.JsConstructObject(AConstructor, @NewArgs[0], L + 1, Result));
end;

function JsValueAsJsBoolean(Value: JsValueRef): JsValueRef;
begin
  ChakraCoreCheck(JsConvertValueToBoolean(Value, Result));
end;

function JsValueAsJsNumber(Value: JsValueRef): JsValueRef;
begin
  ChakraCoreCheck(JsConvertValueToNumber(Value, Result));
end;

function JsValueAsJsObject(Value: JsValueRef): JsValueRef;
begin
  ChakraCoreCheck(JsConvertValueToObject(Value, Result));
end;

function JsValueAsJsString(Value: JsValueRef): JsValueRef;
begin
  ChakraCoreCheck(JsConvertValueToString(Value, Result));
end;

function JsEqual(Value1, Value2: JsValueRef; AStrict: Boolean): Boolean;
var
  B: ByteBool;
begin
  if AStrict then
    ChakraCoreCheck(JsStrictEquals(Value1, Value2, B))
  else
    ChakraCoreCheck(JsEquals(Value1, Value2, B));
  Result := B;
end;

function JsGetErrorMessage(ErrorCode: JsErrorCode): PResStringRec;
begin
  case ErrorCode of
    // JsNoError
    // JsErrorCategoryUsage
    JsErrorInvalidArgument:
      Result := @SJsErrorInvalidArgument;
    JsErrorNullArgument:
      Result := @SJsErrorNullArgument;
    JsErrorNoCurrentContext:
      Result := @SJsErrorNoCurrentContext;
    JsErrorInExceptionState:
      Result := @SJsErrorInExceptionState;
    JsErrorNotImplemented:
      Result := @SJsErrorNotImplemented;
    JsErrorWrongThread:
      Result := @SJsErrorWrongThread;
    JsErrorRuntimeInUse:
      Result := @SJsErrorRuntimeInUse;
    JsErrorBadSerializedScript:
      Result := @SJsErrorBadSerializedScript;
    JsErrorInDisabledState:
      Result := @SJsErrorInDisabledState;
    JsErrorCannotDisableExecution:
      Result := @SJsErrorCannotDisableExecution;
    JsErrorHeapEnumInProgress:
      Result := @SJsErrorHeapEnumInProgress;
    JsErrorArgumentNotObject:
      Result := @SJsErrorArgumentNotObject;
    JsErrorInProfileCallback:
      Result := @SJsErrorInProfileCallback;
    JsErrorInThreadServiceCallback:
      Result := @SJsErrorInThreadServiceCallback;
    JsErrorCannotSerializeDebugScript:
      Result := @SJsErrorCannotSerializeDebugScript;
    JsErrorAlreadyDebuggingContext:
      Result := @SJsErrorAlreadyDebuggingContext;
    JsErrorAlreadyProfilingContext:
      Result := @SJsErrorAlreadyProfilingContext;
    JsErrorIdleNotEnabled:
      Result := @SJsErrorIdleNotEnabled;
    JsCannotSetProjectionEnqueueCallback:
      Result := @SJsCannotSetProjectionEnqueueCallback;
    JsErrorCannotStartProjection:
      Result := @SJsErrorCannotStartProjection;
    JsErrorInObjectBeforeCollectCallback:
      Result := @SJsErrorInObjectBeforeCollectCallback;
    JsErrorObjectNotInspectable:
      Result := @SJsErrorObjectNotInspectable;
    JsErrorPropertyNotSymbol:
      Result := @SJsErrorPropertyNotSymbol;
    JsErrorPropertyNotString:
      Result := @SJsErrorPropertyNotString;
    JsErrorInvalidContext:
      Result := @SJsErrorInvalidContext;
    JsInvalidModuleHostInfoKind:
      Result := @SJsInvalidModuleHostInfoKind;
    JsErrorModuleParsed:
      Result := @SJsErrorModuleParsed;
    JsErrorModuleEvaluated:
      Result := @SJsErrorModuleEvaluated;
    JsNoWeakRefRequired:
      Result := @SJsNoWeakRefRequired;
    JsErrorPromisePending:
      Result := @SJsErrorPromisePending;
    JsErrorModuleNotEvaluated:
      Result := @SJsErrorModuleNotEvaluated;
    // JsErrorCategoryEngine
    JsErrorOutOfMemory:
      Result := @SJsErrorOutOfMemory;
    JsErrorBadFPUState:
      Result := @SJsErrorBadFPUState;
    // JsErrorCategoryScript
    JsErrorScriptException:
      Result := @SJsErrorScriptException;
    JsErrorScriptCompile:
      Result := @SJsErrorScriptCompile;
    JsErrorScriptTerminated:
      Result := @SJsErrorScriptTerminated;
    JsErrorScriptEvalDisabled:
      Result := @SJsErrorScriptEvalDisabled;
    // JsErrorCategoryFatal
    JsErrorFatal:
      Result := @SJsErrorFatal;
    JsErrorWrongRuntime:
      Result := @SJsErrorWrongRuntime;
    // JsErrorCategoryDiagError
    JsErrorDiagAlreadyInDebugMode:
      Result := @SJsErrorDiagAlreadyInDebugMode;
    JsErrorDiagNotInDebugMode:
      Result := @SJsErrorDiagNotInDebugMode;
    JsErrorDiagNotAtBreak:
      Result := @SJsErrorDiagNotAtBreak;
    JsErrorDiagInvalidHandle:
      Result := @SJsErrorDiagInvalidHandle;
    JsErrorDiagObjectNotFound:
      Result := @SJsErrorDiagObjectNotFound;
    JsErrorDiagUnableToPerformAction:
      Result := @SJsErrorDiagUnableToPerformAction;
    else
      Result := nil;
  end;
end;

function JsCreateObject(APrototype: JsValueRef): JsValueRef;
begin
  if Assigned(APrototype) then // Object.create(prototype)
    Result := JsCallFunction('create', [APrototype], JsGetProperty(JsGlobal, 'Object'))
  else
    ChakraCoreCheck(ChakraCommon.JsCreateObject(Result));
end;

function JsCreateSymbol(const Description: UnicodeString): JsValueRef;
var
  Desc: JsValueRef;
begin
  Desc := JsNullValue;
  if Description <> '' then
    Desc := StringToJsString(Description);
  ChakraCoreCheck(ChakraCommon.JsCreateSymbol(Desc, Result));
end;

function JsCreateNativeTypedArray(AType: JsTypedArrayType; ALength: Integer): JsValueRef;
const
  ElementSizes: array[JsTypedArrayType] of Integer = (8, 8, 8, 16, 16, 32, 32, 32, 64);
var
  Base: TChakraCoreNativeArrayBuffer;
begin
  Base := TChakraCoreNativeArrayBuffer.Create(ALength * ElementSizes[AType]);
  try
    ChakraCoreCheck(JsCreateTypedArray(AType, Base.Handle, 0, ALength, Result));
  except
    Base.Free;
    raise;
  end;
end;

function JsGetExternalData(Value: JsValueRef): Pointer;
begin
  ChakraCoreCheck(ChakraCommon.JsGetExternalData(Value, Result));
end;

procedure JsSetExternalData(Value: JsValueRef; Data: Pointer);
begin
  ChakraCoreCheck(ChakraCommon.JsSetExternalData(Value, Data));
end;

function JsGetValueType(Value: JsValueRef): JsValueType;
begin
  ChakraCoreCheck(ChakraCommon.JsGetValueType(Value, Result));
end;

function JsGetTypedArrayType(Value: JsValueRef): JsTypedArrayType;
begin
  ChakraCoreCheck(JsGetTypedArrayInfo(Value, @Result, nil, nil, nil));
end;

function JsCallFunction(Func: JsValueRef; Args: PJsValueRef; ArgCount: Word): JsValueRef;
begin
  ChakraCoreCheck(ChakraCommon.JsCallFunction(Func, Args, ArgCount, @Result));
end;

function JsCallFunction(Func: JsValueRef; const Args: array of JsValueRef; ThisArg: JsValueRef): JsValueRef;
var
  L: Integer;
  NewArgs: array of JsValueRef;
begin
  if not Assigned(ThisArg) then
    ThisArg := JsGlobal;

  L := Length(Args);
  SetLength(NewArgs, L + 1);
  NewArgs[0] := ThisArg;
  if L > 0 then
    Move(Args[0], NewArgs[1], L * SizeOf(JsValueRef));
  Result := JsCallFunction(Func, @NewArgs[0], L + 1);
end;

function JsCallFunction(const FunctionName: UTF8String; const Args: array of JsValueRef;
  ThisArg: JsValueRef): JsValueRef;
var
  Func: JsValueRef;
begin
  if not Assigned(ThisArg) then
    ThisArg := JsGlobal;
  Func := JsGetProperty(ThisArg, FunctionName);
  Result := JsCallFunction(Func, Args, ThisArg);
end;

function JsCallFunction(const FunctionName: UnicodeString; const Args: array of JsValueRef;
  ThisArg: JsValueRef): JsValueRef;
begin
  Result := JsCallFunction(UTF8Encode(FunctionName), Args, ThisArg);
end;

function JsGetProperty(Value, Prop: JsValueRef): JsValueRef;
var
  PropId: JsPropertyIdRef;
begin
  case JsGetValueType(Prop) of
    JsSymbol:
      begin
        ChakraCoreCheck(JsGetPropertyIdFromSymbol(Prop, PropId));
        Result := JsGetProperty(Value, PropId)
      end;
    else
      Result := JsGetProperty(Value, JsStringToUTF8String(Prop));
  end;
end;

function JsGetProperty(Value: JsValueRef; PropId: JsPropertyIdRef): JsValueRef;
begin
  ChakraCoreCheck(ChakraCommon.JsGetProperty(Value, PropId, Result));
end;

function JsGetProperty(Value: JsValueRef; const PropName: UTF8String): JsValueRef;
var
  PropId: JsPropertyIdRef;
begin
  ChakraCoreCheck(JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId));
  Result := JsGetProperty(Value, PropId);
end;

function JsGetProperty(Value: JsValueRef; const PropName: UnicodeString): JsValueRef;
begin
  Result := JsGetProperty(Value, UTF8Encode(PropName));
end;

function JsTryGetProperty(Value, Prop: JsValueRef; out PropValue: JsValueRef): Boolean;
var
  PropName: UTF8String;
  PropId: JsPropertyIdRef;
begin
  case JsGetValueType(Prop) of
    JsSymbol:
      Result := (JsGetPropertyIdFromSymbol(Prop, PropId) = JsNoError) and
        (ChakraCommon.JsGetProperty(Value, PropId, PropValue) = JsNoError);
    else
    begin
      PropName := JsStringToUTF8String(Prop);
      Result := (JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId) = JsNoError) and
        (ChakraCommon.JsGetProperty(Value, PropId, PropValue) = JsNoError);
    end;
  end;
end;

function JsTryGetProperty(Value: JsValueRef; const PropName: UTF8String; out PropValue: JsValueRef): Boolean;
var
  PropId: JsPropertyIdRef;
begin
  Result := (JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId) = JsNoError) and
    (ChakraCommon.JsGetProperty(Value, PropId, PropValue) = JsNoError);
end;

function JsTryGetProperty(Value: JsValueRef; const PropName: UnicodeString; out PropValue: JsValueRef): Boolean;
begin
  Result := JsTryGetProperty(Value, UTF8Encode(PropName), PropValue);
end;

function JsHasException: Boolean;
var
  B: ByteBool;
begin
  ChakraCoreCheck(ChakraCommon.JsHasException(B));
  Result := B;
end;

function JsHasExternalData(Value: JsValueRef): Boolean;
var
  B: ByteBool;
begin
  ChakraCoreCheck(ChakraCommon.JsHasExternalData(Value, B));
  Result := B;
end;

function JsHasProperty(Value, Prop: JsValueRef): Boolean;
var
  PropId: JsPropertyIdRef;
begin
  case JsGetValueType(Prop) of
    JsSymbol:
      begin
        ChakraCoreCheck(JsGetPropertyIdFromSymbol(Prop, PropId));
        Result := JsHasProperty(Value, PropId);
      end;
    else
      Result := JsHasProperty(Value, JsStringToUnicodeString(Prop));
  end;
end;

function JsHasProperty(Value: JsValueRef; const PropId: JsPropertyIdRef): Boolean;
var
  B: ByteBool;
begin
  ChakraCoreCheck(ChakraCore.JsHasOwnProperty(Value, PropId, B));
  Result := B;
end;

function JsHasProperty(Value: JsValueRef; const PropName: UTF8String): Boolean;
var
  PropId: JsPropertyIdRef;
begin
  ChakraCoreCheck(JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId));
  Result := JsHasProperty(Value, PropId);
end;

function JsHasProperty(Value: JsValueRef; const PropName: UnicodeString): Boolean;
begin
  Result := JsHasProperty(Value, UTF8Encode(PropName));
end;

function JsCreatePropertyDescriptor(Configurable, Enumerable: Boolean; GetAccessor, SetAccessor: JsValueRef;
  UseStrictRules: Boolean): JsValueRef;
begin
  Result := JsCreateObject;
  JsSetProperty(Result, 'configurable', BooleanToJsBoolean(Configurable), True);
  JsSetProperty(Result, 'enumerable', BooleanToJsBoolean(Enumerable), True);
  if Assigned(GetAccessor) and (JsGetValueType(GetAccessor) = JsFunction) then
    JsSetProperty(Result, 'get', GetAccessor, UseStrictRules);
  if Assigned(SetAccessor) and (JsGetValueType(SetAccessor) = JsFunction) then
    JsSetProperty(Result, 'set', SetAccessor, UseStrictRules);
end;

function JsDefineProperty(PropId: JsPropertyIdRef; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef; UseStrictRules: Boolean): Boolean;
begin
  if not Assigned(Scope) then
    Scope := JsGlobal;
  ChakraCoreCheck(ChakraCommon.JsDefineProperty(Scope, PropId, JsCreatePropertyDescriptor(Configurable, Enumerable,
    GetAccessor, SetAccessor, UseStrictRules), ByteBool(Result)));
end;

function JsDefineProperty(const PropName: UTF8String; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef; UseStrictRules: Boolean): Boolean;
var
  PropId: JsPropertyIdRef;
begin
  ChakraCoreCheck(JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId));
  Result := JsDefineProperty(PropId, Configurable, Enumerable, GetAccessor, SetAccessor, Scope, UseStrictRules);
end;

function JsDefineProperty(const PropName: UnicodeString; Configurable, Enumerable: Boolean;
  GetAccessor, SetAccessor: JsValueRef; Scope: JsValueRef; UseStrictRules: Boolean): Boolean;
begin
  Result := JsDefineProperty(UTF8Encode(PropName), Configurable, Enumerable, GetAccessor, SetAccessor, Scope,
    UseStrictRules);
end;

function JsRunScript(const Script, Name: UTF8String; SourceContext: NativeUInt; IsLibraryCode: Boolean): JsValueRef;
const
  ParseScriptAttributes: array[Boolean] of JsParseScriptAttributes = ([], [JsParseScriptAttributeLibraryCode]);
var
  ScriptName, ScriptSource: JsValueRef;
begin
  ScriptName := StringToJsString(Name);
  ChakraCoreCheck(JsCreateExternalArrayBuffer(Pointer(Script), Length(Script), nil, nil, ScriptSource));
  ChakraCoreCheck(JsRun(ScriptSource, SourceContext, ScriptName, ParseScriptAttributes[IsLibraryCode], Result));
end;

function JsRunScript(const Script, Name: UnicodeString; SourceContext: NativeUInt; IsLibraryCode: Boolean): JsValueRef;
const
  ParseScriptAttributes: array[Boolean] of JsParseScriptAttributes = ([JsParseScriptAttributeArrayBufferIsUtf16Encoded],
    [JsParseScriptAttributeLibraryCode, JsParseScriptAttributeArrayBufferIsUtf16Encoded]);
var
  ScriptName, ScriptSource: JsValueRef;
begin
  ScriptName := StringToJsString(Name);
  ChakraCoreCheck(JsCreateExternalArrayBuffer(Pointer(PUnicodeChar(Script)), (Length(Script)) * SizeOf(UnicodeChar),
    nil, nil, ScriptSource));
  ChakraCoreCheck(JsRun(ScriptSource, SourceContext, ScriptName, ParseScriptAttributes[IsLibraryCode], Result));
end;

function JsCreateFunction(Callback: JsNativeFunction; CallbackState: Pointer; const Name: UTF8String): JsValueRef;
begin
  if Name = '' then
    ChakraCoreCheck(ChakraCommon.JsCreateFunction(Callback, CallbackState, Result))
  else
    ChakraCoreCheck(ChakraCommon.JsCreateNamedFunction(StringToJsString(Name), Callback, CallbackState, Result));
end;

function JsCreateFunction(Callback: JsNativeFunction; CallbackState: Pointer; const Name: UnicodeString): JsValueRef;
begin
  if Name = '' then
    ChakraCoreCheck(ChakraCommon.JsCreateFunction(Callback, CallbackState, Result))
  else
    ChakraCoreCheck(ChakraCommon.JsCreateNamedFunction(StringToJsString(Name), Callback, CallbackState, Result));
end;

procedure JsCreatePromise(out Promise, ResolveFunc, RejectFunc: JsValueRef);
begin
  ChakraCoreCheck(ChakraCore.JsCreatePromise(Promise, ResolveFunc, RejectFunc));
end;

function JsSetCallback(Instance: JsValueRef; const CallbackName: UTF8String; Callback: JsNativeFunction;
  CallbackState: Pointer; UseStrictRules: Boolean): JsValueRef;
begin
  Result := JsCreateFunction(Callback, CallbackState, CallbackName);
  JsSetProperty(Instance, CallbackName, Result, UseStrictRules);
end;

function JsSetCallback(Instance: JsValueRef; const CallbackName: UnicodeString; Callback: JsNativeFunction;
  CallbackState: Pointer; UseStrictRules: Boolean): JsValueRef;
begin
  Result := JsSetCallback(Instance, UTF8Encode(CallbackName), Callback, CallbackState, UseStrictRules);
end;

procedure JsSetProperty(Instance, Prop, Value: JsValueRef; UseStrictRules: Boolean);
var
  PropId: JsPropertyIdRef;
begin
  case JsGetValueType(Prop) of
    JsSymbol:
      begin
        ChakraCoreCheck(JsGetPropertyIdFromSymbol(Prop, PropId));
        JsSetProperty(Instance, PropId, Value, UseStrictRules);
      end;
    else
      JsSetProperty(Instance, JsStringToUnicodeString(Prop), Value, UseStrictRules);
  end;
end;

procedure JsSetProperty(Instance: JsValueRef; PropId: JsPropertyIdRef; Value: JsValueRef; UseStrictRules: Boolean);
begin
  ChakraCoreCheck(ChakraCommon.JsSetProperty(Instance, PropId, Value, UseStrictRules));
end;

procedure JsSetProperty(Instance: JsValueRef; const PropName: UTF8String; Value: JsValueRef; UseStrictRules: Boolean);
var
  PropId: JsPropertyIdRef;
begin
  ChakraCoreCheck(JsCreatePropertyId(PAnsiChar(PropName), Length(PropName), PropId));
  JsSetProperty(Instance, PropId, Value, UseStrictRules);
end;

procedure JsSetProperty(Instance: JsValueRef; const PropName: UnicodeString; Value: JsValueRef;
  UseStrictRules: Boolean);
begin
  JsSetProperty(Instance, UTF8Encode(PropName), Value, UseStrictRules);
end;

const
  ChakraCoreErrorConstructors: array[TErrorType] of JsErrorConstructor = (
    ChakraCommon.JsCreateError,
    JsCreateRangeError,
    JsCreateReferenceError,
    JsCreateSyntaxError,
    JsCreateTypeError,
    JsCreateUriError
  );

function JsCreateError(const AMessage: UTF8String; ErrorType: TErrorType = etGenericError): JsValueRef;
begin
  ChakraCoreCheck(ChakraCoreErrorConstructors[ErrorType](StringToJsString(AMessage), Result));
end;

function JsCreateError(const AMessage: UnicodeString; ErrorType: TErrorType = etGenericError): JsValueRef;
begin
  ChakraCoreCheck(ChakraCoreErrorConstructors[ErrorType](StringToJsString(AMessage), Result));
end;

procedure JsThrowError(const AMessage: UTF8String; ErrorType: TErrorType = etGenericError);
begin
  JsThrowError(JsCreateError(AMessage, ErrorType));
end;

procedure JsThrowError(const AMessage: UnicodeString; ErrorType: TErrorType);
begin
  JsThrowError(JsCreateError(AMessage, ErrorType));
end;

procedure JsThrowError(Error: JsValueRef);
begin
  ChakraCoreCheck(JsSetException(Error));
end;

function JsGetCurrentContext: JsContextRef;
begin
  ChakraCoreCheck(ChakraCommon.JsGetCurrentContext(Result));
end;

function JsGetCurrentRuntime: JsRuntimeHandle;
begin
  ChakraCoreCheck(ChakraCommon.JsGetRuntime(JsGetCurrentContext, Result));
end;

function JsIsDebugging: Boolean;
var
  DiagAttributes: JsDiagBreakOnExceptionAttributes;
begin
  Result := JsDiagGetBreakOnException(JsGetCurrentRuntime, DiagAttributes) <> JsErrorDiagNotInDebugMode;
end;

end.
