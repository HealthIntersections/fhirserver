unit ChakraCommon;

{
Based on the code published at https://github.com/hsreina/ChakraCore-Delphi,
but with several significant errors fixed.

Apache 2 license, consistent with source.

Stuff that is commented out in this unit is anything not proven to
work by unit tests. note that the original source includes a number
headers that were wrong (either Chakra changed, or they were just
outright wrong)

}

interface

type

  bool = Boolean;
  size_t = LongWord;
//  wchar_t = WideChar;
//  pwchar_t = PWideChar;
  uint = LongWord;
  int = Integer;
  short = Word;
//  int64_t = Int64;
//
  ChakraCookie = ^LongWord;
//  ChakraBytePtr = ^Byte;
//
  JsErrorCode = (
    JsNoError = 0,
    JsErrorCategoryUsage = $10000,
    JsErrorInvalidArgument,
    JsErrorNullArgument,
    JsErrorNoCurrentContext,
    JsErrorInExceptionState,
    JsErrorNotImplemented,
    JsErrorWrongThread,
    JsErrorRuntimeInUse,
    JsErrorBadSerializedScript,
    JsErrorInDisabledState,
    JsErrorCannotDisableExecution,
    JsErrorHeapEnumInProgress,
    JsErrorArgumentNotObject,
    JsErrorInProfileCallback,
    JsErrorInThreadServiceCallback,
    JsErrorCannotSerializeDebugScript,
    JsErrorAlreadyDebuggingContext,
    JsErrorAlreadyProfilingContext,
    JsErrorIdleNotEnabled,
    JsCannotSetProjectionEnqueueCallback,
    JsErrorCannotStartProjection,
    JsErrorInObjectBeforeCollectCallback,
    JsErrorObjectNotInspectable,
    JsErrorPropertyNotSymbol,
    JsErrorPropertyNotString,
    JsErrorInvalidContext,
    JsInvalidModuleHostInfoKind,
    JsErrorModuleParsed,
    JsErrorModuleEvaluated,
    JsErrorCategoryEngine = $20000,
    JsErrorOutOfMemory,
    JsErrorBadFPUState,
    JsErrorCategoryScript = $30000,
    JsErrorScriptException,
    JsErrorScriptCompile,
    JsErrorScriptTerminated,
    JsErrorScriptEvalDisabled,
    JsErrorCategoryFatal = $40000,
    JsErrorFatal,
    JsErrorWrongRuntime,
    JsErrorCategoryDiagError = $50000,
    JsErrorDiagAlreadyInDebugMode,
    JsErrorDiagNotInDebugMode,
    JsErrorDiagNotAtBreak,
    JsErrorDiagInvalidHandle,
    JsErrorDiagObjectNotFound,
    JsErrorDiagUnableToPerformAction
  );

  JsRuntimeHandle = Pointer;
//  PJsRuntimeHandle = ^JsRuntimeHandle;
//
//const
//  JS_INVALID_RUNTIME_HANDLE: JsRuntimeHandle = Pointer(0);
//
type
  JsRef = Pointer;
//
//const
//  JS_INVALID_REFERENCE: JsRef = Pointer(0);
//
type
  JsContextRef = JsRef;
  JsValueRef = JsRef;
  PJsValueRef = ^JsValueRef;
  JsValueRefArray = array [0..10 {arbitrary size}] of JsValueRef;
  PJsValueRefArray = ^JsValueRefArray;
  JsSourceContext = ChakraCookie;
//
//const
//  JS_SOURCE_CONTEXT_NONE: JsSourceContext = JsSourceContext(not LongWord(0));
//
type
  JsPropertyIdRef = JsRef;

  JsRuntimeAttributes = (
    JsRuntimeAttributeNone = $00000000,
    JsRuntimeAttributeDisableBackgroundWork = $00000001,
    JsRuntimeAttributeAllowScriptInterrupt = $00000002,
    JsRuntimeAttributeEnableIdleProcessing = $00000004,
    JsRuntimeAttributeDisableNativeCodeGeneration = $00000008,
    JsRuntimeAttributeDisableEval = $00000010,
    JsRuntimeAttributeEnableExperimentalFeatures = $00000020,
    JsRuntimeAttributeDispatchSetExceptionsToDebugger = $00000040
  );

//  JsTypedArrayType = (
//    JsArrayTypeInt8,
//    JsArrayTypeUint8,
//    JsArrayTypeUint8Clamped,
//    JsArrayTypeInt16,
//    JsArrayTypeUint16,
//    JsArrayTypeInt32,
//    JsArrayTypeUint32,
//    JsArrayTypeFloat32,
//    JsArrayTypeFloat64
//  );
//
//  JsMemoryEventType = (
//    JsMemoryAllocate = 0,
//    JsMemoryFree = 1,
//    JsMemoryFailure = 2
//  );
//
  JsParseScriptAttributes = (
    JsParseScriptAttributeNone = $0,
    JsParseScriptAttributeLibraryCode = $1,
    JsParseScriptAttributeArrayBufferIsUtf16Encoded = $2
  );

//  JsPropertyIdType = (
//    JsPropertyIdTypeString,
//    JsPropertyIdTypeSymbol
//  );
//
  JsValueType = (
    JsUndefined = 0,
    JsNull = 1,
    JsNumber = 2,
    JsString = 3,
    JsBoolean = 4,
    JsObject = 5,
    JsFunction = 6,
    JsError = 7,
    JsArray = 8,
    JsSymbol = 9,
    JsArrayBuffer = 10,
    JsTypedArray = 11,
    JsDataView = 12
  );

const DLL_NAME = 'ChakraCore.dll';

type
//  {
//    typedef bool (CHAKRA_CALLBACK * JsMemoryAllocationCallback)(
//      _In_opt_ void *callbackState,
//      _In_ JsMemoryEventType allocationEvent,
//      _In_ size_t allocationSize
//    );
//  }
//  JsMemoryAllocationCallback = function(
//    callbackState: Pointer;
//    allocationEvent: JsMemoryEventType;
//    allocationSize: size_t
//  ): bool; stdcall;
//
//  {
//    typedef void (CHAKRA_CALLBACK *JsBeforeCollectCallback)(
//      _In_opt_ void *callbackState
//    );
//  }
//  JsBeforeCollectCallback = procedure(
//    callbackState: Pointer
//  ); stdcall;
//
  {
    typedef void (CHAKRA_CALLBACK *JsObjectBeforeCollectCallback)(
      _In_ JsRef ref, _In_opt_ void *callbackState
    );
  }
  JsObjectBeforeCollectCallback = procedure(
    ref: JsRef;
    callbackState: Pointer
  ); stdcall;

  {
    typedef void (CHAKRA_CALLBACK *JsBackgroundWorkItemCallback)(_In_opt_ void *callbackState)
  }
  JsBackgroundWorkItemCallback = procedure(
    callbackState: Pointer
  ); stdcall;

  {
    typedef bool (CHAKRA_CALLBACK *JsThreadServiceCallback)(
      _In_ JsBackgroundWorkItemCallback callback,
      _In_opt_ void *callbackState
    );
  }
  JsThreadServiceCallback = function(
    callback: JsBackgroundWorkItemCallback;
    callbackState: Pointer
  ): bool; stdcall;

//  {
//    typedef void (CHAKRA_CALLBACK * JsSerializedScriptUnloadCallback)(
//      _In_ JsSourceContext sourceContext
//    );
//  }
//  JsSerializedScriptUnloadCallback = procedure(
//    sourceContext: JsSourceContext
//  ); stdcall;
//
//  {
//    typedef bool (CHAKRA_CALLBACK * JsSerializedScriptLoadUtf8SourceCallback)(
//      _In_ JsSourceContext sourceContext,
//      _Outptr_result_z_ const char** scriptBuffer
//    );
//  }
//  JsSerializedScriptLoadUtf8SourceCallback = function(
//    sourceContext: JsSourceContext;
//    var scriptBuffer: PAnsiChar
//  ): bool; stdcall;
//
  {
    typedef void (CHAKRA_CALLBACK *JsFinalizeCallback)(_In_opt_ void *data);
  }
  JsFinalizeCallback = procedure(data: Pointer); stdcall;


  {
    typedef _Ret_maybenull_ JsValueRef(CHAKRA_CALLBACK * JsNativeFunction)(
      _In_ JsValueRef callee,
      _In_ bool isConstructCall,
      _In_ JsValueRef *arguments,
      _In_ unsigned short argumentCount,
      _In_opt_ void *callbackState
    );
  }
  JsNativeFunction = function(
    callee: JsValueRef;
    isConstructCall: bool;
    arguments: PJsValueRef;
    argumentCount: Word;
    callbackState: Pointer
  ): JsValueRef; stdcall;

//  {
//    typedef void (CHAKRA_CALLBACK *JsPromiseContinuationCallback)(
//      _In_ JsValueRef task,
//      _In_opt_ void *callbackState
//    );
//  }
//  JsPromiseContinuationCallback = procedure(
//    task: JsValueRef;
//    callbackState: Pointer
//  ); stdcall;
//
  {
    CHAKRA_API
        JsCreateRuntime(
            _In_ JsRuntimeAttributes attributes,
            _In_opt_ JsThreadServiceCallback threadService,
            _Out_ JsRuntimeHandle *runtime);

  }
  function JsCreateRuntime(
    attributes: JsRuntimeAttributes;
    threadService: JsThreadServiceCallback;
    var runtime: JsRuntimeHandle
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsCollectGarbage(
//            _In_ JsRuntimeHandle runtime);
//
//  }
//  function JsCollectGarbage(
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
   CHAKRA_API
      JsDisposeRuntime(
          _In_ JsRuntimeHandle runtime);

  }
  function JsDisposeRuntime(
    runtime: JsRuntimeHandle
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetRuntimeMemoryUsage(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ size_t *memoryUsage);
//
//  }
//  function JsGetRuntimeMemoryUsage(
//    runtime: JsRuntimeHandle;
//    var memoryUsage: size_t
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetRuntimeMemoryLimit(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ size_t *memoryLimit);
//
//  }
//  function JsGetRuntimeMemoryLimit(
//    runtime: JsRuntimeHandle;
//    var memoryLimit: size_t
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetRuntimeMemoryLimit(
//            _In_ JsRuntimeHandle runtime,
//            _In_ size_t memoryLimit);
//  }
//  function JsSetRuntimeMemoryLimit(
//    runtime: JsRuntimeHandle;
//    var memoryLimit: size_t
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//     CHAKRA_API
//        JsSetRuntimeMemoryAllocationCallback(
//            _In_ JsRuntimeHandle runtime,
//            _In_opt_ void *callbackState,
//            _In_ JsMemoryAllocationCallback allocationCallback);
//  }
//  function JsSetRuntimeMemoryAllocationCallback(
//    runtime: JsRuntimeHandle;
//    callbackState: Pointer;
//    allocationCallback: JsMemoryAllocationCallback
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetRuntimeBeforeCollectCallback(
//            _In_ JsRuntimeHandle runtime,
//            _In_opt_ void *callbackState,
//            _In_ JsBeforeCollectCallback beforeCollectCallback);
//  }
//  function JsSetRuntimeBeforeCollectCallback(
//    runtime: JsRuntimeHandle;
//    callbackState: Pointer;
//    beforeCollectCallback: JsBeforeCollectCallback
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsAddRef(
//            _In_ JsRef ref,
//            _Out_opt_ unsigned int *count);
//  }
//  function JsAddRef(
//    ref: JsRef;
//    var count: LongWord
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsRelease(
//            _In_ JsRef ref,
//            _Out_opt_ unsigned int *count);
//  }
//  function JsRelease(
//    ref: JsRef;
//    var count: LongWord
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsSetObjectBeforeCollectCallback(
            _In_ JsRef ref,
            _In_opt_ void *callbackState,
            _In_ JsObjectBeforeCollectCallback objectBeforeCollectCallback);
  }
  function JsSetObjectBeforeCollectCallback(
    ref: JsRef;
    callbackState: Pointer;
    objectBeforeCollectCallback: JsObjectBeforeCollectCallback
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsCreateContext(
            _In_ JsRuntimeHandle runtime,
            _Out_ JsContextRef *newContext);
  }
  function JsCreateContext(
    runtime: JsRuntimeHandle;
    var newContext: JsContextRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetCurrentContext(
//            _Out_ JsContextRef *currentContext);
//  }
//  function JsGetCurrentContext(
//    var currentContext: JsContextRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsSetCurrentContext(
            _In_ JsContextRef context);
  }
  function JsSetCurrentContext(
    context: JsContextRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetContextOfObject(
//            _In_ JsValueRef object,
//            _Out_ JsContextRef *context);
//  }
//  function JsGetContextOfObject(
//    _object: JsValueRef;
//    var context: JsContextRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//     CHAKRA_API
//        JsGetContextData(
//            _In_ JsContextRef context,
//            _Out_ void **data);
//  }
//  function JsGetContextData(
//    context: JsContextRef;
//    var data: Pointer
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetContextData(
//            _In_ JsContextRef context,
//            _In_ void *data);
//  }
//  function JsSetContextData(
//    context: JsContextRef;
//    data: Pointer
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//     CHAKRA_API
//        JsGetRuntime(
//            _In_ JsContextRef context,
//            _Out_ JsRuntimeHandle *runtime);
//  }
//  function JsGetRuntime(
//    context: JsContextRef;
//    var runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//     CHAKRA_API
//        JsIdle(
//            _Out_opt_ unsigned int *nextIdleTick);
//  }
//  function JsIdle(
//    var nextIdleTick: LongWord
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsParseScriptUtf8(
//            _In_z_ const char *script,
//            _In_ JsSourceContext sourceContext,
//            _In_z_ const char *sourceUrl,
//            _Out_ JsValueRef *result);
//  }
//  function JsParseScript(
//    const script: PChar;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//     CHAKRA_API
//        JsParseScriptWithAttributesUtf8(
//            _In_z_ const char *script,
//            _In_ JsSourceContext sourceContext,
//            _In_z_ const char *sourceUrl,
//            _In_ JsParseScriptAttributes parseAttributes,
//            _Out_ JsValueRef *result);
//  }
//  function JsParseScriptWithAttributesUtf8(
//    const script: PAnsiChar;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    parseAttributes: JsParseScriptAttributes;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsRunScript(
            _In_z_ const char *script,
            _In_ JsSourceContext sourceContext,
            _In_z_ const char *sourceUrl,
            _Out_ JsValueRef *result);
  }
  function JsRunScript(
    const script: PChar;
    sourceContext: JsSourceContext;
    const sourceUrl: PChar;
    var result: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
  CHAKRA_API
    JsRun(
        _In_ JsValueRef script,
        _In_ JsSourceContext sourceContext,
        _In_ JsValueRef sourceUrl,
        _In_ JsParseScriptAttributes parseAttributes,
        _Out_ JsValueRef *result);
  }
 function JsRun(
          script: JsValueRef;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          parseAttributes: JsParseScriptAttributes;
          out result: JsValueRef): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsSerializeScriptUtf8(
//            _In_z_ const char *script,
//            _Out_writes_to_opt_(*bufferSize, *bufferSize) ChakraBytePtr buffer,
//            _Inout_ unsigned int *bufferSize);
//  }
//  function JsSerializeScriptUtf8(
//    const script: PAnsiChar;
//    buffer: ChakraBytePtr;
//    var bufferSize: LongWord
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsParseSerializedScriptUtf8(
//            _In_ JsSerializedScriptLoadUtf8SourceCallback scriptLoadCallback,
//            _In_ JsSerializedScriptUnloadCallback scriptUnloadCallback,
//            _In_ ChakraBytePtr buffer,
//            _In_ JsSourceContext sourceContext,
//            _In_z_ const char *sourceUrl,
//            _Out_ JsValueRef * result);
//  }
//  function JsParseSerializedScriptUtf8(
//    scriptLoadCallback: JsSerializedScriptLoadUtf8SourceCallback;
//    scriptUnloadCallback: JsSerializedScriptUnloadCallback;
//    buffer: ChakraBytePtr;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsRunSerializedScriptUtf8(
//            _In_ JsSerializedScriptLoadUtf8SourceCallback scriptLoadCallback,
//            _In_ JsSerializedScriptUnloadCallback scriptUnloadCallback,
//            _In_ ChakraBytePtr buffer,
//            _In_ JsSourceContext sourceContext,
//            _In_z_ const char *sourceUrl,
//            _Out_opt_ JsValueRef * result);
//  }
//  function JsRunSerializedScriptUtf8(
//    scriptLoadCallback: JsSerializedScriptLoadUtf8SourceCallback;
//    scriptUnloadCallback: JsSerializedScriptUnloadCallback;
//    buffer: ChakraBytePtr;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdFromNameUtf8(
//            _In_z_ const char *name,
//            _Out_ JsPropertyIdRef *propertyId);
//  }
//  function JsGetPropertyIdFromNameUtf8(
//    const name: PAnsiChar;
//    var propertyId: JsPropertyIdRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetPropertyNameFromIdUtf8Copy(
//            _In_ JsPropertyIdRef propertyId,
//            _Outptr_result_z_ char **name);
//  }
//  function JsGetPropertyNameFromIdUtf8Copy(
//    propertyId: JsPropertyIdRef;
//    var name: PAnsiChar
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsPointerToString(
            _In_reads_(stringLength) const char *stringValue,
            _In_ size_t stringLength,
            _Out_ JsValueRef *value);
  }
  function JsPointerToString(
    const stringValue: PChar;
    stringLength: size_t;
    var value: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsStringToPointer(
            _In_ JsValueRef value,
            _Outptr_result_buffer_(*stringLength) char **stringValue,
            _Out_ size_t *stringLength);
  }
  function JsStringToPointer(
    value: JsValueRef;
    var stringValue: PChar;
    var stringLength: size_t
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetSymbolFromPropertyId(
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsValueRef *symbol);
//  }
//  function JsGetSymbolFromPropertyId(
//    propertyId: JsPropertyIdRef;
//    var symbol: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdType(
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsPropertyIdType* propertyIdType);
//  }
//  function JsGetPropertyIdType(
//    propertyId: JsPropertyIdRef;
//    var propertyIdType: JsPropertyIdType
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdFromSymbol(
//            _In_ JsValueRef symbol,
//            _Out_ JsPropertyIdRef *propertyId);
//  }
//  function JsGetPropertyIdFromSymbol(
//    symbol: JsValueRef;
//    var propertyId: JsPropertyIdRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateSymbol(
//            _In_ JsValueRef description,
//            _Out_ JsValueRef *result);
//  }
//  function JsCreateSymbol(
//    description: JsValueRef;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetOwnPropertySymbols(
//            _In_ JsValueRef object,
//            _Out_ JsValueRef *propertySymbols);
//  }
//  function JsGetOwnPropertySymbols(
//    _object: JsValueRef;
//    var propertySymbols: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetUndefinedValue(
//            _Out_ JsValueRef *undefinedValue);
//  }
//  function JsGetUndefinedValue(
//    var undefinedValue: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetNullValue(
            _Out_ JsValueRef *nullValue);
  }
  function JsGetNullValue(
    var nullValue: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetTrueValue(
//            _Out_ JsValueRef *trueValue);
//  }
//  function JsGetTrueValue(
//    var trueValue: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetFalseValue(
//            _Out_ JsValueRef *falseValue);
//  }
//  function JsGetFalseValue(
//    var falseValue: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
     CHAKRA_API
        JsBoolToBoolean(
            _In_ bool value,
            _Out_ JsValueRef *booleanValue);
  }
  function JsBoolToBoolean(
    value: bool;
    var booleanValue: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsBooleanToBool(
            _In_ JsValueRef value,
            _Out_ bool *boolValue);
  }
  function JsBooleanToBool(
    value: JsValueRef;
    var boolValue: bool
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsConvertValueToBoolean(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *booleanValue);
//  }
//  function JsConvertValueToBoolean(
//    value: JsValueRef;
//    var booleanValue: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetValueType(
            _In_ JsValueRef value,
            _Out_ JsValueType *type);
  }
  function JsGetValueType(
    value: JsValueRef;
    var _type: int // if you just use JsValueType directly, you'll get the stack trashed in 64bit
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsDoubleToNumber(
//            _In_ double doubleValue,
//            _Out_ JsValueRef *value);
//  }
//  function JsDoubleToNumber(
//    doubleValue: double;
//    var value: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsIntToNumber(
            _In_ int intValue,
            _Out_ JsValueRef *value);
  }
  function JsIntToNumber(
    intValue: int;
    var value: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsNumberToDouble(
//            _In_ JsValueRef value,
//            _Out_ double *doubleValue);
//  }
//  function JsNumberToDouble(
//    value: JsValueRef;
//    var doubleValue: double
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsNumberToInt(
            _In_ JsValueRef value,
            _Out_ int *intValue);
  }
  function JsNumberToInt(
    value: JsValueRef;
    var intValue: int
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsConvertValueToNumber(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *numberValue);
//  }
//  function JsConvertValueToNumber(
//    value: JsValueRef;
//    var numberValue: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetStringLength(
//            _In_ JsValueRef stringValue,
//            _Out_ int *length);
//  }
//  function JsGetStringLength(
//    stringValue: JsValueRef;
//    var length: int
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsConvertValueToString(
            _In_ JsValueRef value,
            _Out_ JsValueRef *stringValue);
  }
  function JsConvertValueToString(
    value: JsValueRef;
    var stringValue: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsGetGlobalObject(
            _Out_ JsValueRef *globalObject);
  }
  function JsGetGlobalObject(
    var globalObject: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsCreateObject(
            _Out_ JsValueRef *object);
  }
  function JsCreateObject(
    var _object: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;


 {
   CHAKRA_API
    JsCreateString(
        _In_ const char *content,
        _In_ size_t length,
        _Out_ JsValueRef *value);
        }
  function JsCreateString(
        content: PAnsiChar;
        length: size_t;
        out value: JsValueRef): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsCreateExternalObject(
            _In_opt_ void *data,
            _In_opt_ JsFinalizeCallback finalizeCallback,
            _Out_ JsValueRef *object);
  }
  function JsCreateExternalObject(
    data: Pointer;
    finalizeCallback: JsFinalizeCallback;
    var _object: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsConvertValueToObject(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *object);
//  }
//  function JsConvertValueToObject(
//    value: JsValueRef;
//    var _object: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetPrototype(
//            _In_ JsValueRef object,
//            _Out_ JsValueRef *prototypeObject);
//  }
//  function JsGetPrototype(
//    _object: JsValueRef;
//    var prototypeObject: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetPrototype(
//            _In_ JsValueRef object,
//            _In_ JsValueRef prototypeObject);
//  }
//  function JsSetPrototype(
//    _object: JsValueRef;
//    prototypeObject: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsInstanceOf(
//            _In_ JsValueRef object,
//            _In_ JsValueRef constructor,
//            _Out_ bool *result);
//  }
//  function JsInstanceOf(
//    var _object: JsValueRef;
//    _constructor: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetExtensionAllowed(
//            _In_ JsValueRef object,
//            _Out_ bool *value);
//  }
//  function JsGetExtensionAllowed(
//    _objec: JsValueRef;
//    var value: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
  CHAKRA_API
       JsCreatePropertyId(
        _In_z_ const char *name,
        _In_ size_t length,
        _Out_ JsPropertyIdRef *propertyId);
  }
  function JsCreatePropertyId(
        name : PAnsiChar;
        length : cardinal;
        var propertyId : JsPropertyIdRef) : JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsPreventExtension(
//            _In_ JsValueRef object);
//  }
//  function JsPreventExtension(
//    _object: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _Out_ JsValueRef *value);
  }
  function JsGetProperty(
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    var value: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsGetOwnPropertyDescriptor(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsValueRef *propertyDescriptor);
//  }
//  function JsGetOwnPropertyDescriptor(
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    var propertyDescriptor: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetOwnPropertyNames(
            _In_ JsValueRef object,
            _Out_ JsValueRef *propertyNames);
  }
  function JsGetOwnPropertyNames(
    _object: JsValueRef;
    var propertyNames: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsSetProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _In_ JsValueRef value,
            _In_ bool useStrictRules);
  }
  function JsSetProperty(
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    value: JsValueRef;
    useStrictRules: bool
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsHasProperty(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ bool *hasProperty);
//  }
//  function JsHasProperty(
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    var hasProperty: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsDeleteProperty(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _In_ bool useStrictRules,
//            _Out_ JsValueRef *result);
//  }
//  function JsDeleteProperty(
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    useStrictRules: bool;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsDefineProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _In_ JsValueRef propertyDescriptor,
            _Out_ bool *result);
  }
  function JsDefineProperty(
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    propertyDescriptor: JsValueRef;
    var result: bool
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsHasIndexedProperty(
//            _In_ JsValueRef object,
//            _In_ JsValueRef index,
//            _Out_ bool *result);
//  }
//  function JsHasIndexedProperty(
//    _object: JsValueRef;
//    index: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetIndexedProperty(
            _In_ JsValueRef object,
            _In_ JsValueRef index,
            _Out_ JsValueRef *result);
  }
  function JsGetIndexedProperty(
    _object: JsValueRef;
    index: JsValueRef;
    var result: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsSetIndexedProperty(
            _In_ JsValueRef object,
            _In_ JsValueRef index,
            _In_ JsValueRef value);
  }
  function JsSetIndexedProperty(
    _object: JsValueRef;
    index: JsValueRef;
    value: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsDeleteIndexedProperty(
//            _In_ JsValueRef object,
//            _In_ JsValueRef index);
//  }
//  function JsDeleteIndexedProperty(
//    _object: JsValueRef;
//    index: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsHasIndexedPropertiesExternalData(
//            _In_ JsValueRef object,
//            _Out_ bool* value);
//  }
//  function JsHasIndexedPropertiesExternalData(
//    _object: JsValueRef;
//    var value: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetIndexedPropertiesExternalData(
//            _In_ JsValueRef object,
//            _Out_ void** data,
//            _Out_ JsTypedArrayType* arrayType,
//            _Out_ unsigned int* elementLength);
//  }
//  function JsGetIndexedPropertiesExternalData(
//    _object: JsValueRef;
//    data: Pointer;
//    var arrayType: JsTypedArrayType;
//    var elementLength: uint
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetIndexedPropertiesToExternalData(
//            _In_ JsValueRef object,
//            _In_ void* data,
//            _In_ JsTypedArrayType arrayType,
//            _In_ unsigned int elementLength);
//  }
//  function JsSetIndexedPropertiesToExternalData(
//    _object: JsValueRef;
//    data: Pointer;
//    arrayType: JsTypedArrayType;
//    elementLength: uint
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsEquals(
//            _In_ JsValueRef object1,
//            _In_ JsValueRef object2,
//            _Out_ bool *result);
//  }
//  function JsEquals(
//    object1: JsValueRef;
//    object2: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsStrictEquals(
//            _In_ JsValueRef object1,
//            _In_ JsValueRef object2,
//            _Out_ bool *result);
//  }
//  function JsStrictEquals(
//    object1: JsValueRef;
//    object2: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsHasExternalData(
            _In_ JsValueRef object,
            _Out_ bool *value);
  }
  function JsHasExternalData(
    _object: JsValueRef;
    var value: bool
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsGetExternalData(
            _In_ JsValueRef object,
            _Out_ void **externalData);
  }
  function JsGetExternalData(
    _object: JsValueRef;
    var externalData: Pointer
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsSetExternalData(
//            _In_ JsValueRef object,
//            _In_opt_ void *externalData);
//  }
//  function JsSetExternalData(
//    _object: JsValueRef;
//    var externalData: Pointer
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsCreateArray(
            _In_ unsigned int length,
            _Out_ JsValueRef *result);
  }
  function JsCreateArray(
    length: uint;
    var result: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsCreateArrayBuffer(
//            _In_ unsigned int byteLength,
//            _Out_ JsValueRef *result);
//  }
//  function JsCreateArrayBuffer(
//    byteLength: uint;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsCreateExternalArrayBuffer(
            _Pre_maybenull_ _Pre_writable_byte_size_(byteLength) void *data,
            _In_ unsigned int byteLength,
            _In_opt_ JsFinalizeCallback finalizeCallback,
            _In_opt_ void *callbackState,
            _Out_ JsValueRef *result);
  }
  function JsCreateExternalArrayBuffer(
    data: Pointer;
    byteLength: uint;
    finalizeCallback: JsFinalizeCallback;
    callbackState: Pointer;
    var result: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsCreateTypedArray(
//            _In_ JsTypedArrayType arrayType,
//            _In_ JsValueRef baseArray,
//            _In_ unsigned int byteOffset,
//            _In_ unsigned int elementLength,
//            _Out_ JsValueRef *result);
//  }
//  function JsCreateTypedArray(
//    arrayType: JsTypedArrayType;
//    baseArray: JsValueRef;
//    byteOffset: uint;
//    elementLength: uint;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateDataView(
//            _In_ JsValueRef arrayBuffer,
//            _In_ unsigned int byteOffset,
//            _In_ unsigned int byteLength,
//            _Out_ JsValueRef *result);
//  }
//  function JsCreateDataView(
//    arrayBuffer: JsValueRef;
//    byteOffset: uint;
//    byteLength: uint;
//    result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetTypedArrayInfo(
//            _In_ JsValueRef typedArray,
//            _Out_opt_ JsTypedArrayType *arrayType,
//            _Out_opt_ JsValueRef *arrayBuffer,
//            _Out_opt_ unsigned int *byteOffset,
//            _Out_opt_ unsigned int *byteLength);
//  }
//  function JsGetTypedArrayInfo(
//    typedArray: JsValueRef;
//    var arrayType: JsTypedArrayType;
//    var arrayBuffer: JsValueRef;
//    var byteOffset: uint;
//    var byteLength: uint
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetArrayBufferStorage(
//            _In_ JsValueRef arrayBuffer,
//            _Outptr_result_bytebuffer_(*bufferLength) ChakraBytePtr *buffer,
//            _Out_ unsigned int *bufferLength);
//  }
//  function JsGetArrayBufferStorage(
//    arrayBuffer: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//      CHAKRA_API
//        JsGetTypedArrayStorage(
//            _In_ JsValueRef typedArray,
//            _Outptr_result_bytebuffer_(*bufferLength) ChakraBytePtr *buffer,
//            _Out_ unsigned int *bufferLength,
//            _Out_opt_ JsTypedArrayType *arrayType,
//            _Out_opt_ int *elementSize);
//  }
//  function JsGetTypedArrayStorage(
//    typedArray: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint;
//    var arrayType: JsTypedArrayType;
//    var elementSize: int
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsGetDataViewStorage(
//            _In_ JsValueRef dataView,
//            _Outptr_result_bytebuffer_(*bufferLength) ChakraBytePtr *buffer,
//            _Out_ unsigned int *bufferLength);
//  }
//  function JsGetDataViewStorage(
//    dataView: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsCallFunction(
            _In_ JsValueRef function,
            _In_reads_(argumentCount) JsValueRef *arguments,
            _In_ unsigned short argumentCount,
            _Out_opt_ JsValueRef *result);
  }
  function JsCallFunction(
    _function: JsValueRef;
    arguments: PJsValueRef;
    argumentCount: short;
    var result: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsConstructObject(
//            _In_ JsValueRef function,
//            _In_reads_(argumentCount) JsValueRef *arguments,
//            _In_ unsigned short argumentCount,
//            _Out_ JsValueRef *result);
//  }
//  function JsConstructObject(
//    _function: JsValueRef;
//    arguments: PJsValueRef;
//    argumentCount: short;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsCreateFunction(
            _In_ JsNativeFunction nativeFunction,
            _In_opt_ void *callbackState,
            _Out_ JsValueRef *function);
  }
  function JsCreateFunction(
    nativeFunction: JsNativeFunction;
    callbackState: Pointer;
    var _function: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsCreateNamedFunction(
//            _In_ JsValueRef name,
//            _In_ JsNativeFunction nativeFunction,
//            _In_opt_ void *callbackState,
//            _Out_ JsValueRef *function);
//  }
//  function JsCreateNamedFunction(
//    name: JsValueRef;
//    nativeFunction: JsNativeFunction;
//    callbackState: Pointer;
//    var _function: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateRangeError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateRangeError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateReferenceError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateReferenceError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateSyntaxError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateSyntaxError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateTypeError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateTypeError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsCreateURIError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  function JsCreateURIError(
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsHasException(
//            _Out_ bool *hasException);
//  }
//  function JsHasException(
//    var hasException: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
  {
    CHAKRA_API
        JsGetAndClearException(
            _Out_ JsValueRef *exception);
  }
  function JsGetAndClearException(
    var exception: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

  {
    CHAKRA_API
        JsSetException(
            _In_ JsValueRef exception);
  }
  function JsSetException(
    exception: JsValueRef
  ): JsErrorCode; stdcall; external DLL_NAME;

//  {
//    CHAKRA_API
//        JsDisableRuntimeExecution(
//            _In_ JsRuntimeHandle runtime);
//  }
//  function JsDisableRuntimeExecution(
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsEnableRuntimeExecution(
//            _In_ JsRuntimeHandle runtime);
//  }
//  function JsEnableRuntimeExecution(
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsIsRuntimeExecutionDisabled(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ bool *isDisabled);
//  }
//  function JsIsRuntimeExecutionDisabled(
//    runtime: JsRuntimeHandle;
//    var isDisabled: bool
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsSetPromiseContinuationCallback(
//            _In_ JsPromiseContinuationCallback promiseContinuationCallback,
//            _In_opt_ void *callbackState);
//  }
//  function JsSetPromiseContinuationCallback(
//    promiseContinuationCallback: JsPromiseContinuationCallback;
//    callbackState: Pointer
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
//  {
//    CHAKRA_API
//        JsStringFree(
//            _In_ char* stringValue);
//  }
//  function JsStringFree(
//    stringValue: PAnsiChar
//  ): JsErrorCode; stdcall; external DLL_NAME;
//
implementation

end.
