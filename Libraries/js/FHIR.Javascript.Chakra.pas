unit FHIR.Javascript.Chakra;

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

uses
  Windows;

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
  TJsCreateRuntime = function (
    attributes: JsRuntimeAttributes;
    threadService: JsThreadServiceCallback;
    var runtime: JsRuntimeHandle
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsCollectGarbage(
//            _In_ JsRuntimeHandle runtime);
//
//  }
//  function JsCollectGarbage(
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall;
//
  {
   CHAKRA_API
      JsDisposeRuntime(
          _In_ JsRuntimeHandle runtime);

  }
  TJsDisposeRuntime = function(
    runtime: JsRuntimeHandle
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetRuntimeMemoryUsage(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ size_t *memoryUsage);
//
//  }
//  TJsGetRuntimeMemoryUsage = function (
//    runtime: JsRuntimeHandle;
//    var memoryUsage: size_t
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetRuntimeMemoryLimit(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ size_t *memoryLimit);
//
//  }
//  TJsGetRuntimeMemoryLimit = function (
//    runtime: JsRuntimeHandle;
//    var memoryLimit: size_t
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetRuntimeMemoryLimit(
//            _In_ JsRuntimeHandle runtime,
//            _In_ size_t memoryLimit);
//  }
//  TJsSetRuntimeMemoryLimit = function (
//    runtime: JsRuntimeHandle;
//    var memoryLimit: size_t
//  ): JsErrorCode; stdcall;
//
//  {
//     CHAKRA_API
//        JsSetRuntimeMemoryAllocationCallback(
//            _In_ JsRuntimeHandle runtime,
//            _In_opt_ void *callbackState,
//            _In_ JsMemoryAllocationCallback allocationCallback);
//  }
//  TJsSetRuntimeMemoryAllocationCallback = function (
//    runtime: JsRuntimeHandle;
//    callbackState: Pointer;
//    allocationCallback: JsMemoryAllocationCallback
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetRuntimeBeforeCollectCallback(
//            _In_ JsRuntimeHandle runtime,
//            _In_opt_ void *callbackState,
//            _In_ JsBeforeCollectCallback beforeCollectCallback);
//  }
//  TJsSetRuntimeBeforeCollectCallback = function (
//    runtime: JsRuntimeHandle;
//    callbackState: Pointer;
//    beforeCollectCallback: JsBeforeCollectCallback
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsAddRef(
//            _In_ JsRef ref,
//            _Out_opt_ unsigned int *count);
//  }
//  TJsAddRef = function (
//    ref: JsRef;
//    var count: LongWord
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsRelease(
//            _In_ JsRef ref,
//            _Out_opt_ unsigned int *count);
//  }
//  TJsRelease = function (
//    ref: JsRef;
//    var count: LongWord
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsSetObjectBeforeCollectCallback(
            _In_ JsRef ref,
            _In_opt_ void *callbackState,
            _In_ JsObjectBeforeCollectCallback objectBeforeCollectCallback);
  }
  TJsSetObjectBeforeCollectCallback = function (
    ref: JsRef;
    callbackState: Pointer;
    objectBeforeCollectCallback: JsObjectBeforeCollectCallback
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsCreateContext(
            _In_ JsRuntimeHandle runtime,
            _Out_ JsContextRef *newContext);
  }
  TJsCreateContext = function (
    runtime: JsRuntimeHandle;
    var newContext: JsContextRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetCurrentContext(
//            _Out_ JsContextRef *currentContext);
//  }
//  TJsGetCurrentContext = function (
//    var currentContext: JsContextRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsSetCurrentContext(
            _In_ JsContextRef context);
  }
  TJsSetCurrentContext = function (
    context: JsContextRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetContextOfObject(
//            _In_ JsValueRef object,
//            _Out_ JsContextRef *context);
//  }
//  TJsGetContextOfObject = function (
//    _object: JsValueRef;
//    var context: JsContextRef
//  ): JsErrorCode; stdcall;
//
//  {
//     CHAKRA_API
//        JsGetContextData(
//            _In_ JsContextRef context,
//            _Out_ void **data);
//  }
//  TJsGetContextData = function (
//    context: JsContextRef;
//    var data: Pointer
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetContextData(
//            _In_ JsContextRef context,
//            _In_ void *data);
//  }
//  TJsSetContextData = function (
//    context: JsContextRef;
//    data: Pointer
//  ): JsErrorCode; stdcall;
//
//  {
//     CHAKRA_API
//        JsGetRuntime(
//            _In_ JsContextRef context,
//            _Out_ JsRuntimeHandle *runtime);
//  }
//  TJsGetRuntime = function (
//    context: JsContextRef;
//    var runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall;
//
//  {
//     CHAKRA_API
//        JsIdle(
//            _Out_opt_ unsigned int *nextIdleTick);
//  }
//  TJsIdle = function (
//    var nextIdleTick: LongWord
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsParseScriptUtf8(
//            _In_z_ const char *script,
//            _In_ JsSourceContext sourceContext,
//            _In_z_ const char *sourceUrl,
//            _Out_ JsValueRef *result);
//  }
//  TJsParseScript = function (
//    const script: PChar;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
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
//  TJsParseScriptWithAttributesUtf8 = function (
//    const script: PAnsiChar;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    parseAttributes: JsParseScriptAttributes;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsRunScript(
            _In_z_ const char *script,
            _In_ JsSourceContext sourceContext,
            _In_z_ const char *sourceUrl,
            _Out_ JsValueRef *result);
  }
  TJsRunScript = function (
    const script: PChar;
    sourceContext: JsSourceContext;
    const sourceUrl: PChar;
    var result: JsValueRef
  ): JsErrorCode; stdcall;

  {
  CHAKRA_API
    JsRun(
        _In_ JsValueRef script,
        _In_ JsSourceContext sourceContext,
        _In_ JsValueRef sourceUrl,
        _In_ JsParseScriptAttributes parseAttributes,
        _Out_ JsValueRef *result);
  }
 TJsRun = function (
          script: JsValueRef;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          parseAttributes: JsParseScriptAttributes;
          out result: JsValueRef): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsSerializeScriptUtf8(
//            _In_z_ const char *script,
//            _Out_writes_to_opt_(*bufferSize, *bufferSize) ChakraBytePtr buffer,
//            _Inout_ unsigned int *bufferSize);
//  }
//  TJsSerializeScriptUtf8 = function (
//    const script: PAnsiChar;
//    buffer: ChakraBytePtr;
//    var bufferSize: LongWord
//  ): JsErrorCode; stdcall;
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
//  TJsParseSerializedScriptUtf8 = function (
//    scriptLoadCallback: JsSerializedScriptLoadUtf8SourceCallback;
//    scriptUnloadCallback: JsSerializedScriptUnloadCallback;
//    buffer: ChakraBytePtr;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
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
//  TJsRunSerializedScriptUtf8 = function (
//    scriptLoadCallback: JsSerializedScriptLoadUtf8SourceCallback;
//    scriptUnloadCallback: JsSerializedScriptUnloadCallback;
//    buffer: ChakraBytePtr;
//    sourceContext: JsSourceContext;
//    const sourceUrl: PAnsiChar;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdFromNameUtf8(
//            _In_z_ const char *name,
//            _Out_ JsPropertyIdRef *propertyId);
//  }
//  TJsGetPropertyIdFromNameUtf8 = function (
//    const name: PAnsiChar;
//    var propertyId: JsPropertyIdRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetPropertyNameFromIdUtf8Copy(
//            _In_ JsPropertyIdRef propertyId,
//            _Outptr_result_z_ char **name);
//  }
//  TJsGetPropertyNameFromIdUtf8Copy = function (
//    propertyId: JsPropertyIdRef;
//    var name: PAnsiChar
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsPointerToString(
            _In_reads_(stringLength) const char *stringValue,
            _In_ size_t stringLength,
            _Out_ JsValueRef *value);
  }
  TJsPointerToString = function (
    const stringValue: PChar;
    stringLength: size_t;
    var value: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsStringToPointer(
            _In_ JsValueRef value,
            _Outptr_result_buffer_(*stringLength) char **stringValue,
            _Out_ size_t *stringLength);
  }
  TJsStringToPointer = function (
    value: JsValueRef;
    var stringValue: PChar;
    var stringLength: size_t
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetSymbolFromPropertyId(
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsValueRef *symbol);
//  }
//  TJsGetSymbolFromPropertyId = function (
//    propertyId: JsPropertyIdRef;
//    var symbol: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdType(
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsPropertyIdType* propertyIdType);
//  }
//  TJsGetPropertyIdType = function (
//    propertyId: JsPropertyIdRef;
//    var propertyIdType: JsPropertyIdType
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetPropertyIdFromSymbol(
//            _In_ JsValueRef symbol,
//            _Out_ JsPropertyIdRef *propertyId);
//  }
//  TJsGetPropertyIdFromSymbol = function (
//    symbol: JsValueRef;
//    var propertyId: JsPropertyIdRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateSymbol(
//            _In_ JsValueRef description,
//            _Out_ JsValueRef *result);
//  }
//  TJsCreateSymbol = function (
//    description: JsValueRef;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetOwnPropertySymbols(
//            _In_ JsValueRef object,
//            _Out_ JsValueRef *propertySymbols);
//  }
//  TJsGetOwnPropertySymbols = function (
//    _object: JsValueRef;
//    var propertySymbols: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetUndefinedValue(
//            _Out_ JsValueRef *undefinedValue);
//  }
//  TJsGetUndefinedValue = function (
//    var undefinedValue: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetNullValue(
            _Out_ JsValueRef *nullValue);
  }
  TJsGetNullValue = function (
    var nullValue: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetTrueValue(
//            _Out_ JsValueRef *trueValue);
//  }
//  TJsGetTrueValue = function (
//    var trueValue: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetFalseValue(
//            _Out_ JsValueRef *falseValue);
//  }
//  TJsGetFalseValue = function (
//    var falseValue: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
     CHAKRA_API
        JsBoolToBoolean(
            _In_ bool value,
            _Out_ JsValueRef *booleanValue);
  }
  TJsBoolToBoolean = function (
    value: bool;
    var booleanValue: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsBooleanToBool(
            _In_ JsValueRef value,
            _Out_ bool *boolValue);
  }
  TJsBooleanToBool = function (
    value: JsValueRef;
    var boolValue: bool
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsConvertValueToBoolean(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *booleanValue);
//  }
//  TJsConvertValueToBoolean = function (
//    value: JsValueRef;
//    var booleanValue: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetValueType(
            _In_ JsValueRef value,
            _Out_ JsValueType *type);
  }
  TJsGetValueType = function (
    value: JsValueRef;
    var _type: int // if you just use JsValueType directly, you'll get the stack trashed in 64bit
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsDoubleToNumber(
//            _In_ double doubleValue,
//            _Out_ JsValueRef *value);
//  }
//  TJsDoubleToNumber = function (
//    doubleValue: double;
//    var value: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsIntToNumber(
            _In_ int intValue,
            _Out_ JsValueRef *value);
  }
  TJsIntToNumber = function (
    intValue: int;
    var value: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsNumberToDouble(
//            _In_ JsValueRef value,
//            _Out_ double *doubleValue);
//  }
//  TJsNumberToDouble = function (
//    value: JsValueRef;
//    var doubleValue: double
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsNumberToInt(
            _In_ JsValueRef value,
            _Out_ int *intValue);
  }
  TJsNumberToInt = function (
    value: JsValueRef;
    var intValue: int
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsConvertValueToNumber(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *numberValue);
//  }
//  TJsConvertValueToNumber = function (
//    value: JsValueRef;
//    var numberValue: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetStringLength(
//            _In_ JsValueRef stringValue,
//            _Out_ int *length);
//  }
//  TJsGetStringLength = function (
//    stringValue: JsValueRef;
//    var length: int
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsConvertValueToString(
            _In_ JsValueRef value,
            _Out_ JsValueRef *stringValue);
  }
  TJsConvertValueToString = function (
    value: JsValueRef;
    var stringValue: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsGetGlobalObject(
            _Out_ JsValueRef *globalObject);
  }
  TJsGetGlobalObject = function (
    var globalObject: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsCreateObject(
            _Out_ JsValueRef *object);
  }
  TJsCreateObject = function (
    var _object: JsValueRef
  ): JsErrorCode; stdcall;


 {
   CHAKRA_API
    JsCreateString(
        _In_ const char *content,
        _In_ size_t length,
        _Out_ JsValueRef *value);
        }
  TJsCreateString = function (
        content: PAnsiChar;
        length: size_t;
        out value: JsValueRef): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsCreateExternalObject(
            _In_opt_ void *data,
            _In_opt_ JsFinalizeCallback finalizeCallback,
            _Out_ JsValueRef *object);
  }
  TJsCreateExternalObject = function (
    data: Pointer;
    finalizeCallback: JsFinalizeCallback;
    var _object: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsConvertValueToObject(
//            _In_ JsValueRef value,
//            _Out_ JsValueRef *object);
//  }
//  TJsConvertValueToObject = function (
//    value: JsValueRef;
//    var _object: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetPrototype(
//            _In_ JsValueRef object,
//            _Out_ JsValueRef *prototypeObject);
//  }
//  TJsGetPrototype = function (
//    _object: JsValueRef;
//    var prototypeObject: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetPrototype(
//            _In_ JsValueRef object,
//            _In_ JsValueRef prototypeObject);
//  }
//  TJsSetPrototype = function (
//    _object: JsValueRef;
//    prototypeObject: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsInstanceOf(
//            _In_ JsValueRef object,
//            _In_ JsValueRef constructor,
//            _Out_ bool *result);
//  }
//  TJsInstanceOf = function (
//    var _object: JsValueRef;
//    _constructor: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetExtensionAllowed(
//            _In_ JsValueRef object,
//            _Out_ bool *value);
//  }
//  TJsGetExtensionAllowed = function (
//    _objec: JsValueRef;
//    var value: bool
//  ): JsErrorCode; stdcall;
//
  {
  CHAKRA_API
       JsCreatePropertyId(
        _In_z_ const char *name,
        _In_ size_t length,
        _Out_ JsPropertyIdRef *propertyId);
  }
  TJsCreatePropertyId = function (
        name : PAnsiChar;
        length : cardinal;
        var propertyId : JsPropertyIdRef) : JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsPreventExtension(
//            _In_ JsValueRef object);
//  }
//  TJsPreventExtension = function (
//    _object: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _Out_ JsValueRef *value);
  }
  TJsGetProperty = function (
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    var value: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsGetOwnPropertyDescriptor(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ JsValueRef *propertyDescriptor);
//  }
//  TJsGetOwnPropertyDescriptor = function (
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    var propertyDescriptor: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetOwnPropertyNames(
            _In_ JsValueRef object,
            _Out_ JsValueRef *propertyNames);
  }
  TJsGetOwnPropertyNames = function (
    _object: JsValueRef;
    var propertyNames: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsSetProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _In_ JsValueRef value,
            _In_ bool useStrictRules);
  }
  TJsSetProperty = function (
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    value: JsValueRef;
    useStrictRules: bool
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsHasProperty(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _Out_ bool *hasProperty);
//  }
//  TJsHasProperty = function (
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    var hasProperty: bool
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsDeleteProperty(
//            _In_ JsValueRef object,
//            _In_ JsPropertyIdRef propertyId,
//            _In_ bool useStrictRules,
//            _Out_ JsValueRef *result);
//  }
//  TJsDeleteProperty = function (
//    _object: JsValueRef;
//    propertyId: JsPropertyIdRef;
//    useStrictRules: bool;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsDefineProperty(
            _In_ JsValueRef object,
            _In_ JsPropertyIdRef propertyId,
            _In_ JsValueRef propertyDescriptor,
            _Out_ bool *result);
  }
  TJsDefineProperty = function (
    _object: JsValueRef;
    propertyId: JsPropertyIdRef;
    propertyDescriptor: JsValueRef;
    var result: bool
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsHasIndexedProperty(
//            _In_ JsValueRef object,
//            _In_ JsValueRef index,
//            _Out_ bool *result);
//  }
//  TJsHasIndexedProperty = function (
//    _object: JsValueRef;
//    index: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetIndexedProperty(
            _In_ JsValueRef object,
            _In_ JsValueRef index,
            _Out_ JsValueRef *result);
  }
  TJsGetIndexedProperty = function (
    _object: JsValueRef;
    index: JsValueRef;
    var result: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsSetIndexedProperty(
            _In_ JsValueRef object,
            _In_ JsValueRef index,
            _In_ JsValueRef value);
  }
  TJsSetIndexedProperty = function (
    _object: JsValueRef;
    index: JsValueRef;
    value: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsDeleteIndexedProperty(
//            _In_ JsValueRef object,
//            _In_ JsValueRef index);
//  }
//  TJsDeleteIndexedProperty = function (
//    _object: JsValueRef;
//    index: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsHasIndexedPropertiesExternalData(
//            _In_ JsValueRef object,
//            _Out_ bool* value);
//  }
//  TJsHasIndexedPropertiesExternalData = function (
//    _object: JsValueRef;
//    var value: bool
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetIndexedPropertiesExternalData(
//            _In_ JsValueRef object,
//            _Out_ void** data,
//            _Out_ JsTypedArrayType* arrayType,
//            _Out_ unsigned int* elementLength);
//  }
//  TJsGetIndexedPropertiesExternalData = function (
//    _object: JsValueRef;
//    data: Pointer;
//    var arrayType: JsTypedArrayType;
//    var elementLength: uint
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetIndexedPropertiesToExternalData(
//            _In_ JsValueRef object,
//            _In_ void* data,
//            _In_ JsTypedArrayType arrayType,
//            _In_ unsigned int elementLength);
//  }
//  TJsSetIndexedPropertiesToExternalData = function (
//    _object: JsValueRef;
//    data: Pointer;
//    arrayType: JsTypedArrayType;
//    elementLength: uint
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsEquals(
//            _In_ JsValueRef object1,
//            _In_ JsValueRef object2,
//            _Out_ bool *result);
//  }
//  TJsEquals = function (
//    object1: JsValueRef;
//    object2: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsStrictEquals(
//            _In_ JsValueRef object1,
//            _In_ JsValueRef object2,
//            _Out_ bool *result);
//  }
//  TJsStrictEquals = function (
//    object1: JsValueRef;
//    object2: JsValueRef;
//    var result: bool
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsHasExternalData(
            _In_ JsValueRef object,
            _Out_ bool *value);
  }
  TJsHasExternalData = function (
    _object: JsValueRef;
    var value: bool
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsGetExternalData(
            _In_ JsValueRef object,
            _Out_ void **externalData);
  }
  TJsGetExternalData = function (
    _object: JsValueRef;
    var externalData: Pointer
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsSetExternalData(
//            _In_ JsValueRef object,
//            _In_opt_ void *externalData);
//  }
//  TJsSetExternalData = function (
//    _object: JsValueRef;
//    var externalData: Pointer
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsCreateArray(
            _In_ unsigned int length,
            _Out_ JsValueRef *result);
  }
  TJsCreateArray = function (
    length: uint;
    var result: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsCreateArrayBuffer(
//            _In_ unsigned int byteLength,
//            _Out_ JsValueRef *result);
//  }
//  TJsCreateArrayBuffer = function (
//    byteLength: uint;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
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
  TJsCreateExternalArrayBuffer = function (
    data: Pointer;
    byteLength: uint;
    finalizeCallback: JsFinalizeCallback;
    callbackState: Pointer;
    var result: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsCreateTypedArray(
//            _In_ JsTypedArrayType arrayType,
//            _In_ JsValueRef baseArray,
//            _In_ unsigned int byteOffset,
//            _In_ unsigned int elementLength,
//            _Out_ JsValueRef *result);
//  }
//  TJsCreateTypedArray = function (
//    arrayType: JsTypedArrayType;
//    baseArray: JsValueRef;
//    byteOffset: uint;
//    elementLength: uint;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateDataView(
//            _In_ JsValueRef arrayBuffer,
//            _In_ unsigned int byteOffset,
//            _In_ unsigned int byteLength,
//            _Out_ JsValueRef *result);
//  }
//  TJsCreateDataView = function (
//    arrayBuffer: JsValueRef;
//    byteOffset: uint;
//    byteLength: uint;
//    result: JsValueRef
//  ): JsErrorCode; stdcall;
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
//  TJsGetTypedArrayInfo = function (
//    typedArray: JsValueRef;
//    var arrayType: JsTypedArrayType;
//    var arrayBuffer: JsValueRef;
//    var byteOffset: uint;
//    var byteLength: uint
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetArrayBufferStorage(
//            _In_ JsValueRef arrayBuffer,
//            _Outptr_result_bytebuffer_(*bufferLength) ChakraBytePtr *buffer,
//            _Out_ unsigned int *bufferLength);
//  }
//  TJsGetArrayBufferStorage = function (
//    arrayBuffer: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint
//  ): JsErrorCode; stdcall;
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
//  TJsGetTypedArrayStorage = function (
//    typedArray: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint;
//    var arrayType: JsTypedArrayType;
//    var elementSize: int
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsGetDataViewStorage(
//            _In_ JsValueRef dataView,
//            _Outptr_result_bytebuffer_(*bufferLength) ChakraBytePtr *buffer,
//            _Out_ unsigned int *bufferLength);
//  }
//  TJsGetDataViewStorage = function (
//    dataView: JsValueRef;
//    var buffer: ChakraBytePtr;
//    var bufferLength: uint
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsCallFunction(
            _In_ TJsValueRe = function f
            _In_reads_(argumentCount) JsValueRef *arguments,
            _In_ unsigned short argumentCount,
            _Out_opt_ JsValueRef *result);
  }
  TJsCallFunction = function (
    _function: JsValueRef;
    arguments: PJsValueRef;
    argumentCount: short;
    var result: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsConstructObject(
//            _In_ TJsValueRe = function f
//            _In_reads_(argumentCount) JsValueRef *arguments,
//            _In_ unsigned short argumentCount,
//            _Out_ JsValueRef *result);
//  }
//  TJsConstructObject = function (
//    _function: JsValueRef;
//    arguments: PJsValueRef;
//    argumentCount: short;
//    var result: JsValueRef
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsCreateFunction(
            _In_ JsNativeFunction nativeFunction,
            _In_opt_ void *callbackState,
            _Out_ TJsValueRef * = function ;
  }
  TJsCreateFunction = function (
    nativeFunction: JsNativeFunction;
    callbackState: Pointer;
    var _function: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsCreateNamedFunction(
//            _In_ JsValueRef name,
//            _In_ JsNativeFunction nativeFunction,
//            _In_opt_ void *callbackState,
//            _Out_ TJsValueRef * = function ;
//  }
//  TJsCreateNamedFunction = function (
//    name: JsValueRef;
//    nativeFunction: JsNativeFunction;
//    callbackState: Pointer;
//    var _function: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateRangeError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateRangeError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateReferenceError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateReferenceError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateSyntaxError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateSyntaxError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateTypeError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateTypeError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsCreateURIError(
//            _In_ JsValueRef message,
//            _Out_ JsValueRef *error);
//  }
//  TJsCreateURIError = function (
//    _message: JsValueRef;
//    var error: JsValueRef
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsHasException(
//            _Out_ bool *hasException);
//  }
//  TJsHasException = function (
//    var hasException: bool
//  ): JsErrorCode; stdcall;
//
  {
    CHAKRA_API
        JsGetAndClearException(
            _Out_ JsValueRef *exception);
  }
  TJsGetAndClearException = function (
    var exception: JsValueRef
  ): JsErrorCode; stdcall;

  {
    CHAKRA_API
        JsSetException(
            _In_ JsValueRef exception);
  }
  TJsSetException = function (
    exception: JsValueRef
  ): JsErrorCode; stdcall;

//  {
//    CHAKRA_API
//        JsDisableRuntimeExecution(
//            _In_ JsRuntimeHandle runtime);
//  }
//  TJsDisableRuntimeExecution = function (
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsEnableRuntimeExecution(
//            _In_ JsRuntimeHandle runtime);
//  }
//  TJsEnableRuntimeExecution = function (
//    runtime: JsRuntimeHandle
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsIsRuntimeExecutionDisabled(
//            _In_ JsRuntimeHandle runtime,
//            _Out_ bool *isDisabled);
//  }
//  TJsIsRuntimeExecutionDisabled = function (
//    runtime: JsRuntimeHandle;
//    var isDisabled: bool
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsSetPromiseContinuationCallback(
//            _In_ JsPromiseContinuationCallback promiseContinuationCallback,
//            _In_opt_ void *callbackState);
//  }
//  TJsSetPromiseContinuationCallback = function (
//    promiseContinuationCallback: JsPromiseContinuationCallback;
//    callbackState: Pointer
//  ): JsErrorCode; stdcall;
//
//  {
//    CHAKRA_API
//        JsStringFree(
//            _In_ char* stringValue);
//  }
//  TJsStringFree = function (
//    stringValue: PAnsiChar
//  ): JsErrorCode; stdcall;
//

var
  JsCreateRuntime : TJsCreateRuntime;
  JsDisposeRuntime : TJsDisposeRuntime;
//  JsGetRuntimeMemoryUsage : TJsGetRuntimeMemoryUsage;
//  JsGetRuntimeMemoryLimit : TJsGetRuntimeMemoryLimit;
//  JsSetRuntimeMemoryLimit : TJsSetRuntimeMemoryLimit;
//  JsSetRuntimeMemoryAllocationCallback : TJsSetRuntimeMemoryAllocationCallback;
//  JsSetRuntimeBeforeCollectCallback : TJsSetRuntimeBeforeCollectCallback;
//  JsAddRef : TJsAddRef;
//  JsRelease : TJsRelease;
  JsSetObjectBeforeCollectCallback : TJsSetObjectBeforeCollectCallback;
  JsCreateContext : TJsCreateContext;
//  JsGetCurrentContext : TJsGetCurrentContext;
  JsSetCurrentContext : TJsSetCurrentContext;
//  JsGetContextOfObject : TJsGetContextOfObject;
//  JsGetContextData : TJsGetContextData;
//  JsSetContextData : TJsSetContextData;
//  JsGetRuntime : TJsGetRuntime;
//  JsIdle : TJsIdle;
//  JsParseScript : TJsParseScript;
//  JsParseScriptWithAttributesUtf8 : TJsParseScriptWithAttributesUtf8;
  JsRunScript : TJsRunScript;
 JsRun : TJsRun;
//  JsSerializeScriptUtf8 : TJsSerializeScriptUtf8;
//  JsParseSerializedScriptUtf8 : TJsParseSerializedScriptUtf8;
//  JsRunSerializedScriptUtf8 : TJsRunSerializedScriptUtf8;
//  JsGetPropertyIdFromNameUtf8 : TJsGetPropertyIdFromNameUtf8;
//  JsGetPropertyNameFromIdUtf8Copy : TJsGetPropertyNameFromIdUtf8Copy;
  JsPointerToString : TJsPointerToString;
  JsStringToPointer : TJsStringToPointer;
//  JsGetSymbolFromPropertyId : TJsGetSymbolFromPropertyId;
//  JsGetPropertyIdType : TJsGetPropertyIdType;
//  JsGetPropertyIdFromSymbol : TJsGetPropertyIdFromSymbol;
//  JsCreateSymbol : TJsCreateSymbol;
//  JsGetOwnPropertySymbols : TJsGetOwnPropertySymbols;
//  JsGetUndefinedValue : TJsGetUndefinedValue;
  JsGetNullValue : TJsGetNullValue;
//  JsGetTrueValue : TJsGetTrueValue;
//  JsGetFalseValue : TJsGetFalseValue;
  JsBoolToBoolean : TJsBoolToBoolean;
  JsBooleanToBool : TJsBooleanToBool;
//  JsConvertValueToBoolean : TJsConvertValueToBoolean;
  JsGetValueType : TJsGetValueType;
//  JsDoubleToNumber : TJsDoubleToNumber;
  JsIntToNumber : TJsIntToNumber;
//  JsNumberToDouble : TJsNumberToDouble;
  JsNumberToInt : TJsNumberToInt;
//  JsConvertValueToNumber : TJsConvertValueToNumber;
//  JsGetStringLength : TJsGetStringLength;
  JsConvertValueToString : TJsConvertValueToString;
  JsGetGlobalObject : TJsGetGlobalObject;
  JsCreateObject : TJsCreateObject;
  JsCreateString : TJsCreateString;
  JsCreateExternalObject : TJsCreateExternalObject;
//  JsConvertValueToObject : TJsConvertValueToObject;
//  JsGetPrototype : TJsGetPrototype;
//  JsSetPrototype : TJsSetPrototype;
//  JsInstanceOf : TJsInstanceOf;
//  JsGetExtensionAllowed : TJsGetExtensionAllowed;
  JsCreatePropertyId : TJsCreatePropertyId;
//  JsPreventExtension : TJsPreventExtension;
  JsGetProperty : TJsGetProperty;
  JsGetOwnPropertyNames : TJsGetOwnPropertyNames;
//  JsGetOwnPropertyDescriptor : TJsGetOwnPropertyDescriptor;
  JsSetProperty : TJsSetProperty;
//JsHasPropertyJsHasProperty//  JsDeleteProperty : TJsDeleteProperty;
  JsDefineProperty : TJsDefineProperty;
//JsDefinePropertyJsDefineProperty//  JsHasIndexedProperty : TJsHasIndexedProperty;
  JsGetIndexedProperty : TJsGetIndexedProperty;
  JsSetIndexedProperty : TJsSetIndexedProperty;
//  JsDeleteIndexedProperty : TJsDeleteIndexedProperty;
//  JsHasIndexedPropertiesExternalData : TJsHasIndexedPropertiesExternalData;
//  JsGetIndexedPropertiesExternalData : TJsGetIndexedPropertiesExternalData;
//  JsSetIndexedPropertiesToExternalData : TJsSetIndexedPropertiesToExternalData;
//  JsEquals : TJsEquals;
//  JsStrictEquals : TJsStrictEquals;
  JsHasExternalData : TJsHasExternalData;
  JsGetExternalData : TJsGetExternalData;
//  JsSetExternalData : TJsSetExternalData;
  JsCreateArray : TJsCreateArray;
//  JsCreateArrayBuffer : TJsCreateArrayBuffer;
  JsCreateExternalArrayBuffer : TJsCreateExternalArrayBuffer;
//  JsCreateTypedArray : TJsCreateTypedArray;
//  JsCreateDataView : TJsCreateDataView;
//  JsGetTypedArrayInfo : TJsGetTypedArrayInfo;
//  JsGetArrayBufferStorage : TJsGetArrayBufferStorage;
//  JsGetTypedArrayStorage : TJsGetTypedArrayStorage;
//  JsGetDataViewStorage : TJsGetDataViewStorage;
  JsCallFunction : TJsCallFunction;
//  JsConstructObject : TJsConstructObject;
  JsCreateFunction : TJsCreateFunction;
//  JsCreateNamedFunction : TJsCreateNamedFunction;
//  JsCreateError : TJsCreateError;
//  JsCreateRangeError : TJsCreateRangeError;
//  JsCreateReferenceError : TJsCreateReferenceError;
//  JsCreateSyntaxError : TJsCreateSyntaxError;
//  JsCreateTypeError : TJsCreateTypeError;
//  JsCreateURIError : TJsCreateURIError;
//  JsHasException : TJsHasException;
  JsGetAndClearException : TJsGetAndClearException;
  JsSetException : TJsSetException;
//  JsDisableRuntimeExecution : TJsDisableRuntimeExecution;
//  JsEnableRuntimeExecution : TJsEnableRuntimeExecution;
//  JsIsRuntimeExecutionDisabled : TJsIsRuntimeExecutionDisabled;
//  JsSetPromiseContinuationCallback : TJsSetPromiseContinuationCallback;
//  JsStringFree : TJsStringFree;

function loadChakra(chakraPath : String; var msg : String) : boolean;
procedure unloadChakra;

implementation

uses
  FHIR.Support.System;

var
  GHandle : THandle;

function loadChakra(chakraPath : String; var msg : String) : boolean;
var
  ok : boolean;
  function doLoad(name : String) : FARPROC;
  begin
    result := GetProcAddress(GHandle, pchar(name));
    if result = nil then
    begin
      ok := false;
      msg := msg + 'Unable to load function '+name+#13#10;
    end;
  end;
begin
  ok := false;
  msg := '';
  GHandle := LoadLibrary(DLL_NAME);
  If GHandle < 32 Then
    msg := 'Error Loading Chakra.dll: '+ErrorAsString(GetLastError)
  else
  begin
    ok := true;
    @JsCreateRuntime := doload('JsCreateRuntime');
    @JsDisposeRuntime := doLoad('JsDisposeRuntime');
    @JsSetObjectBeforeCollectCallback := doLoad('JsSetObjectBeforeCollectCallback');
    @JsCreateContext := doLoad('JsCreateContext');
    @JsSetCurrentContext := doLoad('JsSetCurrentContext');
    @JsRunScript := doLoad('JsRunScript');
    @JsRun := doLoad('JsRun');
    @JsPointerToString := doLoad('JsPointerToString');
    @JsStringToPointer := doLoad('JsStringToPointer');
    @JsGetNullValue := doLoad('JsGetNullValue');
    @JsBoolToBoolean := doLoad('JsBoolToBoolean');
    @JsBooleanToBool := doLoad('JsBooleanToBool');
    @JsGetValueType := doLoad('JsGetValueType');
    @JsIntToNumber := doLoad('JsIntToNumber');
    @JsNumberToInt := doLoad('JsNumberToInt');
    @JsConvertValueToString := doLoad('JsConvertValueToString');
    @JsGetGlobalObject := doLoad('JsGetGlobalObject');
    @JsCreateObject := doLoad('JsCreateObject');
    @JsCreateString := doLoad('JsCreateString');
    @JsCreateExternalObject := doLoad('JsCreateExternalObject');
    @JsCreatePropertyId := doLoad('JsCreatePropertyId');
    @JsGetProperty := doLoad('JsGetProperty');
    @JsGetOwnPropertyNames := doLoad('JsGetOwnPropertyNames');
    @JsSetProperty := doLoad('JsSetProperty');
    @JsDefineProperty := doLoad('JsDefineProperty');
    @JsGetIndexedProperty := doLoad('JsGetIndexedProperty');
    @JsSetIndexedProperty := doLoad('JsSetIndexedProperty');
    @JsHasExternalData := doLoad('JsHasExternalData');
    @JsGetExternalData := doLoad('JsGetExternalData');
    @JsCreateArray := doLoad('JsCreateArray');
    @JsCreateExternalArrayBuffer := doLoad('JsCreateExternalArrayBuffer');
    @JsCallFunction := doLoad('JsCallFunction');
    @JsCreateFunction := doLoad('JsCreateFunction');
    @JsGetAndClearException := doLoad('JsGetAndClearException');
    @JsSetException := doLoad('JsSetException');
  end;
  result := ok;
end;

procedure unloadChakra;
begin
  @JsCreateRuntime := nil;
  @JsDisposeRuntime := nil;
  @JsSetObjectBeforeCollectCallback := nil;
  @JsCreateContext := nil;
  @JsSetCurrentContext := nil;
  @JsRunScript := nil;
  @JsRun := nil;
  @JsPointerToString := nil;
  @JsStringToPointer := nil;
  @JsGetNullValue := nil;
  @JsBoolToBoolean := nil;
  @JsBooleanToBool := nil;
  @JsGetValueType := nil;
  @JsIntToNumber := nil;
  @JsNumberToInt := nil;
  @JsConvertValueToString := nil;
  @JsGetGlobalObject := nil;
  @JsCreateObject := nil;
  @JsCreateString := nil;
  @JsCreateExternalObject := nil;
  @JsCreatePropertyId := nil;
  @JsGetProperty := nil;
  @JsGetOwnPropertyNames := nil;
  @JsSetProperty := nil;
  @JsDefineProperty := nil;
  @JsGetIndexedProperty := nil;
  @JsSetIndexedProperty := nil;
  @JsHasExternalData := nil;
  @JsGetExternalData := nil;
  @JsCreateArray := nil;
  @JsCreateExternalArrayBuffer := nil;
  @JsCallFunction := nil;
  @JsCreateFunction := nil;
  @JsGetAndClearException := nil;
  @JsSetException := nil;
  FreeLibrary(GHandle);
  GHandle := 0;
end;

end.
