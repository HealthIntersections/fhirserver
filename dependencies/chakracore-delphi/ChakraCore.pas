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

//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
/// \mainpage Chakra Hosting API Reference
///
/// Chakra is Microsoft's JavaScript engine. It is an integral part of Internet Explorer but can
/// also be hosted independently by other applications. This reference describes the APIs available
/// to applications to host Chakra.
///

/// \file
/// \brief The Chakra Core hosting API.
///
/// This file contains a flat C API layer. This is the API exported by ChakraCore.dll.

unit ChakraCore;

{$include common.inc}

{$minenumsize 4}

interface

uses
  Compat, ChakraCommon;

type
  /// <summary>
  ///     A reference to an ES module.
  /// </summary>
  /// <remarks>
  ///     A module record represents an ES module.
  /// </remarks>
  JsModuleRecord = Pointer;

  /// <summary>
  ///     A reference to an object owned by the SharedArrayBuffer.
  /// </summary>
  /// <remarks>
  ///     This represents SharedContents which is heap allocated object, it can be passed through
  ///     different runtimes to share the underlying buffer.
  /// </remarks>
  JsSharedArrayBufferContentHandle = Pointer;

  /// <summary>
  ///     Flags for parsing a module.
  /// </summary>
  JsParseModuleSourceFlags = (
    /// <summary>
    ///     Module source is UTF16.
    /// </summary>
    JsParseModuleSourceFlags_DataIsUTF16LE = $00000000,
    /// <summary>
    ///     Module source is UTF8.
    /// </summary>
    JsParseModuleSourceFlags_DataIsUTF8 = $00000001
  );

  /// <summary>
  ///     The types of host info that can be set on a module record with JsSetModuleHostInfo.
  /// </summary>
  /// <remarks>
  ///     For more information see JsSetModuleHostInfo.
  /// </remarks>
  JsModuleHostInfoKind = (
    /// <summary>
    ///     An exception object - e.g. if the module file cannot be found.
    /// </summary>
    JsModuleHostInfo_Exception = $01,
    /// <summary>
    ///     Host defined info.
    /// </summary>
    JsModuleHostInfo_HostDefined = $02,
    /// <summary>
    ///     Callback for receiving notification when module is ready.
    /// </summary>
    JsModuleHostInfo_NotifyModuleReadyCallback = $03,
    /// <summary>
    ///     Callback for receiving notification to fetch a dependent module.
    /// </summary>
    JsModuleHostInfo_FetchImportedModuleCallback = $04,
    /// <summary>
    ///     Callback for receiving notification for calls to ```import()```
    /// </summary>
    JsModuleHostInfo_FetchImportedModuleFromScriptCallback = $05,
    /// <summary>
    ///     URL for use in error stack traces and debugging.
    /// </summary>
    JsModuleHostInfo_Url = $06
  );

  /// <summary>
  ///     The possible states for a Promise object.
  /// </summary>
  JsPromiseState = (
    JsPromiseStatePending = $0,
    JsPromiseStateFulfilled = $1,
    JsPromiseStateRejected = $2
  );

  /// <summary>
  ///     User implemented callback to fetch additional imported modules in ES modules.
  /// </summary>
  /// <remarks>
  ///     The callback is invoked on the current runtime execution thread, therefore execution is blocked until
  ///     the callback completes. Notify the host to fetch the dependent module. This is the "import" part
  ///     before HostResolveImportedModule in ES6 spec. This notifies the host that the referencing module has
  ///     the specified module dependency, and the host needs to retrieve the module back.
  ///
  ///     Callback should:
  ///     1. Check if the requested module has been requested before - if yes return the existing
  ///         module record
  ///     2. If no create and initialize a new module record with JsInitializeModuleRecord to
  ///         return and schedule a call to JsParseModuleSource for the new record.
  /// </remarks>
  /// <param name="referencingModule">The referencing module that is requesting the dependent module.</param>
  /// <param name="specifier">The specifier coming from the module source code.</param>
  /// <param name="dependentModuleRecord">The ModuleRecord of the dependent module. If the module was requested
  ///                                     before from other source, return the existing ModuleRecord, otherwise
  ///                                     return a newly created ModuleRecord.</param>
  /// <returns>
  ///     Returns a <c>JsNoError</c> if the operation succeeded an error code otherwise.
  /// </returns>
  FetchImportedModuleCallBack = function(referencingModule: JsModuleRecord; specifier: JsValueRef;
      out dependentModuleRecord: JsModuleRecord): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     User implemented callback to fetch imported modules dynamically in scripts.
  /// </summary>
  /// <remarks>
  ///     The callback is invoked on the current runtime execution thread, therefore execution is blocked untill
  ///     the callback completes. Notify the host to fetch the dependent module. This is used for the dynamic
  ///     import() syntax.
  ///
  ///     Callback should:
  ///     1. Check if the requested module has been requested before - if yes return the existing module record
  ///     2. If no create and initialize a new module record with JsInitializeModuleRecord to return and
  ///         schedule a call to JsParseModuleSource for the new record.
  /// </remarks>
  /// <param name="dwReferencingSourceContext">The referencing script context that calls import()</param>
  /// <param name="specifier">The specifier provided to the import() call.</param>
  /// <param name="dependentModuleRecord">The ModuleRecord of the dependent module. If the module was requested
  ///                                     before from other source, return the existing ModuleRecord, otherwise
  ///                                     return a newly created ModuleRecord.</param>
  /// <returns>
  ///     Returns <c>JsNoError</c> if the operation succeeded or an error code otherwise.
  /// </returns>
  FetchImportedModuleFromScriptCallBack = function(dwReferencingSourceContext: JsSourceContext; specifier: JsValueRef;
    out dependentModuleRecord: JsModuleRecord): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     User implemented callback to get notification when the module is ready.
  /// </summary>
  /// <remarks>
  ///     The callback is invoked on the current runtime execution thread, therefore execution is blocked until the
  ///     callback completes. This callback should schedule a call to JsEvaluateModule to run the module that has been loaded.
  /// </remarks>
  /// <param name="referencingModule">The referencing module that has finished running ModuleDeclarationInstantiation step.</param>
  /// <param name="exceptionVar">If nullptr, the module is successfully initialized and host should queue the execution job
  ///                            otherwise it's the exception object.</param>
  /// <returns>
  ///     Returns a JsErrorCode - note, the return value is ignored.
  /// </returns>
  NotifyModuleReadyCallback = function(referencingModule: JsModuleRecord; exceptionVar: JsValueRef): JsErrorCode;
    {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     A structure containing information about a native function callback.
  /// </summary>
  PJsNativeFunctionInfo = ^JsNativeFunctionInfo;
  JsNativeFunctionInfo = record
    thisArg: JsValueRef;
    newTargetArg: JsValueRef;
    isConstructCall: bool;
  end;

  /// <summary>
  ///     A function callback.
  /// </summary>
  /// <param name="callee">
  ///     A function object that represents the function being invoked.
  /// </param>
  /// <param name="arguments">The arguments to the call.</param>
  /// <param name="argumentCount">The number of arguments.</param>
  /// <param name="info">Additional information about this function call.</param>
  /// <param name="callbackState">
  ///     The state passed to <c>JsCreateFunction</c>.
  /// </param>
  /// <returns>The result of the call, if any.</returns>
  JsEnhancedNativeFunction = function(callee: JsValueRef; arguments: PJsValueRef; argumentCount: Word;
      info: PJsNativeFunctionInfo; callbackState: Pointer): JsValueRef; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     A Promise Rejection Tracker callback.
  /// </summary>
  /// <remarks>
  ///     The host can specify a promise rejection tracker callback in <c>JsSetHostPromiseRejectionTracker</c>.
  ///     If a promise is rejected with no reactions or a reaction is added to a promise that was rejected
  ///     before it had reactions by default nothing is done.
  ///     A Promise Rejection Tracker callback may be set - which will then be called when this occurs.
  ///     Note - per draft ECMASpec 2018 25.4.1.9 this function should not set or return an exception
  ///     Note also the promise and reason parameters may be garbage collected after this function is called
  ///     if you wish to make further use of them you will need to use JsAddRef to preserve them
  ///     However if you use JsAddRef you must also call JsRelease and not hold unto them after
  ///     a handled notification (both per spec and to avoid memory leaks)
  /// </remarks>
  /// <param name="promise">The promise object, represented as a JsValueRef.</param>
  /// <param name="reason">The value/cause of the rejection, represented as a JsValueRef.</param>
  /// <param name="handled">Boolean - false for promiseRejected: i.e. if the promise has just been rejected with no handler,
  ///                         true for promiseHandled: i.e. if it was rejected before without a handler and is now being handled.</param>
  /// <param name="callbackState">The state passed to <c>JsSetHostPromiseRejectionTracker</c>.</param>
  JsHostPromiseRejectionTrackerCallback = procedure(promise, reason: JsValueRef; handled: bool; callbackState: Pointer);
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Creates a new enhanced JavaScript function.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="nativeFunction">The method to call when the function is invoked.</param>
  /// <param name="metadata">If this is not <c>JS_INVALID_REFERENCE</c>, it is converted to a string and used as the name of the function.</param>
  /// <param name="callbackState">
  ///     User provided state that will be passed back to the callback.
  /// </param>
  /// <param name="function">The new function object.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateEnhancedFunction(nativeFunction: JsEnhancedNativeFunction; metadata: JsValueRef;
      callbackState: Pointer; out _function: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Initialize a ModuleRecord from host
  /// </summary>
  /// <remarks>
  ///     Bootstrap the module loading process by creating a new module record.
  /// </remarks>
  /// <param name="referencingModule">The parent module of the new module - nullptr for a root module.</param>
  /// <param name="normalizedSpecifier">The normalized specifier for the module.</param>
  /// <param name="moduleRecord">The new module record. The host should not try to call this API twice
  ///                            with the same normalizedSpecifier.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsInitializeModuleRecord(
      referencingModule: JsModuleRecord;
      normalizedSpecifier: JsValueRef;
      out moduleRecord: JsModuleRecord): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Parse the source for an ES module
  /// </summary>
  /// <remarks>
  ///     This is basically ParseModule operation in ES6 spec. It is slightly different in that:
  ///     a) The ModuleRecord was initialized earlier, and passed in as an argument.
  ///     b) This includes a check to see if the module being Parsed is the last module in the
  /// dependency tree. If it is it automatically triggers Module Instantiation.
  /// </remarks>
  /// <param name="requestModule">The ModuleRecord being parsed.</param>
  /// <param name="sourceContext">A cookie identifying the script that can be used by debuggable script contexts.</param>
  /// <param name="script">The source script to be parsed, but not executed in this code.</param>
  /// <param name="scriptLength">The length of sourceText in bytes. As the input might contain a embedded null.</param>
  /// <param name="sourceFlag">The type of the source code passed in. It could be utf16 or utf8 at this time.</param>
  /// <param name="exceptionValueRef">The error object if there is parse error.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsParseModuleSource(
      requestModule: JsModuleRecord;
      sourceContext: JsSourceContext;
      script: PByte;
      scriptLength: Cardinal;
      sourceFlag: JsParseModuleSourceFlags;
      out exceptionValueRef: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Execute module code.
  /// </summary>
  /// <remarks>
  ///     This method implements 15.2.1.1.6.5, "ModuleEvaluation" concrete method.
  ///     This method should be called after the engine notifies the host that the module is ready.
  ///     This method only needs to be called on root modules - it will execute all of the dependent modules.
  ///
  ///     One moduleRecord will be executed only once. Additional execution call on the same moduleRecord will fail.
  /// </remarks>
  /// <param name="requestModule">The ModuleRecord being executed.</param>
  /// <param name="result">The return value of the module.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsModuleEvaluation(
      requestModule: JsModuleRecord;
      out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Set host info for the specified module.
  /// </summary>
  /// <remarks>
  ///     This is used for four things:
  ///     1. Setting up the callbacks for module loading - note these are actually
  ///         set on the current Context not the module so only have to be set for
  ///         the first root module in any given context.
  ///     2. Setting host defined info on a module record - can be anything that
  ///         you wish to associate with your modules.
  ///     3. Setting a URL for a module to be used for stack traces/debugging -
  ///         note this must be set before calling JsParseModuleSource on the module
  ///         or it will be ignored.
  ///     4. Setting an exception on the module object - only relevant prior to it being Parsed.
  /// </remarks>
  /// <param name="requestModule">The request module.</param>
  /// <param name="moduleHostInfo">The type of host info to be set.</param>
  /// <param name="hostInfo">The host info to be set.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsSetModuleHostInfo(
      requestModule: JsModuleRecord;
      moduleHostInfo: JsModuleHostInfoKind;
      hostInfo: Pointer): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Retrieve the host info for the specified module.
  /// </summary>
  /// <remarks>
  ///     This can used to retrieve info previously set with JsSetModuleHostInfo.
  /// </remarks>
  /// <param name="requestModule">The request module.</param>
  /// <param name="moduleHostInfo">The type of host info to be retrieved.</param>
  /// <param name="hostInfo">The retrieved host info for the module.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetModuleHostInfo(
      requestModule: JsModuleRecord;
      moduleHostInfo: JsModuleHostInfoKind;
      out hostInfo: Pointer): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Returns metadata relating to the exception that caused the runtime of the current context
  ///     to be in the exception state and resets the exception state for that runtime. The metadata
  ///     includes a reference to the exception itself.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     If the runtime of the current context is not in an exception state, this API will return
  ///     <c>JsErrorInvalidArgument</c>. If the runtime is disabled, this will return an exception
  ///     indicating that the script was terminated, but it will not clear the exception (the
  ///     exception will be cleared if the runtime is re-enabled using
  ///     <c>JsEnableRuntimeExecution</c>).
  ///     </para>
  ///     <para>
  ///     The metadata value is a javascript object with the following properties: <c>exception</c>, the
  ///     thrown exception object; <c>line</c>, the 0 indexed line number where the exception was thrown;
  ///     <c>column</c>, the 0 indexed column number where the exception was thrown; <c>length</c>, the
  ///     source-length of the cause of the exception; <c>source</c>, a string containing the line of
  ///     source code where the exception was thrown; and <c>url</c>, a string containing the name of
  ///     the script file containing the code that threw the exception.
  ///     </para>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  /// </remarks>
  /// <param name="metadata">The exception metadata for the runtime of the current context.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetAndClearExceptionWithMetadata(
      out metadata: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

type
  /// <summary>
  ///     Called by the runtime to load the source code of the serialized script.
  /// </summary>
  /// <param name="sourceContext">The context passed to Js[Parse|Run]SerializedScriptCallback</param>
  /// <param name="script">The script returned.</param>
  /// <returns>
  ///     true if the operation succeeded, false otherwise.
  /// </returns>
  JsSerializedLoadScriptCallback = function(
      sourceContext: JsSourceContext;
      out value: JsValueRef;
      out parseAttributes: JsParseScriptAttributes): bool; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Create JavascriptString variable from ASCII or Utf8 string
  /// </summary>
  /// <remarks>
  ///     <para>
  ///        Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Input string can be either ASCII or Utf8
  ///     </para>
  /// </remarks>
  /// <param name="content">Pointer to string memory.</param>
  /// <param name="length">Number of bytes within the string</param>
  /// <param name="value">JsValueRef representing the JavascriptString</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateString(
          content: PAnsiChar;
          length: size_t;
          out value: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Create JavascriptString variable from Utf16 string
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Expects Utf16 string
  ///     </para>
  /// </remarks>
  /// <param name="content">Pointer to string memory.</param>
  /// <param name="length">Number of characters within the string</param>
  /// <param name="value">JsValueRef representing the JavascriptString</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateStringUtf16(
          content: PUnicodeChar;
          length: size_t;
          out value: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Write JavascriptString value into C string buffer (Utf8)
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         When size of the `buffer` is unknown,
  ///         `buffer` argument can be nullptr.
  ///         In that case, `length` argument will return the length needed.
  ///     </para>
  /// </remarks>
  /// <param name="value">JavascriptString value</param>
  /// <param name="buffer">Pointer to buffer</param>
  /// <param name="bufferSize">Buffer size</param>
  /// <param name="length">Total number of characters needed or written</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCopyString(
          value: JsValueRef;
          buffer: PAnsiChar;
          bufferSize: size_t;
          length: Psize_t): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Write string value into Utf16 string buffer
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         When size of the `buffer` is unknown,
  ///         `buffer` argument can be nullptr.
  ///         In that case, `written` argument will return the length needed.
  ///     </para>
  ///     <para>
  ///         when start is out of range or &lt; 0, returns JsErrorInvalidArgument
  ///         and `written` will be equal to 0.
  ///         If calculated length is 0 (It can be due to string length or `start`
  ///         and length combination), then `written` will be equal to 0 and call
  ///         returns JsNoError
  ///     </para>
  /// </remarks>
  /// <param name="value">JavascriptString value</param>
  /// <param name="start">start offset of buffer</param>
  /// <param name="length">length to be written</param>
  /// <param name="buffer">Pointer to buffer</param>
  /// <param name="written">Total number of characters written</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCopyStringUtf16(
          value: JsValueRef;
          start: Integer;
          length: Integer;
          buffer: PUnicodeChar;
          written: Psize_t): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Parses a script and returns a function representing the script.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Script source can be either JavascriptString or JavascriptExternalArrayBuffer.
  ///         In case it is an ExternalArrayBuffer, and the encoding of the buffer is Utf16,
  ///         JsParseScriptAttributeArrayBufferIsUtf16Encoded is expected on parseAttributes.
  ///     </para>
  ///     <para>
  ///         Use JavascriptExternalArrayBuffer with Utf8/ASCII script source
  ///         for better performance and smaller memory footprint.
  ///     </para>
  /// </remarks>
  /// <param name="script">The script to run.</param>
  /// <param name="sourceContext">
  ///     A cookie identifying the script that can be used by debuggable script contexts.
  /// </param>
  /// <param name="sourceUrl">The location the script came from.</param>
  /// <param name="parseAttributes">Attribute mask for parsing the script</param>
  /// <param name="result">The result of the compiled script.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsParse(
          script: JsValueRef;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          parseAttributes: JsParseScriptAttributes;
          out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Executes a script.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Script source can be either JavascriptString or JavascriptExternalArrayBuffer.
  ///         In case it is an ExternalArrayBuffer, and the encoding of the buffer is Utf16,
  ///         JsParseScriptAttributeArrayBufferIsUtf16Encoded is expected on parseAttributes.
  ///     </para>
  ///     <para>
  ///         Use JavascriptExternalArrayBuffer with Utf8/ASCII script source
  ///         for better performance and smaller memory footprint.
  ///     </para>
  /// </remarks>
  /// <param name="script">The script to run.</param>
  /// <param name="sourceContext">
  ///     A cookie identifying the script that can be used by debuggable script contexts.
  /// </param>
  /// <param name="sourceUrl">The location the script came from</param>
  /// <param name="parseAttributes">Attribute mask for parsing the script</param>
  /// <param name="result">The result of the script, if any. This parameter can be null.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsRun(
          script: JsValueRef;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          parseAttributes: JsParseScriptAttributes;
          out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Creates the property ID associated with the name.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Property IDs are specific to a context and cannot be used across contexts.
  ///     </para>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  /// </remarks>
  /// <param name="name">
  ///     The name of the property ID to get or create. The name may consist of only digits.
  ///     The string is expected to be ASCII / utf8 encoded.
  /// </param>
  /// <param name="length">length of the name in bytes</param>
  /// <param name="propertyId">The property ID in this runtime for the given name.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreatePropertyId(
          name: PAnsiChar;
          length: size_t;
          out propertyId: JsPropertyIdRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Copies the name associated with the property ID into a buffer.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         When size of the `buffer` is unknown,
  ///         `buffer` argument can be nullptr.
  ///         `length` argument will return the size needed.
  ///     </para>
  /// </remarks>
  /// <param name="propertyId">The property ID to get the name of.</param>
  /// <param name="buffer">The buffer holding the name associated with the property ID, encoded as utf8</param>
  /// <param name="bufferSize">Size of the buffer.</param>
  /// <param name="written">Total number of characters written or to be written</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCopyPropertyId(
          propertyId: JsPropertyIdRef;
          buffer: PAnsiChar;
          bufferSize: size_t;
          out length: size_t): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Serializes a parsed script to a buffer than can be reused.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     <c>JsSerializeScript</c> parses a script and then stores the parsed form of the script in a
  ///     runtime-independent format. The serialized script then can be deserialized in any
  ///     runtime without requiring the script to be re-parsed.
  ///     </para>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Script source can be either JavascriptString or JavascriptExternalArrayBuffer.
  ///         In case it is an ExternalArrayBuffer, and the encoding of the buffer is Utf16,
  ///         JsParseScriptAttributeArrayBufferIsUtf16Encoded is expected on parseAttributes.
  ///     </para>
  ///     <para>
  ///         Use JavascriptExternalArrayBuffer with Utf8/ASCII script source
  ///         for better performance and smaller memory footprint.
  ///     </para>
  /// </remarks>
  /// <param name="script">The script to serialize</param>
  /// <param name="buffer">ArrayBuffer</param>
  /// <param name="parseAttributes">Encoding for the script.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsSerialize(
          script: JsValueRef;
          out buffer: JsValueRef;
          parseAttributes: JsParseScriptAttributes): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Parses a serialized script and returns a function representing the script.
  ///     Provides the ability to lazy load the script source only if/when it is needed.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  /// </remarks>
  /// <param name="buffer">The serialized script as an ArrayBuffer (preferably ExternalArrayBuffer).</param>
  /// <param name="scriptLoadCallback">
  ///     Callback called when the source code of the script needs to be loaded.
  ///     This is an optional parameter, set to null if not needed.
  /// </param>
  /// <param name="sourceContext">
  ///     A cookie identifying the script that can be used by debuggable script contexts.
  ///     This context will passed into scriptLoadCallback.
  /// </param>
  /// <param name="sourceUrl">The location the script came from.</param>
  /// <param name="result">A function representing the script code.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsParseSerialized(
          buffer: JsValueRef;
          scriptLoadCallback: JsSerializedLoadScriptCallback;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Runs a serialized script.
  ///     Provides the ability to lazy load the script source only if/when it is needed.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  ///     <para>
  ///     The runtime will detach the data from the buffer and hold on to it until all
  ///     instances of any functions created from the buffer are garbage collected.
  ///     </para>
  /// </remarks>
  /// <param name="buffer">The serialized script as an ArrayBuffer (preferably ExternalArrayBuffer).</param>
  /// <param name="scriptLoadCallback">Callback called when the source code of the script needs to be loaded.</param>
  /// <param name="sourceContext">
  ///     A cookie identifying the script that can be used by debuggable script contexts.
  ///     This context will passed into scriptLoadCallback.
  /// </param>
  /// <param name="sourceUrl">The location the script came from.</param>
  /// <param name="result">
  ///     The result of running the script, if any. This parameter can be null.
  /// </param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsRunSerialized(
          buffer: JsValueRef;
          scriptLoadCallback: JsSerializedLoadScriptCallback;
          sourceContext: JsSourceContext;
          sourceUrl: JsValueRef;
          out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the state of a given Promise object.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="promise">The Promise object.</param>
  /// <param name="state">The current state of the Promise.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetPromiseState(
      promise: JsValueRef;
      out state: JsPromiseState): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the result of a given Promise object.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="promise">The Promise object.</param>
  /// <param name="result">The result of the Promise.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetPromiseResult(
      promise: JsValueRef;
      out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Creates a new JavaScript Promise object.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="promise">The new Promise object.</param>
  /// <param name="resolveFunction">The function called to resolve the created Promise object.</param>
  /// <param name="rejectFunction">The function called to reject the created Promise object.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreatePromise(
          out promise: JsValueRef;
          out resolveFunction: JsValueRef;
          out rejectFunction: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

type
  /// <summary>
  ///     A weak reference to a JavaScript value.
  /// </summary>
  /// <remarks>
  ///     A value with only weak references is available for garbage-collection. A strong reference
  ///     to the value (<c>JsValueRef</c>) may be obtained from a weak reference if the value happens
  ///     to still be available.
  /// </remarks>
  JsWeakRef = JsRef;

  /// <summary>
  ///     Creates a weak reference to a value.
  /// </summary>
  /// <param name="value">The value to be referenced.</param>
  /// <param name="weakRef">Weak reference to the value.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateWeakReference(
          value: JsValueRef;
          out weakRef: JsWeakRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets a strong reference to the value referred to by a weak reference.
  /// </summary>
  /// <param name="weakRef">A weak reference.</param>
  /// <param name="value">Reference to the value, or <c>JS_INVALID_REFERENCE</c> if the value is
  ///     no longer available.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetWeakReferenceValue(
          weakRef: JsWeakRef;
          value: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Creates a Javascript SharedArrayBuffer object with shared content get from JsGetSharedArrayBufferContent.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="sharedContents">
  ///     The storage object of a SharedArrayBuffer which can be shared between multiple thread.
  /// </param>
  /// <param name="result">The new SharedArrayBuffer object.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateSharedArrayBufferWithSharedContent(
      sharedContents: JsSharedArrayBufferContentHandle;
      out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Get the storage object from a SharedArrayBuffer.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="sharedArrayBuffer">The SharedArrayBuffer object.</param>
  /// <param name="sharedContents">
  ///     The storage object of a SharedArrayBuffer which can be shared between multiple thread.
  ///     User should call JsReleaseSharedArrayBufferContentHandle after finished using it.
  /// </param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetSharedArrayBufferContent(
      sharedArrayBuffer: JsValueRef;
      out sharedContents: JsSharedArrayBufferContentHandle): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Decrease the reference count on a SharedArrayBuffer storage object.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="sharedContents">
  ///     The storage object of a SharedArrayBuffer which can be shared between multiple thread.
  /// </param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsReleaseSharedArrayBufferContentHandle(
      sharedContents: JsSharedArrayBufferContentHandle): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determines whether an object has a non-inherited property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that may contain the property.</param>
  /// <param name="propertyId">The ID of the property.</param>
  /// <param name="hasOwnProperty">Whether the object has the non-inherited property.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsHasOwnProperty(_object: JsValueRef; propertyId: JsPropertyIdRef; out hasOwnProperty: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Write JS string value into char string buffer without a null terminator
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         When size of the `buffer` is unknown,
  ///         `buffer` argument can be nullptr.
  ///         In that case, `written` argument will return the length needed.
  ///     </para>
  ///     <para>
  ///         When start is out of range or &lt; 0, returns JsErrorInvalidArgument
  ///         and `written` will be equal to 0.
  ///         If calculated length is 0 (It can be due to string length or `start`
  ///         and length combination), then `written` will be equal to 0 and call
  ///         returns JsNoError
  ///     </para>
  ///     <para>
  ///         The JS string `value` will be converted one utf16 code point at a time,
  ///         and if it has code points that do not fit in one byte, those values
  ///         will be truncated.
  ///     </para>
  /// </remarks>
  /// <param name="value">JavascriptString value</param>
  /// <param name="start">Start offset of buffer</param>
  /// <param name="length">Number of characters to be written</param>
  /// <param name="buffer">Pointer to buffer</param>
  /// <param name="written">Total number of characters written</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCopyStringOneByte(value: JsValueRef; start, length: Integer; buffer: PAnsiChar;
      written: Psize_t): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Obtains frequently used properties of a data view.
  /// </summary>
  /// <param name="dataView">The data view instance.</param>
  /// <param name="arrayBuffer">The ArrayBuffer backstore of the view.</param>
  /// <param name="byteOffset">The offset in bytes from the start of arrayBuffer referenced by the array.</param>
  /// <param name="byteLength">The number of bytes in the array.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetDataViewInfo(dataView: JsValueRef; arrayBuffer: PJsValueRef;
      byteOffset, byteLength: PCardinal): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determine if one JavaScript value is less than another JavaScript value.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     This function is equivalent to the <c>&lt;</c> operator in Javascript.
  ///     </para>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  /// </remarks>
  /// <param name="object1">The first object to compare.</param>
  /// <param name="object2">The second object to compare.</param>
  /// <param name="result">Whether object1 is less than object2.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsLessThan(object1, object2: JsValueRef; out result: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determine if one JavaScript value is less than or equal to another JavaScript value.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     This function is equivalent to the <c>&lt;=</c> operator in Javascript.
  ///     </para>
  ///     <para>
  ///     Requires an active script context.
  ///     </para>
  /// </remarks>
  /// <param name="object1">The first object to compare.</param>
  /// <param name="object2">The second object to compare.</param>
  /// <param name="result">Whether object1 is less than or equal to object2.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsLessThanOrEqual(object1, object2: JsValueRef; out result: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Creates a new object (with prototype) that stores some external data.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="data">External data that the object will represent. May be null.</param>
  /// <param name="finalizeCallback">
  ///     A callback for when the object is finalized. May be null.
  /// </param>
  /// <param name="prototype">Prototype object or nullptr.</param>
  /// <param name="object">The new object.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsCreateExternalObjectWithPrototype(
      data: Pointer;
      finalizeCallback: JsFinalizeCallback;
      prototype: JsValueRef;
      out _object: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets an object's property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that contains the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="value">The value of the property.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectGetProperty(_object, key: JsValueRef; out value: JsValueRef): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Puts an object's property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that contains the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="value">The new value of the property.</param>
  /// <param name="useStrictRules">The property set should follow strict mode rules.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectSetProperty(_object, key, value: JsValueRef; useStrictRules: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determines whether an object has a property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that may contain the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="hasProperty">Whether the object (or a prototype) has the property.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectHasProperty(_object, key: JsValueRef; out hasProperty: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Defines a new object's own property from a property descriptor.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that has the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="propertyDescriptor">The property descriptor.</param>
  /// <param name="result">Whether the property was defined.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectDefineProperty(_object, key, propertyDescriptor: JsValueRef; out result: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Deletes an object's property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that contains the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="useStrictRules">The property set should follow strict mode rules.</param>
  /// <param name="result">Whether the property was deleted.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectDeleteProperty(_object, key: JsValueRef; useStrictRules: bool; out result: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets a property descriptor for an object's own property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that has the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="propertyDescriptor">The property descriptor.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectGetOwnPropertyDescriptor(_object, key: JsValueRef; out propertyDescriptor: JsValueRef): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determines whether an object has a non-inherited property.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="object">The object that may contain the property.</param>
  /// <param name="key">The key (JavascriptString or JavascriptSymbol) to the property.</param>
  /// <param name="hasOwnProperty">Whether the object has the non-inherited property.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsObjectHasOwnProperty(_object, key: JsValueRef; out hasOwnProperty: bool): JsErrorCode;
      {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Sets whether any action should be taken when a promise is rejected with no reactions
  ///     or a reaction is added to a promise that was rejected before it had reactions.
  ///     By default in either of these cases nothing occurs.
  ///     This function allows you to specify if something should occur and provide a callback
  ///     to implement whatever should occur.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  /// </remarks>
  /// <param name="promiseRejectionTrackerCallback">The callback function being set.</param>
  /// <param name="callbackState">
  ///     User provided state that will be passed back to the callback.
  /// </param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsSetHostPromiseRejectionTracker(
      promiseRejectionTrackerCallback: JsHostPromiseRejectionTrackerCallback;
      callbackState: Pointer): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Retrieve the namespace object for a module.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context and that the module has already been evaluated.
  /// </remarks>
  /// <param name="requestModule">The JsModuleRecord for which the namespace is being requested.</param>
  /// <param name="moduleNamespace">A JsValueRef - the requested namespace object.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetModuleNamespace(
    requestModule: JsModuleRecord;
    out moduleNamespace: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Determines if a provided object is a JavscriptProxy Object and
  ///     provides references to a Proxy's target and handler.
  /// </summary>
  /// <remarks>
  ///     Requires an active script context.
  ///     If object is not a Proxy object the target and handler parameters are not touched.
  ///     If nullptr is supplied for target or handler the function returns after
  ///     setting the isProxy value.
  ///     If the object is a revoked Proxy target and handler are set to JS_INVALID_REFERENCE.
  ///     If it is a Proxy object that has not been revoked target and handler are set to the
  ///     the object's target and handler.
  /// </remarks>
  /// <param name="object">The object that may be a Proxy.</param>
  /// <param name="isProxy">Pointer to a Boolean - is the object a proxy?</param>
  /// <param name="target">Pointer to a JsValueRef - the object's target.</param>
  /// <param name="handler">Pointer to a JsValueRef - the object's handler.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsGetProxyProperties(
      _object: JsValueRef;
      out isProxy: bool;
      out target: JsValueRef;
      out handler: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Parses a script and stores the generated parser state cache into a buffer which can be reused.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///     <c>JsSerializeParserState</c> parses a script and then stores a cache of the parser state
  ///     in a runtime-independent format. The parser state may be deserialized in any runtime along
  ///     with the same script to skip the initial parse phase.
  ///     </para>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Script source can be either JavascriptString or JavascriptExternalArrayBuffer.
  ///         In case it is an ExternalArrayBuffer, and the encoding of the buffer is Utf16,
  ///         JsParseScriptAttributeArrayBufferIsUtf16Encoded is expected on parseAttributes.
  ///     </para>
  ///     <para>
  ///         Use JavascriptExternalArrayBuffer with Utf8/ASCII script source
  ///         for better performance and smaller memory footprint.
  ///     </para>
  /// </remarks>
  /// <param name="scriptVal">The script to parse.</param>
  /// <param name="bufferVal">The buffer to put the serialized parser state cache into.</param>
  /// <param name="parseAttributes">Encoding for the script.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsSerializeParserState(
    scriptVal: JsValueRef;
    out bufferVal: JsValueRef;
    parseAttributes: JsParseScriptAttributes): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Deserializes the cache of initial parser state and (along with the same
  ///     script source) executes the script and returns the result.
  /// </summary>
  /// <remarks>
  ///     <para>
  ///         Requires an active script context.
  ///     </para>
  ///     <para>
  ///         Script source can be either JavascriptString or JavascriptExternalArrayBuffer.
  ///         In case it is an ExternalArrayBuffer, and the encoding of the buffer is Utf16,
  ///         JsParseScriptAttributeArrayBufferIsUtf16Encoded is expected on parseAttributes.
  ///     </para>
  ///     <para>
  ///         Use JavascriptExternalArrayBuffer with Utf8/ASCII script source
  ///         for better performance and smaller memory footprint.
  ///     </para>
  /// </remarks>
  /// <param name="script">The script to run.</param>
  /// <param name="sourceContext">
  ///     A cookie identifying the script that can be used by debuggable script contexts.
  /// </param>
  /// <param name="sourceUrl">The location the script came from</param>
  /// <param name="parseAttributes">Attribute mask for parsing the script</param>
  /// <param name="parserState">
  ///     A buffer containing a cache of the parser state generated by <c>JsSerializeParserState</c>.
  /// </param>
  /// <param name="result">The result of the script, if any. This parameter can be null.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsRunScriptWithParserState(
    script: JsValueRef;
    sourceContext: JsSourceContext;
    sourceUrl: JsValueRef;
    parseAttributes: JsParseScriptAttributes;
    parserState: JsValueRef;
    out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  function JsPointerToString(
    const stringValue: PWideChar;
    stringLength: size_t;
    var value: JsValueRef
  ): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  function JsStringToPointer(
    value: JsValueRef;
    var stringValue: PWideChar;
    var stringLength: size_t
  ): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  {$IFNDEF LINUX}
  // these are omitted on linux.
  function JsRunScript(
          script: PChar;
          sourceContext: JsSourceContext;
          sourceUrl: PChar;
          out result: JsValueRef): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}

  function JsParseScript(
    const script: PChar;
    sourceContext: JsSourceContext;
    const sourceUrl: PChar;
    var result: JsValueRef
  ): JsErrorCode; {$IFDEF MSWINDOWS}stdcall;{$else}cdecl;{$endif}
  {$ENDIF}

implementation

uses
  Math;

{$IFDEF MSWINDOWS}
const
  _chakracore = 'chakracore.dll';
{$endif}

{$ifdef DARWIN}
const
  _chakracore = 'libChakraCore.dylib';
{$endif}

{$ifdef LINUX}
const
  _chakracore = 'libChakraCore.so';
{$endif}

  function JsCreateEnhancedFunction; external _chakracore;
  function JsInitializeModuleRecord; external _chakracore;
  function JsParseModuleSource; external _chakracore;
  function JsModuleEvaluation; external _chakracore;
  function JsSetModuleHostInfo; external _chakracore;
  function JsGetModuleHostInfo; external _chakracore;
  function JsGetAndClearExceptionWithMetadata; external _chakracore;
  function JsCreateString; external _chakracore;
  function JsCreateStringUtf16; external _chakracore;
  function JsCopyString; external _chakracore;
  function JsCopyStringUtf16; external _chakracore;
  function JsParse; external _chakracore;
  function JsRun; external _chakracore;
  function JsCreatePropertyId; external _chakracore;
  function JsCopyPropertyId; external _chakracore;
  function JsSerialize; external _chakracore;
  function JsParseSerialized; external _chakracore;
  function JsRunSerialized; external _chakracore;
  function JsGetPromiseState; external _chakracore;
  function JsGetPromiseResult; external _chakracore;
  function JsCreatePromise; external _chakracore;
  function JsCreateWeakReference; external _chakracore;
  function JsGetWeakReferenceValue; external _chakracore;
  function JsCreateSharedArrayBufferWithSharedContent; external _chakracore;
  function JsGetSharedArrayBufferContent; external _chakracore;
  function JsReleaseSharedArrayBufferContentHandle; external _chakracore;
  function JsHasOwnProperty; external _chakracore;
  function JsCopyStringOneByte; external _chakracore;
  function JsGetDataViewInfo; external _chakracore;
  function JsLessThan; external _chakracore;
  function JsLessThanOrEqual; external _chakracore;
  function JsCreateExternalObjectWithPrototype; external _chakracore;
  function JsObjectGetProperty; external _chakracore;
  function JsObjectSetProperty; external _chakracore;
  function JsObjectHasProperty; external _chakracore;
  function JsObjectDefineProperty; external _chakracore;
  function JsObjectDeleteProperty; external _chakracore;
  function JsObjectGetOwnPropertyDescriptor; external _chakracore;
  function JsObjectHasOwnProperty; external _chakracore;
  function JsSetHostPromiseRejectionTracker; external _chakracore;
  function JsGetModuleNamespace; external _chakracore;
  function JsGetProxyProperties; external _chakracore;
  function JsSerializeParserState; external _chakracore;
  function JsRunScriptWithParserState; external _chakracore;

  function JsStringToPointer; external _chakracore;
  function JsPointerToString; external _chakracore;
  {$IFNDEF LINUX}
  function JsRunScript; external _chakracore;
  function JsParseScript; external _chakracore;
  {$ENDIF}

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

finalization

end.
