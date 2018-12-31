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
/// \mainpage Chakra Hosting Debugging API Reference
///
/// Chakra is Microsoft's JavaScript engine. It is an integral part of Internet Explorer but can
/// also be hosted independently by other applications. This reference describes the APIs available
/// to applications to debug JavaScript.

/// \file
/// \brief The Chakra hosting debugging API.
///
/// This file contains a flat C API layer. This is the API exported by ChakraCore.dll.

unit ChakraDebug;

{$include common.inc}

{$minenumsize 4}

interface

uses
  ChakraCommon;

type
  /// <summary>
  ///     Debug events reported from ChakraCore engine.
  /// </summary>
  JsDiagDebugEvent = (
    /// <summary>
    ///     Indicates a new script being compiled, this includes script, eval, new function.
    /// </summary>
    JsDiagDebugEventSourceCompile = 0,
    /// <summary>
    ///     Indicates compile error for a script.
    /// </summary>
    JsDiagDebugEventCompileError = 1,
    /// <summary>
    ///     Indicates a break due to a breakpoint.
    /// </summary>
    JsDiagDebugEventBreakpoint = 2,
    /// <summary>
    ///     Indicates a break after completion of step action.
    /// </summary>
    JsDiagDebugEventStepComplete = 3,
    /// <summary>
    ///     Indicates a break due to debugger statement.
    /// </summary>
    JsDiagDebugEventDebuggerStatement = 4,
    /// <summary>
    ///     Indicates a break due to async break.
    /// </summary>
    JsDiagDebugEventAsyncBreak = 5,
    /// <summary>
    ///     Indicates a break due to a runtime script exception.
    /// </summary>
    JsDiagDebugEventRuntimeException = 6
  );

  /// <summary>
  ///     Break on Exception attributes.
  /// </summary>
  JsDiagBreakOnExceptionAttributes = (
    /// <summary>
    ///     Don't break on any exception.
    /// </summary>
    JsDiagBreakOnExceptionAttributeNone = $0,
    /// <summary>
    ///     Break on uncaught exception.
    /// </summary>
    JsDiagBreakOnExceptionAttributeUncaught = $1,
    /// <summary>
    ///     Break on first chance exception.
    /// </summary>
    JsDiagBreakOnExceptionAttributeFirstChance = $2
  );

  /// <summary>
  ///     Stepping types.
  /// </summary>
  JsDiagStepType = (
    /// <summary>
    ///     Perform a step operation to next statement.
    /// </summary>
    JsDiagStepTypeStepIn = 0,
    /// <summary>
    ///     Perform a step out from the current function.
    /// </summary>
    JsDiagStepTypeStepOut = 1,
    /// <summary>
    ///     Perform a single step over after a debug break if the next statement is a function call, else behaves as a stepin.
    /// </summary>
    JsDiagStepTypeStepOver = 2,
    /// <summary>
    ///     Perform a single step back to the previous statement (only applicable in TTD mode).
    /// </summary>
    JsDiagStepTypeStepBack = 3,
    /// <summary>
    ///     Perform a reverse continue operation (only applicable in TTD mode).
    /// </summary>
    JsDiagStepTypeReverseContinue = 4,
    /// <summary>
    ///     Perform a forward continue operation. Clears any existing step value.
    /// </summary>
    JsDiagStepTypeContinue = 5
  );

  /// <summary>
  ///     User implemented callback routine for debug events.
  /// </summary>
  /// <remarks>
  ///     Use <c>JsDiagStartDebugging</c> to register the callback.
  /// </remarks>
  /// <param name="debugEvent">The type of JsDiagDebugEvent event.</param>
  /// <param name="eventData">Additional data related to the debug event.</param>
  /// <param name="callbackState">The state passed to <c>JsDiagStartDebugging</c>.</param>
  JsDiagDebugEventCallback = function(debugEvent: JsDiagDebugEvent; eventData: JsValueRef; callbackState: Pointer): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Starts debugging in the given runtime.
  /// </summary>
  /// <param name="runtimeHandle">Runtime to put into debug mode.</param>
  /// <param name="debugEventCallback">Registers a callback to be called on every JsDiagDebugEvent.</param>
  /// <param name="callbackState">User provided state that will be passed back to the callback.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The runtime should be active on the current thread and should not be in debug state.
  /// </remarks>
  function JsDiagStartDebugging(runtimeHandle: JsRuntimeHandle; debugEventCallback: JsDiagDebugEventCallback;
    callbackState: Pointer): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Stops debugging in the given runtime.
  /// </summary>
  /// <param name="runtimeHandle">Runtime to stop debugging.</param>
  /// <param name="callbackState">User provided state that was passed in JsDiagStartDebugging.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The runtime should be active on the current thread and in debug state.
  /// </remarks>
  function JsDiagStopDebugging(runtimeHandle: JsRuntimeHandle; callbackState: PPointer): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Request the runtime to break on next JavaScript statement.
  /// </summary>
  /// <param name="runtimeHandle">Runtime to request break.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The runtime should be in debug state. This API can be called from another runtime.
  /// </remarks>
  function JsDiagRequestAsyncBreak(runtimeHandle: JsRuntimeHandle): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     List all breakpoints in the current runtime.
  /// </summary>
  /// <param name="breakpoints">Array of breakpoints.</param>
  /// <remarks>
  ///     <para>
  ///     [{
  ///         "breakpointId" : 1,
  ///         "scriptId" : 1,
  ///         "line" : 0,
  ///         "column" : 62
  ///     }]
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagGetBreakpoints(out breakpoints: JsValueRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Sets breakpoint in the specified script at give location.
  /// </summary>
  /// <param name="scriptId">Id of script from JsDiagGetScripts or JsDiagGetSource to put breakpoint.</param>
  /// <param name="lineNumber">0 based line number to put breakpoint.</param>
  /// <param name="columnNumber">0 based column number to put breakpoint.</param>
  /// <param name="breakpoint">Breakpoint object with id, line and column if success.</param>
  /// <remarks>
  ///     <para>
  ///     {
  ///         "breakpointId" : 1,
  ///         "line" : 2,
  ///         "column" : 4
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagSetBreakpoint(scriptId, lineNumber, columnNumber: Cardinal; out breakpoint: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Remove a breakpoint.
  /// </summary>
  /// <param name="breakpointId">Breakpoint id returned from JsDiagSetBreakpoint.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagRemoveBreakpoint(breakpointId: Cardinal): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Sets break on exception handling.
  /// </summary>
  /// <param name="runtimeHandle">Runtime to set break on exception attributes.</param>
  /// <param name="exceptionAttributes">Mask of JsDiagBreakOnExceptionAttributes to set.</param>
  /// <remarks>
  ///     <para>
  ///         If this API is not called the default value is set to JsDiagBreakOnExceptionAttributeUncaught in the runtime.
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The runtime should be in debug state. This API can be called from another runtime.
  /// </remarks>
  function JsDiagSetBreakOnException(runtimeHandle: JsRuntimeHandle;
    exceptionAttributes: JsDiagBreakOnExceptionAttributes): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets break on exception setting.
  /// </summary>
  /// <param name="runtimeHandle">Runtime from which to get break on exception attributes, should be in debug mode.</param>
  /// <param name="exceptionAttributes">Mask of JsDiagBreakOnExceptionAttributes.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The runtime should be in debug state. This API can be called from another runtime.
  /// </remarks>
  function JsDiagGetBreakOnException(runtimeHandle: JsRuntimeHandle;
    out exceptionAttributes: JsDiagBreakOnExceptionAttributes): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Sets the step type in the runtime after a debug break.
  /// </summary>
  /// <remarks>
  ///     Requires to be at a debug break.
  /// </remarks>
  /// <param name="resumeType">Type of JsDiagStepType.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagSetStepType(stepType: JsDiagStepType): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets list of scripts.
  /// </summary>
  /// <param name="scriptsArray">Array of script objects.</param>
  /// <remarks>
  ///     <para>
  ///     [{
  ///         "scriptId" : 2,
  ///         "fileName" : "c:\\Test\\Test.js",
  ///         "lineCount" : 4,
  ///         "sourceLength" : 111
  ///       }, {
  ///         "scriptId" : 3,
  ///         "parentScriptId" : 2,
  ///         "scriptType" : "eval code",
  ///         "lineCount" : 1,
  ///         "sourceLength" : 12
  ///     }]
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagGetScripts(out scriptsArray: JsValueRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets source for a specific script identified by scriptId from JsDiagGetScripts.
  /// </summary>
  /// <param name="scriptId">Id of the script.</param>
  /// <param name="source">Source object.</param>
  /// <remarks>
  ///     <para>
  ///     {
  ///         "scriptId" : 1,
  ///         "fileName" : "c:\\Test\\Test.js",
  ///         "lineCount" : 12,
  ///         "sourceLength" : 15154,
  ///         "source" : "var x = 1;"
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagGetSource(scriptId: Cardinal; out source: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the source information for a function object.
  /// </summary>
  /// <param name="function">JavaScript function.</param>
  /// <param name="functionPosition">Function position - scriptId, start line, start column, line number of first statement, column number of first statement.</param>
  /// <remarks>
  ///     <para>
  ///     {
  ///         "scriptId" : 1,
  ///         "fileName" : "c:\\Test\\Test.js",
  ///         "line" : 1,
  ///         "column" : 2,
  ///         "firstStatementLine" : 6,
  ///         "firstStatementColumn" : 0
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     This API can be called when runtime is at a break or running.
  /// </remarks>
  function JsDiagGetFunctionPosition(_function: JsValueRef; out functionPosition: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the stack trace information.
  /// </summary>
  /// <param name="stackTrace">Stack trace information.</param>
  /// <remarks>
  ///     <para>
  ///     [{
  ///         "index" : 0,
  ///         "scriptId" : 2,
  ///         "line" : 3,
  ///         "column" : 0,
  ///         "sourceLength" : 9,
  ///         "sourceText" : "var x = 1",
  ///         "functionHandle" : 1
  ///     }]
  ///    </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagGetStackTrace(out stackTrace: JsValueRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the list of properties corresponding to the frame.
  /// </summary>
  /// <param name="stackFrameIndex">Index of stack frame from JsDiagGetStackTrace.</param>
  /// <param name="properties">Object of properties array (properties, scopes and globals).</param>
  /// <remarks>
  ///     <para>
  ///     propertyAttributes is a bit mask of
  ///         NONE = 0x1,
  ///         HAVE_CHILDRENS = 0x2,
  ///         READ_ONLY_VALUE = 0x4,
  ///         IN_TDZ = 0x8,
  ///     </para>
  ///     <para>
  ///     {
  ///         "thisObject": {
  ///             "name": "this",
  ///             "type" : "object",
  ///             "className" : "Object",
  ///             "display" : "{...}",
  ///             "propertyAttributes" : 1,
  ///             "handle" : 306
  ///         },
  ///         "exception" : {
  ///             "name" : "{exception}",
  ///             "type" : "object",
  ///             "display" : "'a' is undefined",
  ///             "className" : "Error",
  ///             "propertyAttributes" : 1,
  ///             "handle" : 307
  ///         }
  ///         "arguments" : {
  ///             "name" : "arguments",
  ///             "type" : "object",
  ///             "display" : "{...}",
  ///             "className" : "Object",
  ///             "propertyAttributes" : 1,
  ///             "handle" : 190
  ///         },
  ///         "returnValue" : {
  ///             "name" : "[Return value]",
  ///             "type" : "undefined",
  ///             "propertyAttributes" : 0,
  ///             "handle" : 192
  ///         },
  ///         "functionCallsReturn" : [{
  ///                 "name" : "[foo1 returned]",
  ///                 "type" : "number",
  ///                 "value" : 1,
  ///                 "propertyAttributes" : 2,
  ///                 "handle" : 191
  ///             }
  ///         ],
  ///         "locals" : [],
  ///         "scopes" : [{
  ///                 "index" : 0,
  ///                 "handle" : 193
  ///             }
  ///         ],
  ///         "globals" : {
  ///             "handle" : 194
  ///         }
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagGetStackProperties(stackFrameIndex: Cardinal; out properties: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the list of children of a handle.
  /// </summary>
  /// <param name="objectHandle">Handle of object.</param>
  /// <param name="fromCount">0-based from count of properties, usually 0.</param>
  /// <param name="totalCount">Number of properties to return.</param>
  /// <param name="propertiesObject">Array of properties.</param>
  /// <remarks>Handle should be from objects returned from call to JsDiagGetStackProperties.</remarks>
  /// <remarks>For scenarios where object have large number of properties totalCount can be used to control how many properties are given.</remarks>
  /// <remarks>
  ///     <para>
  ///     {
  ///         "totalPropertiesOfObject": 10,
  ///         "properties" : [{
  ///                 "name" : "__proto__",
  ///                 "type" : "object",
  ///                 "display" : "{...}",
  ///                 "className" : "Object",
  ///                 "propertyAttributes" : 1,
  ///                 "handle" : 156
  ///             }
  ///         ],
  ///         "debuggerOnlyProperties" : [{
  ///                 "name" : "[Map]",
  ///                 "type" : "string",
  ///                 "value" : "size = 0",
  ///                 "propertyAttributes" : 2,
  ///                 "handle" : 157
  ///             }
  ///         ]
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagGetProperties(objectHandle, fromCount, totalCount: Cardinal;
    out propertiesObject: JsValueRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Gets the object corresponding to handle.
  /// </summary>
  /// <param name="objectHandle">Handle of object.</param>
  /// <param name="handleObject">Object corresponding to the handle.</param>
  /// <remarks>
  ///     <para>
  ///     {
  ///         "scriptId" : 24,
  ///          "line" : 1,
  ///          "column" : 63,
  ///          "name" : "foo",
  ///          "type" : "function",
  ///          "handle" : 2
  ///     }
  ///    </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagGetObjectFromHandle(objectHandle: Cardinal; out handleObject: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     Evaluates an expression on given frame.
  /// </summary>
  /// <param name="expression">
  ///     Javascript String or ArrayBuffer (incl. ExternalArrayBuffer).
  /// </param>
  /// <param name="stackFrameIndex">Index of stack frame on which to evaluate the expression.</param>
  /// <param name="parseAttributes">
  ///     Defines how `expression` (JsValueRef) should be parsed.
  ///     - `JsParseScriptAttributeNone` when `expression` is a Utf8 encoded ArrayBuffer and/or a Javascript String (encoding independent)
  ///     - `JsParseScriptAttributeArrayBufferIsUtf16Encoded` when `expression` is Utf16 Encoded ArrayBuffer
  ///     - `JsParseScriptAttributeLibraryCode` has no use for this function and has similar effect with `JsParseScriptAttributeNone`
  /// </param>
  /// <param name="forceSetValueProp">Forces the result to contain the raw value of the expression result.</param>
  /// <param name="evalResult">Result of evaluation.</param>
  /// <remarks>
  ///     <para>
  ///     evalResult when evaluating 'this' and return is JsNoError
  ///     {
  ///         "name" : "this",
  ///         "type" : "object",
  ///         "className" : "Object",
  ///         "display" : "{...}",
  ///         "propertyAttributes" : 1,
  ///         "handle" : 18
  ///     }
  ///
  ///     evalResult when evaluating a script which throws JavaScript error and return is JsErrorScriptException
  ///     {
  ///         "name" : "a.b.c",
  ///         "type" : "object",
  ///         "className" : "Error",
  ///         "display" : "'a' is undefined",
  ///         "propertyAttributes" : 1,
  ///         "handle" : 18
  ///     }
  ///     </para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, evalResult will contain the result
  ///     The code <c>JsErrorScriptException</c> if evaluate generated a JavaScript exception, evalResult will contain the error details
  ///     Other error code for invalid parameters or API was not called at break
  /// </returns>
  /// <remarks>
  ///     The current runtime should be in debug state. This API can only be called when runtime is at a break.
  /// </remarks>
  function JsDiagEvaluate(expression: JsValueRef; stackFrameIndex: Cardinal; parseAttributes: JsParseScriptAttributes;
    forceSetValueProp: bool; out evalResult: JsValueRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

type
  /////////////////////
  /// <summary>
  ///     TimeTravel move options as bit flag enum.
  /// </summary>
  JsTTDMoveModes = (
      /// <summary>
      ///     Indicates no special actions needed for move.
      /// </summary>
      JsTTDMoveNone = $0,

      /// <summary>
      ///     Indicates that we want to move to the first event.
      /// </summary>
      JsTTDMoveFirstEvent = $1,

      /// <summary>
      ///     Indicates that we want to move to the last event.
      /// </summary>
      JsTTDMoveLastEvent = $2,

      /// <summary>
      ///     Indicates that we want to move to the kth event -- top 32 bits are event count.
      /// </summary>
      JsTTDMoveKthEvent = $4,

      /// <summary>
      ///     Indicates if we are doing the scan for a continue operation
      /// </summary>
      JsTTDMoveScanIntervalForContinue = $10,

      /// <summary>
      ///     Indicates if we are doing the scan for a continue operation and are in the time-segment where the active breakpoint was
      /// </summary>
      JsTTDMoveScanIntervalForContinueInActiveBreakpointSegment = $20,

      /// <summary>
      ///     Indicates if we want to set break on entry or just run and let something else trigger breakpoints.
      /// </summary>
      JsTTDMoveBreakOnEntry = $100
  );
  JsTTDMoveMode = Cardinal;

  /// <summary>
  ///     A handle for URI's that TTD information is written to/read from.
  /// </summary>
  JsTTDStreamHandle = Pointer;

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Construct a JsTTDStreamHandle that will be used to read/write the event log portion of the TTD data based on the uri
  ///     provided by JsTTDInitializeUriCallback.
  /// </summary>
  /// <remarks>
  ///     <para>Exactly one of read or write will be set to true.</para>
  /// </remarks>
  /// <param name="uriLength">The length of the uri array that the host passed in for storing log info.</param>
  /// <param name="uri">The URI that the host passed in for storing log info.</param>
  /// <param name="asciiNameLength">The length of the ascii name array that the host passed in for storing log info.</param>
  /// <param name="asciiResourceName">An optional ascii string giving a unique name to the resource that the JsTTDStreamHandle will be created for.</param>
  /// <param name="read">If the handle should be opened for reading.</param>
  /// <param name="write">If the handle should be opened for writing.</param>
  /// <returns>A JsTTDStreamHandle opened in read/write mode as specified.</returns>
  TTDOpenResourceStreamCallback = function(uriLength: size_t; uri: PAnsiChar; asciiNameLength: size_t;
    asciiResourceName: PAnsiChar; read, write: bool): JsTTDStreamHandle; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     A callback for reading data from a handle.
  /// </summary>
  /// <param name="handle">The JsTTDStreamHandle to read the data from.</param>
  /// <param name="buff">The buffer to place the data into.</param>
  /// <param name="size">The max number of bytes that should be read.</param>
  /// <param name="readCount">The actual number of bytes read and placed in the buffer.</param>
  /// <returns>true if the read was successful false otherwise.</returns>
  JsTTDReadBytesFromStreamCallback = function(handle: JsTTDStreamHandle; buff: PByte; size: size_t;
    out readCount: size_t): bool; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     A callback for writing data to a handle.
  /// </summary>
  /// <param name="handle">The JsTTDStreamHandle to write the data to.</param>
  /// <param name="buff">The buffer to copy the data from.</param>
  /// <param name="size">The max number of bytes that should be written.</param>
  /// <param name="readCount">The actual number of bytes written to the HANDLE.</param>
  /// <returns>true if the write was successful false otherwise.</returns>
  JsTTDWriteBytesToStreamCallback = function (handle: JsTTDStreamHandle; buff: PByte; size: size_t;
    out writtenCount: size_t): bool; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Flush and close the stream represented by the HANDLE as needed.
  /// </summary>
  /// <remarks>
  ///     <para>Exactly one of read or write will be set to true.</para>
  /// </remarks>
  /// <param name="handle">The JsTTDStreamHandle to close.</param>
  /// <param name="read">If the handle was opened for reading.</param>
  /// <param name="write">If the handle was opened for writing.</param>
  JsTTDFlushAndCloseStreamCallback = procedure(handle: JsTTDStreamHandle; read, write: bool);
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Creates a new runtime in Record Mode.
  /// </summary>
  /// <param name="attributes">The attributes of the runtime to be created.</param>
  /// <param name="enableDebugging">A flag to enable debugging during record.</param>
  /// <param name="snapInterval">The interval to wait between snapshots (measured in millis).</param>
  /// <param name="snapHistoryLength">The amount of history to maintain before discarding -- measured in number of snapshots and controls how far back in time a trace can be reversed.</param>
  /// <param name="openResourceStream">The <c>TTDOpenResourceStreamCallback</c> function for generating a JsTTDStreamHandle to read/write serialized data.</param>
  /// <param name="writeBytesToStream">The <c>JsTTDWriteBytesToStreamCallback</c> function for writing bytes to a JsTTDStreamHandle.</param>
  /// <param name="flushAndCloseStream">The <c>JsTTDFlushAndCloseStreamCallback</c> function for flushing and closing a JsTTDStreamHandle as needed.</param>
  /// <param name="threadService">The thread service for the runtime. Can be null.</param>
  /// <param name="runtime">The runtime created.</param>
  /// <remarks>
  ///     <para>See <c>JsCreateRuntime</c> for additional information.</para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsTTDCreateRecordRuntime(attributes: JsRuntimeAttributes; enableDebugging: bool; snapInterval, snapHistoryLength: size_t;
    openResourceStream: TTDOpenResourceStreamCallback; writeBytesToStream: JsTTDWriteBytesToStreamCallback;
    flushAndCloseStream: JsTTDFlushAndCloseStreamCallback; threadService: JsThreadServiceCallback;
    out runtime: JsRuntimeHandle): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Creates a new runtime in Debug Mode.
  /// </summary>
  /// <param name="attributes">The attributes of the runtime to be created.</param>
  /// <param name="infoUri">The uri where the recorded Time-Travel data should be loaded from.</param>
  /// <param name="enableDebugging">A flag to enable additional debugging operation support during replay.</param>
  /// <param name="openResourceStream">The <c>TTDOpenResourceStreamCallback</c> function for generating a JsTTDStreamHandle to read/write serialized data.</param>
  /// <param name="readBytesFromStream">The <c>JsTTDReadBytesFromStreamCallback</c> function for reading bytes from a JsTTDStreamHandle.</param>
  /// <param name="flushAndCloseStream">The <c>JsTTDFlushAndCloseStreamCallback</c> function for flushing and closing a JsTTDStreamHandle as needed.</param>
  /// <param name="threadService">The thread service for the runtime. Can be null.</param>
  /// <param name="runtime">The runtime created.</param>
  /// <remarks>
  ///     <para>See <c>JsCreateRuntime</c> for additional information.</para>
  /// </remarks>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsTTDCreateReplayRuntime(attributes: JsRuntimeAttributes; infoUri: PAnsiChar; infoUriCount: size_t;
    enableDebugging: bool; openResourceStream: TTDOpenResourceStreamCallback;
    readBytesFromStream: JsTTDReadBytesFromStreamCallback; flushAndCloseStream: JsTTDFlushAndCloseStreamCallback;
    threadService: JsThreadServiceCallback; out runtime: JsRuntimeHandle): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Creates a script context that takes the TTD mode from the log or explicitly is not in TTD mode (regular takes mode from currently active script).
  /// </summary>
  /// <param name="runtime">The runtime the script context is being created in.</param>
  /// <param name="useRuntimeTTDMode">Set to true to use runtime TTD mode false to explicitly be non-TTD context.</param>
  /// <param name="newContext">The created script context.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsTTDCreateContext(runtimeHandle: JsRuntimeHandle; useRuntimeTTDMode: bool;
    out newContext: JsContextRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the time-travel system that a context has been identified as dead by the gc (and is being de-allocated).
  /// </summary>
  /// <param name="context">The script context that is now dead.</param>
  /// <returns>
  ///     The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.
  /// </returns>
  function JsTTDNotifyContextDestroy(context: JsContextRef): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Start Time-Travel record or replay at next turn of event loop.
  /// </summary>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDStart: JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Stop Time-Travel record or replay.
  /// </summary>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDStop: JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Pause Time-Travel recording before executing code on behalf of debugger or other diagnostic/telemetry.
  /// </summary>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDPauseTimeTravelBeforeRuntimeOperation: JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     ReStart Time-Travel recording after executing code on behalf of debugger or other diagnostic/telemetry.
  /// </summary>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDReStartTimeTravelAfterRuntimeOperation: JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the Js runtime we are at a safe yield point in the event loop (i.e. no locals on the stack and we can process as desired).
  /// </summary>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDNotifyYield: JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the TTD runtime that we are doing a weak add on a reference (we may use this in external API calls and the release will happen in a GC callback).
  /// </summary>
  /// <param name="value">The value we are adding the ref to.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDNotifyLongLivedReferenceAdd(value: JsValueRef): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the Js runtime the host is aborting the process and what the status code is.
  /// </summary>
  /// <param name="statusCode">The exit status code.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDHostExit(statusCode: Integer): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the event log that the contents of one buffer have been copied to a second buffer.
  /// </summary>
  /// <param name="dst">The buffer that was written into.</param>
  /// <param name="dstIndex">The first index modified.</param>
  /// <param name="src">The buffer that was copied from.</param>
  /// <param name="srcIndex">The first index copied.</param>
  /// <param name="count">The number of bytes copied.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDRawBufferCopySyncIndirect(dst: JsValueRef; dstIndex: size_t; src: JsValueRef;
    srcIndex, count: size_t): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the event log that the contents of a naked byte* buffer passed to the host have been modified synchronously.
  /// </summary>
  /// <param name="buffer">The buffer that was modified.</param>
  /// <param name="index">The first index modified.</param>
  /// <param name="count">The number of bytes written.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDRawBufferModifySyncIndirect(buffer: JsValueRef; index, count: size_t): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Get info for notifying the TTD system that a raw buffer it shares with the host has been modified.
  /// </summary>
  /// <param name="instance">The array buffer we want to monitor for contents modification.</param>
  /// <param name="initialModPos">The first position in the buffer that may be modified.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDRawBufferAsyncModificationRegister(instance: JsValueRef; initialModPos: PByte): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Notify the event log that the contents of a naked byte* buffer passed to the host have been modified asynchronously.
  /// </summary>
  /// <param name="finalModPos">One past the last modified position in the buffer.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDRawBufferAsyncModifyComplete(finalModPos: PByte): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     A check for unimplemented TTD actions in the host.
  ///     This API is a TEMPORARY API while we complete the implementation of TTD support in the Node host and will be deleted once that is complete.
  /// </summary>
  /// <param name="msg">The message to print if we should be catching this as a TTD operation.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDCheckAndAssertIfTTDRunning(msg: PAnsiChar): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Before calling JsTTDMoveToTopLevelEvent (which inflates a snapshot and replays) check to see if we want to reset the script context.
  ///     We reset the script context if the move will require inflating from a different snapshot that the last one.
  /// </summary>
  /// <param name="runtimeHandle">The runtime handle that the script is executing in.</param>
  /// <param name="moveMode">Flags controlling the way the move it performed and how other parameters are interpreted.</param>
  /// <param name="kthEvent">When <c>moveMode == JsTTDMoveKthEvent</c> indicates which event, otherwise this parameter is ignored.</param>
  /// <param name="targetEventTime">The event time we want to move to or -1 if not relevant.</param>
  /// <param name="targetStartSnapTime">Out parameter with the event time of the snapshot that we should inflate from.</param>
  /// <param name="targetEndSnapTime">Optional Out parameter with the snapshot time following the event.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDGetSnapTimeTopLevelEventMove(runtimeHandle: JsRuntimeHandle; moveMode: JsTTDMoveMode; kthEvent: Cardinal;
    targetEventTime: Int64; out targetStartSnapTime: Int64; targetEndSnapTime: PInt64): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Get the snapshot interval that bounds the target event time.
  /// </summary>
  /// <param name="runtimeHandle">The runtime handle that the script is executing in.</param>
  /// <param name="targetEventTime">The event time we want to get the interval for.</param>
  /// <param name="startSnapTime">The snapshot time that comes before the desired event.</param>
  /// <param name="endSnapTime">The snapshot time that comes after the desired event (-1 if the leg ends before a snapshot appears).</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDGetSnapShotBoundInterval(runtimeHandle: JsRuntimeHandle; targetEventTime: Int64;
    out startSnapTime, endSnapTime: Int64): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Get the snapshot interval that precedes the one given by currentSnapStartTime (or -1 if there is no such interval).
  /// </summary>
  /// <param name="runtimeHandle">The runtime handle that the script is executing in.</param>
  /// <param name="currentSnapStartTime">The current snapshot interval start time.</param>
  /// <param name="previousSnapTime">The resulting previous snapshot interval start time or -1 if no such time.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDGetPreviousSnapshotInterval(runtimeHandle: JsRuntimeHandle; currentSnapStartTime: Int64;
    out previousSnapTime: Int64): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     During debug operations some additional information is populated during replay. This runs the code between the given
  ///     snapshots to populate this information which may be needed by the debugger to determine time-travel jump targets.
  /// </summary>
  /// <param name="runtimeHandle">The runtime handle that the script is executing in.</param>
  ///<param name = "startSnapTime">The snapshot time that we will start executing from.< / param>
  ///<param name = "endSnapTime">The snapshot time that we will stop at (or -1 if we want to run to the end).< / param>
  /// <param name="moveMode">Additional flags for controling how the move is done.</param>
  /// <param name="newTargetEventTime">The updated target event time set according to the moveMode (-1 if not found).</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDPreExecuteSnapShotInterval(runtimeHandle: JsRuntimeHandle; startSnapTime, endSnapTime: Int64;
    moveMode: JsTTDMoveMode; out newTargetEventTime: Int64): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Move to the given top-level call event time (assuming JsTTDPrepContextsForTopLevelEventMove) was called previously to reset any script contexts.
  ///     This also computes the ready-to-run snapshot if needed.
  /// </summary>
  /// <param name="runtimeHandle">The runtime handle that the script is executing in.</param>
  /// <param name="moveMode">Additional flags for controling how the move is done.</param>
  /// <param name="snapshotTime">The event time that we will start executing from to move to the given target time.</param>
  /// <param name="eventTime">The event that we want to move to.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDMoveToTopLevelEvent(runtimeHandle: JsRuntimeHandle; moveMode: JsTTDMoveMode;
    snapshotTime, eventTime: Int64): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Execute from the current point in the log to the end returning the error code.
  /// </summary>
  /// <param name="moveMode">Additional flags for controling how the move is done.</param>
  /// <param name="rootEventTime">The event time that we should move to next or notification (-1) that replay has ended.</param>
  /// <returns>
  ///     If the debugger requested an abort the code is JsNoError -- rootEventTime is the target event time we need to move to and re - execute from.
  ///     If we aborted at the end of the replay log the code is JsNoError -- rootEventTime is -1.
  ///     If there was an unhandled script exception the code is JsErrorCategoryScript.
  /// </returns>
  function JsTTDReplayExecution(moveMode: JsTTDMoveMode; out rootEventTime: Int64): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     Enable or disable autotrace ability from JsRT.
  /// </summary>
  /// <param name="status">True to enable autotracing false to disable it.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDDiagSetAutoTraceStatus(status: bool): JsErrorCode; {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

  /// <summary>
  ///     TTD API -- may change in future versions:
  ///     A way for the debugger to programatically write a trace when it is at a breakpoint.
  /// </summary>
  /// <param name="uri">The URI that the log should be written into.</param>
  /// <param name="uriLength">The length of the uri array that the host passed in for storing log info.</param>
  /// <returns>The code <c>JsNoError</c> if the operation succeeded, a failure code otherwise.</returns>
  function JsTTDDiagWriteLog(uri: PAnsiChar; uriLength: size_t): JsErrorCode;
    {$ifdef WINDOWS}stdcall;{$else}cdecl;{$endif}

implementation

{$ifdef MSWINDOWS}
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

  function JsDiagStartDebugging; external _chakracore;
  function JsDiagStopDebugging; external _chakracore;
  function JsDiagRequestAsyncBreak; external _chakracore;
  function JsDiagGetBreakpoints; external _chakracore;
  function JsDiagSetBreakpoint; external _chakracore;
  function JsDiagRemoveBreakpoint; external _chakracore;
  function JsDiagSetBreakOnException; external _chakracore;
  function JsDiagGetBreakOnException; external _chakracore;
  function JsDiagSetStepType; external _chakracore;
  function JsDiagGetScripts; external _chakracore;
  function JsDiagGetSource; external _chakracore;
  function JsDiagGetFunctionPosition; external _chakracore;
  function JsDiagGetStackTrace; external _chakracore;
  function JsDiagGetStackProperties; external _chakracore;
  function JsDiagGetProperties; external _chakracore;
  function JsDiagGetObjectFromHandle; external _chakracore;
  function JsDiagEvaluate; external _chakracore;
  function JsTTDCreateRecordRuntime; external _chakracore;
  function JsTTDCreateReplayRuntime; external _chakracore;
  function JsTTDCreateContext; external _chakracore;
  function JsTTDNotifyContextDestroy; external _chakracore;
  function JsTTDStart; external _chakracore;
  function JsTTDStop; external _chakracore;
  function JsTTDPauseTimeTravelBeforeRuntimeOperation; external _chakracore;
  function JsTTDReStartTimeTravelAfterRuntimeOperation; external _chakracore;
  function JsTTDNotifyYield; external _chakracore;
  function JsTTDNotifyLongLivedReferenceAdd; external _chakracore;
  function JsTTDHostExit; external _chakracore;
  function JsTTDRawBufferCopySyncIndirect; external _chakracore;
  function JsTTDRawBufferModifySyncIndirect; external _chakracore;
  function JsTTDRawBufferAsyncModificationRegister; external _chakracore;
  function JsTTDRawBufferAsyncModifyComplete; external _chakracore;
  function JsTTDCheckAndAssertIfTTDRunning; external _chakracore;
  function JsTTDGetSnapTimeTopLevelEventMove; external _chakracore;
  function JsTTDGetSnapShotBoundInterval; external _chakracore;
  function JsTTDGetPreviousSnapshotInterval; external _chakracore;
  function JsTTDPreExecuteSnapShotInterval; external _chakracore;
  function JsTTDMoveToTopLevelEvent; external _chakracore;
  function JsTTDReplayExecution; external _chakracore;
  function JsTTDDiagSetAutoTraceStatus; external _chakracore;
  function JsTTDDiagWriteLog; external _chakracore;

end.
