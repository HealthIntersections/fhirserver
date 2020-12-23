unit fsl_java_wrapper;
{
  Copyright (c) 1998+ Jonathan Revusky
  All rights reserved.

  This software was enhanced and ported to 32 bit and 64 bit and
  to all the Delphi XE versions and to FreePascal by
  Amine Moulay Ramdane.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
  3. All advertising materials mentioning features or use of this software
  must display the following acknowledgement:
  This product includes software developed by Jonathan Revusky
  4. The name of the author may not be used to endorse or promote products
  derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}

// an object-oriented wrapper around the JNI.
// The code here (by contrast with JavaRuntime.pas) should be
// cross-platform.

interface


uses 
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  Classes, SysUtils,
  fsl_java_strings, fsl_java_jni, fsl_java_utilities;

type

  // encapsulates a JVM instance,
  // wraps around the PJavaVM handle
  // and provides some static methods
  TJavaVM = class
  private
    pvm: PJavaVM;
  public
    constructor Create(p: PJavaVM);
    destructor Destroy; override;
    // convenience method to call a method's static main
    // uses delphi's native TStrings to pass the
    // array of string args
    class procedure CallMain(const classname: String; strings: TStrings);

    // waits until all the threads have completed.
    procedure Wait;

    // check if an exception was thrown by the last call, and if it was, raise it
    function checkException: jthrowable;
    procedure clearException;

    // Convenience method. Calls Exit procedure
    class procedure CallExit(exitCode: Integer);

    // procedure to explicitly detach a local reference.
    class procedure freeRef(jobj: JObject; isGlobal: Boolean);

    // returns the current JNI environment pointer.
    class function getPenv: PJNIEnv;

    // IMPORTANT: The following method must be called by native methods
    // that receive a penv argument if they intend to use this unit.
    // private...
    class procedure setThreadPenv(p: PJNIEnv);

    function threadAttached : boolean;
    procedure attachThread;
    procedure detachThread;
  end; // class TJavaVM

  TJavaClass = class;
  TJavaClassArray = array of TJavaClass;
  TJavaObject = class;
  TJavaType = (Void, Aobject, Aboolean, Abyte, Achar, Ashort, Aint, Along, Afloat, Adouble, AString, ABooleanArray, AByteArray, ACharArray, AShortArray,
    AIntArray, ALongArray, AFloatArray, ADoubleArray, AStringArray);
  TJavaTypeArray = Array of TJavaType;
  TMethodAttribute = (static, nonstatic, nonvirtual);

  { Delphi class to encapsulate list of params to Java method. }

  TJavaParams = class
  private
    RefList: TList; // a list of references to be freed by the destructor.
    Fsig: String;
    FArgPointer: Pointer;
    bufLength: Integer;
    procedure addToArgBuffer(p: Pointer; NumBytes: Integer); // add an element to buffer.
  public
    constructor Create;
    destructor Destroy; override;

    procedure clear;

    // The following methods add the various types to the parameter list,
    // updating the signature as well.
    procedure addBoolean(val: Boolean);
    procedure addByte(val: JByte);
    procedure addChar(val: JChar);
    procedure addShort(val: JShort);
    procedure addInt(val: JInt);
    procedure addLong(val: Jlong);
    procedure addFloat(val: JFloat);
    procedure addDouble(val: JDouble);
    procedure addString(val: UTF8String);
    procedure addBooleanArray(var arr: array of JBoolean);
    procedure addByteArray(var arr: array of JByte);
    procedure addCharArray(var arr: array of JChar);
    procedure addShortArray(var arr: array of JShort);
    procedure addIntArray(var arr: array of JInt);
    procedure addLongArray(var arr: array of Jlong);
    procedure addFloatArray(var arr: array of JFloat);
    procedure addDoubleArray(var arr: array of JDouble);
    procedure addStringArray(var strings: TStrings);

    // In the following two methods, the second parameter
    // indicates the TJavaClass of which the object is an instance
    procedure addObject(val: TJavaObject; jcl: TJavaClass);
    procedure addObjectArray(arr: array of TJavaObject; jcl: TJavaClass);

    // the java signature of this parameter list.
    property Signature: String read Fsig;
    // a pointer to the buffer that contains the Parameters to be passed.
    property argPointer: Pointer read FArgPointer;
  end;

  { Delphi class to encapsulate a Java method; }

  TJavaMethod = class
  private
    Fclass: TJavaClass;
    Fsig: String;
    FmethodType: TMethodAttribute;
    FmethodID: JMethodID;
    FRetval: TJavaType;
  public
    // the constructor. The retclass is Nil unless returntype is an object.
    // raises a EJavaMethodNotFound exception if method is not found.
    constructor Create(cls: TJavaClass; name: String; methodType: TMethodAttribute; returntype: TJavaType; retclass: TJavaClass; params: TJavaTypeArray;
      paramClasses: TJavaClassArray); overload;
    constructor Create(cls: TJavaClass; name: String; methodType: TMethodAttribute; returntype: TJavaType; params: TJavaTypeArray); overload;
    // a minimal constructor for virtual methods that
    // take no arguments and return nothing.
    constructor CreateVoid(cls: TJavaClass; name: String);
    function Call(params: TJavaParams; jobj: TJavaObject): jvalue;
  end;

  { Delphi class to encapsulate a Java object reference. }

  TJavaObject = class
  private
    FLocalHandle: JObject;
    FGlobalHandle: JObject;
    Fclass: TJavaClass;
    FPenv: PJNIEnv;
    function getPenv: PJNIEnv;
    procedure setGlobal(B: Boolean);
    function isGlobal: Boolean;
    function isValid: Boolean;
    function getHandle: JObject;
  public
    // instantiates a new object of the type passed as the first param,
    // using the constructor with parameters as encapsulated by the params argument.
    constructor Create(jcl: TJavaClass; params: TJavaParams);
    // creates a wrapper object around the low-level JNI handle passed as an argument.
    // to be used when you already have a JNI local object reference but want a delphi wrapper.
    constructor CreateWithHandle(jcl: TJavaClass; jobj: JObject);
    destructor Destroy; override;

    // returns a native delphi string by calling the object's toString()
    // if the object itself is a String, it simply copies it to a Delphi string.
    function ToString: String; override;
    // returns true if the argument represents the same java object.
    function Equals(JavaObject: TObject): Boolean; override;
    // returns true if this object is an instance of the java class.
    function isInstanceOf(JavaClass: TJavaClass): Boolean;
    property Handle: JObject read getHandle;
    property ClassRef: TJavaClass read Fclass;
    property Global: Boolean read isGlobal write setGlobal;
    property Valid: Boolean read isValid;
  end;

  { Delphi class to encapsulate a Java class reference. }

  TJavaClass = class(TJavaObject)
  private
    Fsig: String;
  public
    // the constructor raises a EJavaClassNotFound exception if class is not found.
    constructor Create(name: String);
    // a constructor that creates a TJavaClass wrapper object when it already has
    // a local object ref to the class's JNI handle.
    constructor CreateWithHandle(name: String; jc: jclass);
    // returns a handle to a new instance of this class.
    function Instantiate(params: TJavaParams): TJavaObject;

    function extends(JavaClass: TJavaClass): Boolean;

    property Signature: String read Fsig;
  end;

  { Exceptions to be raised when stuff goes wrong with the Java runtime. }

  EJvmException = class(Exception);
  EJavaClassNotFound = class(EJvmException);
  EJavaMethodNotFound = class(EJvmException);
  EJavaObjectInstantiation = class(EJvmException);
  EInvalidJNIHandle = class(EJvmException);

  { Various utility functions for creating java objects from delphi objects. }
function createJString(s: UTF8String): jstring;
function createJStringArray(var strings: TStrings): jarray;
function createJBooleanArray(var arr: array of JBoolean): jBooleanArray;
function createJByteArray(var arr: array of JByte): jByteArray;
function createJCharArray(var arr: array of JChar): jCharArray;
function createJShortArray(var arr: array of JShort): jShortArray;
function createJIntArray(var arr: array of JInt): jIntArray;
function createJLongArray(var arr: array of Jlong): jLongArray;
function createJFloatArray(var arr: array of JFloat): jFloatArray;
function createJDoubleArray(var arr: array of JDouble): jDoubleArray;
function getStringClass: jclass;

{ various utility functions for creating Delphi objects from Java objects }

function JToDString(js: jstring): String;
function JToTStrings(jarr: JobjectArray): TStrings;
function JstringArrayToDTStrings(jarr: jarray): TStrings;
function JdoubleArrayToDdoubleArray(jarr: jDoubleArray): TDdoubleArray;
function JfloatArrayToDsingleArray(jarr: jFloatArray): TDsingleArray;
function JcharArrayToDwordArray(jarr: jCharArray): TDwordArray;
function JbyteArrayToDshortintArray(jarr: jByteArray): TDshortintArray;
function JshortArrayToDsmallintArray(jarr: jShortArray): TDsmallintArray;
function JbooleanArrayToDbooleanArray(jarr: jBooleanArray): TDbooleanArray;

function JlongArrayToDlongArray(jarr: jLongArray): TDlongArray;
function JintArrayToDintArray(jarr: jIntArray): TDintArray;

implementation

uses
  fsl_java_runtime;

function typeToSig(type_: TJavaType; clss: TJavaClass): String;
begin
  case type_ of
    Aboolean:
      result := 'Z';
    Abyte:
      result := 'B';
    Achar:
      result := 'C';
    Ashort:
      result := 'S';
    Aint:
      result := 'I';
    Along:
      result := 'J';
    Afloat:
      result := 'F';
    Adouble:
      result := 'D';
    AString:
      result := 'Ljava/lang/String;';
    Aobject:
      result := 'L' + clss.Signature + ';';
    ABooleanArray:
      result := '[Z';
    AByteArray:
      result := '[B';
    ACharArray:
      result := '[C';
    AShortArray:
      result := '[S';
    AIntArray:
      result := '[I';
    ALongArray:
      result := '[J';
    AFloatArray:
      result := '[F';
    ADoubleArray:
      result := '[D';
    AStringArray:
      result := '[Ljava/lang/String;';
  else
    result := 'V';
  end;
end;

threadvar
  penvThread: PJNIEnv;

var
  penvGlobal: PJNIEnv;
  sc: jclass = Nil;
  SingleThreaded: Boolean;

function JNIPointer: PJNIEnv;
begin
  result := penvGlobal;
  if (not SingleThreaded) or (penvGlobal = Nil) then
  begin
    result := penvThread;
    if SingleThreaded then
      penvGlobal := penvThread;
  end;
  if result = Nil then
  begin
    TJavaRuntime.getDefault.GetVM;
    result := penvThread;
    if SingleThreaded then
      penvGlobal := penvThread;
  end;
  if result = Nil then
    raise EJvmException.Create('No penv pointer is available (need to call AttachThread?)');
end;

constructor TJavaVM.Create(p: PJavaVM);
begin
  pvm := p;
end;

destructor TJavaVM.Destroy;
begin
  // don't want to terminate the entire process
  // if pvm <> Nil then
  // CallExit(0);
  inherited Destroy;
end;

procedure TJavaVM.detachThread;
begin
  if threadAttached then
  begin
    pvm^.DetachCurrentThread(pvm, @penvThread, nil);
    penvThread := nil;
  end;
end;

procedure TJavaVM.Wait;
begin
  if pvm <> Nil then
    pvm^.DestroyJavaVM(pvm);
  pvm := Nil;
end;

class function TJavaVM.getPenv;
begin
  result := JNIPointer;
end;

class procedure TJavaVM.setThreadPenv(p: PJNIEnv);
begin
  penvThread := p;
  penvGlobal := p;
end;

function TJavaVM.threadAttached: boolean;
begin
  result := penvThread <> nil;
end;

class procedure TJavaVM.freeRef(jobj: JObject; isGlobal: Boolean);
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  if isGlobal then
    penv^.DeleteGlobalRef(penv, jobj)
  else
    penv^.DeleteLocalRef(penv, jobj);
end;

class procedure TJavaVM.CallMain(const classname: String; strings: TStrings);
var
  classID: jclass;
  methodID: JMethodID;
  stringArray: jarray;
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  classID := penv^.FindClass(penv, PUTF8Char(UTF8Encode(dotToSlash(classname))));
  if classID = nil then
    raise EJavaClassNotFound.Create('Could not find class ' + classname);
  methodID := penv^.GetStaticMethodID(penv, classID, PUTF8Char('main'), PUTF8Char('([Ljava/lang/String;)V'));
  if methodID = nil then
    raise EJavaMethodNotFound.Create('Could not find main method in class ' + classname);
  stringArray := createJStringArray(strings);
  penv^.CallStaticVoidMethodV(penv, classID, methodID, @stringArray);
  freeRef(stringArray, false);
end;

function TJavaVM.checkException: jthrowable;
begin
  result := JNIPointer^.ExceptionOccurred(JNIPointer);
end;

procedure TJavaVM.clearException;
begin
  JNIPointer^.ExceptionClear(JNIPointer);
end;

procedure TJavaVM.attachThread;
begin
  if penvThread = nil then
    pvm^.AttachCurrentThread(pvm, @penvThread, nil);
end;

class procedure TJavaVM.CallExit(exitCode: Integer);
var
  classID: jclass;
  methodID: JMethodID;
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  classID := penv^.FindClass(penv, 'java/lang/System');
  methodID := penv^.GetStaticMethodID(penv, classID, 'exit', '(I)V');
  penv^.CallStaticVoidMethodV(penv, classID, methodID, @exitCode);
end;

constructor TJavaClass.Create(name: String);
begin
  FPenv := JNIPointer;
  Fsig := dotToSlash(name);
  FLocalHandle := FPenv^.FindClass(FPenv, PUTF8Char(UTF8Encode(Fsig)));
//  if FLocalHandle = Nil then
//    raise EJavaClassNotFound.Create('class ' + name + ' not found.');
end;

constructor TJavaClass.CreateWithHandle(name: String; jc: jclass);
begin
  FPenv := JNIPointer;
  Fsig := dotToSlash(name);
  FLocalHandle := jc;
end;

function TJavaClass.Instantiate(params: TJavaParams): TJavaObject;
begin
  result := TJavaObject.Create(self, params)
end;

function TJavaClass.extends(JavaClass: TJavaClass): Boolean;
var
  penv: PJNIEnv;
begin
  penv := getPenv;
  result := penv^.isAssignableFrom(penv, Handle, JavaClass.Handle);
end;

constructor TJavaObject.Create(jcl: TJavaClass; params: TJavaParams);
var
  Signature: String;
  methodID: JMethodID;
  argPointer: Pointer;
begin
  Signature := '';
  argPointer := Nil;
  Fclass := jcl;
  FPenv := JNIPointer;
  if params <> Nil then
  begin
    Signature := params.Signature;
    argPointer := params.argPointer;
  end;
  Signature := '(' + Signature + ')V';
  methodID := FPenv^.GetMethodID(FPenv, jcl.Handle, '<init>', PUTF8Char(UTF8Encode(Signature)));
  if methodID = Nil then
    raise EJavaObjectInstantiation.Create('No such constructor ' + Signature);
  FLocalHandle := FPenv^.NewObjectV(FPenv, jcl.Handle, methodID, argPointer);
  if FLocalHandle = Nil then
    raise EJavaObjectInstantiation.Create('Could not create new instance of ' + jcl.Signature);
end;

constructor TJavaObject.CreateWithHandle(jcl: TJavaClass; jobj: JObject);
begin
  FPenv := JNIPointer;
  Fclass := jcl;
  FLocalHandle := jobj;
end;

destructor TJavaObject.Destroy;
begin
  if FGlobalHandle <> Nil then
    TJavaVM.freeRef(FGlobalHandle, true);
  inherited Destroy;
end;

function TJavaObject.getPenv: PJNIEnv;
begin
  if isGlobal or (FPenv = Nil) then
    result := JNIPointer
  else
    result := FPenv;
end;

function TJavaObject.Equals(JavaObject: TObject): Boolean;
var
  penv: PJNIEnv;
begin
  penv := getPenv;
  if (not self.Valid) or not (JavaObject is TJavaObject)  or (not TJavaObject(JavaObject).Valid) then
    raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
  result := penv^.IsSameObject(penv, Handle, TJavaObject(JavaObject).Handle);
end;

function TJavaObject.isInstanceOf(JavaClass: TJavaClass): Boolean;
var
  penv: PJNIEnv;
begin
  penv := getPenv;
  if (not self.Valid) or (not JavaClass.Valid) then
    raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
  result := penv^.isInstanceOf(penv, Handle, JavaClass.Handle);
end;

procedure TJavaObject.setGlobal(B: Boolean);
begin
  if B = Global then
    Exit;
  if B then
    FGlobalHandle := FPenv^.NewGlobalRef(FPenv, FLocalHandle)
  else
  begin
    FPenv := JNIPointer;
    FLocalHandle := FPenv^.NewLocalRef(FPenv, FGlobalHandle);
    FPenv^.DeleteGlobalRef(FPenv, FGlobalHandle);
    FGlobalHandle := Nil;
  end;
end;

function TJavaObject.isGlobal: Boolean;
begin
  result := FGlobalHandle <> Nil;
end;

function TJavaObject.isValid: Boolean;
begin
  if isGlobal then
    result := true
  else
    result := (FLocalHandle <> Nil) and (FPenv = JNIPointer);
end;

function TJavaObject.getHandle: JObject;
begin
  result := FGlobalHandle;
  if result = Nil then
    result := FLocalHandle;
end;

function TJavaObject.toString: String;
var
  toStringMethod: JMethodID;
  js: jstring;
  penv: PJNIEnv;
begin
  penv := getPenv;
  toStringMethod := penv^.GetMethodID(penv, ClassRef.Handle, 'toString', '()Ljava/lang/String;');
  js := penv^.callObjectMethod(penv, Handle, toStringMethod);
  result := JToDString(js);
end;

constructor TJavaParams.Create;
begin
  RefList := TList.Create;
end;

destructor TJavaParams.Destroy;
begin
  clear;
  RefList.Free;

  inherited Destroy;
end;

procedure TJavaParams.addBoolean(val: Boolean);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'Z';
end;

procedure TJavaParams.addByte(val: JByte);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'B';
end;

procedure TJavaParams.addChar(val: JChar);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'C';
end;

procedure TJavaParams.addShort(val: JShort);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'S';
end;

procedure TJavaParams.addInt(val: JInt);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'I';
end;

procedure TJavaParams.addLong(val: Jlong);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'J';
end;

procedure TJavaParams.addFloat(val: JFloat);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'F';
end;

procedure TJavaParams.addDouble(val: JDouble);
begin
  addToArgBuffer(@val, sizeof(val));
  Fsig := Fsig + 'D';
end;

procedure TJavaParams.addString(val: UTF8String);
var
  js: jstring;
begin
  js := createJString(val);
  addToArgBuffer(@js, sizeof(js));
  Fsig := Fsig + 'Ljava/lang/String;';
  RefList.add(js);
end;

procedure TJavaParams.addObject(val: TJavaObject; jcl: TJavaClass);
var
  objHandle: JObject;
begin
  objHandle := val.Handle;
  addToArgBuffer(@objHandle, sizeof(objHandle));
  Fsig := Fsig + 'L' + jcl.Signature + ';';
end;

procedure TJavaParams.addObjectArray(arr: array of TJavaObject; jcl: TJavaClass);
var
  penv: PJNIEnv;
  jarr: JobjectArray;
  I: Integer;
begin
  penv := JNIPointer;
  jarr := penv^.NewObjectArray(penv, High(arr) + 1, jcl.Handle, arr[0].Handle);
  for I := 1 + Low(arr) to High(arr) do
    penv^.setObjectArrayElement(penv, jarr, I, arr[I].Handle);
  addToArgBuffer(@jarr, sizeof(jarr));
  Fsig := Fsig + '[L' + jcl.Signature + ';';
  RefList.add(jarr)
end;

procedure TJavaParams.addBooleanArray(var arr: array of JBoolean);
var
  jbarray: jBooleanArray;
begin
  jbarray := createJBooleanArray(arr);
  addToArgBuffer(@jbarray, sizeof(jbarray));
  Fsig := Fsig + '[Z';
  RefList.add(jbarray)
end;

procedure TJavaParams.addByteArray(var arr: array of JByte);
var
  jbarray: jByteArray;
begin
  jbarray := createJByteArray(arr);
  addToArgBuffer(@jbarray, sizeof(jbarray));
  Fsig := Fsig + '[B';
  RefList.add(jbarray)
end;

procedure TJavaParams.addCharArray(var arr: array of JChar);
var
  jcarray: jCharArray;
begin
  jcarray := createJCharArray(arr);
  addToArgBuffer(@jcarray, sizeof(jcarray));
  Fsig := Fsig + '[C';
  RefList.add(jcarray)
end;

procedure TJavaParams.addShortArray(var arr: array of JShort);
var
  jsarray: jShortArray;
begin
  jsarray := createJShortArray(arr);
  addToArgBuffer(@jsarray, sizeof(jsarray));
  Fsig := Fsig + '[S';
  RefList.add(jsarray)
end;

procedure TJavaParams.addIntArray(var arr: array of JInt);
var
  jiarray: jIntArray;
begin
  jiarray := createJIntArray(arr);
  addToArgBuffer(@jiarray, sizeof(jiarray));
  Fsig := Fsig + '[I';
  RefList.add(jiarray)
end;

procedure TJavaParams.addLongArray(var arr: array of Jlong);
var
  jlarray: jLongArray;
begin
  jlarray := createJLongArray(arr);
  addToArgBuffer(@jlarray, sizeof(jlarray));
  Fsig := Fsig + '[J';
  RefList.add(jlarray)
end;

procedure TJavaParams.addFloatArray(var arr: array of JFloat);
var
  jfarray: jFloatArray;
begin
  jfarray := createJFloatArray(arr);
  addToArgBuffer(@jfarray, sizeof(jfarray));
  Fsig := Fsig + '[F';
  RefList.add(jfarray)
end;

procedure TJavaParams.addDoubleArray(var arr: array of JDouble);
var
  jdarray: jDoubleArray;
begin
  jdarray := createJDoubleArray(arr);
  addToArgBuffer(@jdarray, sizeof(jdarray));
  Fsig := Fsig + '[D';
  RefList.add(jdarray)
end;

procedure TJavaParams.addStringArray(var strings: TStrings);
var
  jsarray: jarray;
begin
  jsarray := createJStringArray(strings);
  addToArgBuffer(@jsarray, sizeof(jsarray));
  Fsig := Fsig + '[Ljava/lang/String;';
  RefList.add(jsarray)
end;

procedure TJavaParams.addToArgBuffer(p: Pointer; NumBytes: Integer);
var
  P1, P2: Pointer;
  I: Integer;
begin
  ReallocMem(FArgPointer, bufLength + NumBytes);
  // P1 := PUTF8Char(FArgPointer) + buflength;
  // P2 := PUTF8Char(P);

  P1 := Pointer(NativeInt(FArgPointer) + bufLength);
  P2 := Pointer(p);

  for I := 0 to (NumBytes - 1) do
    PUTF8Char(NativeInt(P1) + NativeInt(I))^ := PUTF8Char(NativeInt(P2) + NativeInt(I))^;
  inc(bufLength, NumBytes);

end;

procedure TJavaParams.clear;
var
  I: Integer;
begin
  for I := 0 to RefList.Count - 1 do
    TJavaVM.freeRef(RefList.Items[I], false);
  if Assigned(FArgPointer) then
    FreeMem(FArgPointer);
  FArgPointer := nil;
  Fsig := '';
end;

constructor TJavaMethod.Create(cls: TJavaClass; name: String; methodType: TMethodAttribute; returntype: TJavaType; params: TJavaTypeArray);
begin
  Create(cls, name, methodType, returntype, nil, params, []);
end;

constructor TJavaMethod.Create(cls: TJavaClass; name: String; methodType: TMethodAttribute; returntype: TJavaType; retclass: TJavaClass;
  params: TJavaTypeArray; paramClasses: TJavaClassArray);
var
  penv: PJNIEnv;
  I: Integer;
begin
  Fclass := cls;
  Fsig := '(';
  for I := 0 to length(params) - 1 do
  begin
    if I >= length(paramClasses) then
      Fsig := Fsig + typeToSig(params[I], nil)
    else
      Fsig := Fsig + typeToSig(params[I], paramClasses[I]);
  end;
  Fsig := Fsig + ')';
  FmethodType := methodType;
  FRetval := returntype;
  Fsig := Fsig + typeToSig(returntype, retclass);
  penv := JNIPointer;
  if FmethodType = static then
    FmethodID := penv^.GetStaticMethodID(penv, Fclass.Handle, PUTF8Char(UTF8Encode(name)), PUTF8Char(UTF8Encode(Fsig)))
  else
    FmethodID := penv^.GetMethodID(penv, Fclass.Handle, PUTF8Char(UTF8Encode(name)), PUTF8Char(UTF8Encode(Fsig)));
  if FmethodID = Nil then
    raise EJavaMethodNotFound.Create('method ' + name + Fsig + ' not found.');
end;

constructor TJavaMethod.CreateVoid(cls: TJavaClass; name: String);
begin
  Create(cls, name, nonstatic, Void, []);
end;

function TJavaMethod.Call(params: TJavaParams; jobj: TJavaObject): jvalue;
var
  penv: PJNIEnv;
  obj: JObject;
  argPointer: Pointer;
begin
  penv := JNIPointer;
  argPointer := Nil;
  if params <> Nil then
    argPointer := params.argPointer;
  if jobj <> Nil then
    obj := jobj.Handle
  else
    obj := Nil;
  if FmethodType = static then
    case FRetval of
      Void:
        penv^.CallStaticVoidMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Aboolean:
        result.z := penv^.CallStaticBooleanMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Abyte:
        result.B := penv^.CallStaticByteMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Achar:
        result.c := penv^.CallStaticCharMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Ashort:
        result.s := penv^.CallStaticShortMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Aint:
        result.I := penv^.CallStaticIntMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Along:
        result.J := penv^.CallStaticLongMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Afloat:
        result.F := penv^.CallStaticFloatMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Adouble:
        result.D := penv^.CallStaticDoubleMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      Aobject:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AString:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      ADoubleArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      ABooleanArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AByteArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      ACharArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AShortArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AIntArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      ALongArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AFloatArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);
      AStringArray:
        result.l := penv^.CallStaticObjectMethodV(penv, Fclass.Handle, FmethodID, argPointer);

    end;

  if FmethodType = nonvirtual then
    case FRetval of
      Void:
        penv^.CallNonvirtualVoidMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Aboolean:
        result.z := penv^.CallNonVirtualBooleanMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Abyte:
        result.B := penv^.CallNonVirtualByteMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Achar:
        result.c := penv^.CallNonVirtualCharMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Ashort:
        result.s := penv^.CallNonVirtualShortMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Aint:
        result.I := penv^.CallNonVirtualIntMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Along:
        result.J := penv^.CallNonVirtualLongMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Afloat:
        result.F := penv^.CallNonVirtualFloatMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Adouble:
        result.D := penv^.CallNonVirtualDoubleMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      Aobject:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AString:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      ADoubleArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      ABooleanArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AByteArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      ACharArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AShortArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AIntArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      ALongArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AFloatArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);
      AStringArray:
        result.l := penv^.CallNonVirtualObjectMethodV(penv, obj, Fclass.Handle, FmethodID, argPointer);

    end;

  if FmethodType = nonstatic then
    case FRetval of
      Void:
        penv^.CallVoidMethodV(penv, obj, FmethodID, argPointer);
      Aboolean:
        result.z := penv^.CallBooleanMethodV(penv, obj, FmethodID, argPointer);
      Abyte:
        result.B := penv^.CallByteMethodV(penv, obj, FmethodID, argPointer);
      Achar:
        result.c := penv^.CallCharMethodV(penv, obj, FmethodID, argPointer);
      Ashort:
        result.s := penv^.CallShortMethodV(penv, obj, FmethodID, argPointer);
      Aint:
        result.I := penv^.CallIntMethodV(penv, obj, FmethodID, argPointer);
      Along:
        result.J := penv^.CallLongMethodV(penv, obj, FmethodID, argPointer);
      Afloat:
        result.F := penv^.CallFloatMethodV(penv, obj, FmethodID, argPointer);
      Adouble:
        result.D := penv^.CallDoubleMethodV(penv, obj, FmethodID, argPointer);
      Aobject:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AString:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      ADoubleArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      ABooleanArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AByteArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      ACharArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AShortArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AIntArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      ALongArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AFloatArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
      AStringArray:
        result.l := penv^.CallObjectMethodV(penv, obj, FmethodID, argPointer);
    end;
end;

function createJString(s: UTF8String): jstring;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.NewStringUTF(penv, PUTF8Char(s));
end;

function JToDString(js: jstring): String;
var
  penv: PJNIEnv;
  len: NativeInt;
  CharBuf: PUTF8Char;
  IsCopy: JBoolean;
  res : UTF8String;
begin
  penv := JNIPointer;
  CharBuf := penv^.GetStringUTFChars(penv, js, IsCopy);
  len := penv^.GetStringUTFLength(penv, js);
  SetLength(result, len);

{$IFDEF FPC}
  StrLCopy(PUTF8Char(result), CharBuf, len);
{$ELSE}
  FHIR.Java.Strings.StrLCopy(PUTF8Char(res), CharBuf, len);
  result := UTF8ToString(res);
{$ENDIF}
  if IsCopy then
    penv^.ReleaseStringUTFChars(penv, js, CharBuf);
end;

function JToTStrings(jarr: JobjectArray): TStrings;
var
  penv: PJNIEnv;
  jobj: JObject;
  len, I: NativeInt;
begin
  penv := JNIPointer;
  result := TStringList.Create;
  len := penv^.GetArrayLength(penv, jarr);

  for I := 1 to len Do
  begin
    jobj := penv^.GetObjectArrayElement(penv, jarr, I - 1);
    result.add(JToDString(jobj));
  end;
end;

{$R-}

function JstringArrayToDTStrings(jarr: jarray): TStrings;
var
  penv: PJNIEnv;
  jobj: JObject;
  len, I: NativeInt;
begin
  penv := JNIPointer;
  result := TStringList.Create;
  len := penv^.GetArrayLength(penv, jarr);
  I := 0;
  if len > 0 then
  begin
    repeat
      inc(I);
      jobj := penv^.GetObjectArrayElement(penv, jarr, I - 1);
      result.add(JToDString(jobj));
    until I = len;
  end;
end;

function JdoubleArrayToDdoubleArray(jarr: jDoubleArray): TDdoubleArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJDouble;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetDoubleArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPdoubleArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JfloatArrayToDsingleArray(jarr: jFloatArray): TDsingleArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJFloat;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetFloatArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPsingleArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JcharArrayToDwordArray(jarr: jCharArray): TDwordArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJChar;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetCharArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPwordArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JbyteArrayToDshortintArray(jarr: jByteArray): TDshortintArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJByte;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetByteArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPshortintArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JshortArrayToDsmallintArray(jarr: jShortArray): TDsmallintArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJShort;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetShortArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPsmallintArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JbooleanArrayToDbooleanArray(jarr: jBooleanArray): TDbooleanArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJBoolean;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetBooleanArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPbooleanArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JlongArrayToDlongArray(jarr: jLongArray): TDlongArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJLong;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetLongArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPlongArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function JintArrayToDintArray(jarr: jIntArray): TDintArray;
var
  penv: PJNIEnv;
  len, I: NativeInt;
  d1: PJInt;
begin
  penv := JNIPointer;
  len := penv^.GetArrayLength(penv, jarr);
  d1 := penv^.GetIntArrayElements(penv, jarr, nil);
  I := 0;
  if len > 0 then
  begin
    SetLength(result, 1000);
    repeat
      inc(I);
      if (I - 1) = length(result) then
        SetLength(result, length(result) + 1000);
      result[I - 1] := TPintArray(d1)[I - 1];
    until I > len;
  end;
  SetLength(result, len);
end;

function getStringClass: jclass;

var
  penv: PJNIEnv;

begin
  if sc = Nil then
  begin
    penv := JNIPointer;
    sc := penv^.FindClass(JNIPointer, 'java/lang/String');
    sc := penv^.NewGlobalRef(penv, sc);
  end;
  result := sc;
end;

function createJStringArray(var strings: TStrings): jarray;
var
  I, Count: NativeInt;
  js: jstring;
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  Count := 0;
  if strings <> Nil then
    Count := strings.Count;
  js := createJString('');
  result := penv^.NewObjectArray(penv, Count, getStringClass, js);
  for I := 0 to Count - 1 do
  begin
    js := createJString(UTF8String(strings.strings[I]));
    penv^.setObjectArrayElement(penv, result, I, js);
  end;
end;

function createJBooleanArray(var arr: array of JBoolean): jBooleanArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newBooleanArray(penv, High(arr) + 1);
  penv^.setBooleanArrayRegion(penv, result, low(arr), High(arr) + 1, @arr);
end;

function createJByteArray(var arr: array of JByte): jByteArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newByteArray(penv, High(arr) + 1);
  penv^.setByteArrayRegion(penv, result, 0, High(arr) + 1, @arr);
end;

function createJCharArray(var arr: array of JChar): jCharArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newCharArray(penv, High(arr) + 1);
  penv^.setCharArrayRegion(penv, result, low(arr), High(arr) + 1, @arr);
end;

function createJShortArray(var arr: array of JShort): jShortArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newShortArray(penv, High(arr) + 1);
  penv^.setShortArrayRegion(penv, result, 0, High(arr) + 1, @arr);
end;

function createJIntArray(var arr: array of JInt): jIntArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newIntArray(penv, High(arr) + 1);
  penv^.setIntArrayRegion(penv, result, low(arr), High(arr) + 1, @arr);
end;

function createJLongArray(var arr: array of Jlong): jLongArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newLongArray(penv, High(arr) + 1);
  penv^.setLongArrayRegion(penv, result, low(arr), High(arr) + 1, @arr);
end;

function createJFloatArray(var arr: array of JFloat): jFloatArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newFloatArray(penv, High(arr) + 1);
  penv^.setFloatArrayRegion(penv, result, low(arr), High(arr) + 1, @arr);
end;

function createJDoubleArray(var arr: array of JDouble): jDoubleArray;
var
  penv: PJNIEnv;
begin
  penv := JNIPointer;
  result := penv^.newDoubleArray(penv, High(arr) + 1);
  penv^.setDoubleArrayRegion(penv, result, 0, High(arr) + 1, @arr);
end;

end.
