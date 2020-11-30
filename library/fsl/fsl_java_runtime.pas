unit fsl_java_runtime;

{
  Copyright (c) 1998+ Jonathan Revusky
  All rights reserved.

  This software was enhanced and ported to 32 bit and 64 bit by Amine Moulay Ramdane.

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

// This unit is devoted to locating the JVM install directory
// and using the invocation API to create a JVM.
// All of the code here is Win32-specific heuristics.

interface

uses
  {$IFDEF WINDOWS}
  Windows, Registry,
  {$ENDIF}
  Classes, SysUtils,
  fsl_fpc, fsl_java_jni, fsl_java_utilities, fsl_java_strings, fsl_java_wrapper;

type

{$IF Declared(PUTF8Char)} {$ELSE} PUTF8char = PAnsiChar;
{$IFEND}
{$IF Declared(UTF8Char)} {$ELSE} UTF8char = AnsiChar;
{$IFEND}
{$IF Declared(UTF8String)} {$ELSE} UTF8String = AnsiString;
{$IFEND}
  JvmType = (SunJava1, SunJava2{$IFDEF WINDOWS}, MSJava{$ENDIF});
  RuntimeOptions = set of JvmType;
  PPJavaVM = ^PJavaVM;

  TGetDefaultArgs = function(args: Pointer): jint; stdcall;
  TCreateVM = function(vm: PPJavaVM; penv: PPJNIEnv; p: Pointer): jint; stdcall;
  TGetCreatedVMs = function(vmBuf: PPJavaVM; buflen: Integer; nVMs: PJInt): jint; stdcall;
  TExitProc = procedure(exitCode: jint); stdcall;
  TAbortProc = procedure; stdcall;
  TPrintf = function(filepointer: Pointer; const format: PUTF8char; args: va_list): jint; stdcall;

  EJvmException = class(Exception);
  EJavaRuntimeNotFound = class(Exception);
  EJavaRuntimeCreation = class(Exception);
  EClasspathException = class(Exception);

  TClassPath = class(TStringList)
  private

    // Given a name of a class and its filename, perform a sanity check
    // to see if the fully qualified classname is consistent with this
    // filename classpath-wise.
    function sanityCheck(classname, filename: String): String;
    // Performs similar sanity check on a .java file.
    function SanityCheckSource(filename: String): String;
    procedure addDir(dir: String);
    procedure addPath(path: String);
  public
    // creates an instance based on a string.
    constructor Create;
    destructor Destroy; override;
    function FullPath: String;

    class function getDefault: TClassPath;
    class function getBootPath: TClassPath;
  end;

  // class to encapsulate the location of the java runtime
  // and the use of the JNI invocation API.
  TJavaRuntime = class
  private
    FJava11: Boolean;
    FHotspot: Boolean;
    FMS: Boolean;
    FJavaHome: String;
    FRuntimeLib: String;
    DLLHandle: THandle;
    vmargs: JDK1_1InitArgs;
    vmargs2: JavaVMInitArgs;
    FClasspath: TClassPath;
    FProperties: TStrings;
    FExitProc: TExitProc;
    FAbortProc: TAbortProc;
    FPrintf: TPrintf;
    FDebugPort, FVerbose, FDisableAsyncGC, FVerboseGC, FEnableClassGC, FVerifyMode, FCheckSource, FMinHeapSize, FMaxHeapSize, FJavaStackSize,
      FNativeStackSize: Integer;
    FPVMOption : PJavaVMOption;
    FPVMOptionCount : integer;

    function FindJava11: Boolean;
    function FindJava12: Boolean;
    {$IFDEF WINDOWS}
    function FindMSJava: Boolean;
    {$ENDIF}
    function CheckJavaRegistryKey(key: String): Boolean;
    function GetClasspath: String;
    procedure setClasspath(S: String);
    procedure SetNativeStackSize(Size: Integer);
    procedure SetJavaStackSize(Size: Integer);
    procedure setMinHeapSize(Size: Integer);
    procedure setMaxHeapSize(Size: Integer);
    procedure setVerifyMode(Arg: Integer);
    procedure SetCheckSource(Arg: Integer);
    procedure SetEnableClassGC(B: Boolean);
    procedure setVerboseGC(B: Boolean);
    procedure SetDisableAsyncGC(B: Boolean);
    procedure setVerbose(B: Boolean);
    procedure setDebugPort(Port: Integer);
    procedure setDebugging(Arg: Integer);
    procedure setAbortProc(proc: TAbortProc);
    procedure setExitProc(proc: TExitProc);
    procedure setPrintf(printproc: TPrintf);
    procedure Initialize; // Loads the DLL.
    procedure InitJava11;
    procedure InitJava2;
  public
    // processes a command-line option
    procedure processCommandLineOption(S: String);
    // processes a bunch of command line options passed in a container.
    procedure processCommandLine(Options: TStrings);
    procedure addProperty(S: String);
    function sanityCheck(classname, filename: String): String;
    function SanityCheckSource(filename: String): String;
    procedure addToClasspath(filename: String);
    function GetVM: TJavaVM; // Instantiates the JVM
    procedure CallMain(const classname: String; args: TStrings);
    procedure CallExit(val: Integer);
    procedure Wait;
    property RuntimeLib: String read FRuntimeLib;
    property JavaHome: String read FJavaHome;
    property Classpath: String read GetClasspath write setClasspath;
    property IsJava11: Boolean read FJava11;
    property IsMS: Boolean read FMS;
    property Hotspot: Boolean read FHotspot write FHotspot;

    // write-only properties that only work before instantiating VM.
    property NativeStackSize: Integer write SetNativeStackSize;
    property JavaStackSize: Integer write SetJavaStackSize;
    property CheckSource: Integer write SetCheckSource;
    property MinHeapSize: Integer write setMinHeapSize;
    property MaxHeapSize: Integer write setMaxHeapSize;
    property VerifyMode: Integer write setVerifyMode;
    property EnableClassGC: Boolean write SetEnableClassGC;
    property VerboseGC: Boolean write setVerboseGC;
    property DisableAsyncGC: Boolean write SetDisableAsyncGC;
    property Verbose: Boolean write setVerbose;
    property DebugPort: Integer write setDebugPort;
    property Debugging: Integer write setDebugging;
    property AbortProc: TAbortProc write setAbortProc;
    property ExitProc: TExitProc write setExitProc;
    property Printf: TPrintf write setPrintf;

    constructor Create(option: JvmType);
    destructor Destroy; override;
    class function getDefault: TJavaRuntime;
    class procedure SetJava11(Java11: Boolean);
    class procedure SetMSJava(MSJava: Boolean);
    class procedure setAppClassPath(path: String);
    class procedure setBasePath(path: String);
    class procedure setNeedTools(B: Boolean); // a bit of a hack for use by SmartJC.
    class procedure SetClassicVM(B: Boolean);

  end;

function getPackageName(filename: String): String;

implementation

var
  GJavaVM: TJavaVM; // there can only be one of these in the process

{$IFDEF FPC}
  SystemDirBuf: Array [0 .. MAX_PATH] of UTF8char;
{$ELSE}
  SystemDirBuf: Array [0 .. MAX_PATH] of WideChar;
{$ENDIF}
  NeedsJDK: Boolean; // // True, if we need the sun.* classes for compilation, etc.
  Prefers11: Boolean; // Do we look for java 1.1 first?
  PrefersMS: Boolean; // Do we look for MS JVM first?
  UseClassicVM: Boolean; // Do we use the classic VM?
  GetDefaultArgs: TGetDefaultArgs;
  CreateVM: TCreateVM;
  GetCreatedVMs: TGetCreatedVMs;
  instanceCount: Integer;
  searchrec: TSearchRec;
  AppClassPath: String; // classpath specified
  BasePath: String; // The class considered to be the base path, found from snooping in classfile.
  GClassPath: TClassPath; // the singleton TClasspath instance.
  GBootPath: TClassPath; // the TClasspath that represents the boot path
  DefaultRuntime: TJavaRuntime; // singleton JavaRuntime instance.

const
  PLUGIN_11_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.1'; // Home
  IBM_JRE_11_KEY = '\SOFTWARE\IBM\IBM WIN32 Runtime Environment, Java(TM) Edition';
  IBM_JDK_117_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Edition\1.1.7';
  IBM_JDK_118_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Tech. Edition\1.1.8';
  JRE_11_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.1'; // JavaHome, Microversion
  JB_KEY = '\SOFTWARE\JavaSoft\Java Runtime\1.1.6'; // JavaHome
  // JDK_11_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.1';
  JDK_11_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.8.0_45'; // JavaHome, Microversion
  // JavaHome, Microversion
  JRE_12_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.2'; // JavaHome, RuntimeLib
  JRE_13_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.3';
  // JRE_14_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.4';
  // JRE_14_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.8.0_45';
  // JRE_15_KEY='\SOFTWARE\JavaSoft\Java Runtime Environment\1.7.0_25';
  JRE_14_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.8';
  JRE_15_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.7';
  JRE_16_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.6';
  JRE_17_KEY = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.5';

  PLUGIN_12_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.2'; // JavaHome, RuntimeLib
  PLUGIN_13_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.3'; // JavaHome, RuntimeLib
  // PLUGIN_14_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\1.4';
  PLUGIN_14_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\11.45.2';
  PLUGIN_15_KEY = '\SOFTWARE\JavaSoft\Java Plug-in\10.25.2';

  JDK_12_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.2'; // JavaHome, Microversion
  JDK_13_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.3';
  JDK_14_KEY = '\SOFTWARE\JavaSoft\Java Development Kit\1.4';

  JRE11Keys: array [1 .. 3] of String = (PLUGIN_11_KEY, IBM_JRE_11_KEY, JRE_11_KEY);
  JDK11Keys: array [1 .. 4] of String = (IBM_JDK_118_KEY, IBM_JDK_117_KEY, JDK_11_KEY, JB_KEY);
  JRE12Keys: array [1 .. 10] of String = (JRE_14_KEY, PLUGIN_14_KEY, JRE_15_KEY, PLUGIN_15_KEY, JRE_16_KEY, JRE_17_KEY, JRE_13_KEY, PLUGIN_13_KEY,
    JRE_12_KEY, PLUGIN_12_KEY);
  BootClasspath: String = '';

procedure StripComments(var Line: String; var InComment: Boolean); forward;

procedure TJavaRuntime.Initialize;
begin
  if DLLHandle <> 0 then
    exit; // already initialized.

{$IFDEF FPC}
  DLLHandle := LoadLibrary(PChar(FRuntimeLib));
{$ELSE}
  DLLHandle := SafeLoadLibrary(FRuntimeLib);
{$ENDIF}
  if DLLHandle = 0 then
    raise EJavaRuntimeCreation.Create('Could not load DLL ' + FRuntimeLib);
  @CreateVM := getProcAddress(DLLHandle, 'JNI_CreateJavaVM');
  @GetDefaultArgs := getProcAddress(DLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
  @GetCreatedVMs := getProcAddress(DLLHandle, 'JNI_GetCreatedJavaVMs');
  if (@CreateVM = Nil) or (@GetDefaultArgs = Nil) or (@GetCreatedVMs = Nil) then
    raise EJavaRuntimeCreation.Create('Dynamic Link Library ' + FRuntimeLib + ' is not valid.');
  vmargs.version := $00010008;
  vmargs2.version := $00010008;
  GetDefaultArgs(@vmargs);
  GetDefaultArgs(@vmargs2);

end;

function TJavaRuntime.GetVM: TJavaVM;
var
  PVM: PJavaVM;
  penv: PJNIEnv;
  args: Pointer;
  i: Integer;
begin
  if GJavaVM <> Nil then
  begin
    result := GJavaVM;
    exit;
  end;
  if @CreateVM = Nil then
    Initialize;
  if IsJava11 then
  begin
    InitJava11;
    args := @vmargs;
  end
  else
  begin
    InitJava2;
    args := @vmargs2;
  end;
  i := CreateVM(@PVM, @penv, args);
  if i <> 0 then
    raise EJavaRuntimeCreation.Create('Could not create JVM (' + inttostr(i) + ')');
  TJavaVM.setThreadPenv(penv);
  GJavaVM := TJavaVM.Create(PVM);
  result := GJavaVM;
end;

procedure TJavaRuntime.InitJava11;
begin
  vmargs.properties := convertStrings(FProperties);
  vmargs.Classpath := fsl_java_strings.strNew(Classpath);
  vmargs.Verbose := FVerbose;
  vmargs.DisableAsyncGC := FDisableAsyncGC;
  vmargs.EnableVerboseGC := FVerboseGC;
  vmargs.EnableClassGC := FEnableClassGC;
  vmargs.CheckSource := FCheckSource;
  vmargs.VerifyMode := FVerifyMode;
  if Assigned(FExitProc) then
    vmargs.exit := FExitProc;
  if Assigned(FAbortProc) then
    vmargs.abort := FAbortProc;
  if Assigned(FPrintf) then
    vmargs.vfprintf := FPrintf;
  if FDebugPort <> 0 then
    vmargs.DebugPort := FDebugPort;
  if FMinHeapSize > 0 then
    vmargs.MinHeapSize := FMinHeapSize;
  if FMaxHeapSize > 0 then
    vmargs.MaxHeapSize := FMaxHeapSize;
  if FJavaStackSize > 0 then
    vmargs.JavaStackSize := FJavaStackSize;
  if FNativeStackSize > 0 then
    vmargs.NativeStackSize := FNativeStackSize;
end;

procedure TJavaRuntime.InitJava2;
var
  i: Integer;
  S: String;
  PVO: PJavaVMOption;
begin
  // Just handle classpath and properties for now.

  vmargs2.Noptions := 1 + FProperties.Count;
  if (FVerbose <> 0) or (FVerboseGC <> 0) then
    inc(vmargs2.Noptions);
  if FVerboseGC <> 0 then
    inc(vmargs2.Noptions);
  if FMinHeapSize > 0 then
    inc(vmargs2.Noptions);
  if FMaxHeapSize > 0 then
    inc(vmargs2.Noptions);
  if BootClasspath <> '' then
    inc(vmargs2.Noptions);
  if FEnableClassGC <> 0 then
    inc(vmargs2.Noptions);
  if Assigned(FExitProc) then
    inc(vmargs2.Noptions);
  if Assigned(FAbortProc) then
    inc(vmargs2.Noptions);
  if Assigned(FPrintf) then
    inc(vmargs2.Noptions);

  vmargs2.ignoreUnrecognized := True;
  FPVMOptionCount := vmargs2.Noptions;
  FPVMOption := AllocMem(sizeof(JavaVMOPtion) * FPVMOptionCount);
  try
    PVO := FPVMOption;
    S := '-Djava.class.path=' + Classpath;
  {$IFDEF FPC}
    FPVMOption^.optionString := StrNew(PUTF8char(S));
  {$ELSE}
    FPVMOption^.optionString := FHIR.Java.Strings.StrNew(S);
  {$ENDIF}
    FPVMOption^.extraInfo := Nil;
    inc(PVO);

    for i := 0 to FProperties.Count - 1 do
    begin
      S := '-D' + FProperties[i];

  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char(S));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew(S);
  {$ENDIF}
      inc(PVO);
    end;

    if (FVerbose <> 0) or (FVerboseGC <> 0) then
    begin
      S := '-verbose:';
      if FVerbose <> 0 then
        S := S + 'class';
      if FVerboseGC <> 0 then
        S := S + ',';
      if FVerboseGC <> 0 then
        S := S + 'gc';
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char(S));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew(S);
  {$ENDIF}
      inc(PVO);
    end;

    if FMinHeapSize > 0 then
    begin

  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('-Xms' + inttostr(FMinHeapSize)));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('-Xms' + inttostr(FMinHeapSize));
  {$ENDIF}
      inc(PVO);
    end;

    if FMaxHeapSize > 0 then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('-Xmx' + inttostr(FMaxHeapSize)));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('-Xmx' + inttostr(FMaxHeapSize));
  {$ENDIF}
      inc(PVO);
    end;

    if FEnableClassGC <> 0 then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('-Xnoclassgc'));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('-Xnoclassgc');
  {$ENDIF}
      inc(PVO);
    end;

    if BootClasspath <> '' then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('-Xbootclasspath/p:' + BootClasspath));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('-Xbootclasspath/p:' + BootClasspath);
  {$ENDIF}
      inc(PVO);
    end;

    if Assigned(FPrintf) then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('exit'));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('exit');
  {$ENDIF}
      PVO^.extraInfo := @FPrintf;
      inc(PVO);
    end;

    if Assigned(FExitProc) then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('exit'));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('exit');
  {$ENDIF}
      PVO^.extraInfo := @FExitProc;
      inc(PVO);
    end;

    if Assigned(FAbortProc) then
    begin
  {$IFDEF FPC}
      PVO^.optionString := StrNew(PUTF8char('abort'));
  {$ELSE}
      PVO^.optionString := FHIR.Java.Strings.StrNew('abort');
  {$ENDIF}
      PVO^.extraInfo := @FAbortProc;
    end;

    vmargs2.Options := FPVMOption;
    vmargs2.version := $00010008;
  finally
//    FreeMemory(PVMOption^.optionString);
//    FreeMemory(PVMOption)
  end;
end;


// convenience wrappers.

procedure TJavaRuntime.CallMain(const classname: String; args: TStrings);
begin
  TJavaVM.CallMain(classname, args);
end;

procedure TJavaRuntime.Wait;
begin
  if GJavaVM <> Nil then
  begin
    if IsMS then
      Sleep({$IFDEF WINDOWS} INFINITE {$ELSE} -1 {$ENDIF})
    else
      GJavaVM.Wait;
  end;
end;

procedure TJavaRuntime.CallExit(val: Integer);
begin
  TJavaVM.CallExit(val);
end;

procedure TJavaRuntime.processCommandLineOption(S: String);
var
  // S:String;
  L: String;
  function extractSize(S: String): Integer;
  begin
    if S[length(S)] = 'k' then
      result := $400
    else if S[length(S)] = 'm' then
      result := $100000
    else
      result := 1;
    if result <> 1 then
      S := Copy(S, 1, length(S) - 1);
    result := result * StrToIntDef(S, 0);
  end;

begin
  // S:=S1;
  L := LowerCase(S);
  if (L = '-v') or (L = 'verbose') then
    Verbose := True
  else if (L = '-verbosegc') then
    VerboseGC := True
  else if (L = '-noasync') then
    DisableAsyncGC := True
  else if (L = '-noclassgc') then
    EnableClassGC := false
  else if (L = '-verify') then
    VerifyMode := 2
  else if (L = '-noverify') then
    VerifyMode := 0
  else if (L = '-verifyremote') then
    VerifyMode := 1
  else if (L = '-nojit') then
    addProperty('java.compiler=')
  else if Copy(L, 1, 3) = '-cp' then
    FClasspath.addPath(Copy(S, 5, length(S)))
  else if Copy(L, 1, 10) = '-classpath' then
    FClasspath.addPath(Copy(S, 12, length(S)))
  else if Copy(L, 1, 2) = '-d' then
    addProperty(Copy(S, 3, length(S)))
  else if Copy(L, 1, 3) = '-ms' then
    MinHeapSize := extractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-mx' then
    MaxHeapSize := extractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-ss' then
    NativeStackSize := extractSize(Copy(L, 4, length(L)))
  else if Copy(L, 1, 3) = '-oss' then
    NativeStackSize := extractSize(Copy(L, 5, length(L)));
end;

procedure TJavaRuntime.processCommandLine(Options: TStrings);
var
  i: Integer;
begin
  for i := 0 to Options.Count - 1 do
    processCommandLineOption(Options[i]);
end;

class function TJavaRuntime.getDefault: TJavaRuntime;
var
  FirstChoice, SecondChoice, ThirdChoice, temp: JvmType;
begin
  if DefaultRuntime = Nil then
  begin
    FirstChoice := SunJava2;
    SecondChoice := SunJava1;
    {$IFDEF WINDOWS}
    ThirdChoice := MSJava;
    if PrefersMS then
    begin
      FirstChoice := MSJava;
      SecondChoice := SunJava2;
      ThirdChoice := SunJava1;
    end;
    {$ENDIF}
    if Prefers11 then
    begin
      temp := FirstChoice;
      FirstChoice := SunJava1;
      SecondChoice := temp;
    end;
    try
      DefaultRuntime := TJavaRuntime.Create(FirstChoice);
    except
      on EJavaRuntimeNotFound do
        try
          DefaultRuntime := TJavaRuntime.Create(SecondChoice);
        except
          on EJavaRuntimeNotFound do
            DefaultRuntime := TJavaRuntime.Create(ThirdChoice);
        end;
    end;
  end;
  result := DefaultRuntime;
end;

class procedure TJavaRuntime.SetJava11(Java11: Boolean);
begin
  Prefers11 := Java11;
end;

class procedure TJavaRuntime.SetClassicVM(B: Boolean);
begin
  UseClassicVM := B;
end;

class procedure TJavaRuntime.SetMSJava(MSJava: Boolean);
begin
  PrefersMS := MSJava;
end;

class procedure TJavaRuntime.setNeedTools(B: Boolean);
begin
  NeedsJDK := True;
end;

procedure TJavaRuntime.addToClasspath(filename: String);
begin
  FClasspath.addDir(filename);
end;


function TJavaRuntime.GetClasspath: String;
{$IFDEF WINDOWS}
var
  cpath: TClassPath;
  Reg: TRegistry;
  GotKey: Boolean;
begin
  cpath := TClassPath.getDefault;
  if ((not FJava11) and NeedsJDK) then
  begin
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    GotKey := Reg.OpenKey(JDK_13_KEY, false);

    if not GotKey then
      GotKey := Reg.OpenKey(JDK_12_KEY, false);
    if GotKey then
    begin
      if Reg.ValueExists('JavaHome') then
        cpath.addDir(Reg.ReadString('JavaHome') + '\lib\tools.jar');
    end;
    Reg.Free;
  end;
  result := cpath.FullPath;
{$ELSE}
begin
raise exception.create('not done yet');
{$ENDIF}
end;

procedure TJavaRuntime.setClasspath(S: String);
begin
  if FClasspath <> nil then
    FClasspath.Free;
  FClasspath := TClassPath.getDefault;
  FClasspath.addPath(S);
end;

constructor TJavaRuntime.Create(option: JvmType);
begin
  if DefaultRuntime <> Nil then
    raise EJavaRuntimeCreation.Create('Can only instantiate one Java runtime per process');
  case option of
    SunJava1:
      if not FindJava11 then
        raise EJavaRuntimeNotFound.Create('Java 1.1 runtime not found');
    SunJava2:
      if not FindJava12 then
        raise EJavaRuntimeNotFound.Create('Java 2 runtime not found');
    {$IFDEF WINDOWS}
    MSJava:
      if not FindMSJava then
        raise EJavaRuntimeNotFound.Create('MS Java runtime not found');
    {$ENDIF}
  end;
  DefaultRuntime := Self; // set the singleton
  FClasspath := TClassPath.getDefault;
  FProperties := TStringList.Create;
  FVerifyMode := 1;
end;

destructor TJavaRuntime.Destroy;
var
  PVO: PJavaVMOption;
  p : PUTF8Char;
  i : integer;
begin
  if FPVMOption <> nil then
  begin
    PVO := FPVMOption;
    for i := 1 to FPVMOptionCount do
    begin
      p := PVO.OptionString;
      dec(p, sizeof(cardinal));
      FreeMem(p);
    end;
    FreeMem(FPVMOption);
  end;

  FProperties.free;
  DefaultRuntime := Nil;
  if (DLLHandle <> 0) and (instanceCount = 0) then
    if FreeLibrary(DLLHandle) then
      DLLHandle := 0;
  inherited Destroy;
end;


{$IFDEF WINDOWS}
function TJavaRuntime.FindMSJava: Boolean;
var
  DLLPath: String;
begin
  result := false;
{$IFDEF FPC}
  GetSystemDirectory(SystemDirBuf, MAX_PATH);
{$ELSE}
  GetSystemDirectory(@SystemDirBuf, MAX_PATH);
{$ENDIF}
  DLLPath := SystemDirBuf;
  DLLPath := DLLPath + '\msjava.dll';
  if FileExists(DLLPath) then
  begin
    FJava11 := True;
    FRuntimeLib := DLLPath;
    FJavaHome := SystemDirBuf;
    FMS := True;
    result := True;
  end;
end;
{$ENDIF}

function TJavaRuntime.FindJava12: Boolean;
var
  i: Integer;
begin
  result := false;
  for i := Low(JRE12Keys) to High(JRE12Keys) do
    if (CheckJavaRegistryKey(JRE12Keys[i])) then
    begin
      FJava11 := false; // This is a 1.2 VM.
      result := True; // success!
      exit;
    end;
end;

{ heuristics to find a java 1.1 runtime. }

function TJavaRuntime.FindJava11: Boolean;
var
  i: Integer;
begin
  // First look on the system path.
  FRuntimeLib := {$IFDEF WINDOWS} FindOnSystemPath('javai.dll'); {$ELSE} ''; {$ENDIF}
  if FRuntimeLib <> '' then
  begin
    FJavaHome := ExtractFileDir(ExtractFileDir(FRuntimeLib));
    result := True;
    FJava11 := True;
    exit; // success!
  end;

  // Failing that, search the Windows registry for location.

  if not NeedsJDK then
  begin
    for i := Low(JRE11Keys) to High(JRE11Keys) do
    begin
      if (CheckJavaRegistryKey(JRE11Keys[i])) then
      begin
        result := True; // success!
        FJava11 := True;
        exit;
      end;
    end;
  end;
  for i := Low(JDK11Keys) to High(JDK11Keys) do
  begin
    if (CheckJavaRegistryKey(JDK11Keys[i])) then
    begin
      result := True; // success!
      FJava11 := True;
      exit;
    end;
  end;
  result := false; // failure.
end;

{ Checks the Java registry key given as an argument.
  Returns true on success and sets the FJavaLib and FJavaHome
  fields }

function TJavaRuntime.CheckJavaRegistryKey(key: String): Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  S, HotspotLib: String;
begin
  result := false;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(key) then
    begin
      if Reg.ValueExists('RuntimeLib') then
      begin
        S := Reg.ReadString('RuntimeLib');
        if S = '' then
          S := Reg.ReadString('JavaHome') + '\bin\classic\jvm.dll';
        if FileExists(S) then
        begin
          result := True;
          if not UseClassicVM then
          begin
            HotspotLib := ExtractFileDir(ExtractFileDir(S)) + '\hotspot\jvm.dll';
            if FileExists(HotspotLib) then
            begin
              S := HotspotLib;
              FHotspot := True;
            end;
          end;
          FRuntimeLib := S;
          if Reg.ValueExists('JavaHome') then
            FJavaHome := Reg.ReadString('JavaHome')
          else
            FJavaHome := ExtractFileDir(ExtractFileDir(ExtractFileDir(FRuntimeLib)));
        end;
        exit;
      end
      else
      begin
        if Reg.ValueExists('JavaHome') then
          S := Reg.ReadString('JavaHome')
        else if Reg.ValueExists('Home') then
          S := Reg.ReadString('Home')
        else if Reg.ValueExists('java_home') then
          S := Reg.ReadString('java_home')
        else
          exit; // failure!
      end;
    end
    else
      exit;

    // Now check that it's really there.
    if S[length(S)] = Chr(92) then
      S := Copy(S, 1, length(S) - 1);
    if FileExists(S + '\bin\javai.dll') then
    begin
      FRuntimeLib := S + '\bin\javai.dll'; // Success!
      FJavaHome := S;
      result := True;
    end;
  finally
    Reg.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TJavaRuntime.SetNativeStackSize(Size: Integer);
begin
  if Size > 0 then
    FNativeStackSize := Size;
end;

procedure TJavaRuntime.SetJavaStackSize(Size: Integer);
begin
  if Size > 0 then
    FJavaStackSize := Size;
end;

procedure TJavaRuntime.setMinHeapSize(Size: Integer);
begin
  if Size > 0 then
    FMinHeapSize := Size;
end;

procedure TJavaRuntime.setMaxHeapSize(Size: Integer);
begin
  if Size > 0 then
    FMaxHeapSize := Size;
end;

procedure TJavaRuntime.setVerifyMode(Arg: Integer);
begin
  FVerifyMode := Arg;
end;


procedure TJavaRuntime.SetCheckSource(Arg: Integer);
begin
  FCheckSource := Arg;
end;

procedure TJavaRuntime.SetEnableClassGC(B: Boolean);
begin
  FEnableClassGC := Integer(B);
end;

procedure TJavaRuntime.setVerboseGC(B: Boolean);
begin
  FVerboseGC := Integer(B);
end;

procedure TJavaRuntime.SetDisableAsyncGC(B: Boolean);
begin
  FDisableAsyncGC := Integer(B);
end;

procedure TJavaRuntime.setVerbose(B: Boolean);
begin
  FVerbose := Integer(B);
end;

procedure TJavaRuntime.setDebugPort(Port: Integer);
begin
  FDebugPort := Port;
end;

procedure TJavaRuntime.setDebugging(Arg: Integer);
begin
end;

procedure TJavaRuntime.setAbortProc(proc: TAbortProc);
begin
  FAbortProc := proc;
end;

procedure TJavaRuntime.setExitProc(proc: TExitProc);
begin
  FExitProc := proc;
end;

procedure TJavaRuntime.setPrintf(printproc: TPrintf);
begin
  FPrintf := printproc;
end;

function TJavaRuntime.sanityCheck(classname, filename: String): String;
begin
  result := FClasspath.sanityCheck(classname, filename);
end;

function TJavaRuntime.SanityCheckSource(filename: String): String;
begin
  result := FClasspath.SanityCheckSource(filename);
end;

procedure TJavaRuntime.addProperty(S: String);
begin
  FProperties.add(S);
end;

procedure addAllArchives(C: TClassPath; directory, pattern: String);
begin
  if FindFirst(directory + pattern, faAnyFile, searchrec) = 0 then
  begin
    C.addDir(directory + searchrec.Name);
    while FindNext(searchrec) = 0 do
      C.addDir(directory + searchrec.Name);
  end;
  FindClose(searchrec);
end;

class function TClassPath.getDefault: TClassPath;
var
  Home, libjars, ThirdPartyDir: String;
  Runtime: TJavaRuntime;
  searchrec: TSearchRec;
begin
  if GClassPath = Nil then
  begin
    GClassPath := TClassPath.Create;
    Runtime := TJavaRuntime.getDefault;
    Home := Runtime.JavaHome;

    if FileExists(Home + '\classes') then
      GClassPath.addDir(Home + '\classes');

    // Now see if there are any other jars or zips in there and add them.

    libjars := Home + '\lib\*.jar';
    addAllArchives(GClassPath, Home + '\lib\ext\', '*.jar');
    addAllArchives(GClassPath, Home + '\lib\ext\', '*.zip');
    addAllArchives(GClassPath, Home + '\lib\', '*.jar');
    addAllArchives(GClassPath, Home + '\lib\', '*.zip');
    // if BaseClassPath = '' then
    // BaseClassPath := GetEnvironmentString('CLASSPATH');

    ThirdPartyDir := GetEnvironmentString('JARS_DIR');
    if ThirdPartyDir = '' then
      ThirdPartyDir := '.';
    if ThirdPartyDir[length(ThirdPartyDir)] <> '\' then
      ThirdPartyDir := ThirdPartyDir + '\';

    if FindFirst(ThirdPartyDir + '*.jar', 0, searchrec) = 0 then
      repeat
        GClassPath.addDir(ThirdPartyDir + searchrec.Name);
      until FindNext(searchrec) <> 0;

    GClassPath.addPath(GetEnvironmentString('CLASSPATH'));
    GClassPath.addPath(AppClassPath);
    GClassPath.addPath(BasePath);
    GClassPath.addDir(getCurrentDir); // Maybe better off without this.
  end;
  result := GClassPath;
end;

class function TClassPath.getBootPath: TClassPath;
var
  Home, ThirdPartyDir: String;
  searchrec: TSearchRec;
begin
  if GBootpath = Nil then
  begin
    GBootpath := TClassPath.Create;
    Home := TJavaRuntime.getDefault.JavaHome;
    if FileExists(Home + '\classes') then
      GBootpath.addDir(Home + '\classes');

    // Now see if there are any other jars or zips in there and add them.

    addAllArchives(GBootpath, Home + '\lib\ext\', '*.jar');
    addAllArchives(GBootpath, Home + '\lib\ext\', '*.zip');
    addAllArchives(GBootpath, Home + '\lib\', '*.jar');
    addAllArchives(GBootpath, Home + '\lib\', '*.zip');
    // if BaseClassPath = '' then
    // BaseClassPath := GetEnvironmentString('CLASSPATH');

    ThirdPartyDir := GetEnvironmentString('JARS_DIR');
    if ThirdPartyDir = '' then
      ThirdPartyDir := '.';
    if ThirdPartyDir[length(ThirdPartyDir)] <> '\' then
      ThirdPartyDir := ThirdPartyDir + '\';

    if FindFirst(ThirdPartyDir + '*.jar', 0, searchrec) = 0 then
      repeat
        GBootpath.addDir(ThirdPartyDir + searchrec.Name);
      until FindNext(searchrec) <> 0;

    GBootpath.addPath(GetEnvironmentString('CLASSPATH'));
    GBootpath.addPath(AppClassPath);
    GBootpath.addPath(BasePath);
    GBootpath.addDir(getCurrentDir); // Maybe better off without this.
  end;
  result := GBootpath;
end;

constructor TClassPath.Create;
begin
  inherited Create;
end;

destructor TClassPath.Destroy;
begin

  inherited;
end;

procedure TClassPath.addPath(path: String);
var
  Len: Integer;
  Dirs: TStringList;
  i: Integer;
begin
  Dirs := TStringList.Create;
  repeat
    Len := AnsiPos(';', path);
    if Len > 1 then
      Dirs.add(Copy(path, 1, Len - 1));
    path := Copy(path, Len + 1, length(path));
  until Len = 0;
  if length(path) > 0 then
    Dirs.add(path);
  for i := Dirs.Count downto 1 do
    addDir(Dirs[i - 1]);
  Dirs.Free;
end;

procedure TClassPath.addDir(dir: String);
var
  S: String;
  i: Integer;
begin
  S := ExpandFileName(dir);
  if (S[length(S)] = '\') and (S[length(S) - 1] <> ':') then
    S := Copy(S, 1, length(S) - 1);
  i := IndexOf(S);
  if i >= 0 then
    Delete(i);
  add(S);
end;

function TClassPath.FullPath: String;
var
  i: Integer;
begin
  result := '';
  for i := Count downto 1 do
  begin
    if i < Count then
      result := result + ';';
    result := result + Strings[i - 1];
  end;
end;


// Sets the part of the classpath that is specific to the app.

class procedure TJavaRuntime.setAppClassPath(path: String);
begin
  AppClassPath := path;
end;

procedure addAllFilesToPath(directory, pattern: String; var path: String);
begin
  if FindFirst(directory + pattern, faAnyFile, searchrec) = 0 then
  begin
    path := path + ';' + directory + searchrec.Name;
    while FindNext(searchrec) = 0 do
      path := path + ';' + directory + searchrec.Name;
  end;
  FindClose(searchrec);
end;

class procedure TJavaRuntime.setBasePath(path: String);
var
  dir: String;
begin
  BasePath := ExpandFileName(path);
  dir := ExtractFilePath(ExpandFileName(BasePath));
  addAllFilesToPath(dir, '*.zip', BasePath);
  addAllFilesToPath(dir, '*.jar', BasePath);
  addAllFilesToPath(dir + 'lib\', '*.zip', BasePath);
  addAllFilesToPath(dir + 'lib\', '*.jar', BasePath);
  addAllFilesToPath(dir + 'libs\', '*.zip', BasePath);
  addAllFilesToPath(dir + 'libs\', '*.jar', BasePath);
  {
    addAllFilesToPath(Dir + '..\lib\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + '..\lib\', '*.jar', BasePath);
    addAllFilesToPath(Dir + '..\libs\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + '..\libs\', '*.jar', BasePath);
  }
  if GClassPath <> Nil then
    GClassPath.addPath(BasePath);
end;

function TClassPath.sanityCheck(classname, filename: String): String;
var
  fullFile, pathName, package, BasePath, temp: String;
  i: Integer;
  Oops: Boolean;
begin
  fullFile := ExpandFileName(filename);
  pathName := ExtractFileDir(fullFile);
  temp := toBackSlash(classname); // temp is string where the / is now \.
  for i := length(temp) downto 1 do
  begin
    if temp[i] = '\' then
      break;
  end;
  if i = 0 then // no slashes, anonymous package
  begin
    addDir(pathName); // put the filename's path on the classpath
{$IFDEF FPC}
    setCurrentDirectory(PChar(pathName));
{$ELSE}
    setCurrentDirectory(PChar(pathName));
{$ENDIF}
    exit;
  end;
  package := Copy(temp, 1, i - 1);
  Oops := length(Package) > length(pathName) - 3;
  if not Oops then
  begin
    temp := Copy(pathName, 1 + length(pathName) - length(Package), length(Package));
    Oops := (LowerCase(temp) <> LowerCase(Package));
  end;
  if Oops then // There is a problem.
    raise EClasspathException.Create('File ' + fullFile + ' should be on relative path ' + package);
  BasePath := Copy(pathName, 1, length(pathName) - length(temp));
  addDir(BasePath);
  result := BasePath;
end;

function TClassPath.SanityCheckSource(filename: String): String;
var
  package, classname: String;
begin
  Package := getPackageName(filename);
  classname := Package + ExtractFileName(filename);
  ChopExtension(classname);
  result := sanityCheck(classname, filename);
end;

// Get the package name inside a source file.
// This code is a bit messy. Maybe I'll clean it up later.

function getPackageName(filename: String): String;
var
  T: TextFile;
  InComment: Boolean;
  Line: String;
  i: Integer;
begin
  AssignFile(T, filename);
  Reset(T);
  InComment := false;
  while not Eof(T) do
  begin
    ReadLn(T, Line);
    StripComments(Line, InComment);
    i := AnsiPos('package', Line);
    if i > 0 then
    begin
      result := Copy(Line, i + 8, length(Line));
      i := AnsiPos(';', result);
      if i > 0 then
      begin
        result := Trim(Copy(result, 1, i - 1));
        break;
      end;
      if AnsiPos('{', Line) > 0 then
        break;
    end;
  end;
  CloseFile(T);
  if length(result) > 0 then
    result := result + '.';
end;

procedure StripComments(var Line: String; var InComment: Boolean);
var
  S: String;
  i: Integer;
begin
  S := '';
  if InComment then
  begin
    i := AnsiPos('*/', Line);
    if i > 0 then
    begin
      Line := Copy(Line, 2 + i, length(Line));
      InComment := false;
      StripComments(Line, InComment);
    end
    else
      Line := '';
  end
  else
  begin
    i := AnsiPos('/*', Line);
    if i > 0 then
    begin
      InComment := True;
      S := Copy(Line, 1, i - 1);
      Line := Copy(Line, i + 2, length(Line));
      StripComments(Line, InComment);
    end;
    Line := S + Line;
  end;
  i := AnsiPos('//', Line);
  if i > 0 then
    Line := Copy(Line, 1, i - 1);
end;

initialization
finalization
  GClassPath.Free;
  GBootPath.Free;
  GJavaVM.Free;
end.
