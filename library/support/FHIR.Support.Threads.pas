unit FHIR.Support.Threads;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

interface

{$OVERFLOWCHECKS OFF}

uses
  {$IFDEF OSX} FHIR.Support.Osx, {$ENDIF}
  {$IFDEF WINDOWS} Windows, {$IFDEF FPC} JwaTlHelp32, {$ELSE} TlHelp32, {$ENDIF}  {$ENDIF}
  SysUtils, SyncObjs, Classes, Generics.Collections, IdThread,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Fpc;

const
  NO_THREAD = TThreadID(0);

type
  TThreadHandle = THandle;

Procedure ThreadSleep(iTime : Cardinal); Overload;
Function ThreadID : TThreadID; Overload;
{$IFDEF WINDOWS}
Function ThreadHandle : TThreadHandle; Overload;
{$ENDIF}
Procedure ThreadYield; Overload;
Procedure ThreadBreakpoint; Overload;

procedure SetThreadName(name : AnsiString);
procedure SetThreadStatus(status : AnsiString);
function GetThreadInfo : AnsiString;
function GetThreadReport : AnsiString;
function GetThreadCount : Integer;
procedure closeThread;

type
  {$IFNDEF FPC}
  TCriticalSectionProcedure = reference to procedure;
  {$ENDIF}

  TFslLock = class(TObject)
  Private
    FCritSect: TRTLCriticalSection;

    // Pointers in the linked list of critical sections. Link list maintained
    // so as that we can track and report status of each TFslLock
    // instance in the system
    FNext, FPrev: TFslLock;

    FOwnID: Integer;                 // unique serial number assigned to all critical sections
    FCategory: String;                // category in the lock list
    FName: String;                   // Name of the critical section object
    FLockName: Array of String;      // Name of the current Lock (first one to grab)
    FDelayCount: Integer;            // Number of times there has been a failed attempt to lock a critical section
    FUseCount: Integer;              // The amount of times there has been a succesful attempt to lock a critical section
    FCurrLockTime: Int64;            // Time which the owning thread obtained the lock for the thread
    FTimeLocked: Int64;              // Total length of time which the critical section has been locked for
    FDelayTime: Int64;               // Total length of time that threads have been waiting to obtain a lock after a failed attempt
    FEntryCount: Integer;            // Amount of times the thread owning the critical section has called Lock without calling UnLock. Used for recursion
    FLockThread : TThreadID;

    procedure MarkEntered;
    procedure MarkLeft;
    Function DebugSummary : String;
  Public
    constructor Create; Overload;
    constructor Create(AName: String); Overload;
    destructor Destroy; Override;

    // core functionality
    procedure Lock; Overload;
    procedure Lock(const Name: String); Overload;
    procedure Unlock;
    procedure Enter; Overload;
    procedure Enter(const AName: String); Overload;
    procedure Leave;
    function Trylock: Boolean;
    function LockedToMe: Boolean; // mainly for assertion support
    procedure changeName(aName : String);


    {$IFNDEF FPC}
    procedure exec(proc : TCriticalSectionProcedure); overload;
    {$ENDIF}

    // debugging support
    property Category: String Read FCategory Write FCategory;
    class function CurrentCount: Integer;
    // use with caution - not thread safe
    property OwnID: Integer Read FOwnID;
    property Name: String Read FName;
//    property LockName: String Read FLockName;
    property DelayCount: Integer Read FDelayCount;
    property UseCount: Integer Read FUseCount;
    property CurrLockTime: Int64 Read FCurrLockTime;
    property TimeLocked: Int64 Read FTimeLocked;
    property DelayTime: Int64 Read FDelayTime;
    property EntryCount: Integer Read FEntryCount;
    property LockThread: TThreadID Read FLockThread;
  end;

Function CriticalSectionChecksPass(Var sMessage : String) : Boolean;

function DumpLocks : String;

Type
  TFslThreadHandle = TThreadHandle;
  TFslThreadID = TThreadID;

  TFslThreadDelegate = Procedure Of Object;

  { TFslThread }

  TFslThread = Class (TFslObject)
    Private
      FInternal : TThread; // Handle to the Windows thread.
      FID : TFslThreadID;         // Unique ID of the Windows thread.
      FActive : Boolean;          // Run thread has finished.
      FDelegate : TFslThreadDelegate;

    Protected
      Procedure Execute; Virtual;
      Procedure Interrupt; Virtual;
      Function Running : Boolean; Virtual;

      Procedure ExecuteYield(Const iTimeout : Cardinal);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslThread;

      Procedure Open;
      Procedure Close;
      Procedure CloseOut;
      Procedure Stop; Virtual;

      function Terminated : boolean;

      Procedure Wait;
      Function WaitTimeout(iTimeout : Cardinal) : Boolean;

      Procedure Kill;

      Function Active : Boolean;

      Property ID : TFslThreadID Read FID Write FID;
      Property Delegate : TFslThreadDelegate Read FDelegate Write FDelegate;
//    Property Processor : Cardinal Write SetProcessor; // see comments in SetProcessor
  End;

  TFslThreadClass = Class Of TFslThread;

  TBackgroundTaskPackage = TFslObject;
  TBackgroundTaskEngine = class;

  TBackgroundTaskEvent = procedure (id : integer; response : TBackgroundTaskPackage) of object;
  TBackgroundTaskStatus = (btsWaiting, btsProcessing, btsCancelling, btsReady, btsClosed,  btsWaitingForUIResponse, btwRespondingOnUI, btsUIResponded);

  TBackgroundTaskThread = class(TThread)
  private
    FEngine : TBackgroundTaskEngine;
  protected
    procedure Execute; override;
  public
    constructor Create(engine : TBackgroundTaskEngine); // no link
  end;

  TBackgroundTaskUIRequest = class (TFslObject)
  private
  public
    function link : TBackgroundTaskUIRequest; overload;
  end;

  TBackgroundTaskUIResponse = class (TFslObject)
  private
  public
    function link : TBackgroundTaskUIResponse; overload;
  end;

  TBackgroundTaskEngine = class abstract (TFslObject)
  private
    FRequest: TBackgroundTaskPackage;
    FResponse: TBackgroundTaskPackage;
    FOnNotify : TBackgroundTaskEvent;
    procedure SetRequest(const Value: TBackgroundTaskPackage);
    procedure SetResponse(const Value: TBackgroundTaskPackage);
  private
    { private thread management section }
    FId : integer;
    FThread : TBackgroundTaskThread; // owned...
    FWantBreak : boolean;
    FWantStop : boolean;
    FWaiting : TBackgroundTaskPackage;
    FStatus : TBackgroundTaskStatus;
    FUIRequest : TBackgroundTaskUIRequest;
    FUIResponse : TBackgroundTaskUIResponse;
    procedure break;
    procedure stop;
    procedure threadProc;
    procedure doExec(pck : TBackgroundTaskPackage);
    procedure setStatus(v : TBackgroundTaskStatus);
  protected
    FUIException : String;
    FUIExceptionClass : ExceptClass;
  public
    constructor Create(notify : TBackgroundTaskEvent);
    destructor Destroy; override;

    property OnNotify : TBackgroundTaskEvent read FOnNotify write FOnNotify;
    function name : String; virtual; abstract;

    // properties of interest to subclasses:
    // execute is called. There is a request, create a response, calling
    // progress regularly (which allows the task to get killed
    property request : TBackgroundTaskPackage read FRequest write SetRequest;
    property response : TBackgroundTaskPackage read FResponse write SetResponse;
    procedure execute; virtual; abstract;
    procedure progress(state : String; pct : integer); // -1 for no pct. may throw EAbort

    // ask for an interaction with the user, passing request and getting response (or an exception)
    // a task that calls this must override performUIInteraction
    procedure uiInteraction(request : TBackgroundTaskUIRequest; response : TBackgroundTaskUIResponse);

    // override this if the engine will call uiInteraction. It will happen on the main UI thread so is blocking for that thread
    procedure performUIInteraction(request : TBackgroundTaskUIRequest; response : TBackgroundTaskUIResponse); virtual;
  end;

  TBackgroundTaskManager = class (TFslObject)
  private
    FStart : TDateTime;
    FLock : TFslLock;
    FEngines : TFslList<TBackgroundTaskEngine>;
    procedure log(s : String);
  public
    constructor Create; override;
    destructor Destroy; override;

    function registerTaskEngine(engine : TBackgroundTaskEngine) : integer;
    procedure queueTask(id : integer; package : TBackgroundTaskPackage); // where id was return value from registerTaskEngine
    procedure killTask(id : integer);

    procedure stopAll; // shut down preparation

    procedure primaryThreadCheck;
  end;

var
  GBackgroundTasks : TBackgroundTaskManager;

implementation

var
  GHaveCritSect : Boolean = False;
  GCritSct: TRTLCriticalSection;

  GFirst: TFslLock = NIL;
  GCount: Integer = 0;
  GTotal: Integer = 0;

{$IFDEF WINDOWS}

{ Thread tracking is delegated to a .dll to ensure thoroughness (because of DLL Main) }

procedure THThreadName(name : PAnsiChar); stdcall; external 'threadtracker.dll';
procedure THThreadStatus(status : PAnsiChar); stdcall; external 'threadtracker.dll';
function THThreadInfo : PAnsiChar; stdcall; external 'threadtracker.dll';
function THThreadCount : Cardinal; stdcall; external 'threadtracker.dll';
function THGetReport : PAnsiChar; stdcall; external 'threadtracker.dll';
procedure THFreeMem(rep : PAnsiChar); stdcall; external 'threadtracker.dll';

procedure SetThreadName(name : AnsiString);
begin
  THThreadName(pAnsichar(name));
end;

procedure SetThreadStatus(status : AnsiString);
begin
  THThreadStatus(pAnsichar(status));
end;

function GetThreadCount : Integer;
begin
  result := THThreadCount;
end;

function GetThreadInfo : AnsiString;
var
  p : PAnsiChar;
begin
  p := THThreadInfo;
  try
    SetLength(result, length(p));
    Move(p^, result[1], length(p));
  finally
    THFreeMem(p);
  end;
end;

function GetThreadReport : AnsiString;
var
  p : PAnsiChar;
begin
  p := THGetReport;
  try
    SetLength(result, length(p));
    Move(p^, result[1], length(p));
  finally
    THFreeMem(p);
  end;
end;

procedure CloseThread;
begin
  SetThreadStatus('closed');
end;

{$ELSE}

var
  GThreadList : TList;

type
  TTheadRecord = record
    id : TThreadID;
    startTick : UInt64;
    name : AnsiString;
    state : AnsiString;
    stateTick : UInt64;
  end;
  PTheadRecord = ^TTheadRecord;

procedure closeThread;
var
  id : TThreadID;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GCritSct);
  try
    for i := GThreadList.Count - 1 downto 0 do
    begin
      p := GThreadList[i];
      if (p.id = id) then
      begin
        Dispose(p);
        GThreadList.Delete(i);
      end;
    end;
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

procedure SetThreadName(name : AnsiString);
var
  id : TThreadID;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GCritSct);
  try
    for i := GThreadList.Count - 1 downto 0 do
    begin
      p := GThreadList[i];
      if (p.id = id) then
      begin
        p.name := name;
        exit;
      end;
    end;
    new(p);
    p.startTick := GetTickCount64;
    p.id := id;
    p.name := name;
    GThreadList.Add(p);
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

procedure SetThreadStatus(status : AnsiString);
var
  id : TThreadID;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GCritSct);
  try
    for i := GThreadList.Count - 1 downto 0 do
    begin
      p := GThreadList[i];
      if (p.id = id) then
      begin
        p.state := status;
        p.stateTick := GetTickCount64;
        exit;
      end;
    end;
    new(p);
    p.startTick := GetTickCount64;
    p.id := id;
    p.name := 'Unknown';
    p.state := status;
    p.stateTick := GetTickCount64;
    GThreadList.Add(p);
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

function age(tick : UInt64) : AnsiString;
var
  duration : UInt64;
begin
  duration := GetTickCount64 - tick;
  if duration < 2000 then
    result := inttostr(duration)+'ms'
  else
  begin
    duration := duration div 1000;
    if duration < 1000 then
      result := inttostr(duration)+'s'
    else
    begin
      duration := duration div 60;
      if duration < 120 then
        result := inttostr(duration)+'min'
      else
      begin
        duration := duration div 60;
        if duration < 48 then
          result := inttostr(duration)+'hr'
        else
        begin
          duration := duration div 24;
          result := inttostr(duration)+'d'
        end;
      end;
    end;
  end;
end;

function info(p : PTheadRecord; id : boolean) : AnsiString;
begin
  if (id) then
    result := inttohex(NativeUInt(p.id), 8)+': '
  else
    result := '';
  result := result + p.name;
  if p.state <> '' then
  begin
    result := result + ' = '+p.state;
    result := result + ' (born '+age(p.startTick)+', last seen '+age(p.stateTick)+')';
  end
  else
    result := result + ' (born '+age(p.startTick)+')';
end;

function GetThreadInfo : AnsiString;
var
  id : TThreadId;
  i : integer;
  p : PTheadRecord;
  s : AnsiString;
begin
  s := 'Unknown thread';
  id := GetCurrentThreadId;
  EnterCriticalSection(GCritSct);
  try
    for i := GThreadList.Count - 1 downto 0 do
    begin
      p := GThreadList[i];
      if (p.id = id) then
      begin
        s := info(p, false);
        break;
      end;
    end;
  finally
    LeaveCriticalSection(GCritSct);
  end;
  result := s;
end;

function GetThreadCount : Integer;
begin
  EnterCriticalSection(GCritSct);
  try
    result := GThreadList.Count;
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

function GetThreadReport : AnsiString;
var
  i : integer;
  s : AnsiString;
  p : PTheadRecord;
begin
  s := '';
  EnterCriticalSection(GCritSct);
  try
    for i := 0 to GThreadList.Count - 1 do
    begin
      p := GThreadList[i];
      if (s <> '') then
        s := s + '|';
      s := s + info(p, true);
    end;
  finally
    LeaveCriticalSection(GCritSct);
  end;
  result := s;
end;

{$ENDIF}

procedure CloseThreadInternal(name : AnsiString);
begin
  closeThread;
end;

procedure InitUnit;
begin
  InitializeCriticalSection(GCritSct);
  GHaveCritSect := true;
  GBackgroundTasks := TBackgroundTaskManager.Create;
  {$IFNDEF WINDOWS}
  GThreadList := TList.Create;
  {$ENDIF}
  IdThread.fsThreadName := SetThreadName;
  IdThread.fsThreadStatus := SetThreadStatus;
  IdThread.fsThreadClose := CloseThreadInternal;
end;

procedure DoneUnit;
{$IFNDEF WINDOWS}
var
  i : integer;
  p : PTheadRecord;
{$ENDIF}
begin
  IdThread.fsThreadName := nil;
  IdThread.fsThreadStatus := nil;
  IdThread.fsThreadClose := nil;
  {$IFNDEF WINDOWS}
  for i := GThreadList.Count - 1 downto 0 do
  begin
    p := GThreadList[i];
    Dispose(p);
    GThreadList.Delete(i);
  end;
  GThreadList.Free;
  {$ENDIF}
  GBackgroundTasks.Free;
  DeleteCriticalSection(GCritSct);
end;

{ TFslLock }

constructor TFslLock.Create;
begin
  inherited Create;
  FCategory := '';
  FName := ClassName;
  SetLength(FLockName, 0);
  FDelayCount := 0;
  FUseCount := 0;
  FCurrLockTime := 0;
  FTimeLocked := 0;
  FDelayTime := 0;
  FLockThread := NO_THREAD;
  FEntryCount := 0;
  if not GHaveCritSect then
    InitUnit;
  InitializeCriticalSection(FCritSect);
  EnterCriticalSection(GCritSct);
  try
    inc(GCount);
    inc(GTotal);
    FOwnID := GTotal;
    if GFirst = NIL then
      begin
      FNext := NIL;
      end
    else
      begin
      FNext := GFirst;
      FNext.FPrev := self;
      end;
    FPrev := NIL;
    GFirst := self;
  finally
    LeaveCriticalSection(GCritSct);
    end;
end;

procedure TFslLock.changeName(aName: String);
begin
  if LockedToMe then
    FName := aName;
end;

constructor TFslLock.Create(AName: String);
begin
  Create;
  FName := AName;
end;

destructor TFslLock.Destroy;
begin
  EnterCriticalSection(GCritSct);
  try
    dec(GCount);
    if FPrev = NIL then
      GFirst := FNext
    else
      FPrev.FNext := FNext;
    if FNext <> NIL then
      FNext.FPrev := FPrev;
  finally
    LeaveCriticalSection(GCritSct);
    end;
  DeleteCriticalSection(FCritSect);
  inherited;
end;


function threadToString(id : TThreadId) : String;
begin
  {$IFDEF OSX}
  result := inttostr(UInt64(id));
  {$ELSE}
  result := inttostr(id);
  {$ENDIF}
end;

procedure TFslLock.MarkEntered;
begin
  assert((FLockThread = NO_THREAD) or (FLockThread = GetCurrentThreadId),
     'Thread '+threadToString(GetCurrentThreadId)+' entering critical section '+inttohex(integer(Self), 8)+'/'+inttohex(integer(@FCritSect), 8)+' owned '+inttostr(FEntryCount)+' times by '+threadToString(FLockThread));
  if FLockThread = GetCurrentThreadid then
    inc(FEntryCount)
  else
  begin
    FLockThread := GetCurrentThreadId;
    FEntryCount := 1;
    inc(FUseCount);
    FCurrLockTime := GetTickCount64;
  end;
  SetLength(FLockName, FEntryCount);
end;

procedure TFslLock.MarkLeft;
begin
  assert(FLockThread = GetCurrentThreadID);
  dec(FEntryCount);
  SetLength(FLockName, FEntryCount);
  if FEntryCount = 0 then
    begin
    FLockThread := NO_THREAD;
    FTimeLocked := FTimeLocked + (GetTickCount64 - FCurrLockTime);
    FCurrLockTime := 0;
    end;
end;

function TFslLock.LockedToMe: Boolean;
begin
  Result := FLockThread = GetCurrentThreadId;
end;

procedure TFslLock.Lock;
var
  LStartTime: Int64;
begin
  // the current time is set by a successful trylock.
  if not TryLock then
    begin
    LStartTime := GetTickCount64;
    EnterCriticalSection(FCritSect);
    MarkEntered;
    FDelayTime := FDelayTime + (FCurrLockTime - LStartTime);
    inc(FDelayCount);
    end;
end;

procedure TFslLock.Lock(const Name: String);
begin
  Lock;
  FLockName[FEntryCount - 1] := Name;
end;

function TFslLock.TryLock: Boolean;
begin
  Result := TryEnterCriticalSection(FCritSect);
  if Result then
    begin
    MarkEntered;
    end;
end;

procedure TFslLock.Unlock;
begin
  If not LockedToMe then
  begin
    ChangeName('not locked to this thread');
  end
  else
  begin
    MarkLeft;
    LeaveCriticalSection(FCritSect);
  end;
end;

procedure TFslLock.Enter;
begin
  Lock;
end;

procedure TFslLock.Enter(const AName: String);
begin
  Lock(AName);
end;

{$IFNDEF FPC}
procedure TFslLock.exec(proc: TCriticalSectionProcedure);
begin
  lock;
  try
    proc;
  finally
    unlock;
  end;
end;
{$ENDIF}

procedure TFslLock.Leave;
begin
  UnLock;
end;

class function TFslLock.CurrentCount: Integer;
begin
  Result := GCount;
end;

function TFslLock.DebugSummary: String;
var
  i : integer;
begin
  Result := IntToStr(FOwnID)+' "'+FCategory+'" "'+FName+'" '+IntToStr(FDelayCount)+' '+
     IntToStr(FUseCount)+' ' +IntToStr(FCurrLockTime)+' '+IntToStr(FTimeLocked)+' '+IntToStr(FDelayTime)+' '+
     IntToStr(FEntryCount)+' '+threadToString(FLockThread)+' ';
  for i := 0 to High(FLockName) do
    result := result + '/' + FLockName[i];
end;

Function CriticalSectionChecksPass(Var sMessage : String) : Boolean;
var
  oCrit : TFslLock;
  LCheckTime: Int64;
Begin
  result := true;
  LCheckTime := GetTickCount64 - (30 * 1000);
  EnterCriticalSection(GCritSct);
  Try
    oCrit := GFirst;
    While result And (oCrit <> nil) Do
    Begin
      if oCrit.FEntryCount > 0 Then
      Begin
        if LCheckTime > oCrit.FCurrLockTime Then
        Begin
          sMessage := 'Critical Section has been locked more than 30 seconds ('+oCrit.DebugSummary+')';
          result := False;
        End;
      End;
      oCrit := oCrit.FNext;
    End;
  Finally
    LeaveCriticalSection(GCritSct);
  End;
End;

function DumpLocks : String;
var
  oCrit : TFslLock;
Begin
  Result := IntToStr(TFslLock.CurrentCount) + ' Locked Critical Sections (@'+InttoStr(GetTickCount64)+')'+#13#10;
  oCrit := GFirst;
  While oCrit <> nil Do
  Begin
    if oCrit.EntryCount > 0 Then
      Result := Result + oCrit.DebugSummary + #13#10;
    oCrit := oCrit.FNext;
  End;
End;


type
  TInternalThread = class (TThread)
  private
    FOwner : TFslThread;
  protected
    procedure Execute; override;
  public
    constructor Create(thread : TFslThread);
  end;

constructor TFslThread.Create;
Begin
  Inherited;

  FInternal := nil;
End;


destructor TFslThread.Destroy;
Begin
  Inherited;
End;


function TFslThread.Link: TFslThread;
Begin
  Result := TFslThread(Inherited Link);
End;


procedure TFslThread.Execute;
Begin
  SetThreadName(ClassName);
  If Assigned(FDelegate) Then
    FDelegate;
End;


procedure TFslThread.Interrupt;
Begin
End;


function TFslThread.Running: Boolean;
Begin
  Result := True;
End;


procedure TFslThread.Open;
Begin
  If FActive Then
    RaiseError('Open', 'Thread is already active.');

  FActive := True;

  System.IsMultiThread := True;

  FInternal := TInternalThread.create(self);
End;


procedure TFslThread.Close;
Begin
  FInternal.Terminate;
End;

procedure TFslThread.CloseOut;
begin
  Close;
  while Active do
    sleep(20);
  FInternal.Free;
  FInternal := nil;
end;


procedure TFslThread.Kill;
Begin
  FInternal.Terminate;
  {$IFDEF WINDOWS}
  TerminateThread(FInternal.Handle, 0);
  {$ENDIF}
  FInternal.Free;
  FInternal := nil;

  FActive := False;
End;


procedure TFslThread.Stop;
Begin
  FActive := False;

  FInternal.Terminate;
End;

function TFslThread.Terminated: boolean;
begin
  result := FInternal.CheckTerminated;
end;


procedure TFslThread.Wait;
Begin
  FInternal.WaitFor;
End;


function TFslThread.WaitTimeout(iTimeout: Cardinal): Boolean;
begin
  result := FInternal.WaitFor > 0;// todo
end;

procedure TFslThread.ExecuteYield(const iTimeout: Cardinal);
Begin
  ThreadSleep(iTimeout);
End;


function TFslThread.Active: Boolean;
Begin
  Result := FActive And Running;
End;

{ TInternalThread }

constructor TInternalThread.Create(thread: TFslThread);
begin
  FOwner := thread;
  inherited create(false);
end;

procedure TInternalThread.execute;
begin
  SetThreadName('TInternalThread');
  Try
    FOwner.Execute;
  Except
    // ignore any further exceptions
  End;
  FOwner.FActive := False;
  SetThreadName('');
end;

{ TBackgroundTaskEngine }

constructor TBackgroundTaskEngine.Create(notify : TBackgroundTaskEvent);
begin
  inherited create;
  FOnNotify := notify;
  FStatus := btsWaiting;
end;

destructor TBackgroundTaskEngine.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure TBackgroundTaskEngine.doExec(pck: TBackgroundTaskPackage);
begin
  try
    SetStatus(btsProcessing);
    request := pck.Link;
    response := nil;
    GBackgroundTasks.log('Task '+name+' go');
    execute;
    GBackgroundTasks.log('Task '+name+' done');
    if response <> nil then
      setStatus(btsReady)
    else
      setStatus(btsWaiting);
  except
    on e : Exception do
    begin
      GBackgroundTasks.log('Task '+name+' error: '+e.Message);
      SetStatus(btsWaiting);
      request := nil;
      response := nil;
    end;
  end;
end;

procedure TBackgroundTaskEngine.performUIInteraction(request: TBackgroundTaskUIRequest; response: TBackgroundTaskUIResponse);
begin
  raise Exception.Create('The method '+className+'.performUIInteraction needs to be overridden');
end;

procedure TBackgroundTaskEngine.progress(state: String; pct: integer);
begin
  if FWantStop or FWantBreak then
    abort;
end;

procedure TBackgroundTaskEngine.SetRequest(const Value: TBackgroundTaskPackage);
begin
  FRequest.Free;
  FRequest := Value;
end;

procedure TBackgroundTaskEngine.SetResponse(const Value: TBackgroundTaskPackage);
begin
  FResponse.Free;
  FResponse := Value;
end;

procedure TBackgroundTaskEngine.setStatus(v: TBackgroundTaskStatus);
begin
  GBackgroundTasks.FLock.Lock;
  try
    FStatus := v;
  finally
    GBackgroundTasks.FLock.UnLock;
  end;
end;

procedure TBackgroundTaskEngine.stop;
begin
  FWantStop := true;
  setStatus(btsCancelling);
end;

procedure TBackgroundTaskEngine.threadProc;
var
  pck : TBackgroundTaskPackage;
begin
  SetThreadName(name);
  try
    SetThreadStatus('Sleeping');
    while not FWantStop do
    begin
      pck := nil;
      GBackgroundTasks.FLock.Lock;
      try
        if FWaiting <> nil then
        begin
          pck := FWaiting;
          FWaiting := nil;
          GBackgroundTasks.log('Found request for '+name);
        end;
      finally
        GBackgroundTasks.FLock.Unlock;
      end;
      if (pck <> nil) then
      begin
        SetThreadStatus('Working');
        try
          GBackgroundTasks.log('exec for '+name);
          doExec(pck);
        finally
          pck.Free;
        end;
        SetThreadStatus('Sleeping');
      end;
      sleep(50);
    end;
  finally
    GBackgroundTasks.log('close task '+name);
    setStatus(btsClosed);
  end;
  SetThreadStatus('Done');
end;

procedure TBackgroundTaskEngine.uiInteraction(request: TBackgroundTaskUIRequest; response: TBackgroundTaskUIResponse);
begin
  FRequest := request;
  FResponse := response;
  FStatus := btsWaitingForUIResponse;
  while FStatus <> btsUIResponded do
    sleep(50);
  if FUIExceptionClass <> nil then
    raise FUIExceptionClass.Create(FUIException);
end;

procedure TBackgroundTaskEngine.break;
begin
  FWantBreak := true;
  setStatus(btsCancelling);
end;

{ TBackgroundTaskThread }

constructor TBackgroundTaskThread.Create(engine: TBackgroundTaskEngine);
begin
  FEngine := engine;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TBackgroundTaskThread.execute;
begin
  SetThreadName('TBackgroundTaskThread');
  try
    FEngine.threadProc;
  except
  end;
end;



{ TBackgroundTaskManager }

constructor TBackgroundTaskManager.Create;
begin
  inherited;
  FLock := TFslLock.create('BackgroundTaskManager');
  FEngines := TFslList<TBackgroundTaskEngine>.create;
  FStart := now;
end;

destructor TBackgroundTaskManager.Destroy;
begin
  StopAll;
  sleep(200);
  FEngines.Free;
  FLock.Free;
  inherited;
end;

function TBackgroundTaskManager.registerTaskEngine(engine: TBackgroundTaskEngine): integer;
begin
  log('register '+engine.name);
  FLock.Lock;
  try
    result := FEngines.Count;
    engine.FId := result;
    FEngines.Add(engine);
  finally
    FLock.Unlock;
  end;
  engine.FThread := TBackgroundTaskThread.Create(engine);
end;

procedure TBackgroundTaskManager.stopAll;
var
  e : TBackgroundTaskEngine;
begin
  for e in FEngines do
    e.stop;
end;

procedure TBackgroundTaskManager.killTask(id: integer);
begin
  log('kill '+FEngines[id].name);
  FEngines[id].break;
end;

procedure TBackgroundTaskManager.log(s : String);
begin
//  AllocConsole;
//  writeln(DescribePeriod(now - FStart)+' '+IntToHex(GetCurrentThreadId)+' '+s);
end;

procedure TBackgroundTaskManager.primaryThreadCheck;
var
  e : TBackgroundTaskEngine;
  resp : TBackgroundTaskPackage;
  uReq : TBackgroundTaskUIRequest;
  uResp : TBackgroundTaskUIResponse;
begin
  // first round: are any tasks complete
  for e in FEngines do
  begin
    resp := nil;
    FLock.Lock;
    try
      if e.FStatus = btsReady then
      begin
        log('found response for '+e.name);
        e.FStatus := btsWaiting;
        resp := e.Response.Link;
        e.Response := nil;
      end;
    finally
      FLock.Unlock;
    end;
    if resp <> nil then
    begin
      try
        log('notify response for '+e.name);
        e.OnNotify(e.FId, resp);
        log('notified response for '+e.name);
      finally
        resp.Free;
      end;
      exit; // only one outcome per iteration - don't tie up the pimary thread
    end;
  end;
  // second round: any tasks want user interaction
  for e in FEngines do
  begin
    resp := nil;
    FLock.Lock;
    try
      if e.FStatus = btsWaitingForUIResponse then
      begin
        log('UI Response needed for '+e.name);
        e.FStatus := btsWaitingForUIResponse;
        uReq := e.FUIRequest.Link;
        uResp := e.FUIResponse.Link;
      end
      else
      begin
        uReq := nil;
        uResp := nil;
      end;
    finally
      FLock.Unlock;
    end;

    if uReq <> nil then
    begin
      try
        e.FUIExceptionClass := nil;
        e.FUIException := '';
        log('get UI response for '+e.name);
        try
          e.OnNotify(e.FId, resp);
        except
          on ex : Exception do
          begin
            e.FUIException := ex.Message;
            e.FUIExceptionClass := ExceptClass(ex.ClassType);
          end;
        end;
        FLock.Lock;
        try
          e.FStatus := btsUIResponded;
        finally
          FLock.Unlock;
        end;
        log('finished getting UI Response for '+e.name);
      finally
        uReq.Free;
        uResp.Free;
      end;
      exit; // only one outcome per iteration - don't tie up the pimary thread
    end;
  end;

end;

procedure TBackgroundTaskManager.queueTask(id: integer; package: TBackgroundTaskPackage);
begin
  log('queue task for '+inttostr(id)+' ('+FEngines[id].name+')');
  FLock.Lock;
  try
    FEngines[id].FWaiting.Free;
    FEngines[id].FWaiting := package;
  finally
    FLock.Unlock;
  end;
end;


Procedure ThreadSleep(iTime : Cardinal);
Begin
  Sleep(iTime);
End;

Function ThreadID : TThreadID;
Begin
  Result := GetCurrentThreadID;
End;


{$IFDEF WINDOWS}
Function ThreadHandle : TThreadHandle;
Begin
  Result := GetCurrentThread;
End;
{$ENDIF}



Procedure ThreadYield;
Begin
  ThreadSleep(0);
End;


Procedure ThreadBreakpoint;
Begin
  {$IFDEF WIN32}
  Try
    ASM
      int $03
    End;
  Except
    // on some poorly configured Windows systems int $03 can cause unhandled
    // exceptions with improperly installed Dr Watsons etc....
  End;
  {$ELSE}
  // todo: how to do this?
  {$ENDIF}
End;




{ TBackgroundTaskUIRequest }

function TBackgroundTaskUIRequest.link: TBackgroundTaskUIRequest;
begin
  result := TBackgroundTaskUIRequest(inherited link);
end;

{ TBackgroundTaskUIResponse }

function TBackgroundTaskUIResponse.link: TBackgroundTaskUIResponse;
begin
  result := TBackgroundTaskUIResponse(inherited link);
end;

Initialization
  InitUnit;
Finalization
  DoneUnit;
end.
