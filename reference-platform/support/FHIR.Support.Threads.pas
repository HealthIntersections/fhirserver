unit FHIR.Support.Threads;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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


interface

{$OVERFLOWCHECKS OFF}

uses
  {$IFDEF MACOS} FHIR.Support.Osx, {$ELSE} Windows,{$ENDIF}
  SysUtils, SyncObjs, Classes, Generics.Collections,
  FHIR.Support.Objects, FHIR.Support.Generics;

const
  NO_THREAD = 0;

type
  TThreadID = Cardinal;
  TThreadHandle = THandle;


Procedure ThreadSleep(iTime : Cardinal); Overload;
Function ThreadID : TThreadID; Overload;
{$IFDEF MSWINDOWS}
Function ThreadHandle : TThreadHandle; Overload;
{$ENDIF}
Procedure ThreadYield; Overload;
Procedure ThreadBreakpoint; Overload;

procedure SetThreadName(name : String);
function GetThreadName(id : integer) : String;

type
  TCriticalSectionProcedure = reference to procedure;

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


    procedure exec(proc : TCriticalSectionProcedure); overload;
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
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TFslThread;

      Procedure Open;
      Procedure Close;
      Procedure Stop; Virtual;

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
  TBackgroundTaskStatus = (btsWaiting, btsProcessing, btsCancelling, btsReady, btsClosed);

  TBackgroundTaskThread = class(TThread)
  private
    FEngine : TBackgroundTaskEngine;
  protected
    procedure Execute; override;
  public
    Constructor Create(engine : TBackgroundTaskEngine); // no link
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
    procedure break;
    procedure stop;
    procedure threadProc;
    procedure doExec(pck : TBackgroundTaskPackage);
    procedure setStatus(v : TBackgroundTaskStatus);
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

    procedure primaryThreadCheck;
  end;

var
  GBackgroundTasks : TBackgroundTaskManager;

implementation

var
  GHaveCritSect : Boolean = False;
  GCritSct: TRTLCriticalSection;
  GThreadManager : TDictionary<cardinal,String>;
  GQPFrequency : Int64;

  GFirst: TFslLock = NIL;
  GCount: Integer = 0;
  GTotal: Integer = 0;

procedure InitUnit;
begin
  QueryPerformanceFrequency(GQPFrequency);
  GQPFrequency := GQPFrequency div 1000; // in milliseconds
  InitializeCriticalSection(GCritSct);
  GHaveCritSect := true;
  GBackgroundTasks := TBackgroundTaskManager.Create;
  GThreadManager := TDictionary<cardinal,String>.create;
end;

procedure DoneUnit;
begin
  GThreadManager.Free;
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


procedure TFslLock.MarkEntered;
begin
  assert((FLockThread = NO_THREAD) or (FLockThread = GetCurrentThreadId),
     'Thread '+inttostr(GetCurrentThreadId)+' entering critical section '+inttohex(integer(Self), 8)+'/'+inttohex(integer(@FCritSect), 8)+' owned '+inttostr(FEntryCount)+' times by '+inttostr(FLockThread));
  if FLockThread = GetCurrentThreadid then
    inc(FEntryCount)
  else
  begin
    FLockThread := GetCurrentThreadId;
    FEntryCount := 1;
    inc(FUseCount);
    QueryPerformanceCounter(FCurrLockTime);
  end;
  SetLength(FLockName, FEntryCount);
end;

procedure TFslLock.MarkLeft;
var
  LEndTime: Int64;
begin
  assert(FLockThread = GetCurrentThreadID);
  dec(FEntryCount);
  SetLength(FLockName, FEntryCount);
  if FEntryCount = 0 then
    begin
    FLockThread := NO_THREAD;
    QueryPerformanceCounter(LEndTime);
    FTimeLocked := FTimeLocked + (LEndTime - FCurrLockTime);
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
    QueryPerformanceCounter(LStartTime);
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

procedure TFslLock.exec(proc: TCriticalSectionProcedure);
begin
  lock;
  try
    proc;
  finally
    unlock;
  end;
end;

procedure TFslLock.Leave;
begin
  UnLock;
end;

class function TFslLock.CurrentCount: Integer;
begin
  Result := GCount;
end;

function ms(count : int64) : integer;
begin
  result := count div GQPFrequency;
end;

function TFslLock.DebugSummary: String;
var
  i : integer;
begin
  Result := IntToStr(FOwnID)+' "'+FCategory+'" "'+FName+'" '+IntToStr(FDelayCount)+' '+
     IntToStr(FUseCount)+' ' +IntToStr(ms(FCurrLockTime))+' '+IntToStr(ms(FTimeLocked))+' '+IntToStr(ms(FDelayTime))+' '+
     IntToStr(FEntryCount)+' '+IntToStr(FLockThread)+' ';
  for i := 0 to High(FLockName) do
    result := result + '/' + FLockName[i];
end;

Function CriticalSectionChecksPass(Var sMessage : String) : Boolean;
var
  oCrit : TFslLock;
  LCheckTime: Int64;
Begin
  result := true;
  QueryPerformanceCounter(LCheckTime);
  LCheckTime := LCheckTime - (30 * GQPFrequency * 1000);
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
  aTime : Int64;
Begin
  QueryPerformanceCounter(aTime);
  Result := IntToStr(TFslLock.CurrentCount) + ' Locked Critical Sections (@'+InttoStr(ms(aTime))+')'+#13#10;
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
    Constructor Create(thread : TFslThread);
  end;

Constructor TFslThread.Create;
Begin
  Inherited;

  FInternal := nil;
End;


Destructor TFslThread.Destroy;
Begin
  Inherited;
End;


Function TFslThread.Link: TFslThread;
Begin
  Result := TFslThread(Inherited Link);
End;


Procedure TFslThread.Execute;
Begin
  If Assigned(FDelegate) Then
    FDelegate;
End;


Procedure TFslThread.Interrupt;
Begin
End;


Function TFslThread.Running: Boolean;
Begin
  Result := True;
End;



Procedure TFslThread.Open;
Begin
  If FActive Then
    RaiseError('Open', 'Thread is already active.');

  FActive := True;

  System.IsMultiThread := True;

  FInternal := TInternalThread.create(self);
End;


Procedure TFslThread.Close;
Begin
  FInternal.Terminate;
End;


Procedure TFslThread.Kill;
Begin
  FInternal.Terminate;
  {$IFDEF MSWINDOWS}
  TerminateThread(FInternal.Handle, 0);
  {$ENDIF}
  FInternal.Free;
  FInternal := nil;

  FActive := False;
End;


Procedure TFslThread.Stop;
Begin
  FActive := False;

  FInternal.Terminate;
End;


Procedure TFslThread.Wait;
Begin
  FInternal.WaitFor;
End;


function TFslThread.WaitTimeout(iTimeout: Cardinal): Boolean;
begin
  result := FInternal.WaitFor > 0;// todo
end;

Procedure TFslThread.ExecuteYield(Const iTimeout: Cardinal);
Begin
  ThreadSleep(iTimeout);
End;


Function TFslThread.Active : Boolean;
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
        try
          GBackgroundTasks.log('exec for '+name);
          doExec(pck);
        finally
          pck.Free;
        end;
      end;
      sleep(50);
    end;
  finally
    GBackgroundTasks.log('close task '+name);
    setStatus(btsClosed);
  end;
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
  inherited Create;
end;

procedure TBackgroundTaskThread.execute;
begin
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
var
  e : TBackgroundTaskEngine;
begin
  for e in FEngines do
    e.stop;
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

procedure TBackgroundTaskManager.killTask(id: integer);
begin
  log('kill '+FEngines[id].name);
  FEngines[id].break;
end;

procedure TBackgroundTaskManager.log(s : String);
begin
//  writeln(DescribePeriod(now - FStart)+' '+IntToHex(GetCurrentThreadId)+' '+s);
end;

procedure TBackgroundTaskManager.primaryThreadCheck;
var
  e : TBackgroundTaskEngine;
  resp : TBackgroundTaskPackage;
begin
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
end;

procedure TBackgroundTaskManager.queueTask(id: integer; package: TBackgroundTaskPackage);
begin
  log('queue task for  '+FEngines[id].name);
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


{$IFDEF MSWINDOWS}
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


procedure SetThreadName(name : String);
begin
  if not GHaveCritSect then
    InitUnit;

  EnterCriticalSection(GCritSct);
  try
    if name = '' then
      GThreadManager.Remove(GetCurrentThreadId)
    else
      GThreadManager.AddOrSetValue(GetCurrentThreadId, name);
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

function GetThreadName(id : integer) : String;
begin
  EnterCriticalSection(GCritSct);
  try
    if not GThreadManager.TryGetValue(id, result) then
      result := 'n/a';
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;



Initialization
  InitUnit;
Finalization
  DoneUnit;
end.
