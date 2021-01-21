unit fsl_threads;

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
  {$IFDEF WINDOWS} Windows, {$IFDEF FPC} JwaTlHelp32, {$ELSE} TlHelp32, {$ENDIF}  {$ENDIF}
  SysUtils, SyncObjs, Classes, Generics.Collections, IdThread,
  fsl_base, fsl_utilities, fsl_fpc;

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

procedure SetThreadName(name : String);
procedure SetThreadStatus(status : String);
function GetThreadInfo : String;
function GetThreadReport : String;
function GetThreadCount : Integer;
function GetThreadNameStatus : String;
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
    FCategory: String;               // category in the lock list
    FName: String;                   // Name of the critical section object
    FLockName: Array of String;      // Name of the current Lock (first one to grab)
    FDelayCount: Integer;            // Number of times there has been a failed attempt to lock a critical section
    FUseCount: Integer;              // The amount of times there has been a succesful attempt to lock a critical section
    FCurrLockTime: UInt64;           // Time which the owning thread obtained the lock for the thread
    FTimeLocked: UInt64;             // Total length of time which the critical section has been locked for
    FDelayTime: UInt64;              // Total length of time that threads have been waiting to obtain a lock after a failed attempt
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
    property CurrLockTime: UInt64 Read FCurrLockTime;
    property TimeLocked: UInt64 Read FTimeLocked;
    property DelayTime: UInt64 Read FDelayTime;
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

  TFslThread = Class abstract (TFslObject)
  Private
    FAutoFree: boolean;
    FInternal : TThread;
    FRunning : Boolean;
    FTimePeriod: cardinal;

    procedure InternalExecute;
  Protected
    function ThreadName : String; Virtual;
    procedure Initialise; Virtual;
    Procedure Execute; Virtual; Abstract;
    procedure Finalise; Virtual;

    Procedure Sleep(Const iTimeout : Cardinal);

  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TFslThread;

    Procedure Start;
    Procedure Stop;
    procedure StopAndWait(ms : Cardinal);
    Procedure Wait;
    Procedure Kill;

    function Terminated : boolean;
    Property Running : Boolean read FRunning;

    Property AutoFree : boolean read FAutoFree write FAutoFree;
    Property TimePeriod : cardinal read FTimePeriod write FTimePeriod; // milliseconds. If this is >0, execute will called after TimePeriod delay until Stopped
  End;

  TFslThreadClass = Class Of TFslThread;

  TBackgroundTaskPackage = class;
  TBackgroundTaskEngine = class;

  TBackgroundTaskStatus = (btsWaiting, btsProcessing, btsCancelling, btsClosed, btsWaitingForUIResponse, btwRespondingOnUI, btsUIResponded);

  TBackgroundTaskThread = class(TThread)
  private
    FEngine : TBackgroundTaskEngine;
  protected
    procedure Execute; override;
  public
    constructor Create(engine : TBackgroundTaskEngine); // no link
  end;

  { TBackgroundTaskPackage }

  TBackgroundTaskPackage = class (TFslObject)
  public
    function link : TBackgroundTaskPackage; overload;
  end;

  { TBackgroundTaskRequestPackage }

  TBackgroundTaskRequestPackage = class (TBackgroundTaskPackage)
  public
    function link : TBackgroundTaskRequestPackage; overload;
  end;

  { TBackgroundTaskResponsePackage }

  TBackgroundTaskResponsePackage = class (TBackgroundTaskPackage)
  private
    FException : String;
    FExceptionClass : ExceptClass;
  protected
    function sizeInBytesV : cardinal; override;
  public
    function link : TBackgroundTaskResponsePackage; overload;
    property Exception : String read FException write FException;
    property ExceptionClass : ExceptClass read FExceptionClass write FExceptionClass;
  end;

  TWorkProgressEvent = procedure (sender : TObject; pct : integer; done : boolean; desc : String) of object;
  TBackgroundTaskEvent = procedure (id : integer; response : TBackgroundTaskResponsePackage) of object;

  { TBackgroundTaskPackagePair }

  TBackgroundTaskPackagePair = class (TFslObject)
  private
    FOnNotify: TBackgroundTaskEvent;
    FRequest: TBackgroundTaskRequestPackage;
    FResponse: TBackgroundTaskResponsePackage;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage);
    destructor Destroy; override;
    function link : TBackgroundTaskPackagePair; overload;
    property request : TBackgroundTaskRequestPackage read FRequest;
    property response : TBackgroundTaskResponsePackage read FResponse;
    property OnNotify : TBackgroundTaskEvent read FOnNotify write FOnNotify;
  end;

  { TBackgroundTaskUIRequest }

  TBackgroundTaskUIRequest = class (TBackgroundTaskPackage)
  public
    function link : TBackgroundTaskUIRequest; overload;
  end;

  TBackgroundTaskUIResponse = class (TBackgroundTaskPackage)
  private
  public
    function link : TBackgroundTaskUIResponse; overload;
  end;

  { TBackgroundTaskStatusInfo }

  TBackgroundTaskStatusInfo = class (TFslObject)
  private
    FName: String;
    FId: integer;
    FStatus: TBackgroundTaskStatus;
    FPct: integer;
    FMessage: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    function link : TBackgroundTaskStatusInfo; overload;

    property id : integer read FId write FId;
    property name : String read FName write FName;
    property status : TBackgroundTaskStatus read FStatus write FStatus;
    property message : String read FMessage write FMessage;
    property pct : integer read FPct write FPct;

    function StatusDisplay : string;
    function PctDisplay : String;
  end;

  { TBackgroundTaskEngine }

  TBackgroundTaskEngine = class abstract (TFslObject)
  private
    FOnNotify : TBackgroundTaskEvent;
  private
    { private thread management section }
    FId : integer;
    FThread : TBackgroundTaskThread; // owned...
    FWantBreak : boolean;
    FWantStop : boolean;
    FWaiting : TFslList<TBackgroundTaskPackagePair>;
    FDone : TFslList<TBackgroundTaskPackagePair>;
    FStatus : TBackgroundTaskStatus;
    FUIRequest : TBackgroundTaskUIRequest;
    FUIResponse : TBackgroundTaskUIResponse;
    FState : String;
    FPct : Integer;
    procedure break;
    procedure stop;
    procedure terminate;
    function stopped : boolean;
    procedure threadProc;
    procedure doExec(pck : TBackgroundTaskPackagePair);
    procedure setStatus(v : TBackgroundTaskStatus);
  protected
    FUIException : String;
    FUIExceptionClass : ExceptClass;

    function reportStatus : TBackgroundTaskStatusInfo;
  public
    constructor Create; overload; override;
    constructor Create(notify : TBackgroundTaskEvent); overload;
    destructor Destroy; override;

    property OnNotify : TBackgroundTaskEvent read FOnNotify write FOnNotify;
    function name : String; virtual; abstract;

    // properties of interest to subclasses:
    // execute is called. There is a request, create a response, calling
    // progress regularly (which allows the task to get killed
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); virtual; abstract;
    procedure progress(state : String; pct : integer); // -1 for no pct. may throw EAbort

    // ask for an interaction with the user, passing request and getting response (or an exception)
    // a task that calls this must override performUIInteraction
    procedure uiInteraction(request : TBackgroundTaskUIRequest; response : TBackgroundTaskUIResponse);

    // override this if the engine will call uiInteraction. It will happen on the main UI thread so is blocking for that thread
    procedure performUIInteraction(request : TBackgroundTaskUIRequest; response : TBackgroundTaskUIResponse); virtual;
  end;

  { TNullTaskEngine }

  TNullTaskEngine = class (TBackgroundTaskEngine)
  private
  public
    function name : String; override;
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
  end;

  { TBackgroundTaskManager }

  TBackgroundTaskManager = class (TFslObject)
  private
    FStart : TDateTime;
    FLock : TFslLock;
    FEngines : TFslList<TBackgroundTaskEngine>;
    procedure log(s : String);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function registerTaskEngine(engine : TBackgroundTaskEngine) : integer;

    // id is return value from registerTaskEngine
    procedure queueTask(id : integer; request : TBackgroundTaskRequestPackage); overload;
    procedure queueTask(id : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); overload;
    procedure queueTask(id : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage; onNotify : TBackgroundTaskEvent); overload;
    procedure killTask(id : integer);

    procedure start;
    procedure stopAll; // shut down preparation
    procedure wait(grace : cardinal);

    procedure primaryThreadCheck;
    function TasksAreWorking : boolean;

    procedure report(list : TFslList<TBackgroundTaskStatusInfo>); overload;
    function report(taskId : integer) : TBackgroundTaskStatusInfo; overload;
  end;

var
  GBackgroundTasks : TBackgroundTaskManager;

implementation

uses
  fsl_logging; // but don't use at start up

var
  GHaveCritSect : Boolean = False;
  GCritSct: TRTLCriticalSection;

  GFirst: TFslLock = NIL;
  GCount: Integer = 0;
  GTotal: Integer = 0;

var
  GThreadList : TList;

type
  TTheadRecord = record
    id : TThreadID;
    obj : TThread; // the tthread (if it's known)
    startTick : UInt64;
    name : String;
    state : String;
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

procedure SetThreadName(name : String);
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
        {$IFDEF FPC}
        TThread.NameThreadForDebugging(name, p.id);
        {$ENDIF}
        exit;
      end;
    end;
    new(p);
    p.startTick := GetTickCount64;
    p.id := id;
    p.name := name;
    {$IFDEF FPC}
    TThread.NameThreadForDebugging(name, p.id);
    {$ENDIF}

    GThreadList.Add(p);
  finally
    LeaveCriticalSection(GCritSct);
  end;
end;

procedure SetThreadStatus(status : String);
var
  id : TThreadID;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  if not GHaveCritSect then
    exit;
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

function age(tick : UInt64) : String;
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

function info(p : PTheadRecord; id : boolean) : String;
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

function GetThreadNameStatus : String;
var
  id : TThreadId;
  i : integer;
  p : PTheadRecord;
  s : String;
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
        s := p.name+':'+p.state;
        break;
      end;
    end;
  finally
    LeaveCriticalSection(GCritSct);
  end;
  result := s;
end;

function GetThreadInfo : String;
var
  id : TThreadId;
  i : integer;
  p : PTheadRecord;
  s : String;
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

function GetThreadReport : String;
var
  i : integer;
  s : String;
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

procedure CloseThreadInternal(name : String);
begin
  closeThread;
end;

procedure InitUnit;
begin
  InitializeCriticalSection(GCritSct);
  GHaveCritSect := true;
  GBackgroundTasks := TBackgroundTaskManager.Create;
  GThreadList := TList.Create;
  IdThread.fsThreadName := SetThreadName;
  IdThread.fsThreadStatus := SetThreadStatus;
  IdThread.fsThreadClose := CloseThreadInternal;
  GetThreadNameStatusDelegate := GetThreadNameStatus;
end;

procedure DoneUnit;
var
  i : integer;
  p : PTheadRecord;
begin
  GHaveCritSect := false;
  IdThread.fsThreadName := nil;
  IdThread.fsThreadStatus := nil;
  IdThread.fsThreadClose := nil;
  for i := GThreadList.Count - 1 downto 0 do
  begin
    p := GThreadList[i];
    Dispose(p);
    GThreadList.Delete(i);
  end;
  GThreadList.Free;
  GBackgroundTasks.Free;
  DeleteCriticalSection(GCritSct);
end;

{ TNullTaskEngine }

function TNullTaskEngine.name: String;
begin
  result := 'Idle Task';
end;

procedure TNullTaskEngine.execute(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
begin
  // nothing
end;

{ TBackgroundTaskPackagePair }

constructor TBackgroundTaskPackagePair.Create(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
begin
  inherited Create;
  FRequest := request;
  FResponse := response;
end;

destructor TBackgroundTaskPackagePair.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited Destroy;
end;

function TBackgroundTaskPackagePair.link: TBackgroundTaskPackagePair;
begin
  result := TBackgroundTaskPackagePair(inherited link);
end;

function TBackgroundTaskPackagePair.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FRequest.sizeInBytes);
  inc(result, FResponse.sizeInBytes);
end;

{ TBackgroundTaskResponsePackage }

function TBackgroundTaskResponsePackage.link: TBackgroundTaskResponsePackage;
begin
  result := TBackgroundTaskResponsePackage(inherited link);
end;

function TBackgroundTaskResponsePackage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FException.length * sizeof(char)) + 12);
end;

{ TBackgroundTaskRequestPackage }

function TBackgroundTaskRequestPackage.link: TBackgroundTaskRequestPackage;
begin
  result := TBackgroundTaskRequestPackage(inherited link);
end;

{ TBackgroundTaskPackage }

function TBackgroundTaskPackage.link: TBackgroundTaskPackage;
begin
  result := TBackgroundTaskPackage(inherited link);
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
  if GHaveCritSect then
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
    inc(FDelayTime, FCurrLockTime);
    dec(FDelayTime, LStartTime);
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
  if not FAutoFree then
    FInternal.Free;
  Inherited;
End;

procedure TFslThread.Initialise;
begin
 // nothing
end;

procedure TFslThread.Finalise;
begin
 // nothing
end;

procedure TFslThread.InternalExecute;
var
  et : UInt64;
begin
  SetThreadName(ThreadName);
  setThreadStatus('Initialising');
  Logging.log('Start thread '+threadName);
  initialise;
  try
    if FTimePeriod > 0 then
    begin
      while not Terminated do
      begin
        SetThreadStatus('Working');
        Execute;

        setThreadStatus('Sleeping');
        et := GetTickCount64 + TimePeriod;
        repeat
          sleep(50);
        until GetTickCount64 >= et;
      end;
    end
    else
    begin
      SetThreadStatus('Executing');
      Execute;
    end;
  except
    on e : Exception do
      Logging.log('Unhandled Exception in '+threadName+': '+e.message);
  end;
  try
    setThreadStatus('Finalising');
    finalise;
  except
    on e : Exception do
      Logging.log('Unhandled Exception closing '+threadName+': '+e.message);
  end;
  Logging.log('Finish '+threadName);
  SetThreadStatus('Done');
  closeThread;
end;

function TFslThread.Link: TFslThread;
Begin
  Result := TFslThread(Inherited Link);
End;

procedure TFslThread.Start;
Begin
  If FRunning Then
    RaiseError('Open', 'Thread is already running.');
  FRunning := True;
  FInternal := TInternalThread.create(self);
End;

procedure TFslThread.Stop;
Begin
  FInternal.Terminate;
End;

procedure TFslThread.StopAndWait;
begin
  if Running then
  begin
    Stop;
    while FRunning do
      sleep(20);
  end;
end;

procedure TFslThread.Kill;
Begin
  FInternal.Terminate;
  {$IFDEF WINDOWS}
  TerminateThread(FInternal.Handle, 0);
  {$ENDIF}
  FRunning := False;
  FInternal.Free;
  FInternal := nil;
End;

function TFslThread.Terminated: boolean;
begin
  if FInternal = nil then
    result := true
  else
    result := FInternal.CheckTerminated;
end;

function TFslThread.ThreadName: String;
begin
  result := ClassName;
end;

procedure TFslThread.Wait;
Begin
  FInternal.WaitFor;
End;

procedure TFslThread.Sleep(const iTimeout: Cardinal);
Begin
  ThreadSleep(iTimeout);
End;

{ TInternalThread }

constructor TInternalThread.Create(thread: TFslThread);
begin
  FOwner := thread;
  inherited create(false);
end;

procedure TInternalThread.execute;
begin
  SetThreadName(FOwner.ClassName);
  Try
    FOwner.InternalExecute;
  Except
    // ignore any further exceptions
  End;
  FOwner.FRunning := False;
  SetThreadName('');
  if FOwner.AutoFree then
  begin
    FOwner.Free;
    Free;
  end;
end;

{ TBackgroundTaskEngine }

constructor TBackgroundTaskEngine.Create;
begin
  inherited Create;
  FWaiting := TFslList<TBackgroundTaskPackagePair>.create;
  FDone := TFslList<TBackgroundTaskPackagePair>.create;
  FStatus := btsWaiting;
end;

constructor TBackgroundTaskEngine.Create(notify : TBackgroundTaskEvent);
begin
  Create;
  FOnNotify := notify;
end;

destructor TBackgroundTaskEngine.Destroy;
begin
  FWaiting.Free;
  FDone.Free;
  inherited;
end;

procedure TBackgroundTaskEngine.doExec(pck: TBackgroundTaskPackagePair);
begin
  try
    SetStatus(btsProcessing);
    GBackgroundTasks.log('Task '+name+' go');
    execute(pck.request, pck.response);
    GBackgroundTasks.log('Task '+name+' done');
    setStatus(btsWaiting);
  except
    on e : Exception do
    begin
      GBackgroundTasks.log('Task '+name+' error: '+e.Message);
      SetStatus(btsWaiting);
      pck.response.ExceptionClass := ExceptClass(e.ClassType);
      pck.response.Exception := e.message;
    end;
  end;
end;

procedure TBackgroundTaskEngine.performUIInteraction(request: TBackgroundTaskUIRequest; response: TBackgroundTaskUIResponse);
begin
  raise Exception.Create('The method '+className+'.performUIInteraction needs to be overridden');
end;

procedure TBackgroundTaskEngine.progress(state: String; pct: integer);
begin
  FState := state;
  FPct := pct;
  if FWantStop or FWantBreak then
    abort;
end;

function TBackgroundTaskEngine.reportStatus: TBackgroundTaskStatusInfo;
begin
  result := TBackgroundTaskStatusInfo.Create;
  result.id := FId;
  result.name := name;
  result.status := FStatus;
  result.message := FState;
  result.pct := FPct;
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

procedure TBackgroundTaskEngine.terminate;
begin
  setStatus(btsClosed);
  FThread.Terminate;
end;

function TBackgroundTaskEngine.stopped: boolean;
begin
  result := FStatus = btsClosed;
end;

procedure TBackgroundTaskEngine.threadProc;
var
  pck : TBackgroundTaskPackagePair;
begin
  SetThreadName(name);
  try
    SetThreadStatus('Sleeping');
    while not FWantStop do
    begin
      pck := nil;
      GBackgroundTasks.FLock.Lock;
      try
        if FWaiting.Count > 0 then
        begin
          pck := FWaiting[0].link;
          FWaiting.delete(0);
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
          FDone.add(pck.link);
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
  FUIRequest := request;
  FUIResponse := response;
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
  registerTaskEngine(TNullTaskEngine.create); // the main reason for this is so that no real engine has a task id of 0
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
end;

procedure TBackgroundTaskManager.report(list: TFslList<TBackgroundTaskStatusInfo>);
var
  engine : TBackgroundTaskEngine;
begin
  FLock.Lock;
  try
    for engine in FEngines do
      list.Add(engine.reportStatus);
  finally
    FLock.Unlock;
  end;
end;

function TBackgroundTaskManager.report(taskId: integer): TBackgroundTaskStatusInfo;
begin
  FLock.Lock;
  try
    result := FEngines[taskid].reportStatus;
  finally
    FLock.Unlock;
  end;
end;

procedure TBackgroundTaskManager.stopAll;
var
  e : TBackgroundTaskEngine;
begin
  for e in FEngines do
    e.stop;
end;

procedure TBackgroundTaskManager.wait(grace: cardinal);
var
  start : UInt64;
  done : boolean;
  e : TBackgroundTaskEngine;
begin
  start := GetTickCount64;
  repeat
    done := true;
    for e in FEngines do
      if not e.Stopped then
        done := false;
  until done or (GetTickCount64 - start > grace);
  if not done then
    for e in FEngines do
      if not e.Stopped then
        e.terminate;
end;

procedure TBackgroundTaskManager.killTask(id: integer);
begin
  log('kill '+FEngines[id].name);
  FEngines[id].break;
end;

procedure TBackgroundTaskManager.start;
var
  engine : TBackgroundTaskEngine;
begin
  for engine in FEngines do
    engine.FThread := TBackgroundTaskThread.Create(engine);
end;

procedure TBackgroundTaskManager.log(s : String);
begin
//  AllocConsole;
//  writeln(DescribePeriod(now - FStart)+' '+IntToHex(GetCurrentThreadId)+' '+s);
end;

procedure TBackgroundTaskManager.primaryThreadCheck;
var
  e : TBackgroundTaskEngine;
  pck : TBackgroundTaskPackagePair;
//  uReq : TBackgroundTaskUIRequest;
//  uResp : TBackgroundTaskUIResponse;
begin
  // first round: are any tasks complete
  for e in FEngines do
  begin
    pck := nil;
    FLock.Lock;
    try
      if e.FDone.count > 0 then
      begin
        log('found response for '+e.name);
        pck := e.FDone[0].link;
        e.FDone.delete(0);
      end;
    finally
      FLock.Unlock;
    end;
    if pck <> nil then
    begin
      try
        log('notify response for '+e.name);
        if (assigned(pck.OnNotify)) then
          pck.OnNotify(e.FId, pck.response)
        else
          e.OnNotify(e.FId, pck.response);
        log('notified response for '+e.name);
      finally
        pck.Free;
      end;
      exit; // only one outcome per iteration - don't tie up the pimary thread
    end;
  end;
  //// second round: any tasks want user interaction
  //for e in FEngines do
  //begin
  //  resp := nil;
  //  FLock.Lock;
  //  try
  //    if e.FStatus = btsWaitingForUIResponse then
  //    begin
  //      log('UI Response needed for '+e.name);
  //      e.FStatus := btsWaitingForUIResponse;
  //      uReq := e.FUIRequest.Link;
  //      uResp := e.FUIResponse.Link;
  //    end
  //    else
  //    begin
  //      uReq := nil;
  //      uResp := nil;
  //    end;
  //  finally
  //    FLock.Unlock;
  //  end;
  //
  //  if uReq <> nil then
  //  begin
  //    try
  //      e.FUIExceptionClass := nil;
  //      e.FUIException := '';
  //      log('get UI response for '+e.name);
  //      try
  //        e.OnNotify(e.FId, resp);
  //      except
  //        on ex : Exception do
  //        begin
  //          e.FUIException := ex.Message;
  //          e.FUIExceptionClass := ExceptClass(ex.ClassType);
  //        end;
  //      end;
  //      FLock.Lock;
  //      try
  //        e.FStatus := btsUIResponded;
  //      finally
  //        FLock.Unlock;
  //      end;
  //      log('finished getting UI Response for '+e.name);
  //    finally
  //      uReq.Free;
  //      uResp.Free;
  //    end;
  //    exit; // only one outcome per iteration - don't tie up the pimary thread
  //  end;
  //end;
  //
end;

function TBackgroundTaskManager.TasksAreWorking: boolean;
var
  engine : TBackgroundTaskEngine;
begin
  result := false;
  FLock.Lock;
  try
    for engine in FEngines do
      if engine.FStatus in [btsProcessing, btsWaitingForUIResponse, btwRespondingOnUI, btsUIResponded] then
        exit(true);
  finally
    FLock.Unlock;
  end;
end;

procedure TBackgroundTaskManager.queueTask(id : integer; request : TBackgroundTaskRequestPackage);
begin
  queueTask(id, request, TBackgroundTaskResponsePackage.create, nil);
end;

procedure TBackgroundTaskManager.queueTask(id : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage);
begin
  queueTask(id, request, response, nil);
end;

procedure TBackgroundTaskManager.queueTask(id : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage; onNotify : TBackgroundTaskEvent);
var
  engine : TBackgroundTaskEngine;
  pck : TBackgroundTaskPackagePair;
begin
  log('queue task for '+inttostr(id)+' ('+FEngines[id].name+')');
  pck := TBackgroundTaskPackagePair.create(request, response);
  try
    pck.onNotify := onNotify;
    FLock.Lock;
    try
      engine := FEngines[id];
      engine.FWaiting.add(pck.link);
    finally
      FLock.Unlock;
    end;
  finally
    pck.free;
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




function TBackgroundTaskManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FEngines.sizeInBytes);
end;

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

{ TBackgroundTaskStatusInfo }

function TBackgroundTaskStatusInfo.link: TBackgroundTaskStatusInfo;
begin
  result := TBackgroundTaskStatusInfo(inherited link);
end;

function TBackgroundTaskStatusInfo.StatusDisplay: string;
begin
  case FStatus of
    btsWaiting : result := 'Waiting';
    btsProcessing : result := 'Working';
    btsCancelling : result := 'Stopping';
    btsClosed : result := 'Closed';
    btsWaitingForUIResponse : result := 'Waiting (UI)';
    btwRespondingOnUI : result := 'Responding';
    btsUIResponded : result := 'Responded';
  end;
end;

function TBackgroundTaskStatusInfo.PctDisplay: String;
begin
  if FPct < 0 then
    result := ''
  else
    result := inttostr(pct);
end;

function TBackgroundTaskStatusInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FMessage.length * sizeof(char)) + 12);
end;

Initialization
  InitUnit;
Finalization
  DoneUnit;
end.
