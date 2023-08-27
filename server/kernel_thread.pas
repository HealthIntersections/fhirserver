unit kernel_thread;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  {$IFDEF WINDOWS}
  Windows, ActiveX,
  {$ENDIF}
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_fpc, fsl_threads, fsl_logging;

type
  TFhirServerMaintenanceThreadTaskCallBack = procedure (sender : TObject; status : String; percent : integer) of object;
  TFhirServerMaintenanceThreadTaskEvent = procedure (callback : TFhirServerMaintenanceThreadTaskCallBack) of object;

  TFhirServerMaintenanceThreadTaskStatus = (ktsInitialised, ktsPreparing, ktsInProcess, ktsResting);

  TFhirServerMaintenanceThreadTask = class;

  TFhirServerMaintenanceThreadTaskThread = class (TFslThread)
  private
    FTask : TFhirServerMaintenanceThreadTask;
    procedure callback(sender : TObject; status : String; percent : integer);
  protected
    function ThreadName : String; override;
    function logThread : boolean; override;
    procedure Execute; override;
  public
    constructor Create(task : TFhirServerMaintenanceThreadTask);
  end;

  TFhirServerMaintenanceThreadTask = class (TFslObject)
  private
    FName: string;
    FCount: integer;
    FStatus: TFhirServerMaintenanceThreadTaskStatus;
    FState : String;
    FLastError : string;
    FEvent: TFhirServerMaintenanceThreadTaskEvent;
    FLastStarted: UInt64;
    FFrequency: integer;
    FRunTime : UInt64;

    FThread : TFhirServerMaintenanceThreadTaskThread;
    procedure checkStatus;
    procedure wantStop;
    function stopped : boolean;
  public
    function link : TFhirServerMaintenanceThreadTask; overload;

    property name : string read FName write FName;
    property event : TFhirServerMaintenanceThreadTaskEvent read FEvent write FEvent;
    property frequency : integer read FFrequency write FFrequency; // seconds
    property count : integer read FCount;
    property lastStarted : UInt64 read FLastStarted;
    property status : TFhirServerMaintenanceThreadTaskStatus read FStatus;
    property State : String read FState;

    function describe(pad : integer; due : boolean) : String;
  end;

  TFhirServerMaintenanceThread = class (TFslThread)
  private
    FTasks : TFslList<TFhirServerMaintenanceThreadTask>;
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
    procedure Finalise; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure defineTask(name : String; event : TFhirServerMaintenanceThreadTaskEvent; frequency : integer);
    procedure logStatus(workingOnly, due : boolean);
  end;

implementation

{ TFhirServerMaintenanceThreadTaskThread }

function TFhirServerMaintenanceThreadTaskThread.ThreadName: String;
begin
  result := 'kernel:'+FTask.name;
end;

constructor TFhirServerMaintenanceThreadTaskThread.Create(task: TFhirServerMaintenanceThreadTask);
begin
  FTask := task;
  inherited Create;
end;


procedure TFhirServerMaintenanceThreadTaskThread.Execute;
var
  t, p : UInt64;
begin
  SetThreadName(FTask.FName);
  if (FTask.FThread <> self) then
    if FTask.FThread = nil then
      raise EFslException.Create('Error in internal thread: task '+FTask.name+' has no thread associated with it')
    else
      raise EFslException.Create('Error in internal thread: task '+FTask.name+' has a different thread associated with it ('+pointerToString(FTask.FThread)+'/'+pointerToString(self)+')');
  try
    inc(FTask.FCount);
    FTask.FLastStarted := GetTickCount64;
    FTask.FLastError := '';
    FTask.FStatus := ktsInProcess;
    t := GetTickCount64;
    try
      FTask.FEvent(callback);
      p := GetTickCount64 - t;
      //logging.log('Task '+FTask.name+' completed in '+inttostr(p)+'ms');
    except
      on e : Exception do
      begin
        p := GetTickCount64 - t;
        FTask.FLastError := e.Message;
        logging.log('Kernel thread exception ('+FTask.name+'): '+e.message+' ('+inttostr(p)+'ms)');
      end;
    end;
  finally
    FTask.FRunTime := FTask.FRunTime + p;
    FTask.FState := '';
    FTask.FStatus := ktsResting;
  end;
end;

function TFhirServerMaintenanceThreadTaskThread.logThread: boolean;
begin
  result := false;
end;

procedure TFhirServerMaintenanceThreadTaskThread.callback(sender: TObject; status: String; percent: integer);
begin
  SetThreadStatus(status);
  if percent < 0 then
    FTask.FState := status
  else
    FTask.FState := inttostr(percent)+'%: '+status;
end;

{ TFhirServerMaintenanceThreadTask }

procedure TFhirServerMaintenanceThreadTask.checkStatus;
begin
  if (FStatus = ktsResting) and (FThread <> nil) then
  begin
    FThread.free;
    FThread := nil;
  end;

  if (FStatus in [ktsInitialised, ktsResting]) and (FLastStarted + (FFrequency * 1000) < GetTickCount64) then
  begin
    FStatus := ktsPreparing;
    FThread := TFhirServerMaintenanceThreadTaskThread.Create(self);
    FThread.Start;
  end;
end;

{$OVERFLOWCHECKS OFF}
function TFhirServerMaintenanceThreadTask.describe(pad : integer; due : boolean): String;
var
  sfx : string;
begin
  result := StringPadRight(FName, ' ', pad)+' ('+inttostr(FFrequency)+'s)';
  if due then
    sfx := ' (due in '+inttostr((FLastStarted + (FFrequency * 1000)) - GetTickCount64)+'ms)'
  else
    sfx := '';

  case FStatus of
    ktsInitialised: result := result + 'Waiting'+sfx;
    ktsPreparing: result := result + 'Preparing';
    ktsInProcess: result := result + 'In Process ('+inttostr(GetTickCount64 - FLastStarted)+'ms): '+FState;
    ktsResting:
      if FLastError = '' then
        result := result + 'Resting'+sfx;
      else
        result := result + 'Failed'+sfx+': '+FLastError;
  end;
  if (FCount > 1) then
    if FStatus in [ktsPreparing, ktsInProcess] then
      result := result + ' (avg '+inttostr(trunc(FRunTime / (FCount-1)))+'ms)'
    else
      result := result + ' (avg '+inttostr(trunc(FRunTime / FCount))+'ms)'
end;

function TFhirServerMaintenanceThreadTask.link: TFhirServerMaintenanceThreadTask;
begin
  result := TFhirServerMaintenanceThreadTask(inherited link);
end;

function TFhirServerMaintenanceThreadTask.stopped: boolean;
begin
  result := FStatus in [ktsInitialised, ktsResting];
end;

procedure TFhirServerMaintenanceThreadTask.wantStop;
begin
  try
    if FThread <> nil then
      FThread.Stop;
  except
    // can throw an exception if the thread is simultainously stopping, in which case we don't care
  end;
end;

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.Create;
begin
  inherited;
  FTasks := TFslList<TFhirServerMaintenanceThreadTask>.Create;
end;

destructor TFhirServerMaintenanceThread.Destroy;
begin
  FTasks.free;
  inherited;
end;

procedure TFhirServerMaintenanceThread.defineTask(name: String; event: TFhirServerMaintenanceThreadTaskEvent; frequency: integer);
var
  task : TFhirServerMaintenanceThreadTask;
begin
  task := TFhirServerMaintenanceThreadTask.Create;
  try
    task.name := name;
    task.event := event;
    task.frequency := frequency;
    task.FLastStarted := GetTickCount64;
    FTasks.Add(task.Link);
  finally
    task.free;
  end;
end;

function TFhirServerMaintenanceThread.ThreadName: String;
begin
  result := 'kernel';
end;

procedure TFhirServerMaintenanceThread.Initialise;
begin
  {$IFDEF WINDOWS}
  CoInitialize(nil);
  {$ENDIF}
  logStatus(false, false);
end;

procedure TFhirServerMaintenanceThread.Execute;
var
  t : TFhirServerMaintenanceThreadTask;
begin
  while not Terminated do
  begin
    sleep(100);
    ThreadPing;
    for t in FTasks do
      t.checkStatus;
  end;
end;

procedure TFhirServerMaintenanceThread.Finalise;
var
  t : TFhirServerMaintenanceThreadTask;
  b : boolean;
  s, l : Int64;
begin
  try
    for t in FTasks do
      t.WantStop;

    l := 0;
    s := GetTickCount64 + 20000; // we give up after 20 seconds
    repeat
      sleep(50);
      b := true;
      for t in FTasks do
        if not t.stopped then
          b := false;

      if l < GetTickCount64 then
      begin
        l := GetTickCount64 + 5000;
        logStatus(true, false);
      end;
    until b or (s < GetTickCount64);
  except
    on e : Exception do
      Logging.log('Exception closing up kernel thread: '+e.Message);
  end;
  {$IFDEF WINDOWS}
  CoUninitialize;
  {$ENDIF}
end;

procedure TFhirServerMaintenanceThread.logStatus(workingOnly, due : boolean);
var
  t : TFhirServerMaintenanceThreadTask;
  pad : integer;
begin
  pad := 0;
  for t in FTasks do
    pad := IntegerMax(pad, t.name.Length);

  for t in FTasks do
    if (t.FStatus in [ktsPreparing, ktsInProcess]) or not workingOnly then
      Logging.log(t.describe(pad, due));
end;

end.
