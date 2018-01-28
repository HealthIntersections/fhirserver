unit SystemService;

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

uses
  FastMM4,
  AdvObjects,
  psapi,
  ThreadSupport,
  Windows,
  WinSvc;

type
  TSystemService = { Abstract } class (TAdvObject)
  Private
    FHandle : SERVICE_STATUS_HANDLE;
    FSystemName : String;
    FDisplayName : String;
    FDebugMode : boolean;
    FWantStop : boolean;
    FStopReason : String;
    FTellUser : boolean;
    FIsContained: Boolean;
    procedure InternalExecute;
    procedure SetStatus(AState, AControls : DWord);
    procedure CommandInstall;
    procedure CommandStart;
    procedure CommandStop;
    procedure CommandRemove;
    procedure CommandSend;
    procedure CommandStatus;
  Protected
    function CanInstall : boolean; Virtual;
    function CanStart : boolean; Virtual;
    procedure postStart; Virtual;
    procedure DoStop; Virtual;
    procedure DoRemove; Virtual;

    procedure dump; virtual;

    procedure DoReceiveMessage(AMsg : Integer); virtual;
    function CheckClose(var s: String): Boolean; Virtual;
  Public
    constructor Create(const ASystemName, ADisplayName: String);
    function MemoryStatus : String;
    procedure DebugExecute;
    procedure ServiceExecute;
    Procedure ContainedStart;
    Procedure ContainedStop;
    property DebugMode : Boolean read FDebugMode write FDebugMode;
    property IsContained : Boolean read FIsContained;
    property WantStop : Boolean read FWantStop;
    property StopReason: String read FStopReason;
    procedure Stop(AReason : String; ATellUser : boolean = true);
    procedure Execute;
    Property DisplayName : String read FDisplayName;
  end;

var
  GService : TSystemService = nil; // there can only be one....

implementation

uses
  DateSupport,
  ServiceController,
  StringSupport,
  kCritSct,
  SysUtils,
  FHIRLog;

const
  ASSERT_UNIT = 'SystemService';

{ TSystemService }

constructor TSystemService.Create(const ASystemName, ADisplayName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.Create';
begin
  inherited create;
  assert(not assigned(GService), ASSERT_LOCATION+': There can only be one service in a process');
  assert(IsValidIdent(ASystemName), ASSERT_LOCATION+': SystemName is not valid');
  assert(ADisplayName <> '', ASSERT_LOCATION+': Display Name is not valid' );
  FSystemName := ASystemName;
  FDisplayName := ADisplayName;
  FTellUser := true;
  GService := self;
end;

// stub virtual methods
function TSystemService.CanInstall: boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CanInstall';
begin
  result := true;
end;

function TSystemService.CanStart: boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CanStart';
begin
  result := true;
end;

function TSystemService.CheckClose(var s: String): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CheckDebugClose';
begin
  result := false;
end;

procedure TSystemService.DoReceiveMessage(AMsg: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.DoReceiveMessage';
begin
  // nothing
end;

procedure TSystemService.DoRemove;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.DoRemove';
begin
  // nothing
end;

procedure TSystemService.DoStop;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.DoStop';
begin
  // nothing
end;

procedure TSystemService.dump;
begin
  logt(DumpLocks);
end;

function DebugCtrlC(dwCtrlType : DWORD) :BOOL;
begin
  result := true;
  SetConsoleCtrlHandler(@DebugCtrlC, false);
  GService.dump;
  GService.Stop('Console Stop Event '+inttostr(dwCtrlType), false);

end;

procedure TSystemService.DebugExecute;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.DebugExecute';
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  SetConsoleCtrlHandler(@DebugCtrlC, true);
  FDebugMode := True;
  InternalExecute;
  if FTellUser and (FStopReason <> '') then
    begin
    logt('stop because '+FStopReason);
    write('press Enter to close');
    readln;
    end;
end;

Procedure DeathThread(o : TObject); Stdcall;
var
  LMemMgr: {$IFDEF VER130}TMemoryManager {$ELSE}TMemoryManagerEx{$ENDIF};
Begin
  exit;
  DebugThreadName := 'DeathThread';
  Try
    Sleep(150);
    try
      ExitProc := NIL;
      ExceptProc := NIL;
      ErrorProc := NIL;
      {$IFNDEF LINUX}
        {$IFNDEF WIN64}
      SetRaiseList(NIL);
        {$ENDIF}
      {$ENDIF}
      LibModuleList := NIL;
      ModuleUnloadList := NIL;

      {$IFNDEF LINUX}
      TerminateProcess(GetCurrentProcess, 0);
      {$ENDIF}

        {$IFNDEF WIN64}
      // what - still here? Surely not
      while True do
        asm
        pop eax;
        end;
        {$ENDIF}

    finally
      // we don't believe you could ever get here. but if we do,
      // well, we'll just make sure that nothing will ever work again
      // anyway.
      LMemMgr.GetMem := NIL;
      LMemMgr.FreeMem := NIL;
      LMemMgr.ReallocMem := NIL;
      SetMemoryManager(LMemMgr);
    end;
  Except
  End;
End;

procedure LaunchDeathThread;
var
  ThreadID : Cardinal;
Begin
  CloseHandle(CreateThread(Nil, 8192, @DeathThread, nil, 0, ThreadID));
End;


procedure TSystemService.InternalExecute;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.InternalExecute';
var
  LMsg : string;
  LCheckTime : TDateTime;
begin
  LCheckTime := 0;
  try
    if CanStart then
      begin
      try
        if not FDebugMode then
          begin
          SetStatus(SERVICE_RUNNING, SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_SHUTDOWN);
          end;
        postStart;
        repeat
          SetConsoleTitle(pChar(FDisplayName+MemoryStatus));
          if (LCheckTime < Now) then
            begin
            LCheckTime := now + 10 * DATETIME_SECOND_ONE;
            if CheckClose(LMsg) then
              begin
              Stop(LMsg);
              end;
            end;
          Sleep(100);
        until FWantStop;
      finally
        if not FDebugMode then
          begin
          SetStatus(SERVICE_STOP_PENDING, 0);
          LaunchDeathThread; // just to make sure we really really do shut down
          end;
        DoStop;
      end;
      end;
  except
    on e:exception do
      begin
      if FDebugMode then
        begin
        logt('Exception in Service Execution: '+#13#10+#13#10+e.message+' '+#13#10+'['+e.classname+']');
        if DebugMode then
          write('press Enter to close');
        Readln;
        end
      else
        begin
        MessageBox(0, PChar(e.message+#13#10+'['+e.classname+']'), PChar(FDisplayName+' Kernel Error'), MB_SERVICE_NOTIFICATION + MB_TOPMOST);
        end;
      end;
  end;
end;

function memToMb(v : UInt64) : string;
begin
  v := v div 1024;
  v := v div 1024;
  result := inttostr(v)+'MB';
end;

function TSystemService.MemoryStatus: String;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
  v : UInt64;
  hProcess: THandle;
  pmc: PROCESS_MEMORY_COUNTERS;
  total: DWORD;
begin
  GetMemoryManagerState(st);
  v := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
    v := v + sb.UseableBlockSize * sb.AllocatedBlockCount;
  result := ' '+memToMb(v);
  hProcess := GetCurrentProcess;
  if (GetProcessMemoryInfo(hProcess, @pmc, SizeOf(pmc))) then
    result := result +' / '+memToMB(pmc.WorkingSetSize + pmc.QuotaPagedPoolUsage + pmc.QuotaNonPagedPoolUsage);
  CloseHandle(hProcess);
end;

procedure TSystemService.postStart;
begin
// nothing
end;

procedure ServiceCallHandler(fdwControl: DWORD); Stdcall;
begin
  if (fdwControl = SERVICE_CONTROL_STOP) then
    begin
    GService.Stop('Service Controller Request');
    end
  else if (fdwControl = SERVICE_CONTROL_SHUTDOWN) then
    begin
    GService.Stop('Windows Service Controller Shutdown')
    end
  else if fdwControl >= USER_CONTROL_OFFSET then
    begin
    GService.DoReceiveMessage(fdwControl - USER_CONTROL_OFFSET);
    end;
end;

procedure ServiceMainEntry(dwArgc: DWORD; lpszArgv: pointer); Stdcall;
begin
  DebugThreadName := 'Service';
  GService.FHandle := RegisterServiceCtrlHandler(PChar(lpszArgv^), @ServiceCallHandler);
  if GService.FHandle <> 0 then
    begin
    GService.SetStatus(SERVICE_START_PENDING, 0);
    try
      GService.InternalExecute;
    finally
      GService.SetStatus(SERVICE_STOPPED, 0);
    end;
    end;

  DebugThreadName := '';
end;

var
  GServiceInfo : array [0..1] of TServiceTableEntry;


procedure TSystemService.ServiceExecute;
begin
  // problem: if we weren't actually started as a service we are about
  // to hang. But there is no way to deteramine whether we are running
  // as a service
  GServiceInfo[0].lpServiceName := PChar(FSystemName);
  GServiceInfo[0].lpServiceProc := @ServiceMainEntry;
  GServiceInfo[1].lpServiceName := NIL;
  GServiceInfo[1].lpServiceProc := NIL;
  StartServiceCtrlDispatcher(GServiceInfo[0]);
end;

procedure TSystemService.SetStatus(AState, AControls : DWord);
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.SetStatus';
var
  LSvcStatus : TServiceStatus;
begin
  LSvcStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  LSvcStatus.dwCurrentState := AState;
  LSvcStatus.dwControlsAccepted := AControls;
  LSvcStatus.dwWin32ExitCode := 0;
  LSvcStatus.dwServiceSpecificExitCode := 0;
  LSvcStatus.dwCheckPoint := 0;
  LSvcStatus.dwWaitHint := 2000;
  SetServiceStatus(FHandle, LSvcStatus);
end;

procedure TSystemService.Stop(AReason: String; ATellUser : boolean = true);
begin
  FStopReason := AReason;
  FWantStop := true;
  FTellUser := ATellUser;
end;

procedure TSystemService.Execute;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.Execute';
var
  LCanRun : boolean;
begin

  LCanRun := true;
  try
    if FindCmdLineSwitch('install', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandInstall;
      end;
    if FindCmdLineSwitch('start', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandStart;
      end;
    if FindCmdLineSwitch('stop', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandStop;
      end;
    if FindCmdLineSwitch('remove', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandRemove;
      end;
    if FindCmdLineSwitch('send', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandSend;
      end;
    if FindCmdLineSwitch('status', ['-', '/'], true) then
      begin
      LCanRun := false;
      CommandStatus;
      end;
  except
    on e:exception do
      begin
      LCanRun := false;
      logt('Exception in Service Execution: '+e.message+' ['+e.classname+']');
      write('press Enter to close');
      Readln;
      end;
  end;
  If LCanRun then
    begin
    if FindCmdLineSwitch('debug', ['-', '/'], true) then
      DebugExecute
    else
      ServiceExecute;
    end;
end;

procedure TSystemService.CommandInstall;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandInstall';
var
  LSvcMan : TServiceManagerHandle;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  logt('Install Service '+FDisplayName);
  logt('================'+StringPadRight('', '=', length(FDisplayName)));
  logt('');
  if CanInstall then
    begin
    Write('Registering Service '+FDisplayName+'...   ');
    LSvcMan := TServiceManagerHandle.create;
    try
      LSvcMan.Install(FSystemName, FDisplayName, ParamStr(0));
    finally
      LSvcMan.free;
    end;
    logt('Done');
    end;
end;

procedure TSystemService.CommandRemove;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandRemove';
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  DoRemove;
  Write('Unregistering Service '+FDisplayName+'...   ');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      LSvc.Remove;
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
  logt('Done');
end;

procedure TSystemService.CommandSend;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandSend';
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
  i, LCmd : integer;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  LCmd := -1;
  for i := 1 to 20 do
    begin
    if StringIsInteger32(paramStr(i)) then
      begin
      LCmd := StrToInt(paramStr(i));
      break;
      end;
    end;
  Write('Sending Message '+inttostr(LCmd)+' to Service '+FDisplayName+'...   ');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      LSvc.SendMessageToService(0);
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
  logt('Done');
end;

procedure TSystemService.CommandStart;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandStart';
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
  i : integer;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  Write('Starting Service '+FDisplayName+'...');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      if LSvc.ServiceIsRunning then
        begin
        logt('   Service is not stopped');
        write('press Enter to close');
        readln;
        end
      else
        begin
        LSvc.Start;
        i := 0;
        repeat
          sleep(1000);
          write('.');
          inc(i);
        until (LSvc.ServiceIsRunning) or (i = 20);
        if LSvc.ServiceIsRunning then
          begin
          logt('   Done');
          end
        else
          begin
          logt('   Service could not be started');
          write('press Enter to close');
          readln;
          end;
        end;
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
end;

procedure TSystemService.CommandStatus;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandStatus';
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  logt('Status for Service '+FDisplayName);
  logt('==================='+StringPadRight('', '=', length(FDisplayName)));
  logt('');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      if LSvc.ServiceIsRunning then
        begin
        logt('Service is running');
        end
      else
       begin
       logt('Service is not running');
       end;
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
  writeln;
  logt('Press Enter to close');
  readln;
end;

procedure TSystemService.CommandStop;
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.CommandStop';
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
  i : integer;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  Write('Stopping Service '+FDisplayName+'...   ');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      if not LSvc.ServiceIsRunning then
        begin
        logt('   Service is already stopped');
        write('press Enter to close');
        readln;
        end
      else
        begin
        LSvc.Stop;
        i := 0;
        repeat
          sleep(1000);
          write('.');
          inc(i);
        until (not LSvc.ServiceIsRunning) or (i = 20);
        if not LSvc.ServiceIsRunning then
          begin
          logt('   Done');
          end
        else
          begin
          logt('   Service could not be stopped');
          write('press Enter to close');
          readln;
          end;
        end;
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
end;

procedure TSystemService.ContainedStart;
begin
  FIsContained := true;
  if not CanStart then
    Raise Exception.create('unable to start service '+FDisplayName);
end;

procedure TSystemService.ContainedStop;
begin
  DoStop;
end;

end.

