unit fsl_service_win;

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

{$IFDEF WINDOWS}
uses
  {$IFNDEF FPC}
//  FastMM4,
  {$ENDIF}
  Windows, {$IFDEF FPC} JwaWinSvc, {$ELSE}WinSvc, PSApi, TlHelp32, {$ENDIF} SysUtils, Classes,
  fsl_base, fsl_utilities;

type
  TSystemService = class abstract (TFslObject)
  Private
    FHandle : SERVICE_STATUS_HANDLE;
    FSystemName : String;
    FDisplayName : String;
    FDebugMode : boolean;
    FWantStop : boolean;
    FStopReason : String;
    FTellUser : boolean;
    FIsContained: Boolean;
    FStartTime : TDateTime;
    procedure InternalExecute;
    procedure SetStatus(AState, AControls : DWord);
    procedure CommandInstall;
    procedure CommandStart;
    procedure CommandStop;
    procedure CommandRemove;
    procedure CommandSend;
    procedure CommandStatus;
  Protected
    procedure dump; virtual;

    procedure DoReceiveMessage(AMsg : Integer); virtual;
    function CheckClose(var s: String): Boolean; Virtual;
    function command(cmd: String): boolean; virtual;
  Public
    constructor Create(const ASystemName, ADisplayName: String);
    destructor Destroy; override;

    function CanInstall : boolean; Virtual;
    function CanStart : boolean; Virtual;
    procedure postStart; Virtual;
    procedure DoStop; Virtual;
    procedure DoRemove; Virtual;

    procedure ConsoleExecute;
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


const
  USER_CONTROL_OFFSET = $80;

type
  TServiceManagerHandle = class (TFslObject)
  private
    FMachine : String;
    FHandle : SC_HANDLE;
    FHndErr : Cardinal;
  public
    constructor Create(AMachine : String = ''); { default = local }
    destructor Destroy; override;
    procedure Install(ASystemName, ADisplayName, AExecutable : String);
    procedure ListServices(AList : TStringList);
  end;

  TServiceStatusEnum = (ssError, ssNotInstalled, ssRunning, ssStopped, ssStarting, ssStopping, ssOther);

  TServiceHandle = class (TFslObject)
  private
    FManHnd : SC_HANDLE;
    FService : String;
    FHandle : SC_HANDLE;
    FHndErr : Cardinal;
    FStatus: TServiceStatus;
    function GetAutoStart: Boolean;
    procedure SetAutoStart(const Value: Boolean);
  public
    constructor Create(AServiceManager : TServiceManagerHandle; AName : string);
    destructor Destroy; override;
    function ServiceIsRunning : boolean;
    procedure Query;
    procedure SendMessageToService(AMsg : Integer);
    procedure Remove;
    procedure Stop;
    procedure Start;
    procedure ListDependencies(AList : TStrings);
    procedure SetDependencies(AList : TStrings);

    Function AccountName : AnsiString;
    Procedure SetAccount(aUser, aPassword : String);

    function GetStatus(Var VMsg : String) : TServiceStatusEnum;
    property AutoStart : Boolean read GetAutoStart write SetAutoStart;
    function InstalledOK : Boolean;
    procedure Bind;

    procedure Update(ATitle : String; AExecutable : String);
  end;

function DescribeServiceStatus(ACode: DWord): String;

{$ENDIF}

implementation

{$IFDEF WINDOWS}
uses
  fsl_threads,
  fsl_logging;

const
  ASSERT_UNIT = 'fsl_service_win';

{ TSystemService }

constructor TSystemService.Create(const ASystemName, ADisplayName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TSystemService.Create';
begin
  inherited create;
  assert(not assigned(GService), ASSERT_LOCATION+': There can only be one service in a process');
  assert(IsValidIdent(ASystemName), ASSERT_LOCATION+': SystemName is not valid');
  assert(ADisplayName <> '', ASSERT_LOCATION+': Display Name is not valid' );
  FStartTime := now;
  FSystemName := ASystemName;
  FDisplayName := ADisplayName;
  FTellUser := true;
  GService := self;
end;

// stub virtual methods
function TSystemService.CanInstall: boolean;
begin
  result := true;
end;

function TSystemService.CanStart: boolean;
begin
  result := true;
end;

function TSystemService.CheckClose(var s: String): Boolean;
begin
  result := false;
end;

function TSystemService.command(cmd: String): boolean;
begin
  result := true;
  if cmd = 'install' then
    CommandInstall
  else if cmd = 'start' then
    CommandStart
  else if cmd = 'stop' then
    CommandStop
  else if cmd = 'remove' then
    CommandRemove
  else if cmd = 'send' then
    CommandSend
  else if cmd = 'status' then
    CommandStatus
  else
    result := false;
end;

destructor TSystemService.Destroy;
begin
  GService := nil;
  inherited;
end;

procedure TSystemService.DoReceiveMessage(AMsg: Integer);
begin
  // nothing
end;

procedure TSystemService.DoRemove;
begin
  // nothing
end;

procedure TSystemService.DoStop;
begin
  // nothing
end;

type
  TOpenThreadFunc = function(DesiredAccess: DWORD; InheritHandle: BOOL; ThreadID: DWORD): THandle; stdcall;
var
  OpenThreadFunc: TOpenThreadFunc;

function OpenThread(id : DWORD) : THandle;
const
//  THREAD_GET_CONTEXT       = $0008;
  THREAD_QUERY_INFORMATION = $0040;
var
  Kernel32Lib: THandle;
begin
  if @OpenThreadFunc = nil then
  begin
    Kernel32Lib := GetModuleHandle(kernel32);
    OpenThreadFunc := GetProcAddress(Kernel32Lib, 'OpenThread');
  end;
  result := OpenThreadFunc(THREAD_QUERY_INFORMATION, False, id);
end;

function FileTimeZero : TDateTime;
begin
  result := EncodeDate(1601, 1, 1);
end;

procedure TSystemService.dump;
begin
  Logging.log(GetThreadReport);
  Logging.log(DumpLocks);
end;

function DebugCtrlC(dwCtrlType : DWORD) :BOOL;
begin
  Logging.log('Ctrl-C');
  result := true;
  SetConsoleCtrlHandler(@DebugCtrlC, false);
  GService.dump;
  GService.Stop('Console Stop Event '+inttostr(dwCtrlType), false);

end;

procedure TSystemService.ConsoleExecute;
begin
  SetThreadName('Service.Execute.Console');
  SetThreadStatus('Executing');
//  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  SetConsoleCtrlHandler(@DebugCtrlC, true);
  DebugConsoleMessages := true;
  FDebugMode := True;
  InternalExecute;
  if FTellUser and (FStopReason <> '') then
    begin
    Logging.log('stop because '+FStopReason);
    write('press Enter to close');
    readln;
    end;
  SetThreadStatus('Finished');
end;

{$IFNDEF FPC}
Procedure DeathThread(o : TObject); Stdcall;
var
  LMemMgr: {$IFDEF VER130}TMemoryManager {$ELSE}TMemoryManagerEx{$ENDIF};
Begin
  exit;
  SetThreadName('DeathThread');
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
{$ENDIF}


procedure TSystemService.InternalExecute;
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
          SetConsoleTitle(pChar(FDisplayName+' '+Logging.MemoryStatus));
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
          {$IFNDEF FPC}
          LaunchDeathThread; // just to make sure we really really do shut down
          {$ENDIF}
          end;
        DoStop;
      end;
      end;
  except
    on e:exception do
      begin
      if FDebugMode then
        begin
        Logging.log('Exception in Service Execution: '+#13#10+#13#10+e.message+' '+#13#10+'['+e.classname+']');
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
  SetThreadName('Service');
  SetThreadStatus('Starting');
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

  SetThreadStatus('Finished');
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
  StartServiceCtrlDispatcher({$IFDEF FPC}@{$ENDIF}GServiceInfo[0]);
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
      Logging.log('Exception in Service Execution: '+e.message+' ['+e.classname+']');
      write('press Enter to close');
      Readln;
      end;
  end;
  If LCanRun then
    begin
    if FindCmdLineSwitch('console', ['-', '/'], true) or FindCmdLineSwitch('debug', ['-', '/'], true) then
      ConsoleExecute
    else
      ServiceExecute;
    end;
end;

procedure TSystemService.CommandInstall;
var
  LSvcMan : TServiceManagerHandle;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  Logging.log('Install Service '+FDisplayName);
  Logging.log('================'+StringPadRight('', '=', length(FDisplayName)));
  Logging.log('');
  if CanInstall then
    begin
    Write('Registering Service '+FDisplayName+'...   ');
    LSvcMan := TServiceManagerHandle.create;
    try
      LSvcMan.Install(FSystemName, FDisplayName, ParamStr(0));
    finally
      LSvcMan.free;
    end;
    Logging.log('Done');
    end;
end;

procedure TSystemService.CommandRemove;
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
  Logging.log('Done');
end;

procedure TSystemService.CommandSend;
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
  Logging.log('Done');
end;

procedure TSystemService.CommandStart;
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
        Logging.log('   Service is not stopped');
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
          Logging.log('   Done');
          end
        else
          begin
          Logging.log('   Service could not be started');
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
var
  LSvcMan : TServiceManagerHandle;
  LSvc : TServiceHandle;
begin
  AllocConsole;
  SetConsoleTitle(pChar(FDisplayName));
  Logging.log('Status for Service '+FDisplayName);
  Logging.log('==================='+StringPadRight('', '=', length(FDisplayName)));
  Logging.log('');
  LSvcMan := TServiceManagerHandle.create;
  try
    LSvc := TServiceHandle.create(LSvcMan, FSystemName);
    try
      if LSvc.ServiceIsRunning then
        begin
        Logging.log('Service is running');
        end
      else
       begin
       Logging.log('Service is not running');
       end;
    finally
      LSvc.free;
    end;
  finally
    LSvcMan.free;
  end;
  writeln;
  Logging.log('Press Enter to close');
  readln;
end;

procedure TSystemService.CommandStop;
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
        Logging.log('   Service is already stopped');
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
          Logging.log('   Done');
          end
        else
          begin
          Logging.log('   Service could not be stopped');
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
    raise ELibraryException.create('unable to start service '+FDisplayName);
end;

procedure TSystemService.ContainedStop;
begin
  DoStop;
end;


{ TServiceManagerHandle }

constructor TServiceManagerHandle.create(AMachine : String = '');
begin
  inherited create;
  FMachine := AMachine;
  FHandle := OpenSCManager(pchar(AMachine), NIL, SC_MANAGER_ALL_ACCESS);
  FHndErr := GetLastError;
end;

destructor TServiceManagerHandle.destroy;
begin
  if FHandle <> 0 then
    CloseServiceHandle(FHandle);
  inherited;
end;

procedure TServiceManagerHandle.Install(ASystemName, ADisplayName, AExecutable: String);
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  if CreateService(FHandle, pchar(ASystemName), pchar(ADisplayName),
         SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS,
         SERVICE_AUTO_START, SERVICE_ERROR_NORMAL, pchar(AExecutable), NIL, NIL, NIL, NIL, NIL) = 0 then
    begin
    RaiseLastOSError;
    end;
end;

procedure TServiceManagerHandle.ListServices(AList: TStringList);
var
  LSvc: packed array [0..1000] of ENUM_SERVICE_STATUS;
  LNeeded, LReturned, LResume : DWord;
  i : integer;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  LReturned := 0;
  LResume := 0;
  repeat
    if not EnumServicesStatus(FHandle, SERVICE_WIN32, SERVICE_ACTIVE or SERVICE_INACTIVE,
              {$IFDEF FPC}@{$ENDIF}{$IFDEF VER340}@{$ENDIF}
              LSvc[0], sizeof(TEnumServiceStatus) * 1000, LNeeded, LReturned, LResume) then
      RaiseLastOSError;
    for i := 0 to LReturned - 1 do
      AList.Values[LSvc[i].lpServiceName] := LSvc[i].lpDisplayName;
  until LResume = 0;
end;

{ TServiceHandle }

constructor TServiceHandle.create(AServiceManager : TServiceManagerHandle; AName: string);
begin
  inherited create;
  FService := AName;
  FManHnd := AServiceManager.FHandle;
  FHndErr := AServiceManager.FHndErr;
  Bind;
end;

destructor TServiceHandle.destroy;
begin
  if FHandle <> 0 then
    CloseServiceHandle(FHandle);
  inherited;
end;

procedure TServiceHandle.Query;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));

  if not QueryServiceStatus(FHandle, FStatus) then
    begin
    RaiseLastOSError;
    end;
end;

function TServiceHandle.ServiceIsRunning: boolean;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  Query;
  result := FStatus.dwCurrentState <> SERVICE_STOPPED;
end;

procedure TServiceHandle.SendMessageToService(AMsg: Integer);
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  Query;
  win32Check(ControlService(FHandle, AMsg + USER_CONTROL_OFFSET, FStatus));
end;

(*

      ServiceHandle :=
      Write('.');
      if ServiceHandle = 0 then
        writeln(MakeWinError(GetLastError, 'stopping the service'))
      else
        try
          Write('.');
          if (lpServiceStatus.dwCurrentState <> SERVICE_STOPPED) then
          i := 0;
          repeat
            Write('.');
            QueryServiceStatus(serviceHandle, lpServiceStatus);
            inc(i);
            if lpServiceStatus.dwCurrentState <> SERVICE_STOPPED then
              sleep(100);
          until (i = 10) or (lpServiceStatus.dwCurrentState = SERVICE_STOPPED);
          if lpServiceStatus.dwCurrentState <> SERVICE_STOPPED then
            writeln('The ' + FTitle + ' Service could not be stopped')
          else
            writeln('The ' + FTitle + ' Service has been stopped');
        finally
          end;


    *)
procedure TServiceHandle.Remove;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  if ServiceIsRunning then
    begin
    Stop;
    end;
  win32Check(DeleteService(FHandle));
  CloseServiceHandle(FHandle);
  FHandle := 0;
end;

procedure TServiceHandle.Start;
var
  p: PChar;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  p := nil;
  win32Check(StartService(FHandle, 0, p));
end;

procedure TServiceHandle.Stop;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  Query;
  win32Check(ControlService(FHandle, SERVICE_CONTROL_STOP, FStatus));
end;

procedure TServiceHandle.ListDependencies(AList: TStrings);
var
  LNeeded : DWord;
  LConfig : array [0..11] of LPQUERY_SERVICE_CONFIG;
  p : pchar;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(LPQUERY_SERVICE_CONFIG)* 10, LNeeded) then
    RaiseLastOSError;
  p := LConfig[0].lpDependencies;
  while assigned(p) and (p[0] <> #0) do
    begin
    AList.Add(p);
    inc(p, strlen(p)+1);
    end;
end;

procedure TServiceHandle.SetDependencies(AList: TStrings);
var
  i : integer;
  s : String;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  s := '';
  for i := 0 to AList.count - 1 do
    s := s + AList[i]+#0;
  s := s + #0;

  if not ChangeServiceConfig(FHandle,  SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
          nil, nil, nil, pchar(s), nil, nil, nil) then
    RaiseLastOSError;
end;


function TServiceHandle.GetAutoStart: Boolean;
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastOSError;
  result := LConfig[0].dwStartType = SERVICE_AUTO_START;
end;

procedure TServiceHandle.SetAutoStart(const Value: Boolean);
var
  v : DWord;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  if Value then
    v := SERVICE_AUTO_START
  else
    v := SERVICE_DEMAND_START;

  if not ChangeServiceConfig(FHandle,  SERVICE_NO_CHANGE, v, SERVICE_NO_CHANGE,
          nil, nil, nil, nil, nil, nil, nil) then
    RaiseLastOSError;
end;

function DescribeServiceStatus(ACode: DWord): String;
begin
  case ACode of
    SERVICE_STOPPED:
      Result := 'Stopped';
    SERVICE_START_PENDING:
      Result := 'Starting';
    SERVICE_STOP_PENDING:
      Result := 'Stopping';
    SERVICE_RUNNING:
      Result := 'Running';
    SERVICE_CONTINUE_PENDING:
      Result := 'Continuing';
    SERVICE_PAUSE_PENDING:
      Result := 'Pausing';
    SERVICE_PAUSED:
      Result := 'Paused';
    else
      Result := 'Unknown (' + IntToStr(ACode) + ')';
    end;
end;

function TServiceHandle.GetStatus(var VMsg: String): TServiceStatusEnum;
begin
  if FHandle = 0 then
    begin
    if FHndErr = 1060 then
      result := ssNotInstalled
    else
      begin
      result := ssError;
      VMsg := SysErrorMessage(FHndErr);
      end;
    end
  else
    begin
    Query;
    if FStatus.dwCurrentState = SERVICE_RUNNING then
    Begin
      result := ssRunning;
      VMsg := 'Running';
    End
    else if FStatus.dwCurrentState = SERVICE_STOPPED then
    Begin
      result := ssStopped;
      VMsg := 'Stopped';
    End
    else if FStatus.dwCurrentState = SERVICE_START_PENDING then
    Begin
      result := ssStarting;
      VMsg := 'Starting';
    End
    else if FStatus.dwCurrentState = SERVICE_STOP_PENDING then
    Begin
      result := ssStopping;
      VMsg := 'Stopping';
    End
    else
      begin
      result := ssOther;
      VMsg := DescribeServiceStatus(FStatus.dwCurrentState);
      end;
    end;
end;

function TServiceHandle.InstalledOK: Boolean;
begin
  result := FHandle <> 0;
end;

procedure TServiceHandle.Bind;
begin
  if FHandle <> 0 then
    begin
    CloseServiceHandle(FHandle);
    sleep(20);
    end;
  if FManHnd <> 0 then
    begin
    FHandle := OpenService(FManHnd, pchar(FService), SERVICE_ALL_ACCESS);
    FHndErr := GetLastError;
    end;
end;

procedure TServiceHandle.Update(ATitle, AExecutable: String);
begin
  if not ChangeServiceConfig(FHandle,  SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
          pchar(AExecutable), nil, nil, nil, nil, nil, pchar(ATitle)) then
    RaiseLastOSError;
end;

function TServiceHandle.AccountName: AnsiString;
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastOSError;
  result := LConfig[0].lpServiceStartName;
end;

procedure TServiceHandle.SetAccount(aUser, aPassword: String);
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EOSError.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastOSError;
  if (aUser = '') and (aPassword = '') Then
  Begin
    if not changeServiceConfig(FHandle, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, lConfig[0].dwErrorControl, nil,
                       nil, nil, nil, 'LocalSystem', nil, nil) Then
      RaiseLastOSError;
  End
  Else
  Begin
    if not changeServiceConfig(FHandle, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, lConfig[0].dwErrorControl, nil,
                      nil, nil, nil, pchar(AUser), pchar(APassword), nil) Then
      RaiseLastOSError;
  End;
end;

{$ENDIF}

end.

