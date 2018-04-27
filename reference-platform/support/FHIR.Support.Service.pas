unit FHIR.Support.Service;

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
  Windows, WinSvc, PSApi,
  Classes,
  FHIR.Support.Objects, FHIR.Support.System;

type
  TSystemService = { Abstract } class (TFslObject)
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


const
  USER_CONTROL_OFFSET = $80;

type
  TServiceManagerHandle = class (TFslObject)
  private
    FMachine : String;
    FHandle : SC_HANDLE;
    FHndErr : Cardinal;
  public
    Constructor create(AMachine : String = ''); { default = local }
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
    Constructor create(AServiceManager : TServiceManagerHandle; AName : string);
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

implementation

uses
  FHIR.Support.DateTime,
  FHIR.Support.Strings,
  FHIR.Support.Lock,
  SysUtils,
  FHIR.Debug.Logging;

const
  ASSERT_UNIT = 'FHIR.Support.Service';

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


{ TServiceManagerHandle }

constructor TServiceManagerHandle.create(AMachine : String = '');
const ASSERT_LOCATION = ASSERT_UNIT+'.TServiceManagerHandle.Create';
begin
  inherited create;
  FMachine := AMachine;
  FHandle := OpenSCManager(pchar(AMachine), NIL, SC_MANAGER_ALL_ACCESS);
  FHndErr := GetLastError;
end;

destructor TServiceManagerHandle.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TServiceManagerHandle.destroy';
begin
  if FHandle <> 0 then
    CloseServiceHandle(FHandle);
  inherited;
end;

procedure TServiceManagerHandle.Install(ASystemName, ADisplayName, AExecutable: String);
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if CreateService(FHandle, pchar(ASystemName), pchar(ADisplayName),
         SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS,
         SERVICE_AUTO_START, SERVICE_ERROR_NORMAL, pchar(AExecutable), NIL, NIL, NIL, NIL, NIL) = 0 then
    begin
    RaiseLastWin32Error;
    end;
end;

procedure TServiceManagerHandle.ListServices(AList: TStringList);
var
  LSvc: packed array [0..1000] of TEnumServiceStatus;
  LNeeded, LReturned, LResume : DWord;
  i : integer;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  LNeeded := 0;
  LReturned := 0;
  LResume := 0;
  repeat
    if not EnumServicesStatus(FHandle, SERVICE_WIN32, SERVICE_ACTIVE or SERVICE_INACTIVE,
              LSvc[0], sizeof(TEnumServiceStatus) * 1000, LNeeded, LReturned, LResume) then
      RaiseLastWin32Error;
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
    raise EWin32Error.Create(SysErrorMessage(FHndErr));

  if not QueryServiceStatus(FHandle, FStatus) then
    begin
    RaiseLastWin32Error;
    end;
end;

function TServiceHandle.ServiceIsRunning: boolean;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  Query;
  result := FStatus.dwCurrentState <> SERVICE_STOPPED;
end;

procedure TServiceHandle.SendMessageToService(AMsg: Integer);
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
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
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
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
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  p := nil;
  win32Check(StartService(FHandle, 0, p));
end;

procedure TServiceHandle.Stop;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  Query;
  win32Check(ControlService(FHandle, SERVICE_CONTROL_STOP, FStatus));
end;

procedure TServiceHandle.ListDependencies(AList: TStrings);
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIG;
  p : pchar;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIG)* 10, LNeeded) then
    RaiseLastWin32Error;
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
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  s := '';
  for i := 0 to AList.count - 1 do
    s := s + AList[i]+#0;
  s := s + #0;

  if not ChangeServiceConfig(FHandle,  SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE,
          nil, nil, nil, pchar(s), nil, nil, nil) then
    RaiseLastWin32Error;
end;


function TServiceHandle.GetAutoStart: Boolean;
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastWin32Error;
  result := LConfig[0].dwStartType = SERVICE_AUTO_START;
end;

procedure TServiceHandle.SetAutoStart(const Value: Boolean);
var
  v : DWord;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if Value then
    v := SERVICE_AUTO_START
  else
    v := SERVICE_DEMAND_START;

  if not ChangeServiceConfig(FHandle,  SERVICE_NO_CHANGE, v, SERVICE_NO_CHANGE,
          nil, nil, nil, nil, nil, nil, nil) then
    RaiseLastWin32Error;
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
    RaiseLastWin32Error;
end;

function TServiceHandle.AccountName: AnsiString;
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastWin32Error;
  result := LConfig[0].lpServiceStartName;
end;

procedure TServiceHandle.SetAccount(aUser, aPassword: String);
var
  LNeeded : DWord;
  LConfig : array [0..11] of _QUERY_SERVICE_CONFIGA;
begin
  if FHandle = 0 then
    raise EWin32Error.Create(SysErrorMessage(FHndErr));
  if not QueryServiceConfig(FHandle, @LConfig[0], sizeof(_QUERY_SERVICE_CONFIGA)* 10, LNeeded) then
    RaiseLastWin32Error;
  if (aUser = '') and (aPassword = '') Then
  Begin
    if not changeServiceConfig(FHandle, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, lConfig[0].dwErrorControl, nil,
                       nil, nil, nil, 'LocalSystem', nil, nil) Then
      RaiseLastWin32Error;
  End
  Else
  Begin
    if not changeServiceConfig(FHandle, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, lConfig[0].dwErrorControl, nil,
                      nil, nil, nil, pchar(AUser), pchar(APassword), nil) Then
      RaiseLastWin32Error;
  End;
end;
end.

