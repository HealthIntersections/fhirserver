unit ServiceController;

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

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
uses
  Classes,
  AdvObjects,
  Windows,
  WinSvc;

const
  USER_CONTROL_OFFSET = $80;

type
  TServiceManagerHandle = class (TAdvObject)
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

  TServiceHandle = class (TAdvObject)
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
  SysUtils;

const
  ASSERT_UNIT = 'ServiceController';

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