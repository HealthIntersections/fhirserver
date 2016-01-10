; todo: spellchecking
; todo: test v1 upgrade. Test evaluation certificate
; todo: mysql license

[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRServer
AppName=Health Intersections FHIR Server (DEV)
AppVerName=1.0.28 (FHIR Version 1.2.0.7526)

; compilation control
OutputDir=C:\work\fhirserver\install\build
OutputBaseFilename=fhirserver21-1.0.28
Compression=lzma2/ultra64

; 64 bit
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64
                     
; we will be creating a service so we do need this privilege
PrivilegesRequired=admin
AllowUNCPath=no
AlwaysShowDirOnReadyPage=yes
AlwaysShowGroupOnReadyPage=yes
ChangesAssociations=yes
DirExistsWarning=auto
DisableStartUpPrompt=yes
MinVersion=0,5.0
UninstallDisplayIcon=C:\work\fhirserver\Server\fhir.ico
WizardStyle=modern

; directory management
DefaultDirName={pf}\FHIRServer
DefaultGroupName=FHIRServer
UninstallFilesDir={app}

; win2000+ add/remove programs support
AppPublisher=Health Intersections P/L
AppPublisherURL=http://www.healthintersections.com.au
AppVersion=0.01
AppSupportURL=https://github.com/grahamegrieve/fhirserver
AppUpdatesURL=https://github.com/grahamegrieve/fhirserver
AppCopyright=Copyright © Health Intersections Pty Ltd 2011 - 2013
VersionInfoVersion=0.0.0.1

; dialog support
LicenseFile=C:\work\fhirserver\license
InfoBeforeFile=C:\work\fhirserver\install\readme.txt

; directories
;  {app}  - executable, ini file
;  {app}\web - FHIR server specific web content
;  {app}\spec - FHIR specification itself
;
;  {store}\data - terminology caches

[Types]
Name: "normal";   Description: "Normal Installation"; Flags: iscustom;

[Components]

[Tasks]
; Core related tasks
Name: svcInst;   Description: "Install FHIR Server as a Service"
Name: firewall;  Description: "Allow FHIR Server through the Firewall"

[Files]

; root documentation files
Source: "C:\work\fhirserver\license";                                 DestDir: "{app}";            Flags: ignoreversion;      DestName: "license.txt";
Source: "C:\work\fhirserver\readme.md";                               DestDir: "{app}";            Flags: ignoreversion;      DestName: "readme.txt";
Source: "C:\work\fhirserver\install\readme.txt";                      DestDir: "{app}";            Flags: ignoreversion;      DestName: "post-install.txt";
Source: "C:\work\fhirserver\install\LOINC_short_license.txt";         DestDir: "{app}";            Flags: ignoreversion;

; Executable files
Source: "C:\work\fhirserver\Exec\FHIRServer21.exe";        DestDir: "{app}";     DestName: "FHIRServer.exe";       Flags: ignoreversion

Source: "C:\work\fhirserver\Exec\fhir.ini";                           DestDir: "{app}";            Flags: ignoreversion onlyifdoesntexist;       DestName: "fhirserver.ini" 
Source: "C:\work\fhirserver\Exec\auth.example.ini";                   DestDir: "{app}";            Flags: ignoreversion onlyifdoesntexist;       DestName: "auth.ini" 
Source: "C:\work\fhirserver\Libraries\FMM\FastMM_FullDebugMode.dll";  DestDir: "{app}";            Flags: ignoreversion

; Web resources
Source: "C:\work\fhirserver\web\*.*";                                   DestDir: "{app}\web";        Flags: ignoreversion recursesubdirs;
Source: "C:\work\org.hl7.fhir\build\publish\*.*";                       DestDir: "{app}\spec";       Flags: ignoreversion recursesubdirs;  Excludes: "*.zip"
Source: "C:\work\org.hl7.fhir\build\publish\examples.zip";                       DestDir: "{app}\spec";       Flags: ignoreversion;
Source: "C:\work\org.hl7.fhir\build\publish\validation-min.xml.zip";            DestDir: "{app}\spec";       Flags: ignoreversion recursesubdirs;  

; Terminology resources
Source: "C:\work\fhirserver\Exec\ucum-essence.xml";                   DestDir: "{commonappdata}\FHIRServer"
Source: "C:\ProgramData\FHIRServer\dicom.cache";                      DestDir: "{commonappdata}\FHIRServer"
Source: "C:\ProgramData\FHIRServer\loinc.cache";                      DestDir: "{commonappdata}\FHIRServer"
Source: "C:\work\fhirserver\sql\*.sql";                               DestDir: "{app}\sql"

; ssl support files - put in app dir because these may be different to ones already on the machine.
Source: "C:\work\fhirserver\Exec\ssleay64.dll";  DestName: "ssleay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\libeay64.dll";  DestName: "libeay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\openssl64.exe"; DestName: "openssl.dll";    DestDir: "{app}";      Flags: ignoreversion

[INI]
Filename: "{app}\fhirserver.ini"; Section: "fhir";  Key: "source"; String: "{app}\spec"
Filename: "{app}\fhirserver.ini"; Section: "fhir";  Key: "other";  String: "{app}\web"
Filename: "{app}\fhirserver.ini"; Section: "loinc"; Key: "cache";  String: "{commonappdata}\FHIRServer\loinc.cache"
Filename: "{app}\fhirserver.ini"; Section: "ucum"; Key: "cache";  String: "{commonappdata}\FHIRServer\ucum-essence.xml"
Filename: "{app}\fhirserver.ini"; Section: "dicom"; Key: "cache";  String: "{commonappdata}\FHIRServer\dicom.cache"
Filename: "{app}\fhirserver.ini"; Section: "web";  Key: "clients";  String: "{app}\auth.ini"

[Icons]
Name: "{group}\FHIR Server";        Filename: "{app}\FHIRServer.exe";     Parameters: "-debug";  WorkingDir: "{app}"    
Name: "{group}\Ini File";           Filename: "{app}\fhirserver.ini";         
Name: "{group}\FHIR Specification"; Filename: "{app}\web\index.html";         

[Code]
const
  MB_ICONINFORMATION = $40;

// ------ Interfaces ---------------------------------------------------------------------------------

type
  _SERVICE_STATUS = record
    dwServiceType: Longword;
    dwCurrentState: Longword;
    dwControlsAccepted: Longword;
    dwWin32ExitCode: Longword;
    dwServiceSpecificExitCode: Longword;
    dwCheckPoint: Longword;
    dwWaitHint: Longword;
  end;

 SERVICE_CONFIG = record
   dwServiceType: Longword;
   dwStartType: Longword;
   dwErrorControl: Longword;
   lpBinaryPathName: Longword;
   lpLoadOrderGroup: Longword;
   dwTagId: Longword;
   lpDependencies: Longword;
   lpServiceStartName: pansichar;
   lpDisplayName: Longword;
   buffer : Array [ 1..258] of AnsiChar;
 end;
 TServiceConfig = SERVICE_CONFIG;

 SERVICE_CONFIG2 = record // SERVICE_DELAYED_AUTO_START_INFO
   fDelayedAutoStart: LongWord;
 end;
 TServiceConfig2 = SERVICE_CONFIG2;

const
  NO_ERROR = 0;
  STANDARD_RIGHTS_REQUIRED = $F0000;

  //
  // Service Control Manager object specific access types
  //
  SC_MANAGER_CONNECT = $0001;
  SC_MANAGER_CREATE_SERVICE = $0002;
  SC_MANAGER_ENUMERATE_SERVICE = $0004;
  SC_MANAGER_LOCK = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;
  SC_MANAGER_ALL_ACCESS  = (STANDARD_RIGHTS_REQUIRED + SC_MANAGER_CONNECT + SC_MANAGER_CREATE_SERVICE + SC_MANAGER_ENUMERATE_SERVICE + SC_MANAGER_LOCK + SC_MANAGER_QUERY_LOCK_STATUS + SC_MANAGER_MODIFY_BOOT_CONFIG);

  //
  // No change constant
  //
  SERVICE_NO_CHANGE = $FFFFFFFF;

  //
  // Service Types (Bit Mask)
  //
  SERVICE_KERNEL_DRIVER = $00000001;
  SERVICE_FILE_SYSTEM_DRIVER = $00000002;
  SERVICE_ADAPTER = $00000004;
  SERVICE_RECOGNIZER_DRIVER = $00000008;
  SERVICE_DRIVER = (SERVICE_KERNEL_DRIVER + SERVICE_FILE_SYSTEM_DRIVER + SERVICE_RECOGNIZER_DRIVER);

  SERVICE_WIN32_OWN_PROCESS = $00000010;
  SERVICE_WIN32_SHARE_PROCESS = $00000020;
  SERVICE_WIN32 = (SERVICE_WIN32_OWN_PROCESS + SERVICE_WIN32_SHARE_PROCESS);
  SERVICE_INTERACTIVE_PROCESS = $00000100;
  SERVICE_TYPE_ALL = (SERVICE_WIN32 + SERVICE_ADAPTER + SERVICE_DRIVER + SERVICE_INTERACTIVE_PROCESS);

  //
  // Start Type
  //
  SERVICE_BOOT_START = $00000000;
  SERVICE_SYSTEM_START = $00000001;
  SERVICE_AUTO_START = $00000002;
  SERVICE_DEMAND_START = $00000003;
  SERVICE_DISABLED = $00000004;

  //
  // Error control type
  //
  SERVICE_ERROR_IGNORE = $00000000;
  SERVICE_ERROR_NORMAL = $00000001;
  SERVICE_ERROR_SEVERE = $00000002;
  SERVICE_ERROR_CRITICAL = $00000003;

  //
  // Service object specific access type
  //
  SERVICE_QUERY_CONFIG = $0001;
  SERVICE_CHANGE_CONFIG = $0002;
  SERVICE_QUERY_STATUS = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START= $0010;
  SERVICE_STOP= $0020;
  SERVICE_PAUSE_CONTINUE = $0040;
  SERVICE_INTERROGATE = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;
  SERVICE_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED + SERVICE_QUERY_CONFIG + SERVICE_CHANGE_CONFIG + SERVICE_QUERY_STATUS + SERVICE_ENUMERATE_DEPENDENTS + SERVICE_START + SERVICE_STOP + SERVICE_PAUSE_CONTINUE + SERVICE_INTERROGATE + SERVICE_USER_DEFINED_CONTROL);

  //
  // Controls
  //
  SERVICE_CONTROL_STOP = $00000001;
  SERVICE_CONTROL_PAUSE = $00000002;
  SERVICE_CONTROL_CONTINUE = $00000003;
  SERVICE_CONTROL_INTERROGATE = $00000004;

  //
  // Status
  //
  SERVICE_CONTINUE_PENDING = $00000005;
  SERVICE_PAUSE_PENDING = $00000006;
  SERVICE_PAUSED = $00000007;
  SERVICE_RUNNING = $00000004;
  SERVICE_START_PENDING = $00000002;
  SERVICE_STOP_PENDING = $00000003;
  SERVICE_STOPPED = $00000001;


  //
  //  Error codes
  //
  ERROR_DEPENDENT_SERVICES_RUNNING = 1051;
  ERROR_INVALID_SERVICE_CONTROL = 1052;
  ERROR_SERVICE_REQUEST_TIMEOUT = 1053;
  ERROR_SERVICE_NO_THREAD = 1054;
  ERROR_SERVICE_DATABASE_LOCKED = 1055;
  ERROR_SERVICE_ALREADY_RUNNING = 1056;
  ERROR_INVALID_SERVICE_ACCOUNT = 1057;
  ERROR_SERVICE_DISABLED = 1058;
  ERROR_CIRCULAR_DEPENDENCY = 1059;
  ERROR_SERVICE_DOES_NOT_EXIST = 1060;
  ERROR_SERVICE_CANNOT_ACCEPT_CTRL = 1061;
  ERROR_SERVICE_NOT_ACTIVE = 1062;
  ERROR_FAILED_SERVICE_CONTROLLER_CONNECT = 1063;
  ERROR_EXCEPTION_IN_SERVICE = 1064;
  ERROR_DATABASE_DOES_NOT_EXIST = 1065;
  ERROR_SERVICE_SPECIFIC_ERROR = 1066;
  ERROR_PROCESS_ABORTED = 1067;
  ERROR_SERVICE_DEPENDENCY_FAIL = 1068;
  ERROR_SERVICE_LOGON_FAILED = 1069;
  ERROR_SERVICE_START_HANG = 1070;
  ERROR_INVALID_SERVICE_LOCK = 1071;
  ERROR_SERVICE_MARKED_FOR_DELETE = 1072;
  ERROR_SERVICE_EXISTS = 1073;


function OpenSCManager(lpMachineName: Ansistring; lpDatabaseName: Ansistring; dwDesiredAccess: Longword): Longword; external 'OpenSCManagerA@advapi32.dll stdcall';
function OpenService(hSCManager: Longword; lpServiceName: Ansistring; dwDesiredAccess: Longword): Longword; external 'OpenServiceA@advapi32.dll stdcall';
function StartService(hService: Longword; dwNumServiceArgs: Longword; lpServiceArgVectors: PAnsiChar): Longword; external 'StartServiceA@advapi32.dll stdcall';
function CloseServiceHandle(hSCObject: Longword): Longword; external 'CloseServiceHandle@advapi32.dll stdcall';
function ControlService(hService: Longword; dwControl: Longword; var lpServiceStatus: _SERVICE_STATUS): Longword; external 'ControlService@advapi32.dll stdcall';
function CreateService(hSCManager: Longword; lpServiceName: Ansistring; lpDisplayName: Ansistring; dwDesiredAccess: Longword; dwServiceType: Longword; dwStartType: Longword; dwErrorControl: Longword; lpBinaryPathName: Ansistring;
                       lpLoadOrderGroup: Ansistring; lpdwTagId: Longword; lpDependencies: Ansistring; lpServiceStartName: Ansistring; lpPassword: Ansistring): Longword; external 'CreateServiceA@advapi32.dll stdcall';
function DeleteService(hService: Longword): Longword; external 'DeleteService@advapi32.dll stdcall';
function ChangeServiceConfig(hService: Longword; dwServiceType: Longword; dwStartType: Longword; dwErrorControl: Longword; lpBinaryPathName: PAnsiChar; lpLoadOrderGroup: PAnsiChar; lpdwTagId: Longword;
                       lpDependencies: PAnsiChar; lpServiceStartName: PAnsiChar; lpPassword: PAnsiChar; lpDisplayName: PAnsiChar): Longword; external 'ChangeServiceConfigA@advapi32.dll stdcall';
function LockServiceDatabase(hSCManager: Longword): Longword; external 'LockServiceDatabase@advapi32.dll stdcall';
function UnlockServiceDatabase(ScLock: Longword): Longword; external 'UnlockServiceDatabase@advapi32.dll stdcall';
function QueryServiceConfig(hService: Longword; var lpServiceConfig: TServiceConfig; cbBufSize: Cardinal; var pcbBytesNeeded: Cardinal) : Boolean; external 'QueryServiceConfigA@advapi32.dll stdcall';
function QueryServiceConfig2(hService: Longword; dwInfoLevel : Longword; var lpServiceConfig: TServiceConfig2; cbBufSize: Cardinal; var pcbBytesNeeded: Cardinal) : Boolean; external 'QueryServiceConfig2A@advapi32.dll stdcall';
function ChangeServiceConfig2(hService: Longword; dwInfoLevel : Longword; lpServiceConfig: TServiceConfig2) : Longword; external 'ChangeServiceConfig2A@advapi32.dll stdcall';

function SimpleCreateService(AServiceName, ADisplayName, AFileName: Ansistring; AStartType: Longword; AUser, APassword: Ansistring; Interactive: Boolean; IgnoreExisting: Boolean): Boolean;
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  ServiceType: Longword;
  Error: Integer;
begin
  Result := False;
  ServiceType := SERVICE_WIN32_OWN_PROCESS;
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleCreateService: ' + AServiceName + ' ' + SysErrorMessage(DLLGetLastError));
    try
      if AUser = '' then
      begin
        if Interactive then
          ServiceType := ServiceType + SERVICE_INTERACTIVE_PROCESS;
        APassword := '';
      end;
      ServiceHandle := CreateService(SCMHandle, AServiceName, ADisplayName, SERVICE_ALL_ACCESS, ServiceType, AStartType, SERVICE_ERROR_NORMAL, AFileName, '', 0, '', AUser, APassword);
      if ServiceHandle = 0 then
      begin
        Error := DLLGetLastError;
        if IgnoreExisting and (Error = ERROR_SERVICE_EXISTS) then
          Exit
        else
          RaiseException('CreateService@SimpleCreateService: ' + AServiceName + ' ' + SysErrorMessage(Error));
      end;
      Result := True;
    finally
      if ServiceHandle <> 0 then
        CloseServiceHandle(ServiceHandle);
    end;
  finally
    if SCMHandle <> 0 then
      CloseServiceHandle(SCMHandle);
  end;
end;

function SimpleUpdateService(AServiceName, AFileName: Ansistring; AStartType: Longword; AUser, APassword: Ansistring) : Boolean;
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  ServiceType: Longword;
  pcbBytesNeeded: Cardinal;
  Error: Integer;
  lpServiceConfig: TServiceConfig;
begin
  Result := False;
  ServiceType := SERVICE_WIN32_OWN_PROCESS;
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleCreateService: ' + AServiceName + ' ' +SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AServiceName, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleStartService: ' + AServiceName + ' ' + SysErrorMessage(DLLGetLastError));
      if QueryServiceConfig(ServiceHandle, lpServiceConfig, 294, pcbBytesNeeded) Then
        result := ChangeServiceConfig(ServiceHandle, lpServiceConfig.dwServiceType, AStartType, lpServiceConfig.dwErrorControl, AFileName,
                      '', 0, '', AUser, APassword, '') <> 0;
    finally
      if ServiceHandle <> 0 then
        CloseServiceHandle(ServiceHandle);
    end;
  finally
    if SCMHandle <> 0 then
      CloseServiceHandle(SCMHandle);
  end;
end;

function WaitForService(ServiceHandle: Longword; AStatus: Longword): Boolean;
var
  PendingStatus: Longword;
  ServiceStatus: _SERVICE_STATUS;
  Error: Integer;
begin
  Result := False;

  case AStatus of
    SERVICE_RUNNING: PendingStatus := SERVICE_START_PENDING;
    SERVICE_STOPPED: PendingStatus := SERVICE_STOP_PENDING;
  end;

  repeat
    if ControlService(ServiceHandle, SERVICE_CONTROL_INTERROGATE, ServiceStatus) = 0 then
    begin
      Error := DLLGetLastError;
      RaiseException('ControlService@WaitForService: ' + SysErrorMessage(Error));
    end;
    if ServiceStatus.dwWin32ExitCode <> 0 then
      Break;
    Result := ServiceStatus.dwCurrentState = AStatus;
    if not Result and (ServiceStatus.dwCurrentState = PendingStatus) then
      Sleep(ServiceStatus.dwWaitHint)
    else
      Break;
  until Result;
end;

procedure SimpleStopService(AService: Ansistring; Wait, IgnoreStopped: Boolean);
var
  ServiceStatus: _SERVICE_STATUS;
  SCMHandle: Longword;
  ServiceHandle: Longword;
  Error: Integer;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleStopService: ' + AService + ' ' + SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleStopService: ' + AService + ' ' + SysErrorMessage(DLLGetLastError));
      try
        if ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus) = 0 then
        begin
          Error := DLLGetLastError;
          if IgnoreStopped and (Error = ERROR_SERVICE_NOT_ACTIVE) then
            Exit
          else
            RaiseException('ControlService@SimpleStopService: ' + AService + ' ' +
              SysErrorMessage(Error));
          if Wait then
            WaitForService(ServiceHandle, SERVICE_STOPPED);
        end;
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

procedure SimpleStartService(AService: Ansistring; Wait, IgnoreStarted: Boolean);
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  Error: Integer;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleStartService: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleStartService: ' + AService + ' ' +
          SysErrorMessage(DLLGetLastError));
      try
        if StartService(ServiceHandle, 0, '') = 0 then
        begin
          Error := DLLGetLastError;
          if IgnoreStarted and (Error = ERROR_SERVICE_ALREADY_RUNNING) then
            Exit
          else
            RaiseException('StartService@SimpleStartService: ' + AService + ' ' +
              SysErrorMessage(Error));
          if Wait then
          begin
            WaitForService(ServiceHandle, SERVICE_RUNNING);
          end;
        end;
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

procedure SimpleDeleteService(AService: Ansistring);
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleDeleteService: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleDeleteService: ' + AService + ' ' +
          SysErrorMessage(DLLGetLastError));
      try
        if DeleteService(ServiceHandle) = 0 then
          RaiseException('StartService@SimpleDeleteService: ' + AService + ' ' +
            SysErrorMessage(DLLGetLastError));
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

procedure SimpleSetServiceStartup(AService: Ansistring; AStartupType: Longword);
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('SimpleSetServiceStartup@OpenSCManager: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('SimpleSetServiceStartup@OpenService: ' + AService + ' ' +
          SysErrorMessage(DLLGetLastError));
      try
        if ChangeServiceConfig(ServiceHandle, SERVICE_NO_CHANGE, AStartupType, SERVICE_NO_CHANGE,
          '', '', 0, '', '', '', '') = 0 then
          RaiseException('SimpleSetServiceStartup@SetServiceStartup: ' + AService + ' ' +
            SysErrorMessage(DLLGetLastError));
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

procedure SimpleSetServiceDelayed(AService: Ansistring; bDelayed : Boolean);
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  lConfig : TServiceConfig2;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('SimpleSetServiceStartup@OpenSCManager: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('SimpleSetServiceStartup@OpenService: ' + AService + ' ' +
          SysErrorMessage(DLLGetLastError));
      try
        if bDelayed Then
          lConfig.fDelayedAutostart := 1
        else
          lConfig.fDelayedAutostart := 0;

        if ChangeServiceConfig2(ServiceHandle, {SERVICE_CONFIG_DELAYED_AUTO_START_INFO}3, lConfig) = 0 then
          RaiseException('SimpleSetServiceStartup@ChangeServiceConfig2: ' + AService + ' ' +
            SysErrorMessage(DLLGetLastError));
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

function ServiceExists(AService: Ansistring): Boolean;
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  Error: Integer;
begin
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@ServiceExists: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      try
        if ServiceHandle = 0 then
        begin
          Error := DLLGetLastError;
          if Error = ERROR_SERVICE_DOES_NOT_EXIST then
            Result := False
          else
            RaiseException('OpenService@ServiceExists: ' + AService + ' ' +
              SysErrorMessage(Error));
        end
        else
          Result := True;
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

function SimpleQueryService(AService: Ansistring): Longword;
var
  ServiceStatus: _SERVICE_STATUS;
  SCMHandle: Longword;
  ServiceHandle: Longword;
  Error: Integer;
begin
  Result := 0;
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    if SCMHandle = 0 then
      RaiseException('OpenSCManager@SimpleQueryService: ' + AService + ' ' +
        SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AService, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleQueryService: ' + AService + ' ' +
          SysErrorMessage(DLLGetLastError));
      try
        if ControlService(ServiceHandle, SERVICE_CONTROL_INTERROGATE, ServiceStatus) = 0 then
        begin
          Error := DLLGetLastError;
          RaiseException('ControlService@SimpleQueryService: ' + AService + ' ' +
            SysErrorMessage(Error));
        end;
        Result := ServiceStatus.dwCurrentState;
      finally
        if ServiceHandle <> 0 then
          CloseServiceHandle(ServiceHandle);
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  except
    ShowExceptionMessage;
  end;
end;

Function GetServiceUser(aServiceName : String) : String;
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  ServiceType: Longword;
  pcbBytesNeeded: Cardinal;
  Error: Integer;
  lpServiceConfig: TServiceConfig;
begin
  Result := '';
  ServiceType := SERVICE_WIN32_OWN_PROCESS;
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    try
      ServiceHandle := OpenService(SCMHandle, AServiceName, SERVICE_ALL_ACCESS);
      if QueryServiceConfig(ServiceHandle, lpServiceConfig, 294, pcbBytesNeeded) Then
        result := string(lpServiceConfig.lpServiceStartName);
    finally
      if ServiceHandle <> 0 then
        CloseServiceHandle(ServiceHandle);
    end;
  finally
    if SCMHandle <> 0 then
      CloseServiceHandle(SCMHandle);
  end;
End;

Function GetServiceStartIndex(aServiceName : String) : Integer;
var
  SCMHandle: Longword;
  ServiceHandle: Longword;
  ServiceType: Longword;
  pcbBytesNeeded: Cardinal;
  Error: Integer;
  lpServiceConfig: TServiceConfig;
  lpServiceConfig2: TServiceConfig2;
  version : TWindowsVersion;
Begin
  GetWindowsVersionEx(version);
  Result := 2;
  ServiceType := SERVICE_WIN32_OWN_PROCESS;
  try
    SCMHandle := OpenSCManager('', '', SC_MANAGER_ALL_ACCESS);
    try
      ServiceHandle := OpenService(SCMHandle, AServiceName, SERVICE_ALL_ACCESS);
      if QueryServiceConfig(ServiceHandle, lpServiceConfig, 294, pcbBytesNeeded) Then
        Case lpServiceConfig.dwStartType of
          SERVICE_AUTO_START :
            Begin
             if (version.Major >= 6) And QueryServiceConfig2(ServiceHandle, {SERVICE_CONFIG_DELAYED_AUTO_START_INFO}3, lpServiceConfig2, 4, pcbBytesNeeded) And (lpServiceConfig2.fDelayedAutoStart > 0) Then
               result := 3
             Else
               result := 0;
            End;
          SERVICE_DISABLED : result := 2;
        Else
          Result := 1;
        End;
    finally
      if ServiceHandle <> 0 then
        CloseServiceHandle(ServiceHandle);
    end;
  finally
    if SCMHandle <> 0 then
      CloseServiceHandle(SCMHandle);
  end;
End;


const
  NET_FW_SCOPE_ALL = 0;
  NET_FW_IP_VERSION_ANY = 2;


procedure SetFirewallException(AppName,FileName:string);
var
  FirewallObject: Variant;
  FirewallManager: Variant;
  FirewallProfile: Variant;
begin
  try
    FirewallObject := CreateOleObject('HNetCfg.FwAuthorizedApplication');
    FirewallObject.ProcessImageFileName := FileName;
    FirewallObject.Name := AppName;
    FirewallObject.Scope := NET_FW_SCOPE_ALL;
    FirewallObject.IpVersion := NET_FW_IP_VERSION_ANY;
    FirewallObject.Enabled := True;
    FirewallManager := CreateOleObject('HNetCfg.FwMgr');
    FirewallProfile := FirewallManager.LocalPolicy.CurrentProfile;
    FirewallProfile.AuthorizedApplications.Add(FirewallObject);
  except
    MsgBox('Unable to Register '+AppName+' exemption with the firewall: '+GetExceptionMessage, mbInformation, MB_OK)
  end;
end;


procedure RemoveFirewallException( FileName:string );
var
  FirewallManager: Variant;
  FirewallProfile: Variant;
begin
  try
    FirewallManager := CreateOleObject('HNetCfg.FwMgr');
    FirewallProfile := FirewallManager.LocalPolicy.CurrentProfile;
    FireWallProfile.AuthorizedApplications.Remove(FileName);
  except
  end;
end;


// ------ Wizard -------------------------------------------------------------------------------------

var
  ServicePage : TInputQueryWizardPage;
  cbxStartup : TNewComboBox;

         
Procedure LookupUser(Sender : TObject);
begin
  MsgBox('not done yet. sorry', mbError, MB_OK);
End;

const LOGON32_LOGON_INTERACTIVE = 2;
const LOGON32_PROVIDER_DEFAULT = 0;
Const LOGON32_LOGON_SERVICE = 5;

function LogonUser(lpszUsername,lpszDomain,lpszPassword: STRING; dwLogonType,dwLogonProvider: INTEGER; var phToken: INTEGER): INTEGER; external 'LogonUserW@advapi32.dll stdcall';

Function CheckLogin : Boolean;
var
  i : integer;
  d, u , p: String;
  LogonToken: INTEGER;
Begin
  i := pos('\', ServicePage.Values[0]);
  if i = 0 then
  Begin
    d := '';
    u := ServicePage.Values[0];
  End
  Else
  Begin
    d := copy(ServicePage.Values[0], 1, i-1);
    u := copy(ServicePage.Values[0], i+1, $FF);
  End;
  p := ServicePage.Values[1];
  i := LogonUser(u, d, p, LOGON32_LOGON_SERVICE, LOGON32_PROVIDER_DEFAULT, LogonToken);
  result := i > 0;
End;




Procedure CreateServiceUserPage;
var
  lbl : TLabel;
  btn : TButton;
  version : TWindowsVersion;
Begin
  GetWindowsVersionEx(version);
  ServicePage := CreateInputQueryPage(wpSelectTasks, 'Service Details', 'Configure the FHIRServer Service', 'Leave Blank to run as local system, or DOMAIN\Username and password');
  ServicePage.Add('User:', False);
  ServicePage.Add('Password:', True);

  btn := TButton.Create(ServicePage);
  btn.Caption := '...';
  btn.Top := 62;
  btn.Left := ServicePage.SurfaceWidth - 20;
  btn.Width := 20;
  btn.Parent := ServicePage.Surface;
  btn.Height := 20;
  btn.OnClick := @LookupUser;

  lbl := TLabel.Create(ServicePage);
  lbl.Caption := 'Start Up Type:';
  lbl.Top := 130;
  lbl.Parent := ServicePage.Surface;

  cbxStartup := TNewComboBox.Create(ServicePage);
  cbxStartup.Top := 145;
  cbxStartup.Width := ServicePage.SurfaceWidth;
  cbxStartup.Parent := ServicePage.Surface;
  cbxStartup.Style := csDropDownList;
  cbxStartup.Items.Add('Automatic');
  cbxStartup.Items.Add('Manual');
  cbxStartup.Items.Add('Disabled');
  If version.Major >= 6 Then
    cbxStartup.Items.Add('Automatic - Delayed Start');
  cbxStartup.ItemIndex := 0;

  If ServiceExists('HL7Connect') Then
  Begin
    ServicePage.Values[0] := GetServiceUser('FHIRServer');
    if (ServicePage.Values[0] = 'Local System') or (ServicePage.Values[0] = 'LocalSystem') then
      ServicePage.Values[0] := '';
    cbxStartup.ItemIndex := GetServiceStartIndex('HL7Connect');
  End;
End;


function NextButtonClick(CurPageID: Integer): Boolean;
var
  s : String;
begin
  if (CurpageID = ServicePage.Id) Then
  Begin
    if (ServicePage.Values[0] <> '') or (ServicePage.Values[1] <> '') Then
    Begin
      if not CheckLogin Then
        result := MsgBox('Unable to login using this account ('+SysErrorMessage(DLLGetLastError)+'). '+#13#10#13#10+'This may be because the details are wrong, or maybe the installer doesn''t have the rights to log on in service mode. Would you like to continue anyway?',
           mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDYES
      else
        result := true;
    End
    Else
      result := true;
  End
  Else
    Result := True;
end;

Function IsWindows2008 : Boolean;
var
  ver : TWindowsVersion;
Begin
  GetWindowsVersionEx(ver);
  result := ((ver.ProductType = VER_NT_DOMAIN_CONTROLLER) or (ver.ProductType = VER_NT_SERVER)) and (ver.Major >= 6);
End;

procedure InitializeWizard();
Begin
  CreateServiceUserPage;
End;


// ------ Installation Logic -------------------------------------------------------------------------

Function DetermineStartMode : Integer;
Begin
  case cbxStartup.ItemIndex of
    0: result := SERVICE_AUTO_START;
    1: result := SERVICE_DEMAND_START;
    2: result := SERVICE_DISABLED;
    3: result := SERVICE_AUTO_START;
  End;
End;

function BoolToInt(b : Boolean):Integer;
begin
  if b then
    result := 1
  else
    result := 0;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  mode : Longword;
  s : String;
  noauto : integer;
Begin
  if (CurStep = ssInstall) Then
  Begin
    if ServiceExists('HL7Connect') Then
      SimpleStopService('HL7Connect', true, true);
  End;

  if (CurStep = ssPostInstall)  Then
  Begin
    If IsTaskSelected('firewall') Then
      SetFirewallException('Fhir Server', ExpandConstant('{app}')+'\FHIRServer.exe');
  End;
End;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
Begin
  if (CurUninstallStep = usUninstall) And ServiceExists('FHIRServer') Then
  Begin
    SimpleStopService('FHIRServer', true, true);
    SimpleDeleteService('FHIRServer');
    sleep(1000);
  End;

  if (CurUninstallStep = usUninstall) Then
  Begin
    RemoveFirewallException(ExpandConstant('{app}')+'FHIRServer.exe');
  End;
End;
