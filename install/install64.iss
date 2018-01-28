
[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRServer
AppName=Health Intersections FHIR Server
AppVerName=Version 1.0.204

; compilation control
OutputDir=C:\work\fhirserver\install\build
OutputBaseFilename=fhirserver64-1.0.204
Compression=lzma2/ultra64

; 64 bit
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64

; we might be creating a service so we do need this privilege
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
DisableDirPage=false

; directory management
DefaultDirName={pf}\FHIRServer
DefaultGroupName=FHIR Applications
UninstallFilesDir={app}

; win2000+ add/remove programs support
AppPublisher=Health Intersections P/L
AppPublisherURL=http://www.healthintersections.com.au
AppVersion=0.01
AppSupportURL=https://github.com/grahamegrieve/fhirserver
AppUpdatesURL=https://github.com/grahamegrieve/fhirserver
AppCopyright=Copyright � Health Intersections Pty Ltd 2011+
VersionInfoVersion=0.0.0.1

; dialog support
LicenseFile=C:\work\fhirserver\license
InfoBeforeFile=C:\work\fhirserver\install\readme.rtf

; directories
;  {app}  - executable, ini file
;  {app}\web - FHIR server specific web content
;  {app}\spec - FHIR specification itself
;
;  {store}\data - terminology caches

#include <idp.iss>

#define use_msiproduct
#define use_vc2015
#define use_vc2017
#define use_sql2008express
#define use_mysqldb
;#define use_mysqlodbcinstalled
#define use_mysqlodbc

; shared code for installing the products
#include "scripts\products.iss"

; helper functions
#include "scripts\products\stringversion.iss"
#include "scripts\products\winversion.iss"
#include "scripts\products\fileversion.iss"
#include "scripts\products\dotnetfxversion.iss"



#ifdef use_msiproduct
#include "scripts\products\msiproduct.iss"
#endif
#ifdef use_vc2015
#include "scripts\products\vcredist2015.iss"
#endif
#ifdef use_vc2017
#include "scripts\products\vcredist2017.iss"
#endif
#ifdef use_sql2008express
#include "scripts\products\sql2008express.iss"
#endif
#ifdef use_mysqlodbc
#include "scripts\products\mysqlodbc.iss"
#endif
#ifdef use_mysqldb
#include "scripts\products\mysqldb.iss"
#endif


[Types]
Name: "fhir4";   Description: "Install R4 Version (Current Build)"
Name: "fhir3";   Description: "Install R3 Version"
Name: "fhir2";   Description: "Install DSTU2 Version"

[Components]
Name: "r2";   Description: "DSTU2 Components"; Types: fhir2
Name: "r3";   Description: "STU3 Components"; Types: fhir3
Name: "r4";   Description: "R4 Components"; Types: fhir4

[Tasks]
; Core related tasks
Name: dep; Description: "Install Dependencies"
Name: svcInst;   Description: "Install FHIR Server as a Service"
Name: svcInst\start;   Description: "Start FHIR Server after Installation"
Name: firewall;  Description: "Allow FHIR Server through the Firewall"
Name: sct; Description: "Import SNOMED CT (Requires RF2 Snapshot, ~1hr)"; Flags: unchecked
Name: db; Description: "Initialize Data Base";                            Flags: checkablealone
Name: db\pop; Description: "Populate Data Base (~30min)"

[Files]
; installer support
Source: "C:\work\fhirserver\install\installer.dll";               Flags: dontcopy;

; root documentation files
Source: "C:\work\fhirserver\license";                                 DestDir: "{app}";            Flags: ignoreversion;      DestName: "license.txt";
Source: "C:\work\fhirserver\readme.md";                               DestDir: "{app}";            Flags: ignoreversion;      DestName: "readme.txt";
Source: "C:\work\fhirserver\install\readme.rtf";                      DestDir: "{app}";            Flags: ignoreversion;      DestName: "documentation.rtf";
Source: "C:\work\fhirserver\install\LOINC_short_license.txt";         DestDir: "{app}";            Flags: ignoreversion;

; Executable files
Source: "C:\work\fhirserver\Exec\64\FHIRServer2.exe";        DestDir: "{app}";     DestName: "FHIRServer.exe";       Components: r2; Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\64\FHIRServer3.exe";        DestDir: "{app}";     DestName: "FHIRServer.exe";       Components: r3; Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\64\FHIRServer4.exe";        DestDir: "{app}";     DestName: "FHIRServer.exe";       Components: r4; Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\64\FHIRServerUtils.exe";    DestDir: "{app}";     DestName: "FHIRServerUtils.exe";                  Flags: ignoreversion

Source: "C:\work\fhirserver\Exec\fhir.ini";                           DestDir: "{app}";            Flags: ignoreversion onlyifdoesntexist;       DestName: "fhirserver.ini" 
Source: "C:\work\fhirserver\Libraries\FMM\FastMM_FullDebugMode.dll";      DestDir: "{app}";            Flags: ignoreversion
Source: "C:\work\fhirserver\Libraries\js\chakra\x64_release\ChakraCore.dll";                      DestDir: "{app}";            Flags: ignoreversion
Source: "C:\work\org.hl7.fhir\build\publish\org.hl7.fhir.validator.jar";  DestDir: "{app}";            Flags: ignoreversion

; Web resources
Source: "C:\work\fhirserver\web\*.*"; DestDir: {app}\web; Flags: ignoreversion recursesubdirs

; Spec & IGs
; R2
Source: "C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish\examples.zip";                DestDir: {app}\load;                                        Components: r2;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish\validation-min.json.zip";     DestDir: {app}\web;         DestName: validation.json.zip;  Components: r2;   Flags: ignoreversion  

; R3
Source: "C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3\build\publish\definitions.json.zip";        DestDir: {app}\web;                                         Components: r3;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3\build\publish\examples-json.zip";           DestDir: {app}\load;        DestName: fhir.json.zip;        Components: r3;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.us\core\output\examples.json.zip";                                 DestDir: {app}\load;        DestName: us-core.json.zip;     Components: r3;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.us\daf\output\examples.json.zip";                                  DestDir: {app}\load;        DestName: us-daf.json.zip;      Components: r3;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.us\sdc\output\examples.json.zip";                                  DestDir: {app}\load;        DestName: us-sdc.json.zip;      Components: r3;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir.us\sdcde\output\examples.json.zip";                                DestDir: {app}\load;        DestName: us-sdcde.json.zip;    Components: r3;   Flags: ignoreversion

; R4
Source: "C:\work\org.hl7.fhir\build\publish\definitions.json.zip";                               DestDir: {app}\web;                                         Components: r4;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir\build\publish\definitions.xml.zip";                                DestDir: {app}\web;                                         Components: r4;   Flags: ignoreversion
Source: "C:\work\org.hl7.fhir\build\publish\examples-json.zip";                                  DestDir: {app}\load;        DestName: fhir.json.zip;        Components: r4;   Flags: ignoreversion


; Load Data
Source: "C:\work\fhirserver\sql\nucc.xml";                                      DestDir: {app}\sql;     DestName: nucc.xml;            Components: r3 r4; Flags: ignoreversion
Source: "C:\work\fhirserver\sql\tslc.xml";                                      DestDir: {app}\sql;     DestName: tslc.xml;            Components: r3 r4; Flags: ignoreversion
Source: "C:\work\fhirserver\sql\cvx.xml";                             DestDir: {app}\sql;     DestName: cvx.xml;              Components: r3 r4; Flags: ignoreversion
Source: "C:\work\fhirserver\sql\icpc2.xml";                           DestDir: {app}\sql;     DestName: icpc2.xml;            Components: r3 r4; Flags: ignoreversion
Source: "C:\work\fhirserver\sql\country-codes.xml";                   DestDir: {app}\sql;     DestName: country-codes.xml;    Components: r3 r4; Flags: ignoreversion
Source: "C:\work\fhirserver\install\load.ini";                                  DestDir: {app}\load;                                   Components: r3 r4; Flags: ignoreversion

; Terminology resources
Source: "C:\work\fhirserver\Exec\ucum-essence.xml";                   DestDir: "{commonappdata}\FHIRServer"
Source: "C:\ProgramData\FHIRServer\loinc_261.cache";                  DestDir: "{commonappdata}\FHIRServer"
Source: "C:\work\fhirserver\sql\*.sql";                               DestDir: "{app}\sql"
Source: "C:\work\fhirserver\sql\*.txt";                               DestDir: "{app}\sql"

; ssl support files - put in app dir because these may be different to ones already on the machine.
Source: "C:\work\fhirserver\Exec\ssleay64.dll";  DestName: "ssleay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\libeay64.dll";  DestName: "libeay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\openssl64.exe"; DestName: "openssl.dll";    DestDir: "{app}";      Flags: ignoreversion

[INI]
Filename: "{app}\fhirserver.ini"; Section: "fhir";  Key: "web";  String: "{app}\web"
Filename: "{app}\fhirserver.ini"; Section: "loinc"; Key: "cache";  String: "{commonappdata}\FHIRServer\loinc_261.cache"
Filename: "{app}\fhirserver.ini"; Section: "ucum";  Key: "source";  String: "{commonappdata}\FHIRServer\ucum-essence.xml"
Filename: "{app}\fhirserver.ini"; Section: "dicom"; Key: "cache";  String: "{commonappdata}\FHIRServer\dicom.cache"
Filename: "{app}\fhirserver.ini"; Section: "web";   Key: "clients";  String: "{app}\auth.ini"
Filename: "{app}\fhirserver.ini"; Section: "lang";  Key: "source";  String: "{app}\sql\lang.txt"

[Icons]
Name: "{group}\FHIR Server (Desktop Mode)";  Filename: "{app}\FHIRServer.exe";     Parameters: "-debug";  WorkingDir: "{app}"    
Name: "{group}\FHIR Server Utilities";       Filename: "{app}\FHIRServerutils.exe";                       WorkingDir: "{app}"    
Name: "{group}\Documentation";               Filename: "{app}\documentation.rtf";         
Name: "{group}\Ini File";                    Filename: "{app}\fhirserver.ini";         

[Code]
const
  MB_ICONINFORMATION = $40;

var
  DependenciesPage : TWizardPage;
  ServicePage : TInputQueryWizardPage;
  cbxStartup : TNewComboBox;

  ConnectionPage : TInputQueryWizardPage;
  dbDriver : TNewComboBox;
  dbType : TNewComboBox;

  WebPage : TWizardPage;
  webHostName : TNewEdit;
  webOpenPort : TNewEdit;
  webOpenPath : TNewEdit;
  webSecurePort : TNewEdit;
  webSecurePath : TNewEdit;
  certFile : TNewEdit;
  caCertFile : TNewEdit;
  certPWord : TNewEdit;
  webpop : boolean;

  AdminPage : TInputQueryWizardPage;
  SctPage : TInputDirWizardPage;
  cbxModule : TNewComboBox;
  sctVersion : TNewEdit;
  ConfigPage : TInputOptionWizardPage;

  SctInstallPage : TOutputProgressWizardPage;
  LoadInstallPage : TOutputProgressWizardPage;

  VCStatus, MYSQLStatus, MSSQLStatus,  ODBCStatus : TLabel;


// start up check: a jre is required   

function InitializeSetup(): Boolean;
var
 ErrorCode: Integer;
 JavaInstalled : Boolean;
 ResultMsg : Boolean;
 Versions: TArrayOfString;
 I: Integer;
 regRoot: Integer;
begin
  Result := true;
{
  // Check which view of registry should be taken:
  regRoot := HKLM64;
  if not (RegGetSubkeyNames(regRoot, 'SOFTWARE\JavaSoft\Java Runtime Environment', Versions)) and not (RegGetSubkeyNames(regRoot, 'SOFTWARE\JavaSoft\Java Development Kit', Versions)) then
    JavaInstalled := false
  else
    for I := 0 to GetArrayLength(Versions)-1 do
      if JavaInstalled = true then
       //do nothing
      else if ( Versions[I][2]='.' ) and ( ( StrToInt(Versions[I][1]) > 1 ) or ( ( StrToInt(Versions[I][1]) = 1 ) and ( StrToInt(Versions[I][3]) >= 7 ) ) ) then
        JavaInstalled := true
      else
        JavaInstalled := false;
 
  Result := true;
  if not JavaInstalled then
  begin
    ResultMsg := MsgBox('Oracle Java v1.6 (64bit) or newer not found in the system. Java 1.6 or later is required to run this application (can be installed after this installation too). Do you want to continue?', mbConfirmation, MB_YESNO) = idYes;
    if ResultMsg = false then
      Result := false
    else
      ShellExec('open', 'http://www.java.com/getjava/', '','',SW_SHOWNORMAL,ewNoWait,ErrorCode);
  end;
}
end;

// ------ Interfaces ---------------------------------------------------------------------------------
type
  TMyCallback = procedure(IntParam: Integer; StrParam: WideString);

Function MyDllGetString(name : pansichar) : pansichar; external 'MyDllGetString@files:installer.dll stdcall setuponly';
Function MyDllCheckDatabase(DBDriver, Server, Database, Username, Password, Version : PAnsiChar) : PAnsiChar; external 'MyDllCheckDatabase@files:installer.dll stdcall setuponly';
Function MyDllInstallSnomed(ExeName, Source, Dest, Version : PAnsiChar; callback : TMyCallback) : PAnsiChar; external 'MyDllInstallSnomed@files:installer.dll stdcall setuponly';
Function MyDllInstallDatabase(ExeName, IniName, Password : PAnsiChar; load : PAnsiChar; callback : TMyCallback) : PAnsiChar; external 'MyDllInstallDatabase@files:installer.dll stdcall setuponly';

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
      RaiseException('OpenSCManager@SimpleUpdateService: ' + AServiceName + ' ' +SysErrorMessage(DLLGetLastError));
    try
      ServiceHandle := OpenService(SCMHandle, AServiceName, SERVICE_ALL_ACCESS);
      if ServiceHandle = 0 then
        RaiseException('OpenService@SimpleUpdateService: ' + AServiceName + ' ' + SysErrorMessage(DLLGetLastError));
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



procedure dbDriver_OnChange(Sender: TObject);
begin
if (pos('MySQL',dbdriver.text)<>0) then dbtype.itemIndex:=1;
if (pos('SQL Server',dbdriver.text)<>0) then dbtype.itemIndex:=0;
end;

Procedure CreateServiceUserPage;
var
  lbl : TLabel;
  btn : TButton;
  version : TWindowsVersion;
Begin
  GetWindowsVersionEx(version);
  ServicePage := CreateInputQueryPage(DependenciesPage.ID, 'Service Details', 'Configure the FHIRServer Service', 'Leave Blank to run as local system, or DOMAIN\Username and password');
  ServicePage.Add('User:', False);
  ServicePage.Add('Password:', True);

  btn := TButton.Create(ServicePage);
  btn.Caption := '...';
  btn.Top := ScaleX(62);
  btn.Left := ServicePage.SurfaceWidth - ScaleX(20);
  btn.Width := ScaleX(20);
  btn.Parent := ServicePage.Surface;
  btn.Height := ScaleX(20);
  btn.OnClick := @LookupUser;

  lbl := TLabel.Create(ServicePage);
  lbl.Caption := 'Start Up Type:';
  lbl.Top := ScaleX(130);
  lbl.Parent := ServicePage.Surface;

  cbxStartup := TNewComboBox.Create(ServicePage);
  cbxStartup.Top := ScaleX(150);
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


function ShouldSkipPage(PageID: Integer): Boolean;
begin
  if (PageID = DependenciesPage.Id) Then
    result := not IsTaskSelected('dep') else
  if (PageID = SctPage.Id) Then
    result := not IsTaskSelected('sct')
  else if (PageID = AdminPage.Id) or (PageID = ConfigPage.Id) Then
    result := not IsTaskSelected('db')
  else
    result := false;
end;


function NextButtonClick(CurPageID: Integer): Boolean;
var
  dbtypestr, s : String;
  p : integer;
  Dependencies_OK, ResultMsg:boolean;
  SQLSERVER_installed,VCREDIST_installed,MYSQLDB_installed,MYSQLODBC_installed:boolean;
  version:string;
  fver : String;
begin




// Check if Visual C++ Redist 2015 is installed - should support also 2017?
    VCREDIST_installed := 
      (msiproductupgrade(GetString(vcredist2015_upgradecode, vcredist2015_upgradecode_x64, ''), '15')) 
    OR (msiproductupgrade(GetString(vcredist2017_upgradecode, vcredist2017_upgradecode_x64, ''), '15')); 

// Check if SQL Express is installed - this should be improved - it will return false if the Full version is installed
    RegQueryStringValue(HKLM, 'SOFTWARE\Microsoft\Microsoft SQL Server\SQLEXPRESS\MSSQLServer\CurrentVersion', 'CurrentVersion', version);
    SQLSERVER_installed := not (compareversion(version, '10.5') < 0);

// Check if MYSQL ODBC is installed
    MYSQLODBC_installed := RegKeyExists(HKLM, 'SOFTWARE\MySQL AB\MySQL Connector/ODBC 5.3')

// Check if MYSQL is installed - this should be fixed
    MYSQLDB_installed := RegKeyExists(HKLM, 'SOFTWARE\MySQL AB\MySQL 5.5');
    MYSQLDB_installed := true;


if VCREDIST_installed then VCstatus.caption := 'INSTALLED' else VCstatus.caption := 'NOT DETECTED' ;
if MYSQLDB_installed then MYSQLstatus.caption := 'INSTALLED' else MYSQLstatus.caption := 'NOT DETECTED' ;
if MYSQLODBC_installed then ODBCstatus.caption := 'INSTALLED' else ODBCstatus.caption := 'NOT DETECTED'  ;
if SQLSERVER_installed  then MSSQLstatus.caption := 'INSTALLED' else MSSQLstatus.caption := 'NOT DETECTED' ; 





  if (CurpageID = ServicePage.Id) Then
  Begin
    // check service page entries
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
    // populate the next page
    if (not webpop) then
    begin
      ConnectionPage.values[0] := GetIniString('database', 'server', '', ExpandConstant('{app}')+'\fhirserver.ini');
      ConnectionPage.values[1] := GetIniString('database', 'database', '', ExpandConstant('{app}')+'\fhirserver.ini');
      ConnectionPage.values[2] := GetIniString('database', 'username', '', ExpandConstant('{app}')+'\fhirserver.ini');
      ConnectionPage.values[3] := GetIniString('database', 'password', '', ExpandConstant('{app}')+'\fhirserver.ini');
      dbDriver.itemindex:=dbDriver.items.indexof(GetIniString('database', 'driver', '', ExpandConstant('{app}')+'\fhirserver.ini'));

      if (GetIniString('database', 'type', '', ExpandConstant('{app}')+'\fhirserver.ini') = '') then dbDriver_OnChange(dbDriver) else
      dbType.itemindex:=dbType.items.indexof(GetIniString('database', 'type', '', ExpandConstant('{app}')+'\fhirserver.ini'));

      webHostName.Text := GetIniString('web', 'host', '', ExpandConstant('{app}')+'\fhirserver.ini');
      webOpenPort.Text := GetIniString('web', 'http', '', ExpandConstant('{app}')+'\fhirserver.ini');
      webOpenPath.Text := GetIniString('web', 'base', '', ExpandConstant('{app}')+'\fhirserver.ini');
      webSecurePort.Text := GetIniString('web', 'https', '', ExpandConstant('{app}')+'\fhirserver.ini');
      webSecurePath.Text := GetIniString('web', 'secure', '', ExpandConstant('{app}')+'\fhirserver.ini');
      certFile.Text := GetIniString('web', 'certname', '', ExpandConstant('{app}')+'\fhirserver.ini');
      caCertFile.Text := GetIniString('web', 'cacertname', '', ExpandConstant('{app}')+'\fhirserver.ini');
      certPWord.Text := GetIniString('web', 'certpword', '', ExpandConstant('{app}')+'\fhirserver.ini');
      AdminPage.Values[0] := GetIniString('admin', 'email', '', ExpandConstant('{app}')+'\fhirserver.ini');
      AdminPage.Values[1] := GetIniString('admin', 'username', '', ExpandConstant('{app}')+'\fhirserver.ini');
      webpop := true;
    end;
  End
  Else if (ConnectionPage <> Nil) And (CurPageID = ConnectionPage.ID) then
  begin
    if IsComponentSelected('r3') then
      fver := '3.0.1'
    else if IsComponentSelected('r4') then
      fver := '3.2.0'
    else
      fver := '1.0.2';
    s := MyDllCheckDatabase(dbDriver.text, ConnectionPage.values[0], ConnectionPage.values[1], ConnectionPage.values[2], ConnectionPage.values[3], fver);
    result := s = '';
    if not result then
      if (s = 'dbi') then
      begin
        if (isTaskSelected('db')) then
          result := true
        else
          MsgBox('The database must be initialized because the FHIRServer tables were not found. Please go back back to tasks, and select ''Initialize Data Base''', mbError, MB_OK); 
      end
      else if (s <> '') then
      begin
        if (isTaskSelected('db')) then
          result := true
        else
          MsgBox('The database must be reinitialized because the FHIR version has changed (is '+s+', needs to be '+fver+'). Please go back back to tasks, and select ''Initialize Data Base''', mbError, MB_OK); 
      end
      else
        MsgBox(s, mbError, MB_OK); 
  end
  Else if (DependenciesPage <> Nil) And (CurPageID = DependenciesPage.ID) then
    begin

  
 
      result:=true;
      //check if there are still dependencies 
Dependencies_OK:=
(SQLSERVER_installed) OR
(VCREDIST_installed AND MYSQLDB_installed AND  MYSQLODBC_installed );
 



      if not Dependencies_OK

      then begin
        ResultMsg := MsgBox('Not all dependencies are met (can be installed after this installation too). Do you want to continue?', mbConfirmation, MB_YESNO) = idYes;
        if ResultMsg = false then
          Result := false
      end
    end
  Else if (WebPage <> Nil) And (CurPageID = WebPage.ID) then
  begin
    result := false;
    if (webHostName.Text = '') then
    begin
      MsgBox('A web host name must be provided', mbError, MB_OK);
      exit;
    end;
    if (webOpenPort.Text <> '') then
    begin
      p := StrToIntDef(webOpenPort.Text, 0);
      if (p <= 0) or (p > 32760) then
      begin
        MsgBox('Insecure Port is illegal', mbError, MB_OK);
        exit;
      end;
      if (webOpenPath.Text = '') then
      begin
        MsgBox('Insecure Path must be provided', mbError, MB_OK);
        exit;
      end;
     end
    else if (webSecurePort.Text <> '') then
    begin
      p := StrToIntDef(webSecurePort.Text, 0);
      if (p <= 0) or (p > 32760) then
      begin
        MsgBox('Secure Port is illegal', mbError, MB_OK);
        exit;
      end;
      if (webSecurePath.Text = '') then
      begin
        MsgBox('Secure Path must be provided', mbError, MB_OK);
        exit;
      end;
      if (certFile.Text = '') then
      begin
        MsgBox('A certificate file is required. If you need to generate one, you can use openSSL', mbError, MB_OK);
        exit;
      end;
      if (caCertFile.Text = '') then
      begin
        MsgBox('A CA certificate file is required', mbError, MB_OK);
        exit;
      end;
      if (certPWord.Text = '') then
      begin
        MsgBox('A certificate password is required', mbError, MB_OK);
        exit;
      end;
    end
    else
    begin
      MsgBox('Must provide either a secure or an insecure port', mbError, MB_OK);
      exit;
    end;
    Result := True;
  end
  Else if (SctPage <> Nil) And (CurPageID = SctPage.ID) then
  begin
    Result := true;
    if not DirExists(SctPage.Values[0]) then
    begin
      Result := false;
      MsgBox('SNOMED CT source not found', mbError, MB_OK);
    end;
    if (sctVersion.Text = '') or (length(sctVersion.Text) <> 8) or (StrToIntDef(sctVersion.Text, 0) = 0) then
    begin
      Result := false;
      MsgBox('SNOMED CT version required, with format YYYYMMDD', mbError, MB_OK);
    end;
  end
  Else if (AdminPage <> Nil) And (CurPageID = AdminPage.ID) then
  begin
    result := false;
    if (AdminPage.Values[0] = '') then
    begin
      MsgBox('A email address must be provided', mbError, MB_OK);
      exit;
    end;
    if (AdminPage.Values[1] = '') then
    begin
      MsgBox('A username must be provided', mbError, MB_OK);
      exit;
    end;
    if (AdminPage.Values[2] = '') then
    begin
      MsgBox('A password must be provided', mbError, MB_OK);
      exit;
    end;
    if (AdminPage.Values[3] = '') then
    begin
      MsgBox('A confirmation password must be provided', mbError, MB_OK);
      exit;
    end;
    if (AdminPage.Values[2] <> AdminPage.Values[3]) then
    begin
      MsgBox('Passwords do not match', mbError, MB_OK);
      exit;
    end;
    Result := True;
  end
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

// ----- Snomed CT Config  -------------------------------------------------------------------

Procedure InstallSctPage;
var
  lbl : TLabel;
begin
  SctPage := CreateInputDirPage(ConfigPage.id, 'Load Snomed CT', 'Provide SNOMED CT Details', 'Select a directory that contains the RF2 snapshot', false, '');  
  SctPage.Add('');

  lbl := TLabel.Create(SctPage);
  lbl.Caption := 'Module:';
  lbl.Top := ScaleX(60);
  lbl.Parent := SctPage.Surface;

  cbxModule := TNewComboBox.Create(SctPage);
  cbxModule.Top := ScaleX(82);
  cbxModule.Width := SctPage.SurfaceWidth;
  cbxModule.Parent := SctPage.Surface;
  cbxModule.Style := csDropDownList;
  cbxModule.Items.Add('International');
  cbxModule.Items.Add('US');
  cbxModule.Items.Add('Australia');
  cbxModule.Items.Add('Spanish');
  cbxModule.Items.Add('Denmark');
  cbxModule.Items.Add('Netherlands');
  cbxModule.Items.Add('Sweden');
  cbxModule.Items.Add('UK');
  cbxModule.ItemIndex := 0;

  lbl := TLabel.Create(SctPage);
  lbl.Caption := 'Version (YYYYMMDD):';
  lbl.Top := ScaleX(110);
  lbl.Parent := SctPage.Surface;
  sctVersion := TNewEdit.Create(SctPage);
  sctVersion.Parent := SctPage.Surface;
  sctVersion.Width := SctPage.SurfaceWidth;
  sctVersion.Top := ScaleX(132);
  sctVersion.Left := 0;
  sctVersion.Text := '';
  
  lbl := TLabel.Create(SctPage);
  lbl.Caption := 'Importing generally takes ~1hr. If loading fails, you can continue the install, and load SNOMED CT later. Also, you can load additional modules later.';
  lbl.Top := ScaleX(260);
  lbl.Parent := SctPage.Surface;
end;

// ----- Web Server Config  -------------------------------------------------------------------

Procedure WebServerPage;
var
  lbl : TLabel;
Begin
  WebPage := CreateCustomPage(ServicePage.id, 'Web Server Configuration', 'Configure the web server');
  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Host Name (FQDN):';
  lbl.Top := ScaleX(4);
  lbl.Parent := WebPage.Surface;
  webHostName := TNewEdit.Create(WebPage);
  webHostName.Parent := WebPage.Surface;
  webHostName.Width := WebPage.Surface.width - 100;
  webHostName.Top := 0;
  webHostName.Left := ScaleX(100);
  webHostName.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'HTTP (unsecure access)';
  lbl.Top := ScaleX(30);
  lbl.Left := 0;
  lbl.Font.Style := [fsBold];
  lbl.Parent := WebPage.Surface;

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Port (def: 80)';
  lbl.Top := ScaleX(58);
  lbl.Left := ScaleX(10);
  lbl.Parent := WebPage.Surface;
  webOpenPort := TNewEdit.Create(WebPage);
  webOpenPort.Parent := WebPage.Surface;
  webOpenPort.Width := ScaleX(60);
  webOpenPort.Top := ScaleX(54);
  webOpenPort.Left := ScaleX(90);
  webOpenPort.Text := '80';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Path (e.g. /open)';
  lbl.Top := ScaleX(58);
  lbl.Left := ScaleX(170);
  lbl.Parent := WebPage.Surface;
  webOpenPath := TNewEdit.Create(WebPage);
  webOpenPath.Parent := WebPage.Surface;
  webOpenPath.Width := WebPage.Surface.width - ScaleX(280);
  webOpenPath.Top := ScaleX(54);
  webOpenPath.Left := ScaleX(280);
  webOpenPath.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'HTTPS (SSL access)';
  lbl.Top := ScaleX(86);
  lbl.Left := 0;
  lbl.Font.Style := [fsBold];
  lbl.Parent := WebPage.Surface;

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Port (def: 443)';
  lbl.Top := ScaleX(114);
  lbl.Left := ScaleX(10);
  lbl.Parent := WebPage.Surface;
  webSecurePort := TNewEdit.Create(WebPage);
  webSecurePort.Parent := WebPage.Surface;
  webSecurePort.Width := ScaleX(60);
  webSecurePort.Top := ScaleX(110);
  webSecurePort.Left := ScaleX(90);
  webSecurePort.Text := '443';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Path (e.g. /secure)';
  lbl.Top := ScaleX(114);
  lbl.Left := ScaleX(170);
  lbl.Parent := WebPage.Surface;
  webSecurePath := TNewEdit.Create(WebPage);
  webSecurePath.Parent := WebPage.Surface;
  webSecurePath.Width := WebPage.Surface.width - ScaleX(280);
  webSecurePath.Top := ScaleX(110);
  webSecurePath.Left := ScaleX(280);
  webSecurePath.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Cert File';
  lbl.Top := ScaleX(136);
  lbl.Left := ScaleX(10);
  lbl.Parent := WebPage.Surface;

  certFile := TNewEdit.Create(WebPage);
  certFile.Parent := WebPage.Surface;
  certFile.Width := WebPage.Surface.width - ScaleX(90);
  certFile.Top := ScaleX(132);
  certFile.Left := ScaleX(90);
  certFile.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'CA Cert File';
  lbl.Top := ScaleX(158);
  lbl.Left := ScaleX(10);
  lbl.Parent := WebPage.Surface;
  caCertFile := TNewEdit.Create(WebPage);
  caCertFile.Parent := WebPage.Surface;
  caCertFile.Width := WebPage.Surface.width - ScaleX(90);
  caCertFile.Top := ScaleX(154);
  caCertFile.Left := ScaleX(90);
  caCertFile.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Cert Password';
  lbl.Top := ScaleX(180);
  lbl.Left := ScaleX(10);
  lbl.Parent := WebPage.Surface;
  certPWord := TNewEdit.Create(WebPage);
  certPWord.Parent := WebPage.Surface;
  certPWord.Width := WebPage.Surface.width - ScaleX(90);
  certPWord.Top := ScaleX(176);
  certPWord.Left := ScaleX(90);
  certPWord.Text := '';

  lbl := TLabel.Create(WebPage);
  lbl.Caption := 'Use openSSL to generate the certificates and get them counter signed'
  lbl.Top := ScaleX(210);
  lbl.Left := 0;
  lbl.Width := WebPage.Surface.Width;
  lbl.Autosize := false;
  lbl.WordWrap := true;
  lbl.Height := ScaleX(60);
  lbl.Parent := WebPage.Surface;
End;

// ----- Database -----------------------------------------------------------------------------

Procedure CreateConnectionPage;
var
  lbl : TLabel;
  btn : TButton;
  s : string;
  sl : TStringList;
  i : integer;
  shrinkspace: integer;
  index:integer;

  Names: TArrayOfString;
  Ii: Integer;
  Ss: String;


Begin

  shrinkSpace:=scalex(8);    //move each edit a few pixels off so that they fit

  ConnectionPage := CreateInputQueryPage(WebPage.id, 'Database Location', 'Select the location of the SQL Server database', 'Leave Username and Password blank to use Windows Authentication');
  ConnectionPage.add('Server', false);
  ConnectionPage.add('Database', false);
  ConnectionPage.add('UserName', false);
  ConnectionPage.add('Password', true);
//  ConnectionPage.add('Driver (default = SQL Server Native Client 11.0 / MySQL ODBC 5.3 Unicode Driver)', false);



  dbDriver := TNewComboBox.Create(ConnectionPage);
  dbDriver.Width := ConnectionPage.SurfaceWidth;
  dbDriver.Parent := ConnectionPage.Surface;
  dbDriver.Style := csDropDownList;

  dbDriver.OnChange := @dbDriver_OnChange;


  dbType := TNewComboBox.Create(ConnectionPage);
  dbType.Width := ConnectionPage.SurfaceWidth;
  dbType.Parent := ConnectionPage.Surface;
  dbType.Style := csDropDownList;
  dbtype.enabled:=false;



  if RegGetValueNames(HKLM, 'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers', Names) then
  begin
    for iI := 0 to GetArrayLength(Names)-1 do
       dbDriver.Items.Add(Names[Ii]);
  end else
  begin
    // add any code to handle failure here
  end;

       dbType.Items.Add('mssql');
       dbType.Items.Add('mysql');

for index:=0 to 3 do begin
  ConnectionPage.Edits[Index].Top := ConnectionPage.Edits[Index].Top - ShrinkSpace*(index);
  ConnectionPage.PromptLabels[Index].Top := ConnectionPage.PromptLabels[Index].Top - ShrinkSpace*(index) +2;
end;


  dbtype.top:= ConnectionPage.PromptLabels[3].Top + ConnectionPage.PromptLabels[3].Top - ConnectionPage.PromptLabels[2].Top




end;

Procedure CreateAdminPage;
Begin
  AdminPage := CreateInputQueryPage(ConnectionPage.id, 'Administration', '', 'Enter Master Administration details (for administering user accounts)');
  AdminPage.Add('Email:', False);
  AdminPage.Add('Username', False);
  AdminPage.Add('Password', True);
  AdminPage.Add('Password Confirmation', True);
end;

Procedure CreateConfigPage;
Begin
  ConfigPage := CreateInputOptionPage(AdminPage.id, 'Configuration', 'Default Security Configuration', 
    'Choose the Basic Security Profile (or leave it as ''closed'' and configure directly after install)', true, false);
  ConfigPage.Add('Open Access (any user, including anonymous, can perform any operation)');
  ConfigPage.Add('Closed Access (all users must be authenticated)');
  ConfigPage.Add('Read Only (anonymous users can only read/search)');
  ConfigPage.Add('Terminology Server (terminology resources only, any user can read)');
  ConfigPage.values[0] := true;
end;


procedure LaunchJavaInstall(Sender: TObject);
var ResultCode:Integer;
begin

ShellExec('open', 'http://www.java.com/getjava/', '','',SW_SHOWNORMAL,ewNoWait,ResultCode);

end;


procedure InstallMySQL(Sender: TObject);
var ResultCode:Integer;
begin
//to do: Make this function get the URL from the TEdit
idpDownloadFile('https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-community-5.7.20.0.msi', ExpandConstant('{tmp}\mysql.msi'));

//if not ShellExec('', 'msiexec.exe', ExpandConstant(' /i {tmp}\mysql-installer-web-community-5.7.20.0.msi'),'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
if not ShellExec('', 'msiexec.exe', ExpandConstant(' /i {tmp}\mysql.msi'),'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
  MsgBox('Installer failed to run!' + #13#10 + ' ' +
    SysErrorMessage(ResultCode), mbError, MB_OK);
end;


procedure InstallVCRedist(Sender: TObject);
var ResultCode:Integer;
begin
//to do: Make this function get the URL from the TEdit
idpDownloadFile('http://download.microsoft.com/download/1/f/e/1febbdb2-aded-4e14-9063-39fb17e88444/vc_redist.x86.exe', ExpandConstant('{tmp}\vcredist.exe'));
if not ShellExec('', ExpandConstant('{tmp}\vcredist.exe'), '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
  MsgBox('Installer failed to run!' + #13#10 + ' ' +
    SysErrorMessage(ResultCode), mbError, MB_OK);
end;



procedure InstallMSSQL(Sender: TObject);
var ResultCode:Integer;
begin
//to do: Make this function get the URL from the TEdit
idpDownloadFile('http://download.microsoft.com/download/5/1/A/51A153F6-6B08-4F94-A7B2-BA1AD482BC75/SQLEXPR32_x86_ENU.exe', ExpandConstant('{tmp}\mssql.exe'));
if not ShellExec('', ExpandConstant('{tmp}\mssql.exe'), '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
  MsgBox('Installer failed to run!' + #13#10 + ' ' +
    SysErrorMessage(ResultCode), mbError, MB_OK);
end;

procedure InstallODBC(Sender: TObject);
var ResultCode:Integer;
begin
//to do: Make this function get the URL from the TEdit
// Which ODBC Depends on which Database is setup os is being setup
idpDownloadFile('http://download.microsoft.com/download/1/f/e/1febbdb2-aded-4e14-9063-39fb17e88444/vc_redist.x86.exe', ExpandConstant('{tmp}\odbc.exe'));
if not ShellExec('', ExpandConstant('{tmp}\odbc.exe'), '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
  MsgBox('Installer failed to run!' + #13#10 + ' ' +
    SysErrorMessage(ResultCode), mbError, MB_OK);
end;


Procedure CreateDependenciesPage;
var

JRElbl, JREstatus, VClbl, MYSQLlbl,  MSSQLlbl, ODBClbl :Tlabel ;
JREInstall, VCInstall, MYSQLInstall, MSSQLInstall, ODBCInstall:TButton;
VCPath, MYSQLPath, MSSQLPath, ODBCPath:TEdit;


JavaInstalled : Boolean;
ResultMsg : Boolean;
Versions: TArrayOfString;
I: Integer;
regRoot: Integer;


Begin
  DependenciesPage := CreateCustomPage(wpSelectTasks, 'Install Dependencies', 'Choose the dependencies to install');

 ////Check Java
  regRoot := HKLM;
  if not (RegGetSubkeyNames(regRoot, 'SOFTWARE\JavaSoft\Java Runtime Environment', Versions)) and not (RegGetSubkeyNames(regRoot, 'SOFTWARE\JavaSoft\Java Development Kit', Versions)) then
    JavaInstalled := false
  else
    for I := 0 to GetArrayLength(Versions)-1 do
      if JavaInstalled = true then
       //do nothing
      else if ( Versions[I][2]='.' ) and ( ( StrToInt(Versions[I][1]) > 1 ) or ( ( StrToInt(Versions[I][1]) = 1 ) and ( StrToInt(Versions[I][3]) >= 7 ) ) ) then
        JavaInstalled := true
      else
        JavaInstalled := false;


  JRElbl := TLabel.Create(DependenciesPage);
  with JRElbl do begin
  Caption := 'Java Status:';
  Top := ScaleX(0);
  Parent := DependenciesPage.Surface;
  end;

  JREstatus := TLabel.Create(DependenciesPage);
  with JREstatus do begin
  if JavaInstalled = true then Caption := 'INSTALLED' else Caption := 'NOT INSTALLED';
  font.style:=[fsBold];
  Top := ScaleX(0);
  Left := ScaleX(64);
  Parent := DependenciesPage.Surface;
  end;

  JREInstall := TButton.Create(DependenciesPage);
  with JREInstall do begin
  Caption := 'Install JRE';
  Top := ScaleX(16);
  Width := ScaleX(65);
  Parent := DependenciesPage.Surface;
  OnClick := @LaunchJavaInstall;
  end;



  VClbl := TLabel.Create(DependenciesPage);
  with VClbl do begin
  Caption := 'Visual C++ Redistributables (2015 or 2017)';
  Top := ScaleX(60);
  Parent := DependenciesPage.Surface;
  end;

  VCstatus := TLabel.Create(DependenciesPage);
  with VCstatus do begin
  Caption := 'NOT INSTALLED';
  font.style:=[fsBold];
  Top := ScaleX(60);
  Left := ScaleX(220);
  Parent := DependenciesPage.Surface;
  end;

  VCpath := TEdit.Create(DependenciesPage);
  with VCPath do begin
  Text := vcredist2017_url_x64;
  Top := ScaleX(76);
  Left := ScaleX(70);
  Width:=Scalex(300);
  Parent := DependenciesPage.Surface;
  end;

  VCInstall := TButton.Create(DependenciesPage);
  with VCInstall do begin
  Caption := 'Install';
  Top := ScaleX(76);
  Width := ScaleX(65);
  Height := VCpath.Height;
  Parent := DependenciesPage.Surface;
  OnClick := @InstallVCRedist;
  end;


  MYSQLlbl := TLabel.Create(DependenciesPage);
  with MYSQLlbl do begin
  Caption := 'MySQL (needs ODBC Driver)';
  Top := ScaleX(100);
  Parent := DependenciesPage.Surface;
  end;

  MYSQLstatus := TLabel.Create(DependenciesPage);
  with MYSQLstatus do begin
  Caption := '';
  Left := ScaleX(140);
  font.style:=[fsBold];
  Top := ScaleX(100);
  Parent := DependenciesPage.Surface;
  end;

  MYSQLpath := TEdit.Create(DependenciesPage);
  with MYSQLpath do begin
  Text := 'https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-community-5.7.20.0.msi';
  Top := ScaleX(116);
  Left := ScaleX(70);
  Width:=Scalex(300);
  Parent := DependenciesPage.Surface;
  end;

  MYSQLInstall := TButton.Create(DependenciesPage);
  with MYSQLInstall do begin
  Caption := 'Install';
  Top := ScaleX(116);
  Width := ScaleX(65);
  Height := VCpath.Height;
  Parent := DependenciesPage.Surface;
  OnClick := @InstallMySQL;
  end;


  ODBClbl := TLabel.Create(DependenciesPage);
  with ODBClbl do begin
  Caption := 'MYSQL ODBC Driver (5.3+)';
  Top := ScaleX(140);
  Parent := DependenciesPage.Surface;
  end;

  ODBCstatus := TLabel.Create(DependenciesPage);
  with ODBCstatus do begin
  Caption := '';
  font.style:=[fsBold];
  Top := ScaleX(140);
  Left := ScaleX(140);
  Parent := DependenciesPage.Surface;
  end;

  ODBCpath := TEdit.Create(DependenciesPage);
  with ODBCpath do begin
  Text := mysqlodbc_url_x64;
  Top := ScaleX(156);
  Left := ScaleX(70);
  Width:=Scalex(300);
  Parent := DependenciesPage.Surface;
  end;

  ODBCInstall := TButton.Create(DependenciesPage);
  with ODBCInstall do begin
  Caption := 'Install';
  Top := ScaleX(156);
  Width := ScaleX(65);
  Height := VCpath.Height;
  Parent := DependenciesPage.Surface;
  OnClick := @InstallODBC;
  end;



  MSSQLlbl := TLabel.Create(DependenciesPage);
  with MSSQLlbl do begin
  Caption := 'MS SQL Server';
  Top := ScaleX(200);
  Parent := DependenciesPage.Surface;
  end;

  MSSQLstatus := TLabel.Create(DependenciesPage);
  with MSSQLstatus do begin
  font.style:=[fsBold];
  Caption := '';
  Top := ScaleX(200);
  Left := ScaleX(100);
  Parent := DependenciesPage.Surface;
  end;

  MSSQLpath := TEdit.Create(DependenciesPage);
  with MSSQLpath do begin
  Text := sql2008expressr2_url_x64;
  Top := ScaleX(216);
  Left := ScaleX(70);
  Width:=Scalex(300);
  Parent := DependenciesPage.Surface;
  end;

  MSSQLInstall := TButton.Create(DependenciesPage);
  with MSSQLInstall do begin
  Caption := 'Install';
  Top := ScaleX(216);
  Width := ScaleX(65);
  Height := VCpath.Height;
  Parent := DependenciesPage.Surface;
  OnClick := @InstallMSSQL;
  end;

  JREInstall.Height := VCpath.Height;





end;


Procedure CreatePostInstallPage;
Begin
  SctInstallPage := CreateOutputProgressPage('SNOMED CT', 'Load SNOMED CT (~1hr)');
  LoadInstallPage := CreateOutputProgressPage('Initialize Database', '');
End;

// ------ Installation Logic -------------------------------------------------------------------------

procedure InitializeWizard();
var ResultCode: integer;
Begin
  CreateDependenciesPage;
  CreateServiceUserPage;
  WebServerPage;
  CreateConnectionPage;
  CreateAdminPage;
  CreateConfigPage;
  InstallSctPage;
  CreatePostInstallPage;

End;

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

type
  TMyGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

function CoCreateGuid(var Guid:TMyGuid):integer;
 external 'CoCreateGuid@ole32.dll stdcall';

function inttohex(l:longword; digits:integer):string;
var hexchars:string;
begin
 hexchars:='0123456789ABCDEF';
 setlength(result,digits);
 while (digits>0) do begin
  result[digits]:=hexchars[l mod 16+1];
  l:=l div 16;
  digits:=digits-1;
 end;
end;

function GetGuid():string;
var Guid:TMyGuid;
begin
  if CoCreateGuid(Guid)=0 then begin
  result:='{'+IntToHex(Guid.D1,8)+'-'+
           IntToHex(Guid.D2,4)+'-'+
           IntToHex(Guid.D3,4)+'-'+
           IntToHex(Guid.D4[0],2)+IntToHex(Guid.D4[1],2)+'-'+
           IntToHex(Guid.D4[2],2)+IntToHex(Guid.D4[3],2)+
           IntToHex(Guid.D4[4],2)+IntToHex(Guid.D4[5],2)+
           IntToHex(Guid.D4[6],2)+IntToHex(Guid.D4[7],2)+
           '}';
  end else
    result:='{00000000-0000-0000-0000-000000000000}';
end;

procedure ConfigureIni;
begin
  if webpop then
  begin
    SetIniString('web', 'host', webHostName.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'http', webOpenPort.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'base', webOpenPath.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'https', webSecurePort.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'secure', webSecurePath.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'certname', certFile.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'cacertname', caCertFile.Text, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('web', 'certpword', certPWord.Text, ExpandConstant('{app}')+'\fhirserver.ini');

    SetIniString('database', 'server',   ConnectionPage.Values[0], ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('database', 'database', ConnectionPage.Values[1], ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('database', 'username', ConnectionPage.Values[2], ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('database', 'password', ConnectionPage.Values[3], ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('database', 'driver', dbDriver.text, ExpandConstant('{app}')+'\fhirserver.ini');
 if dbType.ItemIndex=0 then SetIniString('database', 'type',  'mssql' , ExpandConstant('{app}')+'\fhirserver.ini');
 if dbType.ItemIndex=1 then SetIniString('database', 'type',  'mysql' , ExpandConstant('{app}')+'\fhirserver.ini');
    


    SetIniString('scim', 'salt', GetGuid, ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('admin', 'email', AdminPage.Values[0], ExpandConstant('{app}')+'\fhirserver.ini');
    SetIniString('admin', 'username', AdminPage.Values[1], ExpandConstant('{app}')+'\fhirserver.ini');
  end;
end;

var
  dll : longint;

procedure SctCallback(IntParam: Integer; StrParam: WideString);
begin
  SctInstallPage.SetProgress(intparam, 100);
  SctInstallPage.SetText(StrParam, '');
end;

function getSnomedModule : String;
begin
  case cbxModule.itemindex of 
    0 { International } : result := '900000000000207008';
    1 { US } :  result := '731000124108';
    2 { Australia } : result := '32506021000036107';
    3 { Spanish } : result := '449081005';
    4 { Denmark } : result := '554471000005108';
    5 { Netherlands } : result := '11000146104';
    6 { Sweden } : result := '45991000052106';
    7 { UK } : result := '999000041000000102';
  end;
end;

procedure LoadSnomed;
var 
  module, dest, ver, msg : String;
begin
  module := getSnomedModule;
  dest := ExpandConstant('{app}')+'\snomed_'+module+'_'+sctVersion.text+'.cache';
  SctInstallPage.SetText('Loading Snomed...', '');
  SctInstallPage.SetProgress(0, 100);
  SctInstallPage.Show;
  try
    msg := MyDllInstallSnomed(ExpandConstant('{app}')+'\fhirserver.exe', SctPage.values[0], dest, 'http://snomed.info/sct/'+module+'/version/'+sctVersion.text, @SctCallback);
    if msg <> '' then
      MsgBox('Loading SNOMED CT failed (but the rest of the installation is not affected): '+msg, mbError, MB_OK);
  finally
    SctInstallPage.Hide;
  end;
  SetIniString('snomed', 'cache', dest, ExpandConstant('{app}')+'\fhirserver.ini');
end;

procedure InitCallback(IntParam: Integer; StrParam: WideString);
begin
  LoadInstallPage.SetProgress(intparam, 100);
  LoadInstallPage.SetText(StrParam, '');
end;

procedure InitialiseDatabase(load : boolean);
var 
  pw, msg : String;
  done : boolean;
begin
  LoadInstallPage.SetText('Creating Database...', '');
  LoadInstallPage.SetProgress(0, 100);
  LoadInstallPage.Show;
  try
    repeat
      done := true;
      pw := AdminPage.Values[2];
      if (load) then
        msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\fhirserver.exe', ExpandConstant('{app}')+'\fhirserver.ini', pw, ExpandConstant('{app}')+'\load\load.ini', @InitCallback)
      else
        msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\loader.dll', ExpandConstant('{app}')+'\fhirserver.ini', pw, '', @InitCallback);
      if msg <> '' then
        done := MsgBox('Initializing the database failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
    until done;
  finally
    LoadInstallPage.Hide;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  mode : Longword;
  s : String;
  noauto : integer;
  disp : string;
  resultcode:integer;
Begin


  if (CurStep = ssInstall) Then
  Begin
    if ServiceExists('FHIRServer') Then
      SimpleStopService('FHIRServer', true, true);
  End;

if IsTaskSelected('vcredist') Then
     MsgBox('Install VCREDIST', mbError, MB_YESNO);
if IsTaskSelected('dbengine') Then
     MsgBox('Install DB', mbError, MB_YESNO) ;
     ShellExec('', 'msiexec.exe', ExpandConstant('/i "{tmp}\mysql-installer-web-community-5.7.20.0.msi"'),'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode)
if IsTaskSelected('odbc') Then
     MsgBox('Install ODBC', mbError, MB_YESNO);

  if (CurStep = ssPostInstall)  Then
  Begin
    If IsTaskSelected('firewall') Then
      SetFirewallException('Fhir Server', ExpandConstant('{app}')+'\FHIRServer.exe');
    ConfigureIni;
    if IsTaskSelected('sct') Then
      LoadSnomed;
    if IsTaskSelected('db') Then


       InitialiseDatabase(isTaskSelected('db\pop'));                                                        
    if IsTaskSelected('svcInst') Then
    begin
      if IsComponentSelected('r2') then
        disp := 'FHIR Server DSTU2'
      else
        disp := 'FHIR Server STU3';
      if ServiceExists('FHIRServer') then
        SimpleDeleteService('FHIRServer');
        
      if not SimpleCreateService('FHIRServer', disp, ExpandConstant('{app}')+'\FHIRServer.exe', DetermineStartMode, ServicePage.Values[0], ServicePage.Values[1], false, false) then
        MsgBox('Unable to Register FHIRServer Service', mbError, MB_OK)
      else if IsTaskSelected('svcInst\start') Then
        SimpleStartService('FHIRServer', false, false);
    end;
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
