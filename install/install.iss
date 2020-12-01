[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRServer
AppName=Health Intersections FHIR Server
AppVerName=FHRServer v1.9.363

; compilation control
OutputDir=..\install\build
OutputBaseFilename=fhirserver-1.9.363
Compression=lzma2/ultra64

; 64 bit
ArchitecturesInstallIn64BitMode=x64

; we might be creating a service so we do need this privilege
PrivilegesRequired=admin
AllowUNCPath=no
AlwaysShowDirOnReadyPage=yes
AlwaysShowGroupOnReadyPage=yes
ChangesAssociations=yes
DirExistsWarning=auto
DisableStartUpPrompt=yes
MinVersion=0,6.0
UninstallDisplayIcon=..\Server\fhir.ico
WizardStyle=modern
DisableDirPage=false

; directory management
DefaultDirName={pf}\FHIRServer
DefaultGroupName=FHIR Applications
UninstallFilesDir={app}\uninstall

; win2000+ add/remove programs support
AppPublisher=Health Intersections P/L
AppPublisherURL=http://www.healthintersections.com.au
AppVersion=1.9.362
AppSupportURL=https://github.com/grahamegrieve/fhirserver
AppUpdatesURL=https://github.com/grahamegrieve/fhirserver
AppCopyright=Copyright (c) Health Intersections Pty Ltd 2011+
VersionInfoVersion=1.9.362.0

; dialog support
LicenseFile=..\license
InfoBeforeFile=..\install\readme.rtf

; #include <idp.iss>

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

[Tasks]
; Core related tasks
Name: svcInst;   Description: "Install FHIR Server as a Service"
Name: firewall;  Description: "Allow FHIR Server through the Firewall"
Name: envPath;   Description: "Add FHIR Server to the system path"

[Files]
; 1. Application executables & Dlls
Source: "..\exec\64\FHIRServer.exe";                          DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\64\FHIRServer.debug.exe";                    DestDir: "{app}\debug"; Flags: ignoreversion
Source: "..\exec\64\fhirconsole.exe";                         DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\FastMM_FullDebugMode64.dll";        DestDir: "{app}\debug"; Flags: ignoreversion
Source: "..\exec\pack\w64\ChakraCore.dll";                    DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\sqlite3.dll";                       DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libcrypto-1_1-x64.dll";             DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libssl-1_1-x64.dll";                DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\zlib1.dll";                         DestDir: "{app}";       Flags: ignoreversion

; 3. Data Files
Source: "..\exec\pack\fhirserver.cfg";                        DestDir: "{app}";       Flags: ignoreversion onlyifdoesntexist; Permissions: users-full
Source: "..\exec\pack\fhirserver.web";                        DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\ucum-essence.xml";                      DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\lang.txt";                              DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\tzdata.tar.gz";                         DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\translations.xml";                      DestDir: "{app}";       Flags: ignoreversion 

; 4. Documentation
Source: "..\license";                                         DestDir: "{app}\doco";  Flags: ignoreversion; DestName: "license.txt";
Source: "..\readme.md";                                       DestDir: "{app}\doco";  Flags: ignoreversion; DestName: "readme.txt";
Source: "readme.rtf";                                         DestDir: "{app}\doco";  Flags: ignoreversion; DestName: "installation-documentation.rtf";

; utilities files - put in app dir because these may be different to ones already on the machine.
Source: "..\exec\pack\w64\openssl.exe";                       DestDir: "{app}\utils"; Flags: ignoreversion
Source: "..\exec\pack\w64\sqldiff.exe";                       DestDir: "{app}\utils"; Flags: ignoreversion
Source: "..\exec\pack\w64\sqlite3.exe";                       DestDir: "{app}\utils"; Flags: ignoreversion
Source: "..\exec\pack\w64\sqlite3_analyzer.exe";              DestDir: "{app}\utils"; Flags: ignoreversion

[Icons]
Name: "{group}\FHIR Server Manager";         Filename: "{app}\fhirconsole.exe";                      WorkingDir: "{app}"    
Name: "{group}\Installation Documentation";  Filename: "{app}\doco\installation-documentation.rtf";         

[Run]
Filename: "{app}\fhirconsole.exe"; Parameters: "-installer ""{app}\fhirserver.cfg"""; Description: "Configure the server"; WorkingDir: "{app}"; Flags: postinstall shellexec skipifsilent

[Code]
const
  MB_ICONINFORMATION = $40;

var
    DependenciesPage : TWizardPage;
  ServicePage : TInputQueryWizardPage;
  cbxStartup : TNewComboBox;

  VCStatus, MYSQLStatus, MSSQLStatus,  ODBCStatus : TLabel;
  VCPath, MYSQLPath, MSSQLPath, ODBCPath:TEdit;

// start up check: a jre is required   

function InitializeSetup(): Boolean;
begin
  Result := true;
end;

// ------ Firewall management ---------------------------------------------------------------------------------

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

// ------ Wizard Pages ---------------------------------------------------------------------------------

var
  SQLSERVER_installed,VCREDIST_installed,MYSQLDB_installed,MYSQLODBC_installed:boolean;

procedure InstallMySQL(Sender: TObject);
var 
  ResultCode:Integer;
  localfilename:string;
begin
//  idpDownloadFile('https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-community-5.7.20.0.msi', ExpandConstant('{tmp}\mysql.msi'));

  localfilename:= MySQLPath.text;
  StringChangeEx(localfilename, '/', '\', True);
  localfilename := ExtractFileName(localfilename);
  localfilename := ExpandConstant('{tmp}'+'\'+localfilename);
//  idpDownloadFile(MySQLPath.text, localfilename);
  if not ShellExec('', localfilename, '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
    MsgBox('Installer failed to run!' + #13#10 + ' ' + SysErrorMessage(ResultCode), mbError, MB_OK);
end;


procedure InstallVCRedist(Sender: TObject);
var 
  ResultCode:Integer;
  localfilename:string;
begin
//  idpDownloadFile('http://download.microsoft.com/download/1/f/e/1febbdb2-aded-4e14-9063-39fb17e88444/vc_redist.x86.exe', ExpandConstant('{tmp}\vcredist.exe'));
  localfilename:= VCPath.text;
  StringChangeEx(localfilename, '/', '\', True);
  localfilename := ExtractFileName(localfilename);
  localfilename := ExpandConstant('{tmp}'+'\'+localfilename);
  //idpDownloadFile(VCPath.text, localfilename);
  if not ShellExec('', localfilename, '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
    MsgBox('Installer failed to run!' + #13#10 + ' ' + SysErrorMessage(ResultCode), mbError, MB_OK);
end;



procedure InstallMSSQL(Sender: TObject);
var 
  ResultCode:Integer;
  localfilename:string;
begin
//  idpDownloadFile('http://download.microsoft.com/download/5/1/A/51A153F6-6B08-4F94-A7B2-BA1AD482BC75/SQLEXPR32_x86_ENU.exe', ExpandConstant('{tmp}\mssql.exe'));
  localfilename:= MSSQLPath.text;
  StringChangeEx(localfilename, '/', '\', True);
  localfilename := ExtractFileName(localfilename);
  localfilename := ExpandConstant('{tmp}'+'\'+localfilename);
  //idpDownloadFile(MSSQLPath.text, localfilename);
  if not ShellExec('', localfilename, '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
    MsgBox('Installer failed to run!' + #13#10 + ' ' + SysErrorMessage(ResultCode), mbError, MB_OK);
end;

procedure InstallODBC(Sender: TObject);
var 
  ResultCode:Integer;
  localfilename:string;
begin
// Which ODBC Depends on which Database is setup os is being setup
//  idpDownloadFile('http://download.microsoft.com/download/1/f/e/1febbdb2-aded-4e14-9063-39fb17e88444/vc_redist.x86.exe', ExpandConstant('{tmp}\odbc.exe'));
  localfilename:= ODBCPath.text;
  StringChangeEx(localfilename, '/', '\', True);
  localfilename := ExtractFileName(localfilename);
  localfilename := ExpandConstant('{tmp}'+'\'+localfilename);
//  idpDownloadFile(ODBCPath.text, localfilename);
  if not ShellExec('', localfilename, '' ,'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then
    MsgBox('Installer failed to run!' + #13#10 + ' ' + SysErrorMessage(ResultCode), mbError, MB_OK);
end;

Procedure CreateDependenciesPage;
var
  JRElbl, JREstatus, VClbl, MYSQLlbl,  MSSQLlbl, ODBClbl :Tlabel ;
  JREInstall, VCInstall, MYSQLInstall, MSSQLInstall, ODBCInstall:TButton;
  ResultMsg : Boolean;
  Versions: TArrayOfString;
  I: Integer;
  regRoot: Integer;
Begin
  DependenciesPage := CreateCustomPage(wpSelectTasks, 'Install Dependencies', 'Choose the dependencies to install');
 
  VClbl := TLabel.Create(DependenciesPage);
  with VClbl do 
  begin
    Caption := 'Visual C++ Redistributables (2015 or 2017)';
    Top := ScaleX(0);
    Parent := DependenciesPage.Surface;
  end;

  VCstatus := TLabel.Create(DependenciesPage);
  with VCstatus do 
  begin
    Caption := 'NOT INSTALLED';
    font.style:=[fsBold];
    Top := ScaleX(0);
    Left := ScaleX(220);
    Parent := DependenciesPage.Surface;
  end;

  VCpath := TEdit.Create(DependenciesPage);
  with VCPath do 
  begin
    Text := vcredist2017_url_x64;
    Top := ScaleX(16);
    Left := ScaleX(70);
    Width:=Scalex(300);
    Parent := DependenciesPage.Surface;
  end;

  VCInstall := TButton.Create(DependenciesPage);
  with VCInstall do 
  begin
    Caption := 'Install';
    Top := ScaleX(16);
    Width := ScaleX(65);
    Height := VCpath.Height;
    Parent := DependenciesPage.Surface;
    OnClick := @InstallVCRedist;
  end;


  MYSQLlbl := TLabel.Create(DependenciesPage);
  with MYSQLlbl do 
  begin
    Caption := 'MySQL (needs ODBC Driver)';
    Top := ScaleX(40);
    Parent := DependenciesPage.Surface;
  end;

  MYSQLstatus := TLabel.Create(DependenciesPage);
  with MYSQLstatus do 
  begin
    Caption := '';
    Left := ScaleX(80);
    font.style:=[fsBold];
    Top := ScaleX(100);
    Parent := DependenciesPage.Surface;
  end;

  MYSQLpath := TEdit.Create(DependenciesPage);
  with MYSQLpath do 
  begin
    Text := 'https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-community-5.7.20.0.msi';
    Top := ScaleX(56);
    Left := ScaleX(70);
    Width:=Scalex(300);
    Parent := DependenciesPage.Surface;
  end;

  MYSQLInstall := TButton.Create(DependenciesPage);
  with MYSQLInstall do 
  begin
    Caption := 'Install';
    Top := ScaleX(56);
    Width := ScaleX(65);
    Height := VCpath.Height;
    Parent := DependenciesPage.Surface;
    OnClick := @InstallMySQL;
  end;

  ODBClbl := TLabel.Create(DependenciesPage);
  with ODBClbl do 
  begin
    Caption := 'MYSQL ODBC Driver (5.3+)';
    Top := ScaleX(80);
    Parent := DependenciesPage.Surface;
  end;

  ODBCstatus := TLabel.Create(DependenciesPage);
  with ODBCstatus do 
  begin
    Caption := '';
    font.style:=[fsBold];
    Top := ScaleX(80);
    Left := ScaleX(140);
    Parent := DependenciesPage.Surface;
  end;

  ODBCpath := TEdit.Create(DependenciesPage);
  with ODBCpath do 
  begin
    if 
      false // this should be changed to see if DB drivers are in x32 or x64- a radiobutton or something?
    then Text := mysqlodbc_url else   
    Text := mysqlodbc_url_x64;   
    Top := ScaleX(96);
    Left := ScaleX(70);
    Width:=Scalex(300);
    Parent := DependenciesPage.Surface;
  end;

  ODBCInstall := TButton.Create(DependenciesPage);
  with ODBCInstall do 
  begin
    Caption := 'Install';
    Top := ScaleX(96);
    Width := ScaleX(65);
    Height := VCpath.Height;
    Parent := DependenciesPage.Surface;
    OnClick := @InstallODBC;
  end;



  MSSQLlbl := TLabel.Create(DependenciesPage);
  with MSSQLlbl do 
  begin
    Caption := 'MS SQL Server';
    Top := ScaleX(140);
    Parent := DependenciesPage.Surface;
  end;

  MSSQLstatus := TLabel.Create(DependenciesPage);
  with MSSQLstatus do 
  begin
    font.style:=[fsBold];
    Caption := '';
    Top := ScaleX(140);
    Left := ScaleX(100);
    Parent := DependenciesPage.Surface;
  end;

  MSSQLpath := TEdit.Create(DependenciesPage);
  with MSSQLpath do 
  begin
    Text := sql2008expressr2_url_x64;
    Top := ScaleX(156);
    Left := ScaleX(70);
    Width:=Scalex(300);
    Parent := DependenciesPage.Surface;
  end;

  MSSQLInstall := TButton.Create(DependenciesPage);
  with MSSQLInstall do 
  begin
    Caption := 'Install';
    Top := ScaleX(156);
    Width := ScaleX(65);
    Height := VCpath.Height;
    Parent := DependenciesPage.Surface;
    OnClick := @InstallMSSQL;
  end;
end;

procedure configureDependenciesPage;
var  
  version : String;
begin
  // Check if Visual C++ Redist 2015 is installed - should support also 2017?
  VCREDIST_installed := (msiproductupgrade(GetString(vcredist2015_upgradecode, vcredist2015_upgradecode_x64, ''), '15')) 
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
end;

function checkDependenciesPage : boolean;
var 
  Dependencies_OK : boolean;
  ResultMsg:boolean;
begin
  result := true;
  //check if there are still dependencies 
  Dependencies_OK := (SQLSERVER_installed) OR (VCREDIST_installed AND MYSQLDB_installed AND  MYSQLODBC_installed );
  if not Dependencies_OK then 
  begin
    ResultMsg := MsgBox('Not all dependencies are met (can be installed after this installation too). Do you want to continue?', mbConfirmation, MB_YESNO) = idYes;
    if ResultMsg = false then
      Result := false
  end;
end;


// ------ Service Installation ---------------------------------------------------------------------------------
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


// ------ Service Details Page ---------------------------------------------------------------------------------

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

Function DetermineStartMode : Integer;
Begin
  case cbxStartup.ItemIndex of
    0: result := SERVICE_AUTO_START;
    1: result := SERVICE_DEMAND_START;
    2: result := SERVICE_DISABLED;
    3: result := SERVICE_AUTO_START;
  End;
End;
Procedure CreateServiceUserPage;
var
  lbl : TLabel;
  version : TWindowsVersion;
Begin
  GetWindowsVersionEx(version);
  ServicePage := CreateInputQueryPage(DependenciesPage.ID, 'Service Details', 'Configure the FHIRServer Service', 'Leave Blank to run as local system, or DOMAIN\Username and password');
  ServicePage.Add('User:', False);
  ServicePage.Add('Password:', True);

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

  If ServiceExists('FHIRServer') Then
  Begin
    ServicePage.Values[0] := GetServiceUser('FHIRServer');
    if (ServicePage.Values[0] = 'Local System') or (ServicePage.Values[0] = 'LocalSystem') then
      ServicePage.Values[0] := '';
    cbxStartup.ItemIndex := GetServiceStartIndex('FHIRServer');
  End;
End;
 
function checkServicePageDetails : boolean;
begin
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

end;

// ------ Wizard Events -------------------------------------------------------------------------------------
            
function ShouldSkipPage(PageID: Integer): Boolean;
begin
  if (PageID = ServicePage.Id) Then
    result := not IsTaskSelected('svcInst')
  else
    result := false;
end;
                        
function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if (CurpageID = wpSelectTasks) Then
  Begin
    configureDependenciesPage;
    Result := True;
  End
  Else if (DependenciesPage <> Nil) And (CurPageID = DependenciesPage.ID) then
  begin
    result := checkDependenciesPage;
  end
  Else if (CurpageID = ServicePage.Id) Then
  Begin
    result := checkServicePageDetails;
  End
  Else
    Result := True;
end;

procedure InitializeWizard();
Begin
  CreateDependenciesPage;
  CreateServiceUserPage;
End;

const EnvironmentKey = 'Environment';

procedure EnvAddPath(instlPath: string);
var
    Paths: string;
begin
    { Retrieve current path (use empty string if entry not exists) }
    if not RegQueryStringValue(HKEY_CURRENT_USER, EnvironmentKey, 'Path', Paths) then
        Paths := '';

    if Paths = '' then
        Paths := instlPath + ';'
    else
    begin
        { Skip if string already found in path }
        if Pos(';' + Uppercase(instlPath) + ';',  ';' + Uppercase(Paths) + ';') > 0 then exit;
        if Pos(';' + Uppercase(instlPath) + '\;', ';' + Uppercase(Paths) + ';') > 0 then exit;

        { Append App Install Path to the end of the path variable }
        Log(Format('Right(Paths, 1): [%s]', [Paths[length(Paths)]]));
        if Paths[length(Paths)] = ';' then
            Paths := Paths + instlPath + ';'  { don't double up ';' in env(PATH) }
        else
            Paths := Paths + ';' + instlPath + ';' ;
    end;

    { Overwrite (or create if missing) path environment variable }
    if RegWriteStringValue(HKEY_CURRENT_USER, EnvironmentKey, 'Path', Paths) then 
      Log(Format('The [%s] added to PATH: [%s]', [instlPath, Paths]))
    else 
      Log(Format('Error while adding the [%s] to PATH: [%s]', [instlPath, Paths]));
end;

procedure EnvRemovePath(instlPath: string);
var
    Paths: string;
    P, Offset, DelimLen: Integer;
begin
    { Skip if registry entry not exists }
    if not RegQueryStringValue(HKEY_CURRENT_USER, EnvironmentKey, 'Path', Paths) then
        exit;

    { Skip if string not found in path }
    DelimLen := 1;     { Length(';') }
    P := Pos(';' + Uppercase(instlPath) + ';', ';' + Uppercase(Paths) + ';');
    if P = 0 then
    begin
        { perhaps instlPath lives in Paths, but terminated by '\;' }
        DelimLen := 2; { Length('\;') }
        P := Pos(';' + Uppercase(instlPath) + '\;', ';' + Uppercase(Paths) + ';');
        if P = 0 then exit;
    end;

    { Decide where to start string subset in Delete() operation. }
    if P = 1 then
        Offset := 0
    else
        Offset := 1;
    { Update path variable }
    Delete(Paths, P - Offset, Length(instlPath) + DelimLen);

    { Overwrite path environment variable }
    if RegWriteStringValue(HKEY_CURRENT_USER, EnvironmentKey, 'Path', Paths)
    then Log(Format('The [%s] removed from PATH: [%s]', [instlPath, Paths]))
    else Log(Format('Error while removing the [%s] from PATH: [%s]', [instlPath, Paths]));
end;

// ------ Installation Logic -------------------------------------------------------------------------

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
  begin
    MsgBox('Install DB', mbError, MB_YESNO) ;
    ShellExec('', 'msiexec.exe', ExpandConstant('/i "{tmp}\mysql-installer-web-community-5.7.20.0.msi"'),'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode);
  end;
  if IsTaskSelected('odbc') Then
    MsgBox('Install ODBC', mbError, MB_YESNO);

  if (CurStep = ssPostInstall)  Then
  Begin
    if IsTaskSelected('envPath') then 
      EnvAddPath(ExpandConstant('{app}'));

    If IsTaskSelected('firewall') Then
      SetFirewallException('Fhir Server', ExpandConstant('{app}')+'\FHIRServer.exe');

    if IsTaskSelected('svcInst') Then
    begin
      disp := 'FHIR Server ('+'{#SetupSetting("AppVerName")}'+')';
      if ServiceExists('FHIRServer') then
        SimpleDeleteService('FHIRServer');
        
      if not SimpleCreateService('FHIRServer', disp, ExpandConstant('{app}')+'\FHIRServer.exe', DetermineStartMode, ServicePage.Values[0], ServicePage.Values[1], false, false) then
        MsgBox('Unable to Register FHIRServer Service', mbError, MB_OK);
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
    EnvRemovePath(ExpandConstant('{app}'));
    RemoveFirewallException(ExpandConstant('{app}')+'FHIRServer.exe');
  End;
End;
