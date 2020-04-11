[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRServer
AppName=Health Intersections FHIR Server
AppVerName=FHRServer v1.0.308

; compilation control
OutputDir=C:\work\fhirserver\install\build
OutputBaseFilename=fhirserver-1.0.308
Compression=lzma2/ultra64

; 64 bit
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x86 x64

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

[Tasks]
; Core related tasks
Name: svcInst;   Description: "Install FHIR Server as a Service"
Name: firewall;  Description: "Allow FHIR Server through the Firewall"
Name: r2; Description: "Configure R2"
Name: r3; Description: "Configure R3"
Name: r4; Description: "Configure R4"
Name: r5; Description: "Configure R5"

[Files]
; installer support
Source: "C:\work\fhirserver\exec\32\installer.dll";               Flags: dontcopy;

; root documentation files
Source: "C:\work\fhirserver\license";                                        DestDir: "{app}";   Flags: ignoreversion; DestName: "license.txt";
Source: "C:\work\fhirserver\readme.md";                                      DestDir: "{app}";   Flags: ignoreversion; DestName: "readme.txt";
Source: "C:\work\fhirserver\install\readme.rtf";                             DestDir: "{app}";   Flags: ignoreversion; DestName: "documentation.rtf";
Source: "C:\work\fhirserver\install\LOINC_short_license.txt";                DestDir: "{app}";   Flags: ignoreversion;

; Executable files
Source: "C:\work\fhirserver\Exec\64\FHIRServer.exe";                         DestDir: "{app}";   Flags: ignoreversion; Check: Is64BitInstallMode
;Source: "C:\work\fhirserver\Exec\64\FHIRServer.debug.exe";                   DestDir: "{app}";   Flags: ignoreversion; Check: Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\64\FastMM_FullDebugMode64.dll";               DestDir: "{app}";   Flags: ignoreversion; Check: Is64BitInstallMode 
Source: "C:\work\fhirserver\Exec\64\ChakraCore.dll";                         DestDir: "{app}";   Flags: ignoreversion; Check: Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\64\sqlite3.dll";                            DestDir: "{app}";   Flags: ignoreversion; Check: Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\32\FHIRServer.exe";                         DestDir: "{app}";   Flags: ignoreversion; Check: not Is64BitInstallMode
;Source: "C:\work\fhirserver\Exec\32\FHIRServer.debug.exe";                   DestDir: "{app}";   Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\32\FastMM_FullDebugMode.dll";               DestDir: "{app}";   Flags: ignoreversion; Check: not Is64BitInstallMode 
Source: "C:\work\fhirserver\Exec\32\ChakraCore.dll";                         DestDir: "{app}";   Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\32\sqlite3.dll";                            DestDir: "{app}";   Flags: ignoreversion; Check: not Is64BitInstallMode

Source: "C:\work\fhirserver\Exec\fhir.ini";                                  DestDir: "{app}";   Flags: ignoreversion onlyifdoesntexist; DestName: "fhirserver.ini"; Permissions: users-full

; Web resources
Source: "C:\work\fhirserver\server\web\*.*";                                        DestDir: {app}\web; Flags: ignoreversion recursesubdirs

; Package Management
Source: "C:\ProgramData\.fhir\packages\packages.ini";               DestDir: "C:\ProgramData\.fhir\packages"; Flags: onlyifdoesntexist

; Terminology resources
Source: "C:\work\fhirserver\Exec\ucum-essence.xml";                   DestDir: "{commonappdata}\FHIRServer"
Source: "C:\work\fhirserver\Exec\lang.txt";                           DestDir: "{commonappdata}\FHIRServer"
Source: "C:\ProgramData\FHIRServer\loinc-2.65.cache";                  DestDir: "{commonappdata}\FHIRServer"

; ssl support files - put in app dir because these may be different to ones already on the machine.
Source: "C:\work\fhirserver\Exec\64\ssleay32.dll";  DestDir: "{app}";      Flags: ignoreversion; Check: Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\64\libeay32.dll";  DestDir: "{app}";      Flags: ignoreversion; Check: Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\32\ssleay32.dll";  DestDir: "{app}";      Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "C:\work\fhirserver\Exec\32\libeay32.dll";  DestDir: "{app}";      Flags: ignoreversion; Check: not Is64BitInstallMode

[INI]
Filename: "{app}\fhirserver.ini"; Section: "web";   Key: "folder";  String: "{app}\web"
Filename: "{app}\fhirserver.ini"; Section: "web";   Key: "clients";  String: "{app}\auth.ini"

[Icons]
Name: "{group}\FHIR Server (Console Mode)";  Filename: "{app}\FHIRServer.exe";     Parameters: "-console";  WorkingDir: "{app}"    
Name: "{group}\FHIR Server Utilities";       Filename: "{app}\FHIRServer.exe";     Parameters: "-manager";  WorkingDir: "{app}"    
Name: "{group}\Documentation";               Filename: "{app}\documentation.rtf";         
Name: "{group}\Ini File";                    Filename: "{app}\fhirserver.ini";         

[Code]
const
  MB_ICONINFORMATION = $40;

var
  DBInstallPageR2 : TWizardPage;
  dbStatusR2 : TLabel;
  dbInstallR2 : TCheckbox;
  dbPackagesR2 : TNewCheckListBox;
  DBInstallPageR3 : TWizardPage;
  dbStatusR3 : TLabel;
  dbInstallR3 : TCheckbox;
  dbPackagesR3 : TNewCheckListBox;
  DBInstallPageR4 : TWizardPage;
  dbStatusR4 : TLabel;
  dbInstallR4 : TCheckbox;
  dbPackagesR4 : TNewCheckListBox;
  DBInstallPageR5 : TWizardPage;
  dbStatusR5 : TLabel;
  dbInstallR5 : TCheckbox;
  dbPackagesR5 : TNewCheckListBox;

  PackagesR2 : TStringList;
  PackagesR3 : TStringList;
  PackagesR4 : TStringList;
  PackagesR5 : TStringList;

  DependenciesPage : TWizardPage;
  ServicePage : TInputQueryWizardPage;
  cbxStartup : TNewComboBox;

  ConnectionPageR2 : TInputQueryWizardPage;
  dbDriverR2 : TNewComboBox;
  dbTypeR2 : TNewComboBox;
  ConnectionPageR3 : TInputQueryWizardPage;
  dbDriverR3 : TNewComboBox;
  dbTypeR3 : TNewComboBox;
  ConnectionPageR4 : TInputQueryWizardPage;
  dbDriverR4 : TNewComboBox;
  dbTypeR4 : TNewComboBox;
  ConnectionPageR5 : TInputQueryWizardPage;
  dbDriverR5 : TNewComboBox;
  dbTypeR5 : TNewComboBox;

  WebPage : TWizardPage;
  webHostName : TNewEdit;
  webOpenPort : TNewEdit;
  webSecurePort : TNewEdit;
  certFile : TNewEdit;
  caCertFile : TNewEdit;
  certPWord : TNewEdit;
  webpop : boolean;

  AdminPage : TInputQueryWizardPage;
  ConfigPage : TInputOptionWizardPage;

  SctInstallPage : TOutputProgressWizardPage;
  LoadInstallPage : TOutputProgressWizardPage;

  VCStatus, MYSQLStatus, MSSQLStatus,  ODBCStatus : TLabel;
  iniName : String;

  VCPath, MYSQLPath, MSSQLPath, ODBCPath:TEdit;

// start up check: a jre is required   

function InitializeSetup(): Boolean;
begin
  Result := true;
  PackagesR2 := TStringList.create;
  PackagesR3 := TStringList.create;
  PackagesR4 := TStringList.create;
  PackagesR5 := TStringList.create;

end;

// ------ Dll Interface ---------------------------------------------------------------------------------
type
  TMyCallback = procedure(IntParam: Integer; StrParam: WideString);

Function MyDllGetString(name : pansichar) : pansichar; external 'MyDllGetString@files:installer.dll stdcall setuponly';
Function MyDllGetIniValue(ini, name : pansichar) : pansichar; external 'MyDllGetIniValue@files:installer.dll stdcall setuponly';
Procedure MyDllSetIniValue(ini, name, value : pansichar); external 'MyDllSetIniValue@files:installer.dll stdcall setuponly';
Function MyDllCheckDatabase(DBDriver, Server, Database, Username, Password, Version : PAnsiChar) : PAnsiChar; external 'MyDllCheckDatabase@files:installer.dll stdcall setuponly';
Function MyDllInstallDatabase(ExeName, IniName, Password : PAnsiChar; load, packages, mode : PAnsiChar; callback : TMyCallback) : PAnsiChar; external 'MyDllInstallDatabase@files:installer.dll stdcall setuponly';
Function MyDllListPackages(mode : PAnsiChar; Version : PAnsiChar) : PAnsiChar; external 'MyDllListPackages@files:installer.dll stdcall setuponly';
Function MyDllDownloadPackages(mode : PAnsiChar; urls : PAnsiChar; callback: TMyCallback) : PAnsiChar; external 'MyDllDownloadPackages@files:installer.dll stdcall setuponly';


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
  idpDownloadFile(MySQLPath.text, localfilename);
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
  idpDownloadFile(VCPath.text, localfilename);
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
  idpDownloadFile(MSSQLPath.text, localfilename);
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
  idpDownloadFile(ODBCPath.text, localfilename);
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

procedure loadWebDetailsPage;
begin
  webHostName.Text := MyDllGetIniValue(iniName, 'web.host');
  webOpenPort.Text := MyDllGetIniValue(iniName, 'web.http');
  webSecurePort.Text := MyDllGetIniValue(iniName, 'web.https');
  CertFile.Text := MyDllGetIniValue(iniName, 'web.certname');
  caCertFile.Text := MyDllGetIniValue(iniName, 'web.cacertname');
  certPWord.Text := MyDllGetIniValue(iniName, 'web.certpword');
end;

function checkWebDetailsPage : boolean;
var
  p : integer;
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
   end
  else if (webSecurePort.Text <> '') then
  begin
    p := StrToIntDef(webSecurePort.Text, 0);
    if (p <= 0) or (p > 32760) then
    begin
      MsgBox('Secure Port is illegal', mbError, MB_OK);
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
end;



Procedure CreateAdminPage;
Begin
  AdminPage := CreateInputQueryPage(WebPage.id, 'Administration', '', 'Enter Master Administration details (for administering user accounts)');
  AdminPage.Add('Email:', False);
  AdminPage.Add('Username', False);
  AdminPage.Add('Password', True);
  AdminPage.Add('Password Confirmation', True);
end;

procedure loadAdminPage;
begin
  AdminPage.Values[0] := MyDllGetIniValue(iniName, 'admin.email');
  AdminPage.Values[1] := MyDllGetIniValue(iniName, 'admin.username');
end;


function checkAdminPage : boolean;
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



// ------ Database Page ---------------------------------------------------------------------------------

procedure dbDriverR2_OnChange(Sender: TObject);
begin
  if (pos('MySQL',dbdriverR2.text)<>0) then 
    dbtypeR2.itemIndex:=1;
  if (pos('SQL Server',dbdriverR2.text)<>0) then 
    dbtypeR2.itemIndex:=0;
end;

procedure dbDriverR3_OnChange(Sender: TObject);
begin
  if (pos('MySQL',dbdriverR3.text)<>0) then 
    dbtypeR3.itemIndex:=1;
  if (pos('SQL Server',dbdriverR3.text)<>0) then 
    dbtypeR3.itemIndex:=0;
end;

procedure dbDriverR4_OnChange(Sender: TObject);
begin
  if (pos('MySQL',dbdriverR4.text)<>0) then 
    dbtypeR4.itemIndex:=1;
  if (pos('SQL Server',dbdriverR4.text)<>0) then 
    dbtypeR4.itemIndex:=0;
end;

procedure dbDriverR5_OnChange(Sender: TObject);
begin
  if (pos('MySQL',dbdriverR5.text)<>0) then 
    dbtypeR5.itemIndex:=1;
  if (pos('SQL Server',dbdriverR5.text)<>0) then 
    dbtypeR5.itemIndex:=0;
end;

// ----- Database -----------------------------------------------------------------------------

Procedure CreateConnectionPageR2;
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
  ConnectionPageR2 := CreateInputQueryPage(ConfigPage.id, 'R2 Database Location', 'Select the location of the R2 database', 'Leave Username and Password blank to use Windows Authentication');
  ConnectionPageR2.add('Server', false);
  ConnectionPageR2.add('Database', false);
  ConnectionPageR2.add('UserName', false);
  ConnectionPageR2.add('Password', true);
//  ConnectionPageR2.add('Driver (default = SQL Server Native Client 11.0 / MySQL ODBC 5.3 Unicode Driver)', false);

  dbDriverR2 := TNewComboBox.Create(ConnectionPageR2);
  dbDriverR2.Width := ConnectionPageR2.SurfaceWidth;
  dbDriverR2.Parent := ConnectionPageR2.Surface;
  dbDriverR2.Style := csDropDownList;
  dbDriverR2.OnChange := @dbDriverR2_OnChange;

  dbTypeR2 := TNewComboBox.Create(ConnectionPageR2);
  dbTypeR2.Width := ConnectionPageR2.SurfaceWidth;
  dbTypeR2.Parent := ConnectionPageR2.Surface;
  dbTypeR2.Style := csDropDownList;
  dbtypeR2.enabled:=false;

  if RegGetValueNames(HKLM, 'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers', Names) then
  begin
    for iI := 0 to GetArrayLength(Names)-1 do
       dbDriverR2.Items.Add(Names[Ii]);
  end 
  else
  begin
    // add any code to handle failure here
  end;

  dbTypeR2.Items.Add('mssql');
  dbTypeR2.Items.Add('mysql');

  for index:=0 to 3 do 
  begin
    ConnectionPageR2.Edits[Index].Top := ConnectionPageR2.Edits[Index].Top - ShrinkSpace*(index);
    ConnectionPageR2.PromptLabels[Index].Top := ConnectionPageR2.PromptLabels[Index].Top - ShrinkSpace*(index) +2;
  end;
  dbtypeR2.top:= ConnectionPageR2.PromptLabels[3].Top + ConnectionPageR2.PromptLabels[3].Top - ConnectionPageR2.PromptLabels[2].Top
end;

Procedure CreateConnectionPageR3;
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
  ConnectionPageR3 := CreateInputQueryPage(DBInstallPageR2.id, 'R3 Database Location', 'Select the location of the R3 database', 'Leave Username and Password blank to use Windows Authentication');
  ConnectionPageR3.add('Server', false);
  ConnectionPageR3.add('Database', false);
  ConnectionPageR3.add('UserName', false);
  ConnectionPageR3.add('Password', true);
//  ConnectionPageR3.add('Driver (default = SQL Server Native Client 11.0 / MySQL ODBC 5.3 Unicode Driver)', false);

  dbDriverR3 := TNewComboBox.Create(ConnectionPageR3);
  dbDriverR3.Width := ConnectionPageR3.SurfaceWidth;
  dbDriverR3.Parent := ConnectionPageR3.Surface;
  dbDriverR3.Style := csDropDownList;
  dbDriverR3.OnChange := @dbDriverR3_OnChange;

  dbTypeR3 := TNewComboBox.Create(ConnectionPageR3);
  dbTypeR3.Width := ConnectionPageR3.SurfaceWidth;
  dbTypeR3.Parent := ConnectionPageR3.Surface;
  dbTypeR3.Style := csDropDownList;
  dbtypeR3.enabled:=false;

  if RegGetValueNames(HKLM, 'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers', Names) then
  begin
    for iI := 0 to GetArrayLength(Names)-1 do
       dbDriverR3.Items.Add(Names[Ii]);
  end 
  else
  begin
    // add any code to handle failure here
  end;

  dbTypeR3.Items.Add('mssql');
  dbTypeR3.Items.Add('mysql');

  for index:=0 to 3 do 
  begin
    ConnectionPageR3.Edits[Index].Top := ConnectionPageR3.Edits[Index].Top - ShrinkSpace*(index);
    ConnectionPageR3.PromptLabels[Index].Top := ConnectionPageR3.PromptLabels[Index].Top - ShrinkSpace*(index) +2;
  end;
  dbtypeR3.top:= ConnectionPageR3.PromptLabels[3].Top + ConnectionPageR3.PromptLabels[3].Top - ConnectionPageR3.PromptLabels[2].Top
end;

Procedure CreateConnectionPageR4;
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
  ConnectionPageR4 := CreateInputQueryPage(DBInstallPageR3.id, 'R4 Database Location', 'Select the location of the R4 database', 'Leave Username and Password blank to use Windows Authentication');
  ConnectionPageR4.add('Server', false);
  ConnectionPageR4.add('Database', false);
  ConnectionPageR4.add('UserName', false);
  ConnectionPageR4.add('Password', true);
//  ConnectionPageR4.add('Driver (default = SQL Server Native Client 11.0 / MySQL ODBC 5.3 Unicode Driver)', false);

  dbDriverR4 := TNewComboBox.Create(ConnectionPageR4);
  dbDriverR4.Width := ConnectionPageR4.SurfaceWidth;
  dbDriverR4.Parent := ConnectionPageR4.Surface;
  dbDriverR4.Style := csDropDownList;
  dbDriverR4.OnChange := @dbDriverR4_OnChange;

  dbTypeR4 := TNewComboBox.Create(ConnectionPageR4);
  dbTypeR4.Width := ConnectionPageR4.SurfaceWidth;
  dbTypeR4.Parent := ConnectionPageR4.Surface;
  dbTypeR4.Style := csDropDownList;
  dbtypeR4.enabled:=false;

  if RegGetValueNames(HKLM, 'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers', Names) then
  begin
    for iI := 0 to GetArrayLength(Names)-1 do
       dbDriverR4.Items.Add(Names[Ii]);
  end 
  else
  begin
    // add any code to handle failure here
  end;

  dbTypeR4.Items.Add('mssql');
  dbTypeR4.Items.Add('mysql');

  for index:=0 to 3 do 
  begin
    ConnectionPageR4.Edits[Index].Top := ConnectionPageR4.Edits[Index].Top - ShrinkSpace*(index);
    ConnectionPageR4.PromptLabels[Index].Top := ConnectionPageR4.PromptLabels[Index].Top - ShrinkSpace*(index) +2;
  end;

  dbtypeR4.top:= ConnectionPageR4.PromptLabels[3].Top + ConnectionPageR4.PromptLabels[3].Top - ConnectionPageR4.PromptLabels[2].Top;
end;

Procedure CreateConnectionPageR5;
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
  ConnectionPageR5 := CreateInputQueryPage(DBInstallPageR4.id, 'R5 Database Location', 'Select the location of the R5 database', 'Leave Username and Password blank to use Windows Authentication');
  ConnectionPageR5.add('Server', false);
  ConnectionPageR5.add('Database', false);
  ConnectionPageR5.add('UserName', false);
  ConnectionPageR5.add('Password', true);
//  ConnectionPageR4.add('Driver (default = SQL Server Native Client 11.0 / MySQL ODBC 5.3 Unicode Driver)', false);

  dbDriverR5 := TNewComboBox.Create(ConnectionPageR5);
  dbDriverR5.Width := ConnectionPageR5.SurfaceWidth;
  dbDriverR5.Parent := ConnectionPageR5.Surface;
  dbDriverR5.Style := csDropDownList;
  dbDriverR5.OnChange := @dbDriverR5_OnChange;

  dbTypeR5 := TNewComboBox.Create(ConnectionPageR5);
  dbTypeR5.Width := ConnectionPageR5.SurfaceWidth;
  dbTypeR5.Parent := ConnectionPageR5.Surface;
  dbTypeR5.Style := csDropDownList;
  dbtypeR5.enabled:=false;

  if RegGetValueNames(HKLM, 'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers', Names) then
  begin
    for iI := 0 to GetArrayLength(Names)-1 do
       dbDriverR5.Items.Add(Names[Ii]);
  end 
  else
  begin
    // add any code to handle failure here
  end;

  dbTypeR5.Items.Add('mssql');
  dbTypeR5.Items.Add('mysql');

  for index:=0 to 3 do 
  begin
    ConnectionPageR5.Edits[Index].Top := ConnectionPageR5.Edits[Index].Top - ShrinkSpace*(index);
    ConnectionPageR5.PromptLabels[Index].Top := ConnectionPageR5.PromptLabels[Index].Top - ShrinkSpace*(index) +2;
  end;

  dbtypeR5.top:= ConnectionPageR5.PromptLabels[3].Top + ConnectionPageR5.PromptLabels[3].Top - ConnectionPageR5.PromptLabels[2].Top;
end;


procedure loadConnectionPageR2;
var
  s : String;
begin
  ConnectionPageR2.values[0] := MyDllGetIniValue(iniName, 'db.r2.server');
  ConnectionPageR2.values[1] := MyDllGetIniValue(iniName, 'db.r2.database');
  ConnectionPageR2.values[2] := MyDllGetIniValue(iniName, 'db.r2.username');
  ConnectionPageR2.values[3] := MyDllGetIniValue(iniName, 'db.r2.password');
  dbDriverR2.itemindex := dbDriverR2.items.indexof(MyDllGetIniValue(iniName, 'db.r2.driver'));

  s := MyDllGetIniValue(iniName, 'db.r2.type');
  if s = '' then 
    dbDriverR2_OnChange(dbDriverR2)
  else
    dbTypeR2.itemindex := dbTypeR2.items.indexof(MyDllGetIniValue(iniName, 'db.r2.type'));
end;

procedure loadConnectionPageR3;
var
  s : String;
begin
  ConnectionPageR3.values[0] := MyDllGetIniValue(iniName, 'db.r3.server');
  ConnectionPageR3.values[1] := MyDllGetIniValue(iniName, 'db.r3.database');
  ConnectionPageR3.values[2] := MyDllGetIniValue(iniName, 'db.r3.username');
  ConnectionPageR3.values[3] := MyDllGetIniValue(iniName, 'db.r3.password');
  dbDriverR3.itemindex := dbDriverR3.items.indexof(MyDllGetIniValue(iniName, 'db.r3.driver'));

  s := MyDllGetIniValue(iniName, 'db.r3.type');
  if s = '' then 
    dbDriverR3_OnChange(dbDriverR3)
  else
    dbTypeR3.itemindex := dbTypeR3.items.indexof(MyDllGetIniValue(iniName, 'db.r3.type'));
end;

procedure loadConnectionPageR4;
var
  s : String;
begin
  ConnectionPageR4.values[0] := MyDllGetIniValue(iniName, 'db.r4.server');
  ConnectionPageR4.values[1] := MyDllGetIniValue(iniName, 'db.r4.database');
  ConnectionPageR4.values[2] := MyDllGetIniValue(iniName, 'db.r4.username');
  ConnectionPageR4.values[3] := MyDllGetIniValue(iniName, 'db.r4.password');
  dbDriverR4.itemindex := dbDriverR4.items.indexof(MyDllGetIniValue(iniName, 'db.r4.driver'));

  s := MyDllGetIniValue(iniName, 'db.r4.type');
  if s = '' then 
    dbDriverR4_OnChange(dbDriverR4)
  else
    dbTypeR4.itemindex := dbTypeR4.items.indexof(MyDllGetIniValue(iniName, 'db.r4.type'));
end;


procedure loadConnectionPageR5;
var
  s : String;
begin
  ConnectionPageR5.values[0] := MyDllGetIniValue(iniName, 'db.r5.server');
  ConnectionPageR5.values[1] := MyDllGetIniValue(iniName, 'db.r5.database');
  ConnectionPageR5.values[2] := MyDllGetIniValue(iniName, 'db.r5.username');
  ConnectionPageR5.values[3] := MyDllGetIniValue(iniName, 'db.r5.password');
  dbDriverR5.itemindex := dbDriverR5.items.indexof(MyDllGetIniValue(iniName, 'db.r5.driver'));

  s := MyDllGetIniValue(iniName, 'db.r5.type');
  if s = '' then 
    dbDriverR5_OnChange(dbDriverR5)
  else
    dbTypeR5.itemindex := dbTypeR5.items.indexof(MyDllGetIniValue(iniName, 'db.r5.type'));
end;


function checkDatabaseR2 : boolean;
var 
  s, c : String;
begin
  s := MyDllCheckDatabase(dbDriverR2.text, ConnectionPageR2.values[0], ConnectionPageR2.values[1], ConnectionPageR2.values[2], ConnectionPageR2.values[3], '1.0.2');
  c := s[1];
  s := copy(s, 2, length(s));
  result := true;
  if c = '1' then // couldn't connect
  begin 
    result := false;
    MsgBox(s, mbError, MB_OK)
  end
  else 
  begin 
    if c = '2' then
    begin
      dbStatusR2.caption := 'Database is not initialized ('+s+')'
      dbInstallR2.checked := true;
    end 
    else if c = '3' then
    begin
      dbStatusR2.caption := 'Database is initialized, but needs re-initialized ('+s+')'
      dbInstallR2.checked := true;
    end 
    else
      dbStatusR2.caption := 'Database is ready for use, but can be re-initialized';
  end;    
end;

function checkDatabaseR3 : boolean;
var 
  s, c : String;
begin
  s := MyDllCheckDatabase(dbDriverR3.text, ConnectionPageR3.values[0], ConnectionPageR3.values[1], ConnectionPageR3.values[2], ConnectionPageR3.values[3], '3.0.2');
  c := s[1];
  s := copy(s, 2, length(s));
  result := true;
  if c = '1' then // couldn't connect
  begin 
    result := false;
    MsgBox(s, mbError, MB_OK)
  end
  else 
  begin 
    if c = '2' then
    begin
      dbStatusR3.caption := 'Database is not initialized ('+s+')'
      dbInstallR3.checked := true;
    end 
    else if c = '3' then
    begin
      dbStatusR3.caption := 'Database is initialized, but needs re-initialized ('+s+')'
      dbInstallR3.checked := true;
    end 
    else
      dbStatusR3.caption := 'Database is ready for use, but can be re-initialized';
  end;    
end;

function checkDatabaseR4 : boolean;
var 
  s, c : String;
begin
  s := MyDllCheckDatabase(dbDriverR4.text, ConnectionPageR4.values[0], ConnectionPageR4.values[1], ConnectionPageR4.values[2], ConnectionPageR4.values[3], '4.0.1');
  c := s[1];
  s := copy(s, 2, length(s));
  result := true;
  if c = '1' then // couldn't connect
  begin 
    result := false;
    MsgBox(s, mbError, MB_OK)
  end
  else 
  begin 
    if c = '2' then
    begin
      dbStatusR4.caption := 'Database is not initialized ('+s+')'
      dbInstallR4.checked := true;
    end 
    else if c = '3' then
    begin
      dbStatusR4.caption := 'Database is initialized, but needs re-initialized ('+s+')'
      dbInstallR4.checked := true;
    end 
    else
      dbStatusR4.caption := 'Database is ready for use, but can be re-initialized';
  end;    
end;


function checkDatabaseR5 : boolean;
var 
  s, c : String;
begin
  s := MyDllCheckDatabase(dbDriverR5.text, ConnectionPageR5.values[0], ConnectionPageR5.values[1], ConnectionPageR5.values[2], ConnectionPageR5.values[3], '4.2.0');
  c := s[1];
  s := copy(s, 2, length(s));
  result := true;
  if c = '1' then // couldn't connect
  begin 
    result := false;
    MsgBox(s, mbError, MB_OK)
  end
  else 
  begin 
    if c = '2' then
    begin
      dbStatusR5.caption := 'Database is not initialized ('+s+')'
      dbInstallR5.checked := true;
    end 
    else if c = '3' then
    begin
      dbStatusR5.caption := 'Database is initialized, but needs re-initialized ('+s+')'
      dbInstallR5.checked := true;
    end 
    else
      dbStatusR5.caption := 'Database is ready for use, but can be re-initialized';
  end;    
end;



Procedure CreateDBInstallPageR2;
var
  lbl : TLabel; 
begin
  DBInstallPageR2 := CreateCustomPage(ConnectionPageR2.id, 'R2 Database Initialization', 'Choose R2 Initialization Options');
  // a status label
  dbStatusR2 := TLabel.Create(DBInstallPageR2);
  dbStatusR2.Caption := '(DB Status)'
  dbStatusR2.Top := ScaleX(5);
  dbStatusR2.Left := 5;
  dbStatusR2.Width := DBInstallPageR2.Surface.Width;
  dbStatusR2.Autosize := false;
  dbStatusR2.WordWrap := true;
  dbStatusR2.Height := ScaleX(30);
  dbStatusR2.Parent := DBInstallPageR2.Surface;

  dbInstallR2 := TCheckBox.create(DBInstallPageR2);
  dbInstallR2.Caption := 'Initialize the database, with the following optional packages:';
  dbInstallR2.Top := ScaleX(25);
  dbInstallR2.Left := 5;
  dbInstallR2.Width := DBInstallPageR2.Surface.Width;
  dbInstallR2.Parent := DBInstallPageR2.Surface;

  dbPackagesR2 := TNewCheckListBox.create(DBInstallPageR2);
  dbPackagesR2.Top := ScaleX(45);
  dbPackagesR2.Left := 5;
  dbPackagesR2.Width := DBInstallPageR2.Surface.Width-ScaleX(10);
  dbPackagesR2.Parent := DBInstallPageR2.Surface;
  dbPackagesR2.Height := DBInstallPageR2.Surface.height - (dbPackagesR2.Top + 20);

  lbl := TLabel.Create(DBInstallPageR2);
  lbl.Caption := 'Packages will be downloaded as needed'
  lbl.Top := DBInstallPageR2.Surface.height - 15;
  lbl.Left := 5;
  lbl.Width := DBInstallPageR2.Surface.Width;
  lbl.Autosize := false;
  lbl.WordWrap := true;
  lbl.Parent := DBInstallPageR2.Surface;
end;

Procedure CreatePackagesPageR3;
var
  lbl : TLabel; 
begin
  DBInstallPageR3 := CreateCustomPage(ConnectionPageR3.id, 'R3 Database Initialization', 'Choose R3 Initialization Options');
  // a status label
  dbStatusR3 := TLabel.Create(DBInstallPageR3);
  dbStatusR3.Caption := '(DB Status)'
  dbStatusR3.Top := ScaleX(5);
  dbStatusR3.Left := 5;
  dbStatusR3.Width := DBInstallPageR3.Surface.Width;
  dbStatusR3.Autosize := false;
  dbStatusR3.WordWrap := true;
  dbStatusR3.Height := ScaleX(30);
  dbStatusR3.Parent := DBInstallPageR3.Surface;

  dbInstallR3 := TCheckBox.create(DBInstallPageR3);
  dbInstallR3.Caption := 'Initialize the database, with the following optional packages:';
  dbInstallR3.Top := ScaleX(25);
  dbInstallR3.Left := 5;
  dbInstallR3.Width := DBInstallPageR3.Surface.Width;
  dbInstallR3.Parent := DBInstallPageR3.Surface;

  dbPackagesR3 := TNewCheckListBox.create(DBInstallPageR3);
  dbPackagesR3.Top := ScaleX(45);
  dbPackagesR3.Left := 5;
  dbPackagesR3.Width := DBInstallPageR3.Surface.Width-ScaleX(10);
  dbPackagesR3.Parent := DBInstallPageR3.Surface;
  dbPackagesR3.Height := DBInstallPageR3.Surface.height - (dbPackagesR3.Top + 20);

  lbl := TLabel.Create(DBInstallPageR3);
  lbl.Caption := 'Packages will be downloaded as needed'
  lbl.Top := DBInstallPageR3.Surface.height - 15;
  lbl.Left := 5;
  lbl.Width := DBInstallPageR3.Surface.Width;
  lbl.Autosize := false;
  lbl.WordWrap := true;
  lbl.Parent := DBInstallPageR3.Surface;
end;

Procedure CreatePackagesPageR4;
var
  lbl : TLabel; 
begin
  DBInstallPageR4 := CreateCustomPage(ConnectionPageR4.id, 'R4 Database Initialization', 'Choose R4 Initialization Options');
  // a status label
  dbStatusR4 := TLabel.Create(DBInstallPageR4);
  dbStatusR4.Caption := '(DB Status)'
  dbStatusR4.Top := ScaleX(5);
  dbStatusR4.Left := 5;
  dbStatusR4.Width := DBInstallPageR4.Surface.Width;
  dbStatusR4.Autosize := false;
  dbStatusR4.WordWrap := true;
  dbStatusR4.Height := ScaleX(30);
  dbStatusR4.Parent := DBInstallPageR4.Surface;

  dbInstallR4 := TCheckBox.create(DBInstallPageR4);
  dbInstallR4.Caption := 'Initialize the database, with the following optional packages:';
  dbInstallR4.Top := ScaleX(25);
  dbInstallR4.Left := 5;
  dbInstallR4.Width := DBInstallPageR4.Surface.Width;
  dbInstallR4.Parent := DBInstallPageR4.Surface;

  dbPackagesR4 := TNewCheckListBox.create(DBInstallPageR4);
  dbPackagesR4.Top := ScaleX(45);
  dbPackagesR4.Left := 5;
  dbPackagesR4.Width := DBInstallPageR4.Surface.Width-ScaleX(10);
  dbPackagesR4.Parent := DBInstallPageR4.Surface;
  dbPackagesR4.Height := DBInstallPageR4.Surface.height - (dbPackagesR4.Top + 20);

  lbl := TLabel.Create(DBInstallPageR4);
  lbl.Caption := 'Packages will be downloaded as needed'
  lbl.Top := DBInstallPageR4.Surface.height - 15;
  lbl.Left := 5;
  lbl.Width := DBInstallPageR4.Surface.Width;
  lbl.Autosize := false;
  lbl.WordWrap := true;
  lbl.Parent := DBInstallPageR4.Surface;
 end;

Procedure CreatePackagesPageR5;
var
  lbl : TLabel; 
begin
  DBInstallPageR5 := CreateCustomPage(ConnectionPageR5.id, 'R5 Database Initialization', 'Choose R5 Initialization Options');
  // a status label
  dbStatusR5 := TLabel.Create(DBInstallPageR5);
  dbStatusR5.Caption := '(DB Status)'
  dbStatusR5.Top := ScaleX(5);
  dbStatusR5.Left := 5;
  dbStatusR5.Width := DBInstallPageR4.Surface.Width;
  dbStatusR5.Autosize := false;
  dbStatusR5.WordWrap := true;
  dbStatusR5.Height := ScaleX(30);
  dbStatusR5.Parent := DBInstallPageR5.Surface;

  dbInstallR5 := TCheckBox.create(DBInstallPageR5);
  dbInstallR5.Caption := 'Initialize the database, with the following optional packages:';
  dbInstallR5.Top := ScaleX(25);
  dbInstallR5.Left := 5;
  dbInstallR5.Width := DBInstallPageR5.Surface.Width;
  dbInstallR5.Parent := DBInstallPageR5.Surface;

  dbPackagesR5 := TNewCheckListBox.create(DBInstallPageR5);
  dbPackagesR5.Top := ScaleX(45);
  dbPackagesR5.Left := 5;
  dbPackagesR5.Width := DBInstallPageR5.Surface.Width-ScaleX(10);
  dbPackagesR5.Parent := DBInstallPageR5.Surface;
  dbPackagesR5.Height := DBInstallPageR5.Surface.height - (dbPackagesR5.Top + 20);

  lbl := TLabel.Create(DBInstallPageR5);
  lbl.Caption := 'Packages will be downloaded as needed'
  lbl.Top := DBInstallPageR5.Surface.height - 15;
  lbl.Left := 5;
  lbl.Width := DBInstallPageR5.Surface.Width;
  lbl.Autosize := false;
  lbl.WordWrap := true;
  lbl.Parent := DBInstallPageR5.Surface;
 end;

procedure loadR2Packages;
var
  pl, s, t, pi : String;
  i : integer;
begin
  pl := MyDllListPackages('system', '1.0.2');
  
  PackagesR2.Text := pl;
  for i := 0 to packagesR2.count - 1 do 
  begin
    s := packagesR2[i];
    s := copy(s, pos('|', s)+1, length(s));
    t := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    pi := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    if (t = '1') then
      dbPackagesR2.AddCheckBox(pi+': '+s+' (Already installed)', '', 0, True, True, False, True, nil)
    else
      dbPackagesR2.AddCheckBox(pi+': '+s, '', 0, False, True, False, True, nil);
  end;
end;

procedure loadR3Packages;
var
  pl, s, t, pi : String;
  i : integer;
begin
  pl := MyDllListPackages('system', '3.0.2');
  
  PackagesR3.Text := pl;
  for i := 0 to packagesR3.count - 1 do 
  begin
    s := packagesR3[i];
    s := copy(s, pos('|', s)+1, length(s));
    t := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    pi := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    if (t = '1') then
      dbPackagesR3.AddCheckBox(pi+': '+s+' (Already installed)', '', 0, True, True, False, True, nil)
    else
      dbPackagesR3.AddCheckBox(pi+': '+s, '', 0, False, True, False, True, nil);
  end;
end;

procedure loadR4Packages;
var
  pl, s, t, pi : String;
  i : integer;
begin
  pl := MyDllListPackages('system', '4.0.0');
  
  PackagesR4.Text := pl;
  for i := 0 to packagesR4.count - 1 do 
  begin
    s := packagesR4[i];
    s := copy(s, pos('|', s)+1, length(s));
    t := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    pi := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    if (t = '1') then
      dbPackagesR4.AddCheckBox(pi+': '+s+' (Already installed)', '', 0, True, True, False, True, nil)
    else
      dbPackagesR4.AddCheckBox(pi+': '+s, '', 0, False, True, False, True, nil);
  end;
end;


procedure loadR5Packages;
var
  pl, s, t, pi : String;
  i : integer;
begin
  pl := MyDllListPackages('system', '4.2.0');
  
  PackagesR5.Text := pl;
  for i := 0 to packagesR5.count - 1 do 
  begin
    s := packagesR5[i];
    s := copy(s, pos('|', s)+1, length(s));
    t := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    pi := copy(s, 1, pos('|', s)-1);
    s := copy(s, pos('|', s)+1, length(s));
    if (t = '1') then
      dbPackagesR5.AddCheckBox(pi+': '+s+' (Already installed)', '', 0, True, True, False, True, nil)
    else
      dbPackagesR5.AddCheckBox(pi+': '+s, '', 0, False, True, False, True, nil);
  end;
end;

// ------ Wizard Events -------------------------------------------------------------------------------------
            
function ShouldSkipPage(PageID: Integer): Boolean;
begin
  if (PageID = ServicePage.Id) Then
    result := not IsTaskSelected('svcInst')
  else if (PageID = AdminPage.Id) or (PageID = ConfigPage.Id) Then
    result := not (IsTaskSelected('r2') or IsTaskSelected('r3') or IsTaskSelected('r4'))
  else if (PageID = DBInstallPageR2.Id) or (PageID = ConnectionPageR2.Id) Then
    result := not (IsTaskSelected('r2'))
  else if (PageID = DBInstallPageR3.Id) or (PageID = ConnectionPageR3.Id) Then
    result := not (IsTaskSelected('r3'))
  else if (PageID = DBInstallPageR4.Id) or (PageID = ConnectionPageR4.Id) Then
    result := not (IsTaskSelected('r4'))
  else if (PageID = DBInstallPageR5.Id) or (PageID = ConnectionPageR5.Id) Then
    result := not (IsTaskSelected('r5'))
  else
    result := false;
end;
                        
function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if (CurpageID = wpSelectTasks) Then
  Begin
    iniName := ExpandConstant('{app}')+'\fhirserver.ini';
    configureDependenciesPage;
    loadWebDetailsPage;
    loadAdminPage;
    loadConnectionPageR2;
    loadR2Packages;
    loadConnectionPageR3;
    loadR3Packages;
    loadConnectionPageR4;
    loadR4Packages;
    loadConnectionPageR5;
    loadR5Packages;
    webpop := true;
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
  Else if (WebPage <> Nil) And (CurPageID = WebPage.ID) then
  begin
    result := checkWebDetailsPage;
  end
  Else if (AdminPage <> Nil) And (CurPageID = AdminPage.ID) then
  begin
    result := checkAdminPage;
  end
  Else if (CurPageID = ConnectionPageR2.ID) then
  begin
    result := checkDatabaseR2;
  end
  Else if (CurPageID = ConnectionPageR3.ID) then
  begin
    result := checkDatabaseR3;
  end
  Else if (CurPageID = ConnectionPageR4.ID) then
  begin
    result := checkDatabaseR4;
  end
  Else if (CurPageID = ConnectionPageR5.ID) then
  begin
    result := checkDatabaseR5;
  end
  Else
    Result := True;
end;

Procedure CreatePostInstallPage;
Begin
  LoadInstallPage := CreateOutputProgressPage('Perform Installation tasks', '');
End;

procedure InitializeWizard();
Begin
  CreateDependenciesPage;
  CreateServiceUserPage;
  WebServerPage;
  CreateAdminPage;
  CreateConfigPage;

  CreateConnectionPageR2;
  CreateDBInstallPageR2;
  CreateConnectionPageR3;
  CreatePackagesPageR3;
  CreateConnectionPageR4;
  CreatePackagesPageR4;
  CreateConnectionPageR5;
  CreatePackagesPageR5;
  CreatePostInstallPage;
End;



// ------ Installation Logic -------------------------------------------------------------------------

procedure ConfigureIni;
begin
  MyDllSetIniValue(iniName, 'web.host', webHostName.Text);
  MyDllSetIniValue(iniName, 'web.http', webOpenPort.Text);
  MyDllSetIniValue(iniName, 'web.https', webSecurePort.Text);
  MyDllSetIniValue(iniName, 'web.certname', CertFile.Text);
  MyDllSetIniValue(iniName, 'web.cacertname', caCertFile.Text);
  MyDllSetIniValue(iniName, 'web.certpword', certPWord.Text);
  MyDllSetIniValue(iniName, 'admin.email', AdminPage.Values[0]);
  MyDllSetIniValue(iniName, 'admin.username', AdminPage.Values[1]);
  MyDllSetIniValue(iniName, 'tx.loinc', 'type: loinc; source: '+ExpandConstant('{commonappdata}')+'\FHIRServer\loinc-2.65.cache');
  MyDllSetIniValue(iniName, 'tx.ucum', 'type: ucum; source: '+ExpandConstant('{commonappdata}')+'\FHIRServer\ucum-essence.xml');
  MyDllSetIniValue(iniName, 'tx.lang', 'type: lang; source: '+ExpandConstant('{commonappdata}')+'\FHIRServer\lang.txt')

  if IsTaskSelected('r2') Then
  begin
    MyDllSetIniValue(iniName, 'db.r2.server', ConnectionPageR2.values[0]);
    MyDllSetIniValue(iniName, 'db.r2.database', ConnectionPageR2.values[1]);
    MyDllSetIniValue(iniName, 'db.r2.username', ConnectionPageR2.values[2]);
    MyDllSetIniValue(iniName, 'db.r2.password', ConnectionPageR2.values[3]);
    MyDllSetIniValue(iniName, 'db.r2.driver', dbDriverR2.text);
    if dbTypeR2.ItemIndex=0 then 
      MyDllSetIniValue(iniName, 'db.r2.type', 'mssql');
    if dbTypeR2.ItemIndex=1 then 
      MyDllSetIniValue(iniName, 'db.r2.type', 'mysql');
    MyDllSetIniValue(iniName, 'endpoint.r2', 'path: /r2; version: r2; database: dbr2; validate: true');
   end;

  if IsTaskSelected('r3') Then
  begin
    MyDllSetIniValue(iniName, 'db.r3.server', ConnectionPageR3.values[0]);
    MyDllSetIniValue(iniName, 'db.r3.database', ConnectionPageR3.values[1]);
    MyDllSetIniValue(iniName, 'db.r3.username', ConnectionPageR3.values[2]);
    MyDllSetIniValue(iniName, 'db.r3.password', ConnectionPageR3.values[3]);
    MyDllSetIniValue(iniName, 'db.r3.driver', dbDriverR3.text);
    if dbTypeR3.ItemIndex=0 then 
      MyDllSetIniValue(iniName, 'db.r3.type', 'mssql');
    if dbTypeR3.ItemIndex=1 then 
      MyDllSetIniValue(iniName, 'db.r3.type', 'mysql');
    MyDllSetIniValue(iniName, 'endpoint.r3', 'path: /r3; version: r3; database: dbr3; validate: true');
   end;

  if IsTaskSelected('r4') Then
  begin
    MyDllSetIniValue(iniName, 'db.r4.server', ConnectionPageR4.values[0]);
    MyDllSetIniValue(iniName, 'db.r4.database', ConnectionPageR4.values[1]);
    MyDllSetIniValue(iniName, 'db.r4.username', ConnectionPageR4.values[2]);
    MyDllSetIniValue(iniName, 'db.r4.password', ConnectionPageR4.values[3]);
    MyDllSetIniValue(iniName, 'db.r4.driver', dbDriverR3.text);
    if dbTypeR4.ItemIndex=0 then 
      MyDllSetIniValue(iniName, 'db.r4.type', 'mssql');
    if dbTypeR4.ItemIndex=1 then 
      MyDllSetIniValue(iniName, 'db.r4.type', 'mysql');
    MyDllSetIniValue(iniName, 'endpoint.r4', 'path: /r4; version: r4; database: dbr4; validate: true');
   end;

  if IsTaskSelected('r5') Then
  begin
    MyDllSetIniValue(iniName, 'db.r5.server', ConnectionPageR5.values[0]);
    MyDllSetIniValue(iniName, 'db.r5.database', ConnectionPageR5.values[1]);
    MyDllSetIniValue(iniName, 'db.r5.username', ConnectionPageR5.values[2]);
    MyDllSetIniValue(iniName, 'db.r5.password', ConnectionPageR5.values[3]);
    MyDllSetIniValue(iniName, 'db.r5.driver', dbDriverR5.text);
    if dbTypeR5.ItemIndex=0 then 
      MyDllSetIniValue(iniName, 'db.r5.type', 'mssql');
    if dbTypeR5.ItemIndex=1 then 
      MyDllSetIniValue(iniName, 'db.r5.type', 'mysql');
    MyDllSetIniValue(iniName, 'endpoint.r5', 'path: /r5; version: r5; database: dbr5; validate: true');
   end;

   MyDllSetIniValue(iniName, 'scim.salt', '');
end;

var
  dll : longint;

procedure InitCallback(IntParam: Integer; StrParam: WideString);
begin
  LoadInstallPage.SetProgress(intparam, 100);
  LoadInstallPage.SetText(StrParam, '');
end;

procedure InitialiseDatabase();
var 
  pw, msg : String;
  done : boolean;
  pl2, s, p, u : String;
  i : integer;
  mode : String;
begin
  case ConfigPage.SelectedValueIndex of 
    0 : mode := 'open';
    1 : mode := 'closed';
    2 : mode := 'read-only';
    3 : mode := 'terminology';
  end;
  LoadInstallPage.SetText('Installing Packages...', '');
  LoadInstallPage.SetProgress(0, 100);
  pl2 := '';
  if IsTaskSelected('r2') and dbInstallR2.checked Then
    for i := 0 to dbPackagesR2.items.count - 1 do 
      if dbPackagesR2.checked[i] then
      begin
        s := dbPackagesR2.items[i];
        s := copy(s, 1, pos(':', s)-1);
        pl2 := pl2+','+s;
      end;
  if IsTaskSelected('r3') and dbInstallR3.checked Then 
    for i := 0 to dbPackagesR3.items.count - 1 do 
      if dbPackagesR3.checked[i] then
      begin
        s := dbPackagesR3.items[i];
        s := copy(s, 1, pos(':', s)-1);
        pl2 := pl2+','+s;
      end;
  if IsTaskSelected('r4') and dbInstallR4.checked Then 
    for i := 0 to dbPackagesR4.items.count - 1 do 
     if dbPackagesR4.checked[i] then
     begin
       s := dbPackagesR4.items[i];
        s := copy(s, 1, pos(':', s)-1);
//        if pl2='' then pl2 := s else
       pl2 := pl2+','+s;
     end;
  if IsTaskSelected('r5') and dbInstallR5.checked Then 
    for i := 0 to dbPackagesR5.items.count - 1 do 
     if dbPackagesR5.checked[i] then
     begin
       s := dbPackagesR5.items[i];
        s := copy(s, 1, pos(':', s)-1);
//        if pl2='' then pl2 := s else
       pl2 := pl2+','+s;
     end;

    LoadInstallPage.Show;
    try
      repeat
        done := true;
        msg := MyDllDownloadPackages('system', pl2, @InitCallback);
        if msg <> '' then
          done := MsgBox('Downloading the packages failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
      until done;

    if IsTaskSelected('r2') and dbInstallR2.checked Then
    begin
      pl2 := '';
      for i := 0 to dbPackagesR2.items.count - 1 do 
        if dbPackagesR2.checked[i] then
        begin
          s := dbPackagesR2.items[i];
          s := copy(s, 1, pos(':', s)-1);
          pl2 := pl2+','+s;
        end;
      LoadInstallPage.SetText('Initializing R2 Database...', '');
      LoadInstallPage.SetProgress(0, 100);
      repeat
        done := true;
        pw := ConnectionPageR2.values[3];
        msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\fhirserver.exe', ExpandConstant('{app}')+'\fhirserver.ini', pw, 'r2', pl2, mode, @InitCallback)
        if msg <> '' then
          done := MsgBox('Initializing the database failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
      until done;
    end;

    if IsTaskSelected('r3') and dbInstallR3.checked Then
    begin
      pl2 := '';
      for i := 0 to dbPackagesR3.items.count - 1 do 
        if dbPackagesR3.checked[i] then
        begin
          s := dbPackagesR3.items[i];
          s := copy(s, 1, pos(':', s)-1);
          pl2 := pl2+','+s;
        end;
      LoadInstallPage.SetText('Initializing R3 Database...', '');
      LoadInstallPage.SetProgress(0, 100);
      repeat
        done := true;
        pw := ConnectionPageR3.values[3];
        msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\fhirserver.exe', ExpandConstant('{app}')+'\fhirserver.ini', pw, 'r3', pl2, mode, @InitCallback)
        if msg <> '' then
          done := MsgBox('Initializing the database failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
      until done;
    end;

    if IsTaskSelected('r4') and dbInstallR4.checked Then
    begin
      pl2 := '';
      for i := 0 to dbPackagesR4.items.count - 1 do 
        if dbPackagesR4.checked[i] then
        begin
          s := dbPackagesR4.items[i];
          s := copy(s, 1, pos(':', s)-1);
          if pl2='' then pl2 := s else
            pl2 := pl2+','+s;
        end;
      LoadInstallPage.SetText('Initializing R4 Database...', '');
      LoadInstallPage.SetProgress(0, 100);
      repeat
        done := true;
        pw := ConnectionPageR4.values[3];
          msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\fhirserver.exe', ExpandConstant('{app}')+'\fhirserver.ini', pw, 'r4', pl2, mode, @InitCallback)
        if msg <> '' then
          done := MsgBox('Initializing the database failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
      until done;
    end;

    if IsTaskSelected('r5') and dbInstallR5.checked Then
    begin
      pl2 := '';
      for i := 0 to dbPackagesR5.items.count - 1 do 
        if dbPackagesR5.checked[i] then
        begin
          s := dbPackagesR5.items[i];
          s := copy(s, 1, pos(':', s)-1);
          if pl2='' then pl2 := s else
            pl2 := pl2+','+s;
        end;
      LoadInstallPage.SetText('Initializing R5 Database...', '');
      LoadInstallPage.SetProgress(0, 100);
      repeat
        done := true;
        pw := ConnectionPageR5.values[3];
          msg := MyDllInstallDatabase(ExpandConstant('{app}')+'\fhirserver.exe', ExpandConstant('{app}')+'\fhirserver.ini', pw, 'r5', pl2, mode, @InitCallback)
        if msg <> '' then
          done := MsgBox('Initializing the database failed : '+msg+#13#10+'Try again?', mbError, MB_YESNO) = mrNo;
      until done;
    end;


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
  begin
    MsgBox('Install DB', mbError, MB_YESNO) ;
    ShellExec('', 'msiexec.exe', ExpandConstant('/i "{tmp}\mysql-installer-web-community-5.7.20.0.msi"'),'', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode);
  end;
  if IsTaskSelected('odbc') Then
    MsgBox('Install ODBC', mbError, MB_YESNO);

  if (CurStep = ssPostInstall)  Then
  Begin
    If IsTaskSelected('firewall') Then
      SetFirewallException('Fhir Server', ExpandConstant('{app}')+'\FHIRServer.exe');
    if IsTaskSelected('r2') or IsTaskSelected('r3') or IsTaskSelected('r4') Then
    begin
      ConfigureIni;
      InitialiseDatabase();
    end;
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
    RemoveFirewallException(ExpandConstant('{app}')+'FHIRServer.exe');
  End;
End;
