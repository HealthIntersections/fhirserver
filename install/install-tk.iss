[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRToolkit
AppName=Health Intersections FHIR Toolkit
AppVerName=FHIRToolkit v4.0.0

; compilation control
OutputDir=..\install\build
OutputBaseFilename=fhirtoolkit-win64-4.0.0
Compression=lzma2/ultra64

; 64 bit
ArchitecturesInstallIn64BitMode=x64

; we might be creating a service so we do need this privilege
AllowUNCPath=no
AlwaysShowDirOnReadyPage=yes
ChangesAssociations=yes
DirExistsWarning=auto
DisableStartUpPrompt=yes
MinVersion=0,6.1
UninstallDisplayIcon={app}\fhir.ico
WizardStyle=modern
DisableDirPage=false

; directory management
DefaultDirName={pf}\FHIR Toolkit
DefaultGroupName=FHIR Applications
UninstallFilesDir={app}\uninstall

; win2000+ add/remove programs support
AppPublisher=Health Intersections P/L
AppPublisherURL=http://www.healthintersections.com.au
AppVersion=4.0.0
AppSupportURL=https://github.com/grahamegrieve/fhirserver
AppUpdatesURL=https://github.com/grahamegrieve/fhirserver
AppCopyright=Copyright (c) Health Intersections Pty Ltd 2020+
VersionInfoVersion=4.0.0.0

; dialog support
LicenseFile=..\license

[Files]
; 1. Application executables & Dlls
Source: "..\exec\64\FHIRToolkit.exe";                          DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libsqlite3.dll";                    DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libcrypto-1_1-x64.dll";             DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libssl-1_1-x64.dll";                DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\zlib1.dll";                         DestDir: "{app}";       Flags: ignoreversion
Source: "..\exec\pack\w64\libpdf.dll";                        DestDir: "{app}";       Flags: ignoreversion

; 3. Data Files
Source: "..\exec\pack\ucum.dat";                              DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\lang.dat";                              DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\tz.dat";                                DestDir: "{app}";       Flags: ignoreversion 
Source: "..\exec\pack\fhir-lang.dat";                         DestDir: "{app}";       Flags: ignoreversion 

; 4. Documentation
Source: "..\license";                                         DestDir: "{app}\doco";  Flags: ignoreversion; DestName: "license.txt";
Source: "..\toolkit2\readme.md";                                       DestDir: "{app}\doco";  Flags: ignoreversion; DestName: "readme.txt";

[Icons]
Name: "{group}\FHIR Toolkit";         Filename: "{app}\FHIRToolkit.exe";                      WorkingDir: "{app}"    

[Run]
Filename: "{app}\FHIRToolkit.exe";  Description: "Open the toolkit"; WorkingDir: "{app}"; Flags: postinstall shellexec skipifsilent
