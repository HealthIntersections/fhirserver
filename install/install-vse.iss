; todo: spellchecking
; todo: test v1 upgrade. Test evaluation certificate
; todo: mysql license

[Setup]
; identification.
; AppID can never be changed as subsequent installations require the same installation ID each time
AppID=FHIRValueSetEditor
AppName=Health Intersections Value Set Editor
AppVerName=Health Intersections Value Set Editor 0.10.003

; compilation control
OutputDir=C:\work\fhirserver\install\build
OutputBaseFilename=ValueSetEditor_Install_DEV_0.10.0001
Compression=lzma2/ultra64

AlwaysShowDirOnReadyPage=yes
AlwaysShowGroupOnReadyPage=yes
DirExistsWarning=auto
DisableStartUpPrompt=yes
UninstallDisplayIcon=C:\work\fhirserver\Server\fhir.ico
WizardStyle=modern

; directory management
DefaultDirName={pf}\Health Intersections\Value Set Editor
DefaultGroupName=Value Set Editor
UninstallFilesDir={app}

; win2000+ add/remove programs support
AppPublisher=Health Intersections P/L
AppPublisherURL=http://www.healthintersections.com.au
AppVersion=0.01
AppSupportURL=https://www.healthintersections.com.au/txeditor
AppUpdatesURL=https://www.healthintersections.com.au/txeditor
AppCopyright=Copyright © Health Intersections Pty Ltd 2011 - 2014
VersionInfoVersion=0.10.003

; dialog support
LicenseFile=C:\work\fhirserver\install\licence.txt

; directories
;  {app}  - executable
;  {app}\doco - doco
;  {app}\ssl - ssl support 

[Types]
Name: "normal";   Description: "Normal Installation"

[Files]
; root documentation files
Source: "C:\work\com.healthintersections.fhir\ValueSetEditor\install\licence.txt";         DestDir: "{app}";            Flags: ignoreversion;
Source: "C:\work\com.healthintersections.fhir\ValueSetEditor\install\LOINC_short_license.txt";         DestDir: "{app}";            Flags: ignoreversion;

; Executable file
Source: "C:\work\com.healthintersections.fhir\ValueSetEditor\Win32\Debug\ValueSetEditor.exe";       DestDir: "{app}";            Flags: ignoreversion
Source: "C:\work\fhirserver\Libraries\FMM\FastMM_FullDebugMode.dll";  DestDir: "{app}";            Flags: ignoreversion

; doco
Source: "C:\work\com.healthintersections.fhir\ValueSetEditor\doco\*.*";  DestDir: "{app}\doco";            Flags: ignoreversion

; ssl support files - put in app dir because these may be different to ones already on the machine.
Source: "C:\work\fhirserver\Exec\ssleay32.dll";  DestName: "ssleay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\libeay32.dll";  DestName: "libeay32.dll";   DestDir: "{app}";      Flags: ignoreversion
Source: "C:\work\fhirserver\Exec\openssl.exe";   DestName: "openssl.dll";    DestDir: "{app}";      Flags: ignoreversion

[Icons]
Name: "{group}\Value Set Editor";        Filename: "{app}\ValueSetEditor.exe";     WorkingDir: "{app}"    

