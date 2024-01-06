:: requirements
::
:: you need to have the following:
::  * you need to have run windows-all.bat, and pass the same parameter to this batch
::  * dcc64 + the wind64 release dcus from a delphi install in c:\tools\dcc64\dcc64
::  * ksign installed (https://www.ksoftware.net/code-signing-certificates), along with the signing certificate (which is not in the github repo)
::  * innosetup 6 installed 
::  * gh installed (https://cli.github.com/)

:: ---- initial set up ----------
:: user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
:: the folder must exist 

:: =========================================================================================
:: parameters
set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

:: %1 is the version we're going to release

IF %2.==. GOTO No2
set "tmp=%2"

:No2

@echo off

:: =========================================================================================
:: checks
echo ## getting ready ##

:: check a version was provided
IF %1.==. GOTO NoVer
GOTO OK

:NoVer
echo You need to provide the new version as a parameter to this batch file
pause
exit /b 1

:OK

call load-password.bat
echo %HI_PASSWORD%


:: write version and date to source code 
utilities\codescan\codescan.exe -check library\version.inc -message "Not run from the right directory - run in the root directory of the repo" || goto :error
del library\version.inc
utilities\codescan\codescan.exe -check !library\version.inc -message "setting up the version failed" || goto :error
utilities\codescan\codescan.exe -version %1
utilities\codescan\codescan.exe -check library\version.inc -message "saving the version failed" || goto :error

utilities\codescan\codescan.exe -check release-notes.md -message "Please provide some release notes" || goto :error
utilities\codescan\codescan.exe -check install\healthintersections.pfx -message "Code signing Certificate is missing" || goto :error

del exec\64\*.exe /q /s 1>nul
del install\build\*.exe 1>nul
del release-notes-old.md 1>nul

:: ok. we're good to go...

:: =========================================================================================
echo ## build FHIRConsole ## 
call clean 1>nul
utilities\codescan\codescan.exe -proj-version c:\work\fhirserver\server\fhirconsole.lpi -version %1 -debug false || goto :error
%tmp%\tools\lazarus\lazbuild.exe -B server\fhirconsole.lpi --build-mode=win64-release -q -q --build-all
utilities\codescan\codescan.exe -check exec\64\fhirconsole.exe -message "Building the console failed" || goto :error

:: =========================================================================================
echo ## build FHIRServer (debug) ## 
call clean 1>nul
del server\FHIRServer.cfg
copy server\FHIRServer.debug.cfg server\FHIRServer.cfg
utilities\codescan\codescan.exe -proj-version server\fhirserver.dpr -version %1 -debug true || goto :error
cd server
..\install\tools\dcc64.exe FHIRServer.dpr -Q
cd ..
utilities\codescan\codescan.exe -check exec\64\fhirserver.exe -message "Building the Debug server failed" || goto :error
copy exec\64\fhirserver.exe exec\64\FHIRServer.debug.exe
del exec\64\fhirserver.exe 

:: =========================================================================================
echo ## build FHIRServer (release) ## 
call clean 1>nul
del server\FHIRServer.cfg
copy server\FHIRServer.release.cfg server\FHIRServer.cfg
utilities\codescan\codescan.exe -proj-version server\fhirserver.dpr -version %1 -debug false || goto :error
cd server
..\install\tools\dcc64 FHIRServer.dpr -Q
cd ..
utilities\codescan\codescan.exe -check exec\64\fhirserver.exe -message "Building the server failed" || goto :error

:: =========================================================================================
echo ## build FHIRToolkit ##
call clean 1>nul
utilities\codescan\codescan.exe -proj-version toolkit2\fhirtoolkit.lpi -version %1 -debug false || goto :error
%tmp%\tools\lazarus\lazbuild.exe -B toolkit2\fhirtoolkit.lpi --build-mode=win64-release -q -q --build-all
utilities\codescan\codescan.exe -check exec\64\fhirtoolkit.exe -message "Building the toolkit failed" || goto :error

echo All compile done

:: =========================================================================================
:: todo: sign the 4 binaries
:: echo ## sign the binaries ##
:: the password batch file is never in git, but has the format
:: @echo off
:: setlocal
:: set HI_PASSWORD="...."

install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRConsole.exe
install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRServer.debug.exe
install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRServer.exe
install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRToolkit.exe

:: =========================================================================================
:: build the web file
del exec\pack\fhirserver.web 
utilities\codescan\codescan.exe -check !exec\pack\fhirserver.web -message "Deleting the web file failed" || goto :error
cd server
cd web
..\..\install\tools\7z a -r -tzip ..\..\exec\pack\fhirserver.web *.*
cd ..
cd ..
utilities\codescan\codescan.exe -check exec\pack\fhirserver.web -message "Creating the web file failed" || goto :error

:: =========================================================================================
:: OK, now build the installers 
echo ## Build Installs ##
utilities\codescan\codescan.exe -install install\install.iss -product FHIRServer -version %1 || goto :error
install\tools\iscc.exe install\install.iss -q
utilities\codescan\codescan.exe -install install\install-tk.iss -product FHIRToolkit -version %1 || goto :error
install\tools\iscc.exe install\install-tk.iss -q
utilities\codescan\codescan.exe -check install\build\fhirserver-win64-%1.exe -message "Creating the server install failed" || goto :error
utilities\codescan\codescan.exe -check install\build\fhirtoolkit-win64-%1.exe -message "Creating the toolkit install failed" || goto :error

install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com install\build\fhirserver-win64-%1.exe
install\tools\signtool sign /f install\healthintersections.pfx /p %HI_PASSWORD% /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com install\build\fhirtoolkit-win64-%1.exe

set HI_PASSWORD=null
:: =========================================================================================
:: now time to do the github release

echo ## GitHub Release ##
git commit library\version.inc -m "Release Version"
git push 
install\tools\gh release create v%1 "install\build\fhirserver-win64-%1.exe#Windows Server Installer" "install\build\fhirtoolkit-win64-%1.exe#Windows Toolkit Installer" -F release-notes.md
rename release-notes.md release-notes-old.md

utilities\codescan\codescan.exe -next-version %1

:: =========================================================================================
:: echo Post note on Zulip
zulip-send --stream tooling/releases --subject "FHIR Tools" --message "New FHIRServer and Toolkit v"%1" released (see https://github.com/HealthIntersections/fhirserver/releases/v"%1")" --config-file .zuliprc

:: =========================================================================================
echo Build/Release Process Finished
pause
exit /b 0

:: =========================================================================================
:error
echo Build failed!
pause
exit /b %errorlevel%



