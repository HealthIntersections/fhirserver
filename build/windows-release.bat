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


git pull

if errorlevel 1 goto Quit

set FSDIR=%CD%
setlocal
set "tmp=c:\temp"

:: %1 is the version we're going to release

IF %2.==. GOTO No2
set "tmp=%2"

:No2

:: check a version was provided
IF %1.==. GOTO NoVer
GOTO OK

:NoVer
echo You need to provide the new version as a parameter to this batch file
pause
exit /b 1

:OK


@echo off

call build\windows-libraries.bat %tmp%

utilities\codescan\codescan.exe -proj-version c:\work\fhirserver\server\fhirconsole.lpi -version %1 -debug false || goto :error
utilities\codescan\codescan.exe -proj-version server\fhirserver.lpi -version %1 -debug true || goto :error
utilities\codescan\codescan.exe -proj-version server\fhirserver.lpi -version %1 -debug false || goto :error
utilities\codescan\codescan.exe -proj-version toolkit2\fhirtoolkit.lpi -version %1 -debug false || goto :error

del exec\64\*.exe /q /s 1>nul
del install\build\*.exe 1>nul
del release-notes-old.md 1>nul
del *.ppu /s /q
del *.o /s /q


%tmp%\tools\lazarus\lazbuild.exe utilities/codescan/codescan.lpi --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64 -q -q --build-all

copy exec\64\fhirserver.exe exec\64\FHIRServer.debug.exe
del exec\64\fhirserver.exe 
copy exec\64\fhirconsole.exe exec\64\FHIRConsole.debug.exe
del exec\64\fhirconsole.exe 
copy exec\64\fhirtoolkit.exe exec\64\FHIRToolkit.debug.exe
del exec\64\fhirtoolkit.exe 

del *.ppu /s /q
del *.o /s /q

%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64-release -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64-release -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64-release -q -q --build-all

utilities\codescan\codescan.exe -check exec\64\fhirconsole.exe -message "Building the console failed" || goto :error
utilities\codescan\codescan.exe -check exec\64\fhirserver.debug.exe -message "Building the Debug server failed" || goto :error
utilities\codescan\codescan.exe -check exec\64\fhirserver.exe -message "Building the server failed" || goto :error
utilities\codescan\codescan.exe -check exec\64\fhirtoolkit.exe -message "Building the toolkit failed" || goto :error

exec\64\fhirserver.debug.exe -tests -test-settings exec\64\fhir-tests.ini -mode brief

if errorlevel 1 goto Quit

:: =========================================================================================
:: todo: sign the 4 binaries
:: echo ## sign the binaries ##
:: the password batch file is never in git, but has the format
:: @echo off
:: setlocal
:: set HI_PASSWORD="...."

signtool sign /f install\cert\healthintersections.cer /d "FHIRServer" /fd SHA256 /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRConsole.exe
signtool sign /f install\cert\healthintersections.cer /d "FHIRServer" /fd SHA256 /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRServer.debug.exe
signtool sign /f install\cert\healthintersections.cer /d "FHIRServer" /fd SHA256 /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRServer.exe
signtool sign /f install\cert\healthintersections.cer /d "FHIRServer" /fd SHA256 /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com exec\64\FHIRToolkit.exe

:: =========================================================================================
:: OK, now build the installers 
echo ## Build Installs ##
utilities\codescan\codescan.exe -install install\install.iss -product FHIRServer -version %1 || goto :error
install\tools\iscc.exe install\install.iss -q
utilities\codescan\codescan.exe -install install\install-tk.iss -product FHIRToolkit -version %1 || goto :error
install\tools\iscc.exe install\install-tk.iss -q
utilities\codescan\codescan.exe -check install\build\fhirserver-win64-%1.exe -message "Creating the server install failed" || goto :error
utilities\codescan\codescan.exe -check install\build\fhirtoolkit-win64-%1.exe -message "Creating the toolkit install failed" || goto :error

signtool sign /f install\cert\healthintersections.cer /fd SHA256 /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com install\build\fhirserver-win64-%1.exe
signtool sign /f install\cert\healthintersections.cer /fd SHA256 /d "FHIRServer" /du "https://github.com/HealthIntersections/fhirserver" /t http://timestamp.sectigo.com install\build\fhirtoolkit-win64-%1.exe

cd install
cd build
..\..\install\tools\7z a -r -tzip fhirserver-win64-%1.zip fhirserver-win64-%1.exe
cd ..
cd ..

:: =========================================================================================
:: now time to do the github release

echo ## GitHub Release ##

install\tools\gh release create v%1 "install\build\fhirserver-win64-%1.exe#Windows Server Installer" "install\build\fhirserver-win64-%1.zip#Windows Server Installer Zip" "install\build\fhirtoolkit-win64-%1.exe#Windows Toolkit Installer" -F release-notes.md

echo ## GitHub Release Done ##

utilities\codescan\codescan.exe -next-version %1

copy release-notes-template.md release-notes.md

echo ## GitHub Push##
git commit -a -m "Release Version %1"
git push 

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


:Quit