:: requirements
:: you need to have gh installed (https://cli.github.com/)
:: you need to have dcc64 + the wind64 release dcus from a dellphi install in c:\tools\dcc64\dcc64
:: you need ksign installed (https://www.ksoftware.net/code-signing-certificates), along with signing certificate (which is not in the github repo)
:: you need to have innosetup 6 installed 

:: lazbuild, dcc, gh, iscc and signtool have to be on the path

:: ---- initial set up ----------
:: user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
:: the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

:: %1 is the version we're going to release

IF %2.==. GOTO No2
set "tmp=%2"

:No2

:: @echo off
:: checks
echo === getting ready ===

:: write version and date to source code 
utilities\codescan\codescan.exe -check library\version.inc -message "Not run from the right directory - run in the root directory of the repo" || goto :error
del library\version.inc
utilities\codescan\codescan.exe -check !library\version.inc -message "setting up the version failed" || goto :error
utilities\codescan\codescan.exe -version %1
utilities\codescan\codescan.exe -check library\version.inc -message "saving the version failed" || goto :error


:: ok. we're good to go...

echo === build FHIRConsole === 
del exec\64\*.exe /q /s 1>nul
call clean 1>nul
%tmp%\tools\lazarus\lazbuild.exe -B c:\work\fhirserver\server\fhirconsole.lpi --build-mode=win64-release -q -q
utilities\codescan\codescan.exe -check exec\64\fhirconsole.exe -message "Building the console failed" || goto :error

echo === build FHIRServer (debug) === 
call clean 1>nul
del server\FHIRServer.cfg
copy server\FHIRServer.debug.cfg server\FHIRServer.cfg
cd server
c:\tools\dcc64\dcc64 FHIRServer.dpr -Q
cd ..
utilities\codescan\codescan.exe -check exec\64\fhirserver.exe -message "Building the Debug server failed" || goto :error
copy exec\64\fhirserver.exe exec\64\fhirserver.debug.exe
del exec\64\fhirserver.exe 

echo === build FHIRServer (release) === 
call clean 1>nul
del server\FHIRServer.cfg
copy server\FHIRServer.release.cfg server\FHIRServer.cfg
cd server
c:\tools\dcc64\dcc64 FHIRServer.dpr -Q
cd ..
utilities\codescan\codescan.exe -check exec\64\fhirserver.exe -message "Building the server failed" || goto :error

echo === build FHIRToolkit ===
call clean 1>nul
%tmp%\tools\lazarus\lazbuild.exe -B c:\work\fhirserver\toolkit2\fhirtoolkit.lpi --build-mode=win64-release -q -q
utilities\codescan\codescan.exe -check exec\64\fhirtoolkit.exe -message "Building the toolkit failed" || goto :error

echo All compile done

:: todo: sign the 4 binaries
:: echo === sign the binaries ===

:: build the web file

del exec\pack\fhirserver.web 
utilities\codescan\codescan.exe -check !exec\pack\fhirserver.web -message "Deleting the web file failed" || goto :error
"C:\Program Files\7-Zip\7z" a -r exec\pack\fhirserver.web server\web\*.*
utilities\codescan\codescan.exe -check exec\pack\fhirserver.web -message "Creating the web file failed" || goto :error

:: OK, now build the installers 

del install\build\*.exe

utilities\codescan\codescan.exe -install install\install.iss -product FHIRServer -version %1 || goto :error
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" install\install.iss
utilities\codescan\codescan.exe -install install\install-tk.iss -product FHIRToolkit -version %1 || goto :error
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" install\install-tk.iss

:: now time to do the github release

:: do github release

echo All OK
pause
exit /b 0

:error
echo Build failed!
pause
exit /b %errorlevel%
