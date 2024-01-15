
git pull

if errorlevel 1 goto Quit
pause

Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than c:\temp (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=c:\temp"

IF %2.==. GOTO No2
set "tmp=%2"

:No2

call build\windows-libraries.bat %tmp%
rem pause

call build\windows-fhirserver.bat %tmp%
rem pause

exec\64\fhirserver.exe -tests -test-settings exec\64\fhir-tests.ini

if errorlevel 1 goto Quit
pause


call build\windows-release %1 %tmp%

:Quit