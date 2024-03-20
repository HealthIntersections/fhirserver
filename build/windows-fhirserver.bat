Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=c:\temp"

md exec\64
copy exec\pack\*.cfg exec\64\
copy exec\pack\*.dat exec\64\
copy exec\pack\w64\*.dll exec\64\

del exec\64\*.exe 

IF %1.==. GOTO No1
set "tmp=%1"

:No1

del *.ppu /s /q
del *.o /s /q

%tmp%\tools\lazarus\lazbuild.exe packages/fhir_indy.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fsl.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fcomp.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir2.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir3.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4b.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir5.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_xver.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fui.lpk -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe utilities/codescan/codescan.lpi --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64 -q -q --build-all
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64 -q -q --build-all
    
    
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

	
rem IF EXIST "C:\Users\graha\Health Intersections Dropbox\Health Intersections Team Folder\fhirserver\win64" (    
copy exec\64\*.exe "C:\Users\graha\Health Intersections Dropbox\Health Intersections Team Folder\fhirserver\win64"
copy exec\pack\fhirserver.web "C:\Users\graha\Health Intersections Dropbox\Health Intersections Team Folder\fhirserver\win64"
rem }

IF EXIST exec\64\fhirserver.exe echo Success!
rem 
rem echo Failed (no server executable found)
rem 

chdir /d %FSDIR% 
