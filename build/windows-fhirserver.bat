Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

md ..\exec\64
copy ..\exec\pack\*.cfg ..\exec\64\
copy ..\exec\pack\*.dat ..\exec\64\
copy ..\exec\pack\w64\*.dll ..\exec\64\

IF %1.==. GOTO No1
set "tmp=%1"

:No1


echo ## compile packages/fhir_indy.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_indy.lpk -q -q --build-all

echo ## compile packages/fhir_fsl.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fsl.lpk -q -q --build-all

echo ## compile packages/fcomp.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fcomp.lpk -q -q --build-all

echo ## compile packages/fhir.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir.lpk -q -q --build-all

echo ## compile packages/fhir2.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir2.lpk -q -q --build-all

echo ## compile packages/fhir3.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir3.lpk -q -q --build-all

echo ## compile packages/fhir4.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4.lpk -q -q --build-all

echo ## compile packages/fhir4b.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4b.lpk -q -q --build-all

echo ## compile packages/fhir5.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir5.lpk -q -q --build-all

echo ## compile packages/fhir_xver.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_xver.lpk -q -q --build-all

echo ## compile packages/fhir_fui.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fui.lpk -q -q --build-all

echo ## compile code tools
%tmp%\tools\lazarus\lazbuild.exe utilities/codescan/codescan.lpi --build-mode=win64 -q -q --build-all

echo ## compile console
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64 -q -q --build-all

echo ## compile server
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64 -q -q --build-all

echo ## compile toolkit
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64 -q -q --build-all
    
copy exec\64\fhirserver.exe exec\64\FHIRServer.debug.exe
del exec\64\fhirserver.exe 
copy exec\64\fhirconsole.exe exec\64\FHIRConsole.debug.exe
del exec\64\fhirconsole.exe 
copy exec\64\fhirtoolkit.exe exec\64\FHIRToolkit.debug.exe
del exec\64\fhirtoolkit.exe 

echo ## compile console
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64-release -q -q --build-all

echo ## compile server
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64-release -q -q --build-all

echo ## compile toolkit
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64-release -q -q --build-all
    
copy exec\64\*.exe "C:\Users\graha\Health Intersections Dropbox\Health Intersections Team Folder\executables\win64"
	
chdir /d %FSDIR% 
