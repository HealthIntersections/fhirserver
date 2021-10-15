Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

IF %1.==. GOTO No1
set "tmp=%1"

:No1

cd ..

echo ## compile packages/fhir_fsl.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fsl.lpk -q

echo ## compile packages/fcomp.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fcomp.lpk -q

echo ## compile packages/fhir.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir.lpk -q

echo ## compile packages/fhir2.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir2.lpk -q

echo ## compile packages/fhir3.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir3.lpk -q

echo ## compile packages/fhir4.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4.lpk -q

echo ## compile packages/fhir5.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir5.lpk -q

echo ## compile packages/fhir_xver.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_xver.lpk -q

echo ## compile packages/fhir_fui.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fui.lpk -q

echo ## compile console
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi --build-mode=win64 -q

echo ## compile server
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr --build-mode=win64 -q

echo ## compile toolkit
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr --build-mode=win64 -q
    
chdir /d %FSDIR% 
