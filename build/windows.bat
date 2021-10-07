Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

IF %1.==. GOTO No1
set "tmp=%1"

:No1

cd /d %tmp%

Rem ---- install the compiler ----------

rem ---- download the installer

md tools
curl -L https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0b/fpclazup-x86_64-win64.exe --output tools\fpclazup.exe

rem -- run the installer- will finish with a full install of Lazarus 

tools\fpclazup --fpcVersion=trunk.gitlab --lazVersion=trunk.gitlab --installdir=tools --noconfirm 

Rem ---- get the source we depend on ----------

md source
cd source

git clone https://github.com/dezlov/PascalTZ 
git clone https://github.com/grahamegrieve/delphi-markdown 
git clone https://github.com/mriscoc/extrasyn 
git clone https://github.com/grahamegrieve/HtmlViewer
git clone https://github.com/grahamegrieve/lazarus-ide-tester 
git clone https://github.com/Xor-el/QRCodeGenLib4Pascal

cd PascalTZ 
git pull
cd ..

cd delphi-markdown 
git pull
cd ..

cd extrasyn 
git pull
cd ..

cd HtmlViewer
git pull
cd ..

cd lazarus-ide-tester 
git pull
cd ..

cd QRCodeGenLib4Pascal
git pull
cd ..

cd ..

Rem ---- register the source with lazarus ----------

tools\lazarus\lazbuild.exe source\PascalTZ\package\pascaltz.lpk 
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters.lpk 
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters_dsgn.lpk 
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\package\idetester.lpk 
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\ide\idetester_dsgn.lpk 
tools\lazarus\lazbuild.exe source\HtmlViewer\package\FrameViewer09.lpk 
tools\lazarus\lazbuild.exe source\QRCodeGenLib4Pascal\QRCodeGenLib\src\Packages\FPC\QRCodeGenLib4PascalPackage.lpk
tools\lazarus\lazbuild.exe source\delphi-markdown\packages\markdownengine.lpk 
tools\lazarus\lazbuild.exe source\delphi-markdown\tests\markdowntests.lpk

Rem ----  back to the server ----------

chdir /d %FSDIR% &rem restore current directory
cd ..

%tmp%\tools\lazarus\lazbuild.exe packages/fhir.lpk 
%tmp%\tools\lazarus\lazbuild.exe packages/fhir2.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir3.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir4.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir5.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_xver.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fsl.lpk
%tmp%\tools\lazarus\lazbuild.exe packages/fhir_fui.lpk
%tmp%\tools\lazarus\lazbuild.exe server/fhirconsole.lpi
%tmp%\tools\lazarus\lazbuild.exe server/fhirserver.lpr
%tmp%\tools\lazarus\lazbuild.exe toolkit2/fhirtoolkit.lpr
    
pause
