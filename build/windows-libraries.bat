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

md source
cd source

git clone https://github.com/grahamegrieve/tzdb
git clone https://github.com/grahamegrieve/delphi-markdown 
git clone https://github.com/grahamegrieve/extrasyn 
git clone https://github.com/grahamegrieve/HtmlViewer
git clone https://github.com/grahamegrieve/lazarus-ide-tester 
git clone https://github.com/grahamegrieve/ZXing.Delphi
git clone https://github.com/FHIR/fhir-test-cases
git clone https://github.com/grahamegrieve/PdfiumLib
git clone --recurse-submodules https://github.com/grahamegrieve/DelphiAST 

cd tzdb 
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

cd ZXing.Delphi
git pull
cd ..

cd fhir-test-cases
git pull
cd ..

cd PdfiumLib
git pull
cd ..

cd DelphiAST
git pull
cd ..

cd ..

Rem -- now build 

tools\lazarus\lazbuild.exe source\tzdb\dist\tzdb_fpc.lpk -q -q --build-all
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters_dsgn.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\ZXing.Delphi\Lazarus\Package\zxing.lpk -q -q --build-all
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\package\idetester.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\ide\idetester_dsgn.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\HtmlViewer\package\FrameViewer09.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\delphi-markdown\packages\markdownengine.lpk  -q -q --build-all
tools\lazarus\lazbuild.exe source\delphi-markdown\tests\markdowntests.lpk -q -q --build-all
tools\lazarus\lazbuild.exe source\PdfiumLib\Package\Pdfium.lpk -q -q --build-all
tools\lazarus\lazbuild.exe source\DelphiAST\Package\pascalast.lpk -q -q --build-all


chdir /d %FSDIR% &rem restore current directory
