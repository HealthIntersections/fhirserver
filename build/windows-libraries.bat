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
git clone https://github.com/mriscoc/extrasyn 
git clone https://github.com/grahamegrieve/HtmlViewer
git clone https://github.com/grahamegrieve/lazarus-ide-tester 

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

cd ..

Rem -- now build 

tools\lazarus\lazbuild.exe source\tzdb\dist\tzdb_fpc.lpk -q
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters.lpk  -q
tools\lazarus\lazbuild.exe source\extrasyn\extrahighlighters_dsgn.lpk  -q
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\package\idetester.lpk  -q
tools\lazarus\lazbuild.exe source\lazarus-ide-tester\ide\idetester_dsgn.lpk  -q
tools\lazarus\lazbuild.exe source\HtmlViewer\package\FrameViewer09.lpk  -q
tools\lazarus\lazbuild.exe source\delphi-markdown\packages\markdownengine.lpk  -q
tools\lazarus\lazbuild.exe source\delphi-markdown\tests\markdowntests.lpk -q

chdir /d %FSDIR% &rem restore current directory
