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

rem ---- download the installer

md tools

curl -L https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0e/fpclazup-x86_64-win64.exe --output tools\fpclazup.exe

rem -- run the installer- will finish with a full install of Lazarus 

tools\fpclazup --fpcVersion=trunk.gitlab --lazVersion=trunk.gitlab --installdir=tools --noconfirm --include=anchordocking,lazprojectgroups,virtualtreeview,fpdebug --verbose

:: tools\fpclazup --ostarget="linux" --cputarget="x86_64" --only="FPCCleanOnly,FPCBuildOnly" --installdir=tools. --noconfirm --verbose


Rem ----  back to the fhirserver directory ----------

chdir /d %FSDIR% 
