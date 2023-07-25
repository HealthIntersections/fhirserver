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

curl -L https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0q/fpclazup-x86_64-win64.exe --output tools\fpclazup.exe
curl -L https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/crosslibs_v1.3/CrossLibsLinuxx64.zip --output tools\CrossLibsLinuxx64.zip
curl -L https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/download/wincrossbins_v1.0/WinCrossBinsLinuxx64.zip --output tools\WinCrossBinsLinuxx64.zip

powershell -command "Expand-Archive -Force tools\CrossLibsLinuxx64.zip tools"
powershell -command "Expand-Archive -Force tools\WinCrossBinsLinuxx64.zip tools"

pause

rem -- run the installer- will finish with a full install of Lazarus 

tools\fpclazup --fpcVersion="stable.gitlab" --lazVersion="stable.gitlab" --installdir=tools --noconfirm --include=anchordocking,lazprojectgroups,virtualtreeview,fpdebug
rem tools\fpclazup --installdir=tools --noconfirm --include=anchordocking,lazprojectgroups,virtualtreeview,fpdebug
tools\fpclazup --ostarget="linux" --cputarget="x86_64" --only="FPCCleanOnly,FPCBuildOnly" --installdir=tools --noconfirm 

Rem ----  back to the fhirserver directory ----------

chdir /d %FSDIR% 
