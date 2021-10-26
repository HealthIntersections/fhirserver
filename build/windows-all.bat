Rem ---- initial set up ----------

REM user can pass in a parameter if they want the temporary scratch area to be somewhere else than r:\fsbuild (which is a RAM drive)
REM the folder must exist 

set FSDIR=%CD%
setlocal
set "tmp=r:\fsbuild"

IF %1.==. GOTO No1
set "tmp=%1"

:No1

call windows-toolchain.bat %tmp%
rem pause

call windows-libraries.bat %tmp%
rem pause

call windows-fhirserver.bat %tmp%
rem pause
