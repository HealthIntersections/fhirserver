#!/usr/bin/env bash

set -e

BUILD=${2:-"/tmp/fsbuild"}

#--------------------------------------------------------
# echo "## scrub exec directory"
# rm -r -f exec/64/*
# utilities/codescan/codescan -check !exec/64/FHIRToolkit -message "deleting executables failed"
# ./clean.sh

#--------------------------------------------------------
# echo "## compile toolkit"
# if [[ `uname -m` == 'arm64' ]]; then
#   $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpr --widgetset=cocoa --build-mode=osx-m1-release -q -q
# else
#   $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpi --widgetset=cocoa --build-mode=osx-release -q -q
# fi 

#--------------------------------------------------------
echo "## assemble app"
# copy the app that was built
cp -r exec/64/FHIRToolkit.app install/build/FHIRToolkit.app
# copy the compiled exec
cp exec/64/FHIRToolkit install/build/FHIRToolkit.app/Contents/MacOS/FHIRToolkit
# copy the files from /exec/pack
cp exec/pack/*.dat install/build/FHIRToolkit.app/Contents/MacOS

