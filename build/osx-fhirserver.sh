#!/usr/bin/env bash

set -e

BUILD=${1:-"/tmp/fsbuild"}

echo "## set up exec"

mkdir -p exec/64/

cp exec/pack/*.cfg exec/pack/*.dat exec/64/
cp exec/pack/linux/*.so exec/64/

echo "## compile packages/fhir_indy.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_indy.lpk -q -q

echo "## compile packages/fhir_fsl.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_fsl.lpk -q -q

echo "## compile packages/fcomp.lpk"
$BUILD/tools/lazarus/lazbuild packages/fcomp.lpk -q -q

echo "## compile packages/fhir.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir.lpk -q -q

echo "## compile packages/fhir2.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir2.lpk -q -q

echo "## compile packages/fhir3.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir3.lpk -q -q

echo "## compile packages/fhir4.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir4.lpk -q -q

echo "## compile packages/fhir4b.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir4b.lpk -q -q

echo "## compile packages/fhir5.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir5.lpk -q -q

echo "## compile packages/fhir_xver.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_xver.lpk -q -q

echo "## compile packages/fhir_fui.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_fui.lpk --widgetset=cocoa -q -q

echo "## compile console"
if [[ `uname -m` == 'arm64' ]]; then
  $BUILD/tools/lazarus/lazbuild server/fhirconsole.lpi --widgetset=cocoa --build-mode=osx-m1 -q -q
else
  $BUILD/tools/lazarus/lazbuild server/fhirconsole.lpi --widgetset=cocoa --build-mode=osx -q -q
fi 

echo "## compile toolkit"
if [[ `uname -m` == 'arm64' ]]; then
  $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpr --widgetset=cocoa --build-mode=osx-m1 -q -q
else
  $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpi --widgetset=cocoa --build-mode=osx -q -q
fi 

echo "## compile server"
if [[ `uname -m` == 'arm64' ]]; then
  $BUILD/tools/lazarus/lazbuild server/fhirserver.lpr --widgetset=cocoa --build-mode=osx-m1 -q -q
else
  $BUILD/tools/lazarus/lazbuild server/fhirserver.lpi --widgetset=cocoa --build-mode=osx -q -q
fi 

