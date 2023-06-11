#!/usr/bin/env bash

set -e

BUILD=${1:-"/tmp/fsbuild"}

mkdir -p exec/64/
cp exec/pack/*.cfg exec/pack/*.dat exec/64/
# cp exec/pack/linux/*.so exec/64/

echo compile libraries

echo "## compile packages/fhir_fsl.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_fsl.lpk -q -q

echo "## compile packages/fhir_indy.lpk"
$BUILD/tools/lazarus/lazbuild packages/fhir_indy.lpk -q -q

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
$BUILD/tools/lazarus/lazbuild packages/fhir_fui.lpk -q -q

echo "## compile codescanner"
$BUILD/tools/lazarus/lazbuild utilities/codescan/codescan.lpi --build-mode=linux -q -q

echo "## compile console"
$BUILD/tools/lazarus/lazbuild server/fhirconsole.lpi --build-mode=linux -q -q

echo "## compile toolkit"
$BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpr --build-mode=linux -q -q

echo "## compile server"
$BUILD/tools/lazarus/lazbuild server/fhirserver.lpr --build-mode=linux -q -q

