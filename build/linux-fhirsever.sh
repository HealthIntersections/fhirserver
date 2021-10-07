#!/usr/bin/env bash

BUILD=${1:-"/tmp/fsbuild"}

$BUILD/tools/lazarus/lazbuild packages/fhir.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir2.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir3.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir4.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir5.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir_xver.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir_fsl.lpk
$BUILD/tools/lazarus/lazbuild packages/fhir_fui.lpk
$BUILD/tools/lazarus/lazbuild server/fhirconsole.lpi
$BUILD/tools/lazarus/lazbuild server/fhirserver.lpr
$BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpr

# The path with C:/ in it needs to be fixed, but it reflects current behavior
cp ./exec/pack/*.cfg ./exec/pack/*.dat  exec/pack/linux/* /work/fhirserver/server/C:/work/fhirserver/Exec/64/
