#!/usr/bin/env bash

set -e

BUILD=${1:-"/tmp/fsbuild"}
OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

echo "## compile server"
$BUILD/tools/lazarus/lazbuild server/fhirserver.lpr --build-mode=linux -q -q --build-all

find ./exec/64 -type f ! -name "*.*" -exec strip {} \;