#!/usr/bin/env bash

odbcinst -j

# the build directory: pass as a parameter or defaults to /tmp/fsbuild - will be created if necessary

BASEDIR=$(dirname "$0")
BUILDDIR=${1:-"/tmp/fsbuild"}

$BASEDIR/linux-toolchain.sh $BUILDDIR;
$BASEDIR/unix-libraries.sh $BUILDDIR;
$BASEDIR/linux-fhirserver.sh $BUILDDIR;
