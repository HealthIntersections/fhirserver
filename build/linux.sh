#!/usr/bin/env bash

# the build directory: pass as a parameter or defaults to /tmp/fsbuild - will be created if necessary

BASEDIR=$(dirname "$0")
BUILDDIR=${1:-"/tmp/fsbuild"}

$BASEDIR/linux-dependencies.sh $BUILDDIR;
$BASEDIR/linux-toolchain.sh $BUILDDIR;
$BASEDIR/linux-libraries.sh $BUILDDIR;
$BASEDIR/linux-fhirserver.sh $BUILDDIR;
