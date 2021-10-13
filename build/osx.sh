#!/usr/bin/env bash

# the build directory: pass as a parameter or defaults to /tmp/fsbuild - will be created if necessary

BASEDIR=$(dirname "$0")
BUILDDIR=${1:-"/tmp/fsbuild"}

$BASEDIR/osx-toolchain.sh $BUILDDIR;
$BASEDIR/unix-libraries.sh $BUILDDIR;
$BASEDIR/osx-fhirserver.sh $BUILDDIR;
