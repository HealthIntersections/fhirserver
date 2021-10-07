#!/usr/bin/env bash

BASEDIR=$(dirname "$0")
BUILDDIR=${1:-"/tmp/fsbuild"}

$BASEDIR/linux-dependencies.sh $BUILDDIR;
$BASEDIR/linux-fhirserver.sh $BUILDDIR;
