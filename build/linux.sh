#!/usr/bin/env bash

# the build directory: pass as a parameter or defaults to /tmp/fsbuild - will be created if necessary

BASEDIR=$(dirname "$0")
BUILDDIR=${1:-"/tmp/fsbuild"}
OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

export OPENSSL_DIR

mkdir -p $OPENSSL_DIR
sudo mkdir /7Zip
sudo chmod 777 /7Zip

# $BASEDIR/linux-dependencies.sh $BUILDDIR;
$BASEDIR/linux-toolchain.sh $BUILDDIR;
$BASEDIR/linux-libraries.sh $BUILDDIR;
$BASEDIR/linux-fhirserver.sh $BUILDDIR;
