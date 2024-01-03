#!/usr/bin/env bash

echo "Updating dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
$OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

mkdir -p $BUILDDIR
mkdir -p $OPENSSL_DIR
pushd $BUILDDIR

# Download and build OpenSSL 1.1.1w
cd /tmp
wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz
tar -xf openssl-1.1.1w.tar.gz
cd openssl-1.1.1w
./config --prefix=$OPENSSL_DIR --openssldir=$OPENSSL_DIR
make 
make test 
make install

popd