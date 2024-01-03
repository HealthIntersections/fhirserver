#!/usr/bin/env bash

echo "Updating dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
pushd $BUILDDIR

# Download and build OpenSSL 1.1.1w
cd /tmp
wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz 
tar -xf openssl-1.1.1w.tar.gz 
cd openssl-1.1.1w 
./config 
make 
make test 
make install

cp /usr/local/lib/*.so* /usr/lib/

popd
