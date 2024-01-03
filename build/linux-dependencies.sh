#!/usr/bin/env bash

echo "Updating dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
OPENSSL_DIR="$BUILDDIR/openssl"
mkdir -p $OPENSSL_DIR
sudo mkdir /7Zip
sudo chmod 777 /7Zip
pushd $BUILDDIR

# Download and build OpenSSL 1.1.1w
cd /tmp
wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz 
tar -xf openssl-1.1.1w.tar.gz 
cd openssl-1.1.1w 
./config --prefix="$OPENSSL_DIR" --openssldir="$OPENSSL_DIR" 
make 
make test 
make install

sudo export PATH=$OPENSSL_DIR/bin:$PATH
sudo export LD_LIBRARY_PATH=$OPENSSL_DIR/lib:$LD_LIBRARY_PATH
sudo export PKG_CONFIG_PATH=$OPENSSL_DIR/lib/pkgconfig:$PKG_CONFIG_PATH
