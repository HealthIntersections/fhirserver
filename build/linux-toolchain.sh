#!/usr/bin/env bash

echo "Preparing dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
$OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

mkdir -p $BUILDDIR
pushd $BUILDDIR

# ---- install the compiler ----------

echo "Get fpclazup"

mkdir tools

wget -q https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0q/fpclazup-x86_64-linux -O tools/fpclazup

chmod +x tools/fpclazup

echo "Build Lazarus"

tools/fpclazup --noconfirm --fpcVersion=stable.gitlab --lazVersion=stable.gitlab --installdir=tools

popd

# --------- setting up mysql -------------------------------------
# create user 'test'@'%' identified by 'test';
# create database test;
# GRANT ALL PRIVILEGES ON test.* TO 'test'@'%';
# flush privileges;

