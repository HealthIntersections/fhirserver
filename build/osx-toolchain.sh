#!/usr/bin/env bash

echo "Preparing dependencies for osx"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
pushd $BUILDDIR

# ---- install the compiler ----------

mkdir tools

echo "get fpcup"

wget -q https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0c/fpclazup-aarch64-darwin -O tools/fpclazup

echo sign it

sudo codesign --force --deep --sign - tools/fpclazup

echo make it executable

chmod +x tools/fpclazup

Echo build Lazarus 

./tools/fpclazup --noconfirm --fpcVersion=trunk.gitlab --lazVersion=trunk.gitlab --installdir=tools

Echo done

popd
