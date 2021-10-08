#!/usr/bin/env bash

echo "Preparing dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
pushd $BUILDDIR

# ---- install the compiler ----------

mkdir tools

wget -q https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0b/fpclazup-x86_64-linux -O tools/fpclazup

chmod +x tools/fpclazup

tools/fpclazup --noconfirm --fpcVersion=trunk.gitlab --lazVersion=trunk.gitlab --installdir=tools

popd
