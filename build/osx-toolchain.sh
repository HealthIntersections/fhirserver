#!/usr/bin/env bash

echo "Preparing dependencies for osx"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
pushd $BUILDDIR

# ---- install the compiler ----------

mkdir tools

echo "get fpcup"

if [[ `uname -m` == 'arm64' ]]; then
  wget -q https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0h/fpclazup-aarch64-darwin -O tools/fpclazup
else 
  wget -q https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/download/v2.2.0h/fpclazup-x86_64-darwin -O tools/fpclazup
fi

echo sign it

sudo codesign --force --deep --sign - tools/fpclazup

echo make it executable

chmod +x tools/fpclazup

Echo build Lazarus 

./tools/fpclazup --noconfirm --fpcVersion=trunk.gitlab --lazVersion=trunk.gitlab --installdir=tools --include=anchordocking,lazprojectgroups,virtualtreeview,fpdebug

Echo install cross compiler 

./tools/fpclazup --ostarget="Darwin" --cputarget="x86_64" --only="FPCCleanOnly,FPCBuildOnly" --installdir=tools --noconfirm

Echo done

popd
