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

# ---- get the source we depend on ----------

mkdir source
cd source

git clone https://github.com/dezlov/PascalTZ
git clone https://github.com/grahamegrieve/delphi-markdown
git clone https://github.com/mriscoc/extrasyn
git clone https://github.com/grahamegrieve/HtmlViewer
git clone https://github.com/grahamegrieve/lazarus-ide-tester
git clone https://github.com/Xor-el/QRCodeGenLib4Pascal

popd
