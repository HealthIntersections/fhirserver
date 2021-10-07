#!/usr/bin/env bash

echo "Preparing dependencies for linux"

# ---- initial set up ----------
# user can pass in a parameter if they want the temporary scratch area to be somewhere else than /tmp/fsbuild 
# fsbuild must already exist 

# todo... fix this for .sh

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

cd ..

# ---- register the source with lazarus ----------

tools/lazarus/lazbuild source/PascalTZ/package/pascaltz.lpk
tools/lazarus/lazbuild source/extrasyn/extrahighlighters.lpk
tools/lazarus/lazbuild source/extrasyn/extrahighlighters_dsgn.lpk
tools/lazarus/lazbuild source/lazarus-ide-tester/package/idetester.lpk
tools/lazarus/lazbuild source/lazarus-ide-tester/ide/idetester_dsgn.lpk
tools/lazarus/lazbuild source/HtmlViewer/package/FrameViewer09.lpk
tools/lazarus/lazbuild source/QRCodeGenLib4Pascal/QRCodeGenLib/src/Packages/FPC/QRCodeGenLib4PascalPackage.lpk
tools/lazarus/lazbuild source/delphi-markdown/packages/markdownengine.lpk
tools/lazarus/lazbuild source/delphi-markdown/tests/markdowntests.lpk

# ----  back to the server ----------

popd
