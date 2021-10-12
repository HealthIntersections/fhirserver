#!/usr/bin/env bash

echo "Updating dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
mkdir -p $BUILDDIR
pushd $BUILDDIR

mkdir source
cd source

if [ ! -d "PascalTZ" ] ; then
    git clone https://github.com/grahamegrieve/tzdb
else
    cd tzdb && git pull && cd ..
fi

if [ ! -d "delphi-markdown" ] ; then
    git clone https://github.com/grahamegrieve/delphi-markdown
else
    cd delphi-markdown && git pull && cd ..
fi

if [ ! -d "extrasyn" ] ; then
    git clone https://github.com/mriscoc/extrasyn
else
    cd extrasyn && git pull && cd ..
fi

if [ ! -d "HtmlViewer" ] ; then
    git clone https://github.com/grahamegrieve/HtmlViewer
else
    cd HtmlViewer  && git pull && cd ..
fi

if [ ! -d "lazarus-ide-tester" ] ; then
    git clone https://github.com/grahamegrieve/lazarus-ide-tester
else
    cd lazarus-ide-tester && git pull && cd ..
fi

if [ ! -d "QRCodeGenLib4Pascal" ] ; then
    git clone https://github.com/Xor-el/QRCodeGenLib4Pascal
else
    cd QRCodeGenLib4Pascal  && git pull && cd ..
fi

if [ ! -d "fhir-test-cases" ] ; then
    git clone https://github.com/FHIR/fhir-test-cases
else
    cd fhir-test-cases  && git pull && cd ..
fi

cd ..

# ---- register the source with lazarus ----------

tools/lazarus/lazbuild source/tzdb/dist/tzdb_fpc.lpk -q
tools/lazarus/lazbuild source/extrasyn/extrahighlighters.lpk -q
tools/lazarus/lazbuild source/extrasyn/extrahighlighters_dsgn.lpk -q
tools/lazarus/lazbuild source/lazarus-ide-tester/package/idetester.lpk -q
tools/lazarus/lazbuild source/lazarus-ide-tester/ide/idetester_dsgn.lpk -q
tools/lazarus/lazbuild source/HtmlViewer/package/FrameViewer09.lpk -q
tools/lazarus/lazbuild source/QRCodeGenLib4Pascal/QRCodeGenLib/src/Packages/FPC/QRCodeGenLib4PascalPackage.lpk -q
tools/lazarus/lazbuild source/delphi-markdown/packages/markdownengine.lpk -q
tools/lazarus/lazbuild source/delphi-markdown/tests/markdowntests.lpk -q


# ----  back to the server ----------

popd
