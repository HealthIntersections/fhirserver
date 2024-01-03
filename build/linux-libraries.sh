#!/usr/bin/env bash

echo "Updating dependencies for linux"

# ---- initial set up ----------

BUILDDIR=${1:-"/tmp/fsbuild"}
OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

mkdir -p $BUILDDIR
mkdir -p $OPENSSL_DIR

pushd $BUILDDIR
mkdir source

# Download and build OpenSSL 1.1.1w
cd /tmp
wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz
tar -xf openssl-1.1.1w.tar.gz
cd openssl-1.1.1w
./config --prefix=$OPENSSL_DIR --openssldir=$OPENSSL_DIR
make 
make test 
make install

cd source

echo "## git tzdb"

if [ ! -d "tzdb" ] ; then
    git clone https://github.com/grahamegrieve/tzdb
else
    cd tzdb && git pull && cd ..
fi

echo "## git markdown"

if [ ! -d "delphi-markdown" ] ; then
    git clone https://github.com/grahamegrieve/delphi-markdown
else
    cd delphi-markdown && git pull && cd ..
fi

echo "## git extrasyn"

if [ ! -d "extrasyn" ] ; then
    git clone https://github.com/grahamegrieve/extrasyn
else
    cd extrasyn && git pull && cd ..
fi

echo "## git html"

if [ ! -d "HtmlViewer" ] ; then
    git clone https://github.com/grahamegrieve/HtmlViewer
else
    cd HtmlViewer  && git pull && cd ..
fi

echo "## git laz-ide-tester"

if [ ! -d "lazarus-ide-tester" ] ; then
    git clone https://github.com/grahamegrieve/lazarus-ide-tester
else
    cd lazarus-ide-tester && git pull && cd ..
fi

echo "## git test cases"

if [ ! -d "fhir-test-cases" ] ; then
    git clone https://github.com/FHIR/fhir-test-cases
else
    cd fhir-test-cases  && git pull && cd ..
fi

if [ ! -d "ZXing.Delphi" ] ; then
    git clone https://github.com/grahamegrieve/ZXing.Delphi
else
    cd ZXing.Delphi && git pull && cd ..
fi

if [ ! -d "PdfiumLib" ] ; then
    git clone https://github.com/grahamegrieve/PdfiumLib
else
    cd PdfiumLib && git pull && cd ..
fi

if [ ! -d "DelphiAST" ] ; then
    git clone --recurse-submodules https://github.com/grahamegrieve/DelphiAST
else
    cd DelphiAST && git pull && cd ..
fi

cd ..

# ---- register the source with lazarus ----------

echo "## compile package source/tzdb/dist/tzdb_fpc.lpk"
tools/lazarus/lazbuild source/tzdb/dist/tzdb_fpc.lpk -q --build-all

echo "## compile package source/extrasyn/extrahighlighters.lpk"
tools/lazarus/lazbuild source/extrasyn/extrahighlighters.lpk -q --build-all

echo "## compile package source/extrasyn/extrahighlighters_dsgn.lpk"
tools/lazarus/lazbuild source/extrasyn/extrahighlighters_dsgn.lpk -q --add-package --build-all

echo "## compile package source/ZXing.Delphi/Lazarus/Package/zxing.lpk"
tools/lazarus/lazbuild source/ZXing.Delphi/Lazarus/Package/zxing.lpk -q --add-package --build-all --build-ide= 

echo "## compile package source/lazarus-ide-tester/package/idetester.lpk"
tools/lazarus/lazbuild source/lazarus-ide-tester/package/idetester.lpk -q  --add-package --build-all

echo "## compile package source/lazarus-ide-tester/ide/idetester_dsgn.lpk"
tools/lazarus/lazbuild source/lazarus-ide-tester/ide/idetester_dsgn.lpk -q --add-package --build-all

echo "## compile package source/HtmlViewer/package/FrameViewer09.lpk"
tools/lazarus/lazbuild source/HtmlViewer/package/FrameViewer09.lpk -q --add-package --build-all

echo "## compile package source/delphi-markdown/packages/markdownengine.lpk"
tools/lazarus/lazbuild source/delphi-markdown/packages/markdownengine.lpk -q --build-all

echo "## compile package source/PdfiumLib/Package/Pdfium.lpk"
tools/lazarus/lazbuild source/PdfiumLib/Package/Pdfium.lpk -q --add-package --build-all

echo "## compile package source/delphi-markdown/tests/markdowntests.lpk"
tools/lazarus/lazbuild source/delphi-markdown/tests/markdowntests.lpk -q --build-all

echo "## compile package source/DelphiAST/Package/pascalast.lpk"
tools/lazarus/lazbuild source/DelphiAST/Package/pascalast.lpk --quiet  --build-all

# ----  back to the server ----------

popd
