#!/usr/bin/env bash

set -e

BUILD=${2:-"/tmp/fsbuild"}

#--------------------------------------------------------
echo "## scrub exec directory"
rm -r -f exec/64/*
rm -r -f install/build/*.app
rm -r -f install/build/*.zip

utilities/codescan/codescan -check !exec/64/FHIRToolkit -message "deleting executables failed"
./clean.sh

#--------------------------------------------------------
echo "## compile toolkit"
if [[ `uname -m` == 'arm64' ]]; then
  $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpr --widgetset=cocoa --build-mode=osx-m1-release -q -q
else
  $BUILD/tools/lazarus/lazbuild toolkit2/fhirtoolkit.lpi --widgetset=cocoa --build-mode=osx-release -q -q
fi 

#--------------------------------------------------------
# echo "## assemble app"
# copy the app that was built
cp -r exec/64/FHIRToolkit.app install/build/FHIRToolkit.app
# copy the compiled exec
cp exec/64/FHIRToolkit install/build/FHIRToolkit.app/Contents/MacOS/FHIRToolkit
# copy the files from /exec/pack
cp exec/pack/*.dat install/build/FHIRToolkit.app/Contents/Resources
# copy the signed openSSL dlls
cp exec/pack/osx/*.dylib install/build/FHIRToolkit.app/Contents/MacOS
cp install/info.plist install/build/FHIRToolkit.app/Contents/info.plist
sed -i '' 's/#ver#/'$1'/g' install/build/FHIRToolkit.app/Contents/info.plist

#--------------------------------------------------------
# echo "## sign and notarise"
codesign --force --options runtime --timestamp --deep --sign "Developer ID Application: Health Intersections Pty Ltd (632X4VRLLD)" install/build/FHIRToolkit.app/Contents/MacOS/FHIRToolkit
codesign --force --options runtime --timestamp --deep --sign "Developer ID Application: Health Intersections Pty Ltd (632X4VRLLD)" install/build/FHIRToolkit.app
/usr/bin/ditto -c -k --keepParent install/build/FHIRToolkit.app install/build/FHIRToolkit.zip
xcrun notarytool submit install/build/FHIRToolkit.zip --keychain-profile "AC_PASSWORD" --wait
xcrun stapler staple install/build/FHIRToolkit.app
rm -r -f install/build/FHIRToolkit.zip
/usr/bin/ditto -c -k --keepParent install/build/FHIRToolkit.app install/build/FHIRToolkit.zip

