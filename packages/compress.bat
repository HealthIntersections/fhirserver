cd fhir.tx.support\1.0.2 
"C:\Program Files\7-Zip\7z.exe" a -ttar package.tar package
"C:\Program Files\7-Zip\7z.exe" a -tgzip package.tgz package.tar
del package.tar
cd ..
 
cd fhir.tx.support\3.0.1 
"C:\Program Files\7-Zip\7z.exe" a -ttar package.tar package
"C:\Program Files\7-Zip\7z.exe" a -tgzip package.tgz package.tar
del package.tar
cd ..

cd fhir.tx.support\3.5.0 
"C:\Program Files\7-Zip\7z.exe" a -ttar package.tar package
"C:\Program Files\7-Zip\7z.exe" a -tgzip package.tgz package.tar
del package.tar
cd ..

pause