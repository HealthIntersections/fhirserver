
git pull

pause

call build\windows-all.bat c:\temp

exec\64\fhirserver.exe -tests -test-settings exec\64\fhir-tests.ini

call build\windows-release %1 c:\temp

