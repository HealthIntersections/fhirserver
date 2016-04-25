
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\fhirserver\reference-platform\dstu2 C:\work\fhirserver\reference-platform\dstu3 pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\fhirserver\reference-platform\dstu2\tests C:\work\fhirserver\reference-platform\dstu3\tests pas

echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\reference-platform\gen21 pas
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal C:\work\fhirserver\Libraries\refplat2 txt
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations C:\work\fhirserver\Libraries\refplat2 xml
echo COPY DSTU21
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\Libraries\refplat21 pas
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\Libraries\refplat21 txt
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
echo C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations C:\work\fhirserver\Libraries\refplat21 xml
echo 
echo copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.rc Libraries\refplat21\*.*
echo "C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\brcc32.exe" Libraries\refplat21\FHIRTranslations.rc
echo 
echo copy /Y C:\work\org.hl7.fhir\build\tools\schematron\*.xs* web\*.*
echo copy /Y C:\work\org.hl7.fhir\build\implementations\xmltools\*.xs* web\*.*


