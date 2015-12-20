echo COPY DSTU2
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal C:\work\fhirserver\Libraries\refplat2 pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations C:\work\fhirserver\Libraries\refplat2 xml

echo COPY DSTU21
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\Libraries\refplat21 pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations C:\work\fhirserver\Libraries\refplat21 xml

copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.rc Libraries\refplat21\*.*
"C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\brcc32.exe" Libraries\refplat21\FHIRTranslations.rc

copy /Y C:\work\org.hl7.fhir\build\tools\schematron\*.xs* web\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\xmltools\*.xs* web\*.*


