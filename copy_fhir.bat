C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal C:\work\fhirserver\Libraries\refplat pas
copy /Y C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\*.rc Libraries\refplat\*.*
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
C:\work\org.hl7.fhir.dstu2\build\tools\bin\dirmerge C:\work\org.hl7.fhir.dstu2\build\implementations C:\work\fhirserver\Libraries\refplat xml
"C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\brcc32.exe" Libraries\refplat\FHIRTranslations.rc

copy /Y C:\work\org.hl7.fhir.dstu2\build\tools\schematron\*.xs* web\*.*
copy /Y C:\work\org.hl7.fhir.dstu2\build\implementations\xmltools\*.xs* web\*.*


