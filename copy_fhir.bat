C:\work\org.hl7.fhir\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\Libraries\refplat pas
copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.rc Libraries\refplat\*.*
C:\work\org.hl7.fhir\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations C:\work\fhirserver\Libraries\refplat xml
"C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\brcc32.exe" Libraries\refplat\FHIRTranslations.rc

C:\work\org.hl7.fhir\build\tools\bin\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support c:\work\org.hl7.fhir\build\implementations\pascal\support pas

copy /Y C:\work\org.hl7.fhir\build\tools\schematron\*.xs* web\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\xmltools\*.xs* web\*.*

pause

