C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\pascal C:\work\fhirserver\Libraries\refplat pas
copy /Y C:\work\org.hl7.fhir.dstu\build\implementations\pascal\*.rc Libraries\refplat\*.*
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\translations C:\work\fhirserver\Libraries\refplat xml
"C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\brcc32.exe" Libraries\refplat\FHIRTranslations.rc

copy /Y C:\work\org.hl7.fhir.dstu\build\tools\schematron\*.xsl web\*.*
copy /Y"C:\work\org.hl7.fhir.dstu\build\implementations\xmltools\*.xsl web\*.*
