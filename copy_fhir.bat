C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\pascal C:\work\fhirserver\Libraries\refplat-dstu pas
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal C:\work\fhirserver\Libraries\refplat-dev pas
copy /Y C:\work\org.hl7.fhir.dstu\build\implementations\pascal\*.rc Libraries\refplat-dstu\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.rc Libraries\refplat-dev\*.*
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support pas
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support C:\work\fhirserver\libraries\support inc
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir.dstu\build\implementations\translations C:\work\fhirserver\Libraries\refplat-dstu xml
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\translations C:\work\fhirserver\Libraries\refplat-dev xml
"C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\brcc32.exe" Libraries\refplat-dstu\FHIRTranslations.rc
"C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\brcc32.exe" Libraries\refplat-dev\FHIRTranslations.rc
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support c:\work\org.hl7.fhir.dstu\build\implementations\pascal\support pas
C:\work\org.hl7.fhir.dstu\build\tools\dirmerge C:\work\org.hl7.fhir\build\implementations\pascal\support c:\work\org.hl7.fhir.dstu\build\implementations\pascal\support inc

copy /Y C:\work\org.hl7.fhir\build\tools\schematron\*.xsl web\*.*
copy /Y"C:\work\org.hl7.fhir\build\implementations\xmltools\*.xsl web\*.*
