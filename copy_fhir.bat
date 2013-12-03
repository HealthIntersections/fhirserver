copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.pas Libraries\refplat\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\*.rc Libraries\refplat\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\support\*.pas libraries\support\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\pascal\support\*.inc libraries\support\*.*
copy /Y C:\work\org.hl7.fhir\build\implementations\translations.xml Libraries\refplat\FHIRTranslations.xml
"C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\brcc32.exe" Libraries\refplat\FHIRTranslations.rc