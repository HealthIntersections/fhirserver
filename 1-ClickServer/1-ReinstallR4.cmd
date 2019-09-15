if not exist fhirR4.db copy fhirRx_empty.db fhirR4.db
cd server
FHIRServer.exe -cmd remount -password sa -ini ".\fhirserver.ini" -packages hl7.fhir.core#4.0.0 -endpoint r4 -mode open
cd ..
