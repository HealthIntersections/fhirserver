## Change Notes:

This release of the software corrects an error where 'xml', 'json', and 'ttl' 
were considered to be valid mime type codes. This will require either upgrading
to the latest validator for FHIR R2-R5, or using the policy advisor framework to
turn off validation of the relevant elements, which are found in CapabilityStatements
and TestScripts.

For discussion, see [FHIR-48679 (Issue with invalid MIME types)](http://jira.hl7.org/browse/FHIR-48679) and 
also [chat.fhir.org](https://chat.fhir.org/#narrow/channel/179239-tooling/topic/mandatory.20validator.20upgrade).

## Conformance Notes:

* tx.fhir.org passed 305 HL7 terminology service tests (mode 'tx.fhir.org', tests v1.7.6, runner v6.5.10)
