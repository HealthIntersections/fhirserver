## Change Notes for 3.8.3:

* fix non-conformant terminology capabilities
* Support supplements for language and country codes
* Preindex parent relationships in codesystems for performance reasons + fix memory leaks
* Only load/index codesystems once from tx-resources for improved performance + more leak fixing

## Conformance Notes:

* tx.fhir.org passed 337 HL7 terminology service tests (mode 'tx.fhir.org', tests v1.7.7-SNAPSHOT, runner v6.6.0)
