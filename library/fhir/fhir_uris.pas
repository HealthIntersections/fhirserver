unit fhir_uris;

{$i fhir.inc}

interface

uses
  SysUtils;

const
  URI_SNOMED = 'http://snomed.info/sct';
  URI_LOINC = 'http://loinc.org';
  URI_UCUM = 'http://unitsofmeasure.org';
  URI_RXNORM = 'http://www.nlm.nih.gov/research/umls/rxnorm';
  URI_CVX = 'http://hl7.org/fhir/sid/cvx';
  URI_ATC = 'http://www.whocc.no/atc';
  URI_NDC = 'http://hl7.org/fhir/sid/ndc';
  URI_GTIN = 'https://www.gs1.org/gtin';
  URI_BCP13 = 'urn:ietf:bcp:13';
  URI_BCP47 = 'urn:ietf:bcp:47';
  URI_11073 = 'urn:iso:std:iso:11073:10101';
  URI_3166 = 'urn:iso:std:iso:3166';
  URI_URIs = 'urn:ietf:rfc:3986';
  URI_DICOM = 'http://dicom.nema.org/resources/ontology/DCM';
  URI_PCLOCD = 'https://fhir.infoway-inforoute.ca/CodeSystem/pCLOCD';
  URI_CPT = 'http://www.ama-assn.org/go/cpt';
  URI_NDFRT = 'http://hl7.org/fhir/ndfrt';
  URI_MEDRT = 'http://hl7.org/fhir/medrt';
  URI_UNII = 'http://fdasis.nlm.nih.gov';
  URI_ICD10 = 'http://hl7.org/fhir/sid/icd-10';
  URI_ICD9 = 'http://hl7.org/fhir/sid/icd-9';

  URI_AUSTRALIAN_PASSPORT_NUMBER = 'urn:oid:2.16.840.1.113883.4.330.36';

  // audit trail
  URI_FHIR_AUDIT_OBJECT_ROLE_R4 = 'http://terminology.hl7.org/CodeSystem/object-role';
  URI_FHIR_AUDIT_OBJECT_ROLE_R3 = 'http://hl7.org/fhir/object-role';
  URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R3_DICOM = 'http://hl7.org/fhir/dicom-audit-lifecycle';
  URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R3_ISO = 'http://hl7.org/fhir/iso-21089-lifecycle';
  URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R4_DICOM = 'http://terminology.hl7.org/CodeSystem/dicom-audit-lifecycle';
  URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R4_ISO = 'http://terminology.hl7.org/CodeSystem/iso-21089-lifecycle';
  URI_FHIR_SECURITY_SOURCE_TYPE_R3 = 'http://hl7.org/fhir/security-source-type';
  URI_FHIR_SECURITY_SOURCE_TYPE_R4 = 'http://terminology.hl7.org/CodeSystem/security-source-type';
  URI_FHIR_AUDIT_EVENT_TYPE_R4 = 'http://terminology.hl7.org/CodeSystem/audit-event-type';
  URI_FHIR_AUDIT_EVENT_TYPE_R3 = 'http://hl7.org/fhir/audit-event-type';
  URI_FHIR_AUDIT_ENTITY_TYPE_R4 = 'http://terminology.hl7.org/CodeSystem/audit-entity-type';
  URI_FHIR_AUDIT_ENTITY_TYPE_R3 = 'http://hl7.org/fhir/audit-entity-type';
  URI_FHIR_AUDIT_EVENT_OUTCOME = 'http://hl7.org/fhir/audit-event-outcome';
  URI_FHIR_RESTFUL_OP = 'http://hl7.org/fhir/restful-operation';













type
  TCommonURLs = record
    SecuritySourceType : string;
  end;


function UriForKnownOid(oid : String) : String;

implementation

function UriForKnownOid(oid : String) : String;
begin
  if oid = '2.16.840.1.113883.6.96' then
    exit(URI_SNOMED);
  if oid = '2.16.840.1.113883.6.1' then
    exit(URI_LOINC);
  if oid = '2.16.840.1.113883.6.8' then
    exit(URI_UCUM);
  if oid = '2.16.840.1.113883.6.3' then
    exit(URI_ICD10);
  if oid = '2.16.840.1.113883.6.42' then
    exit(URI_ICD9);
  if oid = '2.16.840.1.113883.6.104' then
    exit(URI_ICD9);
  if oid = '2.16.840.1.113883.6.103' then
    exit(URI_ICD9); //todo: confirm this
  if oid = '2.16.840.1.113883.6.73' then
    exit(URI_ATC);
  if oid = '2.16.840.1.113883.6.88' then
    exit(URI_RXNORM); // todo: confirm this

  if oid = '2.16.840.1.113883.5.1008' then
    exit('http://hl7.org/fhir/v3/NullFlavor');
  if oid = '2.16.840.1.113883.5.111' then
    exit('http://hl7.org/fhir/v3/RoleCode');
  if oid = '2.16.840.1.113883.5.4' then
    exit('http://hl7.org/fhir/v3/ActCode');
  if oid = '2.16.840.1.113883.5.8' then
    exit('http://hl7.org/fhir/v3/ActReason');
  if oid = '2.16.840.1.113883.5.83' then
    exit('http://hl7.org/fhir/v3/ObservationInterpretation');
  if oid = '2.16.840.1.113883.6.238' then
    exit('http://hl7.org/fhir/v3/Race');

  if oid = '2.16.840.1.113883.6.59' then
    exit(URI_CVX);
  if oid = '2.16.840.1.113883.12.292' then
    exit(URI_CVX);

  if oid = '2.16.840.1.113883.6.12' then
    exit(URI_CPT);

  if oid = '2.16.840.1.113883.12.' then
    exit('http://hl7.org/fhir/sid/v2-'+oid.substring(21));

  if (oid = '2.16.840.1.113883.6.96') then
    exit(URI_SNOMED);
  if (oid = '2.16.840.1.113883.6.1') then
    exit(URI_LOINC);
  if (oid = '2.16.840.1.113883.6.8') then
    exit(URI_UCUM);
  if (oid = '2.16.840.1.113883.6.3') then
    exit(URI_ICD10);
  if (oid = '2.16.840.1.113883.6.42') then
    exit(URI_ICD9);
  if (oid = '2.16.840.1.113883.6.104') then
    exit(URI_ICD9);
  if (oid = '2.16.840.1.113883.6.103') then
    exit(URI_ICD9);
  if (oid = '2.16.840.1.113883.6.73') then
    exit(URI_ATC);
  if (oid = '2.16.840.1.113883.6.88') then
    exit(URI_RXNORM);

  if (oid = '2.16.840.1.113883.5.1008') then
    exit('http://terminology.hl7.org/v3/NullFlavor');
  if (oid = '2.16.840.1.113883.5.111') then
    exit('http://terminology.hl7.org/v3/RoleCode');
  if (oid = '2.16.840.1.113883.5.4') then
    exit('http://terminology.hl7.org/v3/ActCode');
  if (oid = '2.16.840.1.113883.5.8') then
    exit('http://terminology.hl7.org/v3/ActReason');
  if (oid = '2.16.840.1.113883.5.83') then
    exit('http://terminology.hl7.org/v3/ObservationInterpretation');
  if (oid = '2.16.840.1.113883.6.238') then
    exit('http://terminology.hl7.org/v3/Race');

  if (oid = '2.16.840.1.113883.6.59') then
    exit(URI_CVX);
  if (oid = '2.16.840.1.113883.12.292') then
    exit(URI_CVX);

  if (oid = '2.16.840.1.113883.6.12') then
    exit(URI_CPT);

  if (oid.startsWith('2.16.840.1.113883.12.')) then
    exit('http://hl7.org/fhir/sid/v2-'+oid.substring(21));

  result := '';
end;


end.
