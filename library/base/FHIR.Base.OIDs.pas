unit FHIR.Base.OIDs;

interface

Uses
  SysUtils;

function getUriForOid(r : String) : String;

implementation

function getUriForOid(r : String) : String;
begin
  result := '';
  if (r = '2.16.840.1.113883.6.96') then
    exit('http://snomed.info/sct');
  if (r = '2.16.840.1.113883.6.1') then
    exit('http://loinc.org');
  if (r = '2.16.840.1.113883.6.8') then
    exit('http://unitsofmeasure.org');
  if (r = '2.16.840.1.113883.6.3') then
    exit('http://hl7.org/fhir/sid/icd-10');
  if (r = '2.16.840.1.113883.6.42') then
    exit('http://hl7.org/fhir/sid/icd-9');
  if (r = '2.16.840.1.113883.6.104') then
    exit('http://hl7.org/fhir/sid/icd-9');
  if (r = '2.16.840.1.113883.6.103') then
    exit('http://hl7.org/fhir/sid/icd-9');
  if (r = '2.16.840.1.113883.6.73') then
    exit('http://hl7.org/fhir/sid/atc');
  if (r = '2.16.840.1.113883.3.26.1.1') then
    exit('http://ncimeta.nci.nih.gov');
  if (r = '2.16.840.1.113883.3.26.1.1.1') then
    exit('http://ncimeta.nci.nih.gov');
  if (r = '2.16.840.1.113883.6.88') then
    exit('http://www.nlm.nih.gov/research/umls/rxnorm');

  if (r = '2.16.840.1.113883.5.1008') then
    exit('http://terminology.hl7.org/v3/NullFlavor');
  if (r = '2.16.840.1.113883.5.111') then
    exit('http://terminology.hl7.org/v3/RoleCode');
  if (r = '2.16.840.1.113883.5.4') then
    exit('http://terminology.hl7.org/v3/ActCode');
  if (r = '2.16.840.1.113883.5.8') then
    exit('http://terminology.hl7.org/v3/ActReason');
  if (r = '2.16.840.1.113883.5.83') then
    exit('http://terminology.hl7.org/v3/ObservationInterpretation');
  if (r = '2.16.840.1.113883.6.238') then
    exit('http://terminology.hl7.org/v3/Race');

  if (r = '2.16.840.1.113883.6.59') then
    exit('http://hl7.org/fhir/sid/cvx');
  if (r = '2.16.840.1.113883.12.292') then
    exit('http://hl7.org/fhir/sid/cvx');

  if (r = '2.16.840.1.113883.6.12') then
    exit('http://www.ama-assn.org/go/cpt');

  if (r.startsWith('2.16.840.1.113883.12.')) then
    exit('http://hl7.org/fhir/sid/v2-'+r.substring(21));


    end;
end.
