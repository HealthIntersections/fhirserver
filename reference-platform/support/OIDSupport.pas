Unit OIDSupport;

Interface

uses
  RegExpr;

Const
  OID_LOINC = '2.16.840.1.113883.6.1';
  OID_SNOMED = '2.16.840.1.113883.6.96';
  OID_REGEX = '[0-2](\.(0|[1-9][0-9]*))*';

Function isOid(oid : String) : Boolean;

function UriForKnownOid(oid : String) : String;

Implementation

Uses
  IdHttp,
  SysUtils,
  StringSupport;

Function isOid(oid : String) : Boolean;
var
  regex : TRegExpr;
Begin
  if (pos('.', oid) = 0) or (length(oid) > 64) then
    result := false
  else
  begin
    regex := TRegExpr.Create;
    try
      regex.Expression := OID_REGEX;
      regex.Compile;
      result := regex.Exec(oid);
    finally
      regex.Free;
    end;
  end;
End;

function UriForKnownOid(oid : String) : String;
begin
  if oid = '2.16.840.1.113883.6.96' then
    exit('http://snomed.info/sct');
  if oid = '2.16.840.1.113883.6.1' then
    exit('http://loinc.org');
  if oid = '2.16.840.1.113883.6.8' then
    exit('http://unitsofmeasure.org');
  if oid = '2.16.840.1.113883.6.3' then
    exit('http://hl7.org/fhir/sid/icd-10');
  if oid = '2.16.840.1.113883.6.42' then
    exit('http://hl7.org/fhir/sid/icd-9');
  if oid = '2.16.840.1.113883.6.104' then
    exit('http://hl7.org/fhir/sid/icd-9');
  if oid = '2.16.840.1.113883.6.103' then
    exit('http://hl7.org/fhir/sid/icd-9'); //todo: confirm this
  if oid = '2.16.840.1.113883.6.73' then
    exit('http://hl7.org/fhir/sid/atc');
  if oid = '2.16.840.1.113883.3.26.1.1' then
    exit('http://ncimeta.nci.nih.gov');
  if oid = '2.16.840.1.113883.3.26.1.1.1' then
    exit('http://ncimeta.nci.nih.gov');
  if oid = '2.16.840.1.113883.6.88' then
    exit('http://www.nlm.nih.gov/research/umls/rxnorm'); // todo: confirm this

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
    exit('http://hl7.org/fhir/sid/cvx');
  if oid = '2.16.840.1.113883.12.292' then
    exit('http://hl7.org/fhir/sid/cvx');

  if oid = '2.16.840.1.113883.6.12' then
    exit('http://www.ama-assn.org/go/cpt');

  if oid = '2.16.840.1.113883.12.' then
    exit('http://hl7.org/fhir/sid/v2-'+oid.substring(21));
  result := '';
end;

End.