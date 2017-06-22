Unit OIDSupport;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}


Interface

uses
  RegularExpressions;

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
  regex : TRegEx;
Begin
  if (pos('.', oid) = 0) or (length(oid) > 64) then
    result := false
  else
  begin
    regex := TRegEx.Create(OID_REGEX, [roCompiled]);
    result := regex.IsMatch(oid);
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