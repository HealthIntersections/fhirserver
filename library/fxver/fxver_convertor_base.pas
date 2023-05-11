unit fxver_convertor_base;

{
Copyright (c) 2018+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}


interface

uses
  fsl_base;

type
  TVersionConvertorBase = class (TFslObject)
  protected
    // the R3 -> R4 change to remove URL as a value set option
    class function vsToRef(url : String) : String;
    class function refToVS(url : String) : String;
  end;

implementation

{ TVersionConvertorBase }

class function TVersionConvertorBase.refToVS(url: String): String;
begin
  if url = 'http://www.genenames.org' then
    result := 'http://hl7.org/fhir/ValueSet/genenames'
  else if url = 'http://varnomen.hgvs.org/' then
    result := 'http://hl7.org/fhir/ValueSet/variants'
  else if url = 'http://www.ncbi.nlm.nih.gov/nuccore?db=nuccore' then
    result := 'http://hl7.org/fhir/ValueSet/ref-sequences'
  else if url = 'http://www.ensembl.org/' then
    result := 'http://hl7.org/fhir/ValueSet/ensembl'
  else if url = 'http://www.ncbi.nlm.nih.gov/clinvar/variation' then
    result := 'http://hl7.org/fhir/ValueSet/clinvar'
  else if url = 'http://cancer.sanger.ac.uk/cancergenome/projects/cosmic/' then
    result := 'http://hl7.org/fhir/ValueSet/cosmic'
  else if url = 'http://www.ncbi.nlm.nih.gov/projects/SNP/' then
    result := 'http://hl7.org/fhir/ValueSet/bbsnp'
  else if url = 'http://www.sequenceontology.org/' then
    result := 'http://hl7.org/fhir/ValueSet/sequenceontology'
  else if url = 'http://www.ebi.ac.uk/' then
    result := 'http://hl7.org/fhir/ValueSet/allelename'
  else if url = 'https://www.iso.org/iso-4217-currency-codes.html' then
    result := 'http://hl7.org/fhir/ValueSet/currencies'
  else if url = 'http://www.rfc-editor.org/bcp/bcp13.txt' then
    result := 'http://hl7.org/fhir/ValueSet/mimetypes'
  else
    result := url;
end;

class function TVersionConvertorBase.vsToRef(url: String): String;
begin
  if url = 'http://hl7.org/fhir/ValueSet/genenames' then
    result := 'http://www.genenames.org'
  else if url = 'http://hl7.org/fhir/ValueSet/variants' then
    result := 'http://varnomen.hgvs.org/'
  else if url = 'http://hl7.org/fhir/ValueSet/ref-sequences' then
    result := 'http://www.ncbi.nlm.nih.gov/nuccore?db=nuccore'
  else if url = 'http://hl7.org/fhir/ValueSet/ensembl' then
    result := 'http://www.ensembl.org/'
  else if url = 'http://hl7.org/fhir/ValueSet/clinvar' then
    result := 'http://www.ncbi.nlm.nih.gov/clinvar/variation'
  else if url = 'http://hl7.org/fhir/ValueSet/cosmic' then
    result := 'http://cancer.sanger.ac.uk/cancergenome/projects/cosmic/'
  else if url = 'http://hl7.org/fhir/ValueSet/bbsnp' then
    result := 'http://www.ncbi.nlm.nih.gov/projects/SNP/'
  else if url = 'http://hl7.org/fhir/ValueSet/sequenceontology' then
    result := 'http://www.sequenceontology.org/'
  else if url = 'http://hl7.org/fhir/ValueSet/allelename' then
    result := 'http://www.ebi.ac.uk/'
  else if url = 'http://hl7.org/fhir/ValueSet/currencies' then
    result := 'https://www.iso.org/iso-4217-currency-codes.html'
  else if url = 'http://hl7.org/fhir/ValueSet/mimetypes' then
    result := 'http://www.rfc-editor.org/bcp/bcp13.txt'
  else
    result := '';
end;

end.
