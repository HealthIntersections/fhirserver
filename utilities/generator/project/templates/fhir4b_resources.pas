unit fhir4b_resources;

{$I fhir4b.inc}

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

interface

{{mark}}

uses
  SysUtils, Classes, 
  fsl_base, fsl_utilities, fsl_stream, 
  fhir_objects, fhir_utilities,  
  fhir4b_base, fhir4b_enums, fhir4b_types, fhir4b_resources_base,
  fhir4b_resources_admin, fhir4b_resources_canonical, fhir4b_resources_clinical, fhir4b_resources_financial, fhir4b_resources_medications, fhir4b_resources_other;

type
 TFhirResourceType = fhir4b_resources_base.TFhirResourceType;
  TFhirResourceTypeSet = fhir4b_resources_base.TFhirResourceTypeSet;
  TFhirResource = fhir4b_resources_base.TFhirResource;
  TFhirResourceClass = fhir4b_resources_base.TFhirResourceClass;
  TFhirResourceList = fhir4b_resources_base.TFhirResourceList;
  TFhirDomainResource = fhir4b_resources_base.TFhirDomainResource;

  TFhirCanonicalResource = fhir4b_resources_canonical.TFhirCanonicalResource;
  TFhirMetadataResource = fhir4b_resources_canonical.TFhirMetadataResource;
  
{{redeclare}}


implementation

end.

