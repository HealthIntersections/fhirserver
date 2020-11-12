unit fhir4_tests_Context;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_testing,
  fsl_utilities,
  fhir_objects,
  fhir4_parser, fhir4_types, fhir4_resources, fhir4_constants, fhir4_context, fhir4_pathengine, fhir4_tests_worker;

Type
  TFhirHTTPMetadataResourceManagerTests4 = class (TFslTestCase)
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testSingleNoVersion;
  end;

procedure registerTests;

implementation

{ TFhirHTTPMetadataResourceManagerTests4 }

procedure TFhirHTTPMetadataResourceManagerTests4.setup;
begin
end;

procedure TFhirHTTPMetadataResourceManagerTests4.TearDown;
begin
end;

procedure TFhirHTTPMetadataResourceManagerTests4.testSingleNoVersion;
begin
  assertPass(); // till we get around to testing
end;

procedure registerTests;
begin
  registerTest('R4', TFhirHTTPMetadataResourceManagerTests4.Suite);
end;

end.

