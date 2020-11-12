unit fhir4_tests_Utilities;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  fsl_testing, fsl_stream,
  fsl_http, fsl_crypto,
  fhir_objects, fhir4_parser,
  fhir4_types, fhir4_resources, fhir4_utilities;

type
  TFHIRUtilityTests4 = Class (TFslTestCase)
  public
    Procedure TestBundleSigningXml;
    Procedure TestBundleSigningJson;
  published
    Procedure TestZipPartCreation;
    Procedure TestReferenceAnalysis;
  end;

procedure registerTests;

implementation

{ TFHIRUtilityTests4 }

procedure TFHIRUtilityTests4.TestBundleSigningXml;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(TestSettings.fhirTestFile(['R4', 'examples', 'document-example-dischargesummary.xml'])) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffXml, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, 'c:\temp\signed.xml', ffXml, OutputStylePretty);
    assertTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestBundleSigningJson;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(TestSettings.fhirTestFile(['R4', 'examples', 'document-example-dischargesummary.xml'])) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffJson, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, 'c:\temp\signed.json', ffJson, OutputStylePretty);
    assertTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestReferenceAnalysis;
var
  ref : TFhirReference;
begin
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example';
    assertTrue(not ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example/history/2';
    assertTrue(not ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'Patient/example';
    assertTrue(ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestZipPartCreation;
var
  att : TFhirAttachment;
  p : TFslZipPart;
begin
  att := TFHIRAttachment.create;
  try
    att.title := 'test';
    att.data := TEncoding.UTF8.GetBytes('Some test text');
    att.contentType := 'text/plain';
    p := att.asZipPart(0);
    try
      assertTrue(p.Name = 'test.txt');
      assertTrue(p.Size > 0);
      assertTrue(p.Comment = 'text/plain');
    finally
      p.Free;
    end;
  finally
    att.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('R4', TFHIRUtilityTests4.Suite);
end;

end.
