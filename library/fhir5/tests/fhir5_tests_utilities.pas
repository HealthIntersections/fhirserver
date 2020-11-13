unit FHIR.R5.Tests.Utilities;

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
interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  IdSSLOpenSSLHeaders, FHIR.Support.Certs, fsl_stream, fsl_tests,
  fhir_objects, FHIR.Version.Parser, 
  fhir5_types, fhir5_resources, fhir5_utilities;

type
  [TextFixture]
  TFHIRUtilityTests = Class (TObject)
  public
    [SetUp] Procedure SetUp;

    [TestCase] Procedure TestZipPartCreation;
    Procedure TestZipGeneration;
    [TestCase] Procedure TestReferenceAnalysis;
    {[TestCase] }Procedure TestBundleSigningXml;
    {[TestCase] }Procedure TestBundleSigningJson;
  end;


implementation

{ TFHIRUtilityTests }

procedure TFHIRUtilityTests.TestBundleSigningXml;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(FHIR_PUB_FILE('document-example-dischargesummary.xml')) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffXml, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, 'c:\temp\signed.xml', ffXml, OutputStylePretty);
    Assert.IsTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

procedure TFHIRUtilityTests.Setup;
begin
  IdSSLOpenSSLHeaders.Load;
  LoadEAYExtensions;
end;

procedure TFHIRUtilityTests.TestBundleSigningJson;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(FHIR_PUB_FILE('document-example-dischargesummary.xml')) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffJson, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, 'c:\temp\signed.json', ffJson, OutputStylePretty);
    Assert.IsTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

procedure TFHIRUtilityTests.TestReferenceAnalysis;
var
  ref : TFhirReference;
begin
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example';
    Assert.IsTrue(not ref.isRelative);
    Assert.IsTrue(ref.getType = 'Patient');
    Assert.IsTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example/history/2';
    Assert.IsTrue(not ref.isRelative);
    Assert.IsTrue(ref.getType = 'Patient');
    Assert.IsTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'Patient/example';
    Assert.IsTrue(ref.isRelative);
    Assert.IsTrue(ref.getType = 'Patient');
    Assert.IsTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
end;

procedure TFHIRUtilityTests.TestZipGeneration;
var
  dr : TFHIRDocumentReference;
  fn : String;
  f : TFileStream;
  s : TStream;
begin
  dr := TFhirDocumentReference(TFHIRParsers.ParseFile(nil, ffJson, THTTPLanguages.create('en'), 'C:\Users\Grahame Grieve\AppData\Roaming\Skype\My Skype Received Files\DocWithTwoJPGs.json'));//PUB_HOME+'\documentreference-example.xml'));
  try
    s:= dr.asZip(fn);
    try
      s.Position := 0;
      f := TFIleStream.create('c:\temp\test.zip', fmCreate);
      try
        f.CopyFrom(s, s.Size)
      finally
        f.Free;
      end;
    finally
     s.Free;
    end;
    Assert.IsTrue(fn <> '');
  finally
    dr.Free;
  end;
end;

procedure TFHIRUtilityTests.TestZipPartCreation;
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
      Assert.IsTrue(p.Name = 'test.txt');
      Assert.IsTrue(p.Size > 0);
      Assert.IsTrue(p.Comment = 'text/plain');
    finally
      p.Free;
    end;
  finally
    att.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRUtilityTests);
end.
