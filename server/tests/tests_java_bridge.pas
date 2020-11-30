unit tests_java_bridge;

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

uses
  SysUtils, Classes, Generics.Collections,
  TextUtilities,
  fsl_json,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, FHIRJavascript, FHIR.Version.Parser,
  FHIR.Server.Java,
  DUnitX.TestFramework;


Type
  [TextFixture]
  TJavaBridgeTests = Class (TObject)
  Private
    FJavaBridge : TJavaLibraryWrapper;
    function getStatusCount(name : String): integer; overload;
    procedure checkStatus(name : String; value : integer); overload;
    procedure checkStatus(name : String; value : string); overload;
    procedure checkStatusMissing(name : String); overload;
  Published
    [SetupFixture]    Procedure SetUp;
    [TearDownFixture] procedure TearDown;
    [TestCase]        Procedure TestStatus;
    [TestCase]        Procedure TestSeeResource;
    [TestCase]        Procedure TestDropResource;
    [TestCase]        Procedure TestValidation;
    [TestCase]        Procedure TestConvert;
    [TestCase]        Procedure TestUnConvert;
  End;

implementation


{ TJavaBridgeTests }

procedure TJavaBridgeTests.checkStatus(name: String; value: integer);
var
  st : TJsonObject;
begin
  st := FJavaBridge.status;
  try
    Assert.isTrue(st.has(name));
    Assert.isTrue(st.str[name] = inttostr(value));
  finally
    st.Free;
  end;
end;

procedure TJavaBridgeTests.checkStatus(name: String; value: string);
var
  st : TJsonObject;
begin
  st := FJavaBridge.status;
  try
    Assert.isTrue(st.has(name));
    Assert.isTrue(st.str[name] = value);
  finally
    st.Free;
  end;
end;

procedure TJavaBridgeTests.checkStatusMissing(name: String);
var
  st : TJsonObject;
begin
  st := FJavaBridge.status;
  try
    Assert.isFalse(st.has(name));
  finally
    st.Free;
  end;
end;

function TJavaBridgeTests.getStatusCount(name: String): integer;
var
  st : TJsonObject;
begin
  st := FJavaBridge.status;
  try
    result := StrToIntDef(st.str[name], 0);
  finally
    st.Free;
  end;
end;

procedure TJavaBridgeTests.Setup;
begin
  FJavaBridge := TJavaLibraryWrapper.create(PUB_HOME+'\org.hl7.fhir.validator.jar');
  FJavaBridge.init(PUB_HOME+'\definitions.xml.zip');
  FJavaBridge.txConnect('http://tx.fhir.org/r4');
end;

procedure TJavaBridgeTests.TearDown;
begin
  FJavaBridge.free;
end;

procedure TJavaBridgeTests.TestConvert;
var
  p : TFHIRPatient;
  b : TBytes;
begin
  b := FJavaBridge.convertResource(FileToBytes('C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish\patient-example.xml'), ffXml, fhirVersionRelease2);
  p := bytesToResource(b) as TFhirPatient;
  try
    Assert.isTrue(p.id = 'example');
  finally
    p.Free;
  end;
end;

procedure TJavaBridgeTests.TestDropResource;
begin
  if getStatusCount('custom-resource-count') = 0 then
    TestSeeResource;

  checkStatus('custom-resource-count', 1);
  FJavaBridge.dropResource('ValueSet', 'dicm-2-AnatomicModifier');
  checkStatus('custom-resource-count', 0);
end;

procedure TJavaBridgeTests.TestSeeResource;
var
  r : TFHIRResource;
begin
  if getStatusCount('custom-resource-count') = 1 then
    TestDropResource;

  checkStatus('custom-resource-count', 0);
  r := FileToResource('C:\work\fhirserver\resources\dicom\CID_2.xml');
  try
    FJavaBridge.seeResource(r, []);
  finally
    r.Free;
  end;
  checkStatus('custom-resource-count', 1);
end;

procedure TJavaBridgeTests.TestStatus;
begin
  checkStatusMissing('last-exception');
end;

procedure TJavaBridgeTests.TestUnConvert;
var
  b : TBytes;
begin
  b := FJavaBridge.unConvertResource(FileToBytes(PUB_HOME+'\patient-example.xml'), ffxml, fhirVersionRelease2);
  Assert.isTrue(length(b) > 0);
end;

procedure TJavaBridgeTests.TestValidation;
var
  oo : TFHIROperationOutcome;
begin
  oo := FJavaBridge.validateResource('my-loc', fileToBytes('C:\work\fhirserver\resources\r3\patient-group.xml'), ffXML, '');
  try
    Assert.IsTrue(oo.issueList.Count > 0);
  finally
    oo.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJavaBridgeTests);
end.
