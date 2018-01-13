unit JavaBridgeTests;

interface

uses
  SysUtils, Classes, Generics.Collections,
  TextUtilities,
  AdvJson,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRJavascript, FHIRParser,
  JavaBridge,
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
    [SetupFixture]    Procedure Setup;
    [TearDownFixture] Procedure TearDown;
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
  FJavaBridge := TJavaLibraryWrapper.create('C:\work\org.hl7.fhir\build\publish\org.hl7.fhir.validator.jar');
  FJavaBridge.init('C:\work\org.hl7.fhir\build\publish\definitions.xml.zip');
  FJavaBridge.txConnect('http://tx.fhir.org/r3');
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
  b := FJavaBridge.unConvertResource(FileToBytes('C:\work\org.hl7.fhir\build\publish\patient-example.xml'), ffxml, fhirVersionRelease2);
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
