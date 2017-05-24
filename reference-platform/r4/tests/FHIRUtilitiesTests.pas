unit FHIRUtilitiesTests;

interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  AdvZipParts,
  FHIRTypes, FHIRResources, FHIRParser, FHIRUtilities;

type
  [TextFixture]
  TFHIRUtilityTests = Class (TObject)
  private
  public
    [TestCase] Procedure TestZipPartCreation;
    [TestCase] Procedure TestZipGeneration;
    [TestCase] Procedure TestReferenceAnalysis;
  end;


implementation

{ TFHIRUtilityTests }

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
  dr := TFhirDocumentReference(TFHIRJsonParser.ParseFile(nil, 'en', 'C:\Users\Grahame Grieve\AppData\Roaming\Skype\My Skype Received Files\DocWithTwoJPGs.json'));//'C:\work\org.hl7.fhir\build\publish\documentreference-example.xml'));
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
  p : TAdvZipPart;
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
