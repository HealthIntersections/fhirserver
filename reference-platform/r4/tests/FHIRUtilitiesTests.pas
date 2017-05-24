unit FHIRUtilitiesTests;

interface

uses
  SysUtils,
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
  end;


implementation

{ TFHIRUtilityTests }

procedure TFHIRUtilityTests.TestZipGeneration;
var
  dr : TFHIRDocumentReference;
  fn : String;
begin
  dr := TFhirDocumentReference(TFHIRJsonParser.ParseFile(nil, 'en', 'C:\Users\Grahame Grieve\AppData\Roaming\Skype\My Skype Received Files\dr.json'));//'C:\work\org.hl7.fhir\build\publish\documentreference-example.xml'));
  try
    dr.asZip(fn).Free;
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
