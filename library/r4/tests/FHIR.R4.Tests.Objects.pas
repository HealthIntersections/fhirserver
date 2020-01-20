unit FHIR.R4.Tests.Objects;

interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  IdSSLOpenSSLHeaders, FHIR.Support.Certs, FHIR.Support.Stream, FHIR.Support.Tests,
  FHIR.Base.Objects, FHIR.Version.Parser,
  FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Json;

type
  [TextFixture]
  TFHIRObjectTests4 = Class (TObject)
  private
    function json(o : TFHIRResource) : String;
  public
    [TestCase] Procedure TestDropEmptySimple;
    [TestCase] Procedure TestDropEmptyComplex;
  end;

implementation

{ TFHIRObjectTests4 }

function TFHIRObjectTests4.json(o: TFHIRResource): String;
var
  c : TFHIRJsonComposer;
begin
  c := TFHIRJsonComposer.Create(nil, OutputStyleCanonical, 'en');
  try
    result := c.Compose(o);
  finally
    c.Free;
  end;
end;

procedure TFHIRObjectTests4.TestDropEmptySimple;
var
  o : TFHIRPatient;
begin
  o := TFHIRPatient.Create;
  try
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement = nil);
    o.id := 'test';
    Assert.IsTrue(json(o) = '{"id":"test","resourceType":"Patient"}');
    Assert.IsTrue(o.idElement <> nil);
    o.id := '';
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement <> nil);
    o.dropEmpty;
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement = nil);
  finally
    o.Free;
  end;
  Assert.IsTrue(true);
end;

procedure TFHIRObjectTests4.TestDropEmptyComplex;
var
  o : TFHIRPatient;
begin
  o := TFHIRPatient.Create;
  try
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 0);
    o.identifierList.Append.value := 'test';
    Assert.IsTrue(json(o) = '{"identifier":[{"value":"test"}],"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 1);
    o.identifierList[0].value := '';
    Assert.IsTrue(json(o) = '{"identifier":[{}],"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 1);
    o.dropEmpty;
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 0);
  finally
    o.Free;
  end;
  Assert.IsTrue(true);
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRObjectTests4);
end.
