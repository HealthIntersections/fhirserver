unit FHIR.R3.Tests.Liquid;

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Json,
  FHIR.R3.Resources, FHIR.R3.Parser, FHIR.R3.Liquid, FHIR.R3.PathEngine, FHIR.R3.Xml,
  DUnitX.TestFramework, FHIR.Support.Tests, FHIR.R3.Tests.Worker;

type
  LiquidTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TLiquidEngineTest = Class (TObject)
  private
    engine : TFHIRLiquidEngine;
    test : TJsonObject;
    function findTest(name : String) : TJsonObject;
    function FetchInclude(sender : TFHIRLiquidEngine; name : String; var content : String) : boolean;
    function loadResource : TFhirResource;
  Published
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

    [LiquidTestCase]
    procedure FHIRPathTest(Name : String);
  End;

implementation

var
  gTestDoc : TJsonObject;
  gResources : TFslMap<TFHIRResource>;

{ LiquidTestCaseAttribute }

function LiquidTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : TJsonArray;
  test : TJsonObject;
  i : integer;
begin
  if gResources = nil then
    gResources := TFslMap<TFHIRResource>.create;
  if gTestDoc = nil then
    gTestDoc := TJSONParser.ParseFile('C:\work\org.hl7.fhir\build\tests\resources\liquid-tests.json');
  tests := gTestDoc.arr['tests'];
  SetLength(result, tests.Count);
  for i := 0 to tests.Count - 1 do
  begin
    test := tests[i] as TJsonObject;
    result[i].Name := test.str['name'];
    SetLength(result[i].Values, 1);
    result[i].Values[0] := test.str['name'];
  end;
end;


function TLiquidEngineTest.FetchInclude(sender : TFHIRLiquidEngine; name: String; var content: String): boolean;
begin
  result := test.has('includes') and test.obj['includes'].has(name);
  if result then
    content := test.obj['includes'].str[name];
end;

function TLiquidEngineTest.findTest(name: String): TJsonObject;
var
  tests : TJsonArray;
  test : TJsonObject;
  i : integer;
begin
  result := nil;
  tests := gTestDoc.arr['tests'];
  for i := 0 to tests.Count - 1 do
  begin
    test := tests[i] as TJsonObject;
    if name = test.str['name'] then
      exit(test);
  end;
end;

function TLiquidEngineTest.loadResource : TFhirResource;
var
  fn : String;
  p : TFHIRXmlParser;
  f : TFileStream;
begin
  if not gResources.ContainsKey(test.str['focus']) then
  begin
    fn := FHIR_PUB_FILE(test.str['focus'].replace('/', '-')+'.xml');
    p := TFHIRXmlParser.create(TTestingWorkerContext.Use, THTTPLanguages.create('en'));
    try
      f := TFileStream.Create(fn, fmOpenRead);
      try
        p.source := f;
        p.parse;
        gResources.add(test.str['focus'], p.resource.link as TFhirResource);
      finally
        f.Free;
      end;
    finally
      p.Free;
    end;
  end;
  result := gResources[test.str['focus']];
end;

procedure TLiquidEngineTest.Setup;
begin
  engine := TFHIRLiquidEngine.Create(TFHIRPathEngine.Create(TTestingWorkerContext.Use, nil));
  engine.OnFetchInclude := FetchInclude;
end;

procedure TLiquidEngineTest.TearDown;
begin
  engine.Free;
end;

procedure TLiquidEngineTest.FHIRPathTest(Name: String);
var
  doc : TFHIRLiquidDocument;
  output : String;
begin
  test := findTest(name);
  doc := engine.parse(test.str['template'], 'test-script');
  try
    output := engine.evaluate(doc, loadResource, nil);
    Assert.IsTrue(test.str['output'] = output);
  finally
    doc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLiquidEngineTest);
finalization
  gTestDoc.Free;
  gResources.Free;
end.

