unit FHIR.R4.Tests.Liquid;

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Json,
  FHIR.R4.Resources, FHIR.R4.Parser, FHIR.R4.Liquid, FHIR.R4.PathEngine, FHIR.R4.Xml,
  DUnitX.TestFramework, FHIR.Support.Tests, FHIR.R4.Tests.Worker;

type
  LiquidTestCase4Attribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TLiquidEngineTest4 = Class (TObject)
  private
    engine : TFHIRLiquidEngine;
    test : TJsonObject;
    function findTest(name : String) : TJsonObject;
    function FetchInclude(sender : TFHIRLiquidEngine; name : String; var content : String) : boolean;
    function loadResource : TFhirResource;
  Published
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

    [LiquidTestCase4]
    procedure FHIRPathTest(Name : String);
  End;

implementation

var
  gTestDoc : TJsonObject;
  gResources : TFslMap<TFHIRResource>;

{ LiquidTestCase4Attribute }

function LiquidTestCase4Attribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : TJsonArray;
  test : TJsonObject;
  i : integer;
begin
  if gResources = nil then
    gResources := TFslMap<TFHIRResource>.create('resources');
  if gTestDoc = nil then
    gTestDoc := TJSONParser.ParseFile(FHIR_TESTING_FILE(4, 'liquid', 'liquid-tests.json'));
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


function TLiquidEngineTest4.FetchInclude(sender : TFHIRLiquidEngine; name: String; var content: String): boolean;
begin
  result := test.has('includes') and test.obj['includes'].has(name);
  if result then
    content := test.obj['includes'].str[name];
end;

function TLiquidEngineTest4.findTest(name: String): TJsonObject;
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

function TLiquidEngineTest4.loadResource : TFhirResource;
var
  fn : String;
  p : TFHIRXmlParser;
  f : TFileStream;
begin
  if not gResources.ContainsKey(test.str['focus']) then
  begin
    fn := FHIR_TESTING_FILE(4, 'examples', test.str['focus'].replace('/', '-')+'.xml');
    p := TFHIRXmlParser.create(TTestingWorkerContext4.Use, engine.engine.context.lang);
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

procedure TLiquidEngineTest4.Setup;
begin
  engine := TFHIRLiquidEngine.Create(TFHIRPathEngine.Create(TTestingWorkerContext4.Use, nil));
  engine.OnFetchInclude := FetchInclude;
end;

procedure TLiquidEngineTest4.TearDown;
begin
  engine.Free;
end;

procedure TLiquidEngineTest4.FHIRPathTest(Name: String);
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
  TDUnitX.RegisterTestFixture(TLiquidEngineTest4);
finalization
  gTestDoc.Free;
  gResources.Free;
end.

