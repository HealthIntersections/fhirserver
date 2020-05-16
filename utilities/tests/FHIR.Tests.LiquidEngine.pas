unit FHIR.Tests.LiquidEngine;

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base,
  FHIR.R4.Resources, FHIR.R4.Parser, FHIR.R4.Liquid, FHIR.R4.PathEngine, FHIR.R4.Xml,
  DUnitX.TestFramework, FHIR.Support.Tests, FHIR.R4.Tests.Worker;

type
  [TextFixture]
  TLiquidEngineTests = Class (TObject)
  private
    patient : TFHIRPatient;
    engine : TFHIRLiquidEngine;
    pages : TDictionary<String, String>;
    procedure checkProcess(source, expected : String);
    function FetchInclude(sender : TFHIRLiquidEngine; name : String; var content : String) : boolean;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [TestCase] Procedure testConstant;
    [TestCase] Procedure testSimple;
    [TestCase] Procedure testSimple1;
    [TestCase] Procedure testSimple2;
    [TestCase] Procedure testMix1;
    [TestCase] Procedure testIf;
    [TestCase] Procedure testLoop;
    [TestCase] procedure testInclude;
  End;

implementation

function TLiquidEngineTests.FetchInclude(sender : TFHIRLiquidEngine; name: String; var content: String): boolean;
begin
  result := pages.TryGetValue(name, content);
end;

procedure TLiquidEngineTests.Setup;
var
  p : TFHIRXmlParser;
  f : TFileStream;
begin
  pages := TDictionary<String, String>.create;
  engine := TFHIRLiquidEngine.Create(TFHIRPathEngine.Create(TTestingWorkerContext.Use, nil));
  engine.OnFetchInclude := FetchInclude;
  p := TFHIRXmlParser.create(TTestingWorkerContext.Use, THTTPLanguages.create('en'));
  try
    f := TFileStream.Create(FHIR_PUB_FILE('patient-example.xml'), fmOpenRead);
    try
      p.source := f;
      p.parse;
      patient := p.resource.Link as TFHIRPatient;
    finally
      f.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TLiquidEngineTests.TearDown;
begin
  patient.free;
  engine.Free;
  pages.free;
end;

procedure TLiquidEngineTests.testConstant();
begin
  checkProcess('test', 'test');
end;

procedure TLiquidEngineTests.testSimple();
begin
  checkProcess('{{ Patient.id}}', 'example');
end;

procedure TLiquidEngineTests.testSimple1();
begin
  checkProcess('{{ Patient.id }}', 'example');
end;

procedure TLiquidEngineTests.testSimple2();
begin
  checkProcess('{{Patient.id}}', 'example');
end;

procedure TLiquidEngineTests.testMix1();
begin
  checkProcess('t{{Patient.id}}t', 'texamplet');
end;

procedure TLiquidEngineTests.testIf();
begin
  checkProcess('{% if Patient.id = ''example''%} yes {%else%} no {%endif%}', ' yes ');
end;

procedure TLiquidEngineTests.testLoop();
begin
  checkProcess('{%loop name in Patient.name%}{{name.family}}{%endloop%}', 'ChalmersWindsor');
end;

procedure TLiquidEngineTests.testInclude();
begin
  pages.AddOrSetValue('humanname.html', '{{include.name.family}}');
  checkProcess('{%loop name in Patient.name%}{%include humanname.html name=name pat=''patient'' %}{%endloop%}', 'ChalmersWindsor');
end;

procedure TLiquidEngineTests.checkProcess(source, expected : String);
var
  doc : TFHIRLiquidDocument;
  output : String;
begin
  doc := engine.parse(source, 'test-script');
  try
    output := engine.evaluate(doc, patient, nil);
    Assert.IsTrue(expected = output);
  finally
    doc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLiquidEngineTests);
end.
