unit FHIR.R5.Tests.Liquid;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_json,
  fhir5_resources, fhir5_parser, fhir5_liquid, fhir5_pathengine, fhir5_xml,
  DUnitX.TestFramework, fsl_tests, FHIR.R5.Tests.Worker;

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
    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [LiquidTestCase]
    procedure FHIRPathTest(Name : String);
  protected
    function sizeInBytesV : cardinal; override;
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
    gTestDoc := TJSONParser.ParseFile('C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\liquid\liquid-tests.json');
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

function TLiquidEngineTest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, engine.sizeInBytes);
  inc(result, test.sizeInBytes);
end;

initialization
  TDUnitX.RegisterTestFixture(TLiquidEngineTest);
finalization
  gTestDoc.Free;
  gResources.Free;

end.

