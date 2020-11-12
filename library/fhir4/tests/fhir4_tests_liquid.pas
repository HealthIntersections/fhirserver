unit fhir4_tests_Liquid;

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
  fsl_testing, fsl_base, fsl_json, fsl_tests,
  fhir4_tests_worker,
  fhir4_resources, fhir4_parser, fhir4_liquid, fhir4_pathengine, fhir4_xml;

type
  TLiquidEngineTest4 = Class (TFslTestSuiteCase)
  private
    engine : TFHIRLiquidEngine;
    test : TJsonObject;
    function findTest(name : String) : TJsonObject;
    function FetchInclude(sender : TFHIRLiquidEngine; name : String; var content : String) : boolean;
    function loadResource : TFhirResource;
  Public
    Procedure SetUp; override;
    procedure TearDown; override;
  Published
    procedure TestCase(Name : String); override;
  End;

  TLiquidEngineTest4Suite = class (TFslTestSuite)
  public
    constructor Create; override;
  end;

procedure registerTests;

implementation

var
  gTestDoc : TJsonObject;
  gResources : TFslMap<TFHIRResource>;

{ TLiquidEngineTest4Suite }

constructor TLiquidEngineTest4Suite.Create;
var
  tests : TJsonArray;
  test : TJsonObject;
  i : integer;
begin
  inherited Create;
  if gResources = nil then
    gResources := TFslMap<TFHIRResource>.create('resources');
  if gTestDoc = nil then
    gTestDoc := TJSONParser.ParseFile(TestSettings.fhirTestFile(['r4', 'liquid', 'liquid-tests.json']));
  tests := gTestDoc.arr['tests'];
  for i := 0 to tests.Count - 1 do
  begin
    test := tests[i] as TJsonObject;
    AddTest(TLiquidEngineTest4.Create(test.str['name']));
  end;
end;

{ TLiquidEngineTest4 }

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

function TLiquidEngineTest4.FetchInclude(sender : TFHIRLiquidEngine; name: String; var content: String): boolean;
begin
  result := test.has('includes') and test.obj['includes'].has(name);
  if result then
    content := test.obj['includes'].str[name];
end;

function TLiquidEngineTest4.loadResource : TFhirResource;
var
  fn : String;
  p : TFHIRXmlParser;
  f : TFileStream;
begin
  if not gResources.ContainsKey(test.str['focus']) then
  begin
    fn := TestSettings.fhirTestFile(['r4', 'examples', test.str['focus'].replace('/', '-')+'.xml']);
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

procedure TLiquidEngineTest4.TestCase(Name: String);
var
  doc : TFHIRLiquidDocument;
  output : String;
begin
  test := findTest(name);
  doc := engine.parse(test.str['template'], 'test-script');
  try
    output := engine.evaluate(doc, loadResource, nil);
    assertTrue(test.str['output'] = output);
  finally
    doc.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('R4', TLiquidEngineTest4Suite.create);
end;

initialization
finalization
  gTestDoc.Free;
  gResources.Free;
end.

