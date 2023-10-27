unit fhir4b_tests_graphdefinition;

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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, IOUtils,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} DUnitX.TestFramework, {$ENDIF}

  fsl_base, fsl_utilities, fsl_stream, fsl_xml,
  fhir_objects, fhir4b_types, fhir4b_resources, FHIR.Version.Parser,
  fsl_http, fsl_graphql, fhir_graphql, FHIR.Server.GraphDefinition,
  fhir4b_graphdefinition,
  fhir4b_tests_worker, fsl_tests;

{$IFNDEF FPC}
type
  GraphDefinitionTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphDefinitionTests = class (TObject)
  private
    function findTest(name : String) : TMXmlElement;
    function loadContext(context : String) : TFhirResource;
    function ResolveReference(appInfo : TFslObject; context : TFHIRResourceV; reference : TFHIRObject; out targetContext, target : TFHIRResourceV) : boolean;
    procedure ListResources(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>; list : TFslList<TFHIRResourceV>);
  public
    [GraphDefinitionTestCase]  procedure TestCase(name : String);
  end;
{$ENDIF}

implementation

{$IFNDEF FPC}

var
  tests : TMXmlElement;

{ GraphDefinitionTestCaseAttribute }

function GraphDefinitionTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  test : TMXmlElement;
  i: integer;
begin
  if tests = nil then
    tests := TMXmlParser.ParseFile(FHIR_TESTING_FILE(4, 'graphdefinition', 'manifest.xml'), [xpDropWhitespace]);

  test := tests.document.first;
  i := 0;
  while (test <> nil) and (test.Name = 'test') do
  begin
    inc(i);
    test := test.Next;
  end;
  setLength(result, i);
  i := 0;
  test := tests.document.first;
  while (test <> nil) and (test.Name = 'test') do
  begin
    result[i].Name := test.attribute['name'];
    SetLength(result[i].Values, 1);
    result[i].Values[0] := result[i].Name;
    inc(i);
    test := test.Next;
  end;
end;

{ TFHIRGraphDefinitionTests }

procedure TFHIRGraphDefinitionTests.ListResources(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>; list: TFslList<TFHIRResourceV>);
begin
  if requestType = 'Condition' then
    list.add(TFHIRParsers.ParseFile(nil, ffXml, nil, FHIR_TESTING_FILE(4, 'examples', 'condition-example.xml')))
  else if requestType = 'Patient' then
  begin
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, nil, FHIR_TESTING_FILE(4, 'examples', 'patient-example.xml')));
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, nil, FHIR_TESTING_FILE(4, 'examples', 'patient-example-xds.xml')));
  end;
end;

function TFHIRGraphDefinitionTests.loadContext(context: String): TFhirResource;
var
  p : TArray<String>;
  x : TFHIRXmlParser;
  f : TFileStream;
begin
  p := context.Split(['/']);
  if p[length(p)-1] <> '$graph' then
    raise ELibraryException.Create('not understood');
  f := TFileStream.Create(FHIR_TESTING_FILE(4, 'examples', p[0]+'-'+p[1]+'.xml'), fmOpenRead + fmShareDenyWrite);
  try
    x := TFHIRXmlParser.Create(nil, nil);
    try
      x.source := f;
      x.Parse;
      result := x.resource.Link as TFHIRResource;
    finally
      x.free;
    end;
  finally
    f.free;
  end;
end;

function TFHIRGraphDefinitionTests.ResolveReference(appInfo : TFslObject; context: TFHIRResourceV; reference: TFHIRObject; out targetContext, target: TFHIRResourceV): boolean;
var
  parts : TArray<String>;
  res : TFHIRResource;
  filename, src : String;
begin
  targetContext := nil;
  target := nil;
  if (reference as TFHIRReference).reference.startsWith('#') then
  begin
    if not (context is TFhirDomainResource) then
      exit(false);
    for res in TFhirDomainResource(context).containedList do
      if '#'+res.id = (reference as TFHIRReference).reference then
      begin
        targetContext := context.Link;
        target := res.Link;
        exit(true);
      end;
    result := false;
  end
  else
  begin
    parts := (reference as TFHIRReference).reference.Split(['/']);
    filename := FHIR_TESTING_FILE(4, 'examples', parts[0].ToLower+'-'+parts[1].ToLower+'.xml');
    result := FileExists(filename);
    if result then
      target := TFHIRParsers.ParseFile(nil, ffXml, nil, filename)
    else
    begin
      for filename in TDirectory.GetFiles(FHIR_TESTING_FILE(4, 'examples', ''), parts[0].ToLower+'-*.xml', TSearchOption.soTopDirectoryOnly) do
      begin
        if FileExists(ChangeFileExt(filename, '.json')) then
        begin
          src := fileToString(filename, TEncoding.UTF8);
          if (src.Contains('<id value="'+parts[1]+'"/>')) then
          begin
            target := TFHIRParsers.ParseFile(nil, ffXml, nil, filename);
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TFHIRGraphDefinitionTests.findTest(name : String): TMXmlElement;
var
  test : TMXmlElement;
begin
  if tests = nil then
    tests := TMXmlParser.ParseFile(FHIR_TESTING_FILE(4, 'graphdefinition', 'manifest.xml'), [xpDropWhitespace]);

  result := nil;
  test := tests.document.first;
  while (test <> nil) and (test.Name = 'test') do
  begin
    if (test.attribute['name'] = name) then
      exit(test);
    test := test.Next;
  end;
end;

procedure TFHIRGraphDefinitionTests.TestCase(name: String);
var
  test, rule : TMXmlElement;
  src : String;
  gd : TFHIRGraphDefinition;
  parser : TFHIRGraphDefinitionParser4;
  engine : TFHIRGraphDefinitionEngine4;
  x : TFHIRXmlComposer;
  f : TFileStream;
begin
  Assert.IsTrue(name <> '');
  test := findTest(name);
  src := FileToString(FHIR_TESTING_FILE(4, 'graphdefinition', test.attribute['source']), TEncoding.UTF8);
  gd := nil;
  try
    parser := TFHIRGraphDefinitionParser4.Create;
    try
      gd := parser.parseV(src) as TFHIRGraphDefinition;
    finally
      parser.free;
    end;
//    StringToFile(TFHIRGraphDefinitionParser4.asString(gd, false), path([FHIR_TESTING_FILE(4, 'examples', 'graphdefinition'), test.attribute['source']+'.out']), TEncoding.UTF8);
    engine := TFHIRGraphDefinitionEngine4.Create(TTestingWorkerContext4.Use);
    try
      engine.OnListResources := ListResources;
      engine.OnFollowReference := ResolveReference;

      engine.baseURL := 'http://hl7.org/fhir/test';
      engine.definition := gd.link;
      engine.start := loadContext(test.attribute['context']);
      engine.bundle := TFhirBundle.Create;
      engine.depthLimit := 25;
      engine.validating := false;

      engine.execute;

      f := TFileStream.Create(FHIR_TESTING_FILE(4, 'graphdefinition', test.attribute['source']+'.bundle'), fmCreate);
      try
        x := TFHIRXmlComposer.Create(nil, OutputStylePretty, nil);
        try
          x.Compose(f, engine.bundle);
        finally
          x.free;
        end;
      finally
        f.free;
      end;
      for rule in test.Children do
        Assert.IsTrue(engine.PathEngine.evaluateToBoolean(nil, engine.bundle, engine.bundle, rule.attribute['expression']), 'Failed rule: '+rule.attribute['description']);
    finally
      engine.free;
    end;
  finally
    gd.free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TFHIRGraphDefinitionTests);
finalization
  tests.free;
{$ENDIF}
end.
