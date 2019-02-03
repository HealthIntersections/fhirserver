unit FHIR.Tests.GraphDefinition;

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


interface

uses
  SysUtils, Classes, DUnitX.TestFramework, Variants, IOUtils,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.MXml,
  FHIR.Base.Objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Parser,
  FHIR.Web.GraphQL, FHIR.Tools.GraphQL, FHIR.Server.GraphDefinition,
  FHIR.R4.GraphDefinition,
  FHIR.R4.Tests.Worker, FHIR.Support.Tests;

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


implementation

var
  tests : TMXmlElement;

{ GraphDefinitionTestCaseAttribute }

function GraphDefinitionTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  test : TMXmlElement;
  i: integer;
begin
  if tests = nil then
    tests := TMXmlParser.ParseFile('C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\graphdefinition\manifest.xml', [xpDropWhitespace]);

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
    list.add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('condition-example.xml')))
  else if requestType = 'Patient' then
  begin
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example.xml')));
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example-xds.xml')));
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
  f := TFileStream.Create(FHIR_PUB_FILE(p[0]+'-'+p[1]+'.xml'), fmOpenRead + fmShareDenyWrite);
  try
    x := TFHIRXmlParser.Create(nil, 'en');
    try
      x.source := f;
      x.Parse;
      result := x.resource.Link as TFHIRResource;
    finally
      x.Free;
    end;
  finally
    f.Free;
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
    filename := FHIR_PUB_FILE(parts[0].ToLower+'-'+parts[1].ToLower+'.xml');
    result := FileExists(filename);
    if result then
      target := TFHIRParsers.ParseFile(nil, ffXml, 'en', filename)
    else
    begin
      for filename in TDirectory.GetFiles(FHIR_PUB_HOME_1, parts[0].ToLower+'-*.xml', TSearchOption.soTopDirectoryOnly) do
      begin
        if FileExists(ChangeFileExt(filename, '.json')) then
        begin
          src := fileToString(filename, TEncoding.UTF8);
          if (src.Contains('<id value="'+parts[1]+'"/>')) then
          begin
            target := TFHIRParsers.ParseFile(nil, ffXml, 'en', filename);
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
    tests := TMXmlParser.ParseFile('C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\graphdefinition\manifest.xml', [xpDropWhitespace]);

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
  src := FileToString(path(['C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\graphdefinition', test.attribute['source']]), TEncoding.UTF8);
  gd := nil;
  try
    parser := TFHIRGraphDefinitionParser4.create;
    try
      gd := parser.parseV(src) as TFHIRGraphDefinition;
    finally
      parser.free;
    end;
//    StringToFile(TFHIRGraphDefinitionParser4.asString(gd, false), path(['C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\graphdefinition', test.attribute['source']+'.out']), TEncoding.UTF8);
    engine := TFHIRGraphDefinitionEngine4.Create(TTestingWorkerContext.Use);
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

      f := TFileStream.Create(path(['C:\work\org.hl7.fhir\org.hl7.fhir.core\org.hl7.fhir.r4\src\main\resources\graphdefinition', test.attribute['source']+'.bundle']), fmCreate);
      try
        x := TFHIRXmlComposer.Create(nil, OutputStylePretty, 'en');
        try
          x.Compose(f, engine.bundle);
        finally
          x.Free;
        end;
      finally
        f.Free;
      end;
      for rule in test.Children do
        Assert.IsTrue(engine.PathEngine.evaluateToBoolean(nil, engine.bundle, engine.bundle, rule.attribute['expression']), 'Failed rule: '+rule.attribute['description']);
    finally
      engine.Free;
    end;
  finally
    gd.Free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TFHIRGraphDefinitionTests);
finalization
  tests.Free;
end.
