unit fhir4_tests_graphql;

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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Variants,
  fsl_base, fsl_testing, fsl_utilities, fsl_stream, fsl_xml, fsl_http, fsl_graphql, fsl_comparisons,
  fhir_objects, fhir_common, fhir_graphql,
  fhir4_types, fhir4_resources, fhir4_parser, fhir4_factory,
  fhir4_tests_worker;

type
  TFHIRGraphQLParserTest = class (TFslTestSuiteCase)
  private
    FBody : String;
  public
    constructor Create(name, body : string); overload;

    procedure TestCase(name : String); override;
  end;

  TFHIRGraphQLParserTests = class (TFslTestSuite)
  public
    constructor Create; overload; override;
  end;


  TFHIRGraphQLTest = class (TFslTestSuiteCase)
  private
    source, output, context, resource, opName: String;

    function ResolveReference(appInfo : TFslObject; context : TFHIRResourceV; reference : TFHIRObject; out targetContext, target : TFHIRResourceV) : boolean;
//    procedure ResolveReverseReference(appInfo : TFslObject; focusType, focusId, requestType, requestParam : String; params : TFslList<TGraphQLArgument>; start, limit : integer; list : TFslList<TFhirResource>);
    function LookupResource(appInfo : TFslObject; requestType, id : String; var res : TFHIRResourceV) : boolean;
    procedure ListResources(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>; list : TFslList<TFHIRResourceV>);
    function Search(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>) : TFHIRBundleW;
  public
    constructor Create(name, source, output, context, resource, opName : string); overload;

    procedure TestCase(name : String); override;
  end;

  TFHIRGraphQLTests = class (TFslTestSuite)
  public
    constructor Create; overload; override;
  end;

procedure registerTests;

implementation

{ TFHIRGraphQLParserTest }

constructor TFHIRGraphQLParserTest.Create(name, body: string);
begin
  inherited Create(name);
  FBody := body;
end;

procedure TFHIRGraphQLParserTest.TestCase(name: String);
var
  doc : TGraphQLPackage;
begin
  doc := TGraphQLParser.parse(FBody);
  try
    assertTrue(doc <> nil);
  finally
    doc.Free;
  end;
end;

{ TFHIRGraphQLParserTests }

constructor TFHIRGraphQLParserTests.Create;
var
  tests, test, name, body : String;
begin
  inherited Create;

  tests := FileToString(TestSettings.fhirTestFile(['r4', 'graphql', 'parser-tests.gql']), TEncoding.UTF8);
  while (tests <> '') do
  begin
    StringSplit(tests, '####', test, tests);
    if (test.Trim <> '') then
    begin
      StringSplit(test, #13#10, name, body);
      AddTest(TFHIRGraphQLParserTest.create(name, body));
    end;
  end;
end;


{ TFHIRGraphQLTest }

constructor TFHIRGraphQLTest.Create(name, source, output, context, resource, opName: string);
begin
  inherited create(name);
  self.source := source;
  self.output := output;
  self.context := context;
  self.resource := resource;
  self.opName := opName;
end;

procedure TFHIRGraphQLTest.ListResources(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>; list: TFslList<TFHIRResourceV>);
begin
  if requestType = 'Condition' then
    list.add(TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), TestSettings.fhirTestFile(['r4', 'examples', 'condition-example.xml'])))
  else if requestType = 'Patient' then
  begin
    list.Add(TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), TestSettings.fhirTestFile(['r4', 'examples', 'patient-example.xml'])));
    list.Add(TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), TestSettings.fhirTestFile(['r4', 'examples', 'patient-example-xds.xml'])));
  end;
end;

function TFHIRGraphQLTest.LookupResource(appInfo: TFslObject; requestType, id: String; var res: TFHIRResourceV): boolean;
var
  filename : String;
begin
  filename := TestSettings.fhirTestFile(['r4', 'examples', requestType+'-'+id+'.xml']);
  result := FileExists(filename);
  if result then
    res := TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), filename);
end;

function TFHIRGraphQLTest.ResolveReference(appInfo : TFslObject; context: TFHIRResourceV; reference: TFHIRObject; out targetContext, target: TFHIRResourceV): boolean;
var
  parts : TArray<String>;
  res : TFHIRResource;
  filename : String;
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
    filename := TestSettings.fhirTestFile(['r4', 'examples', parts[0].ToLower+'-'+parts[1].ToLower+'.xml']);
    result := FileExists(filename);
    if result then
      target := TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), filename);
  end;
end;

function TFHIRGraphQLTest.Search(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>): TFHIRBundleW;
var
  bnd : TFhirBundle;
  f : TFHIRFactoryX;
begin
  bnd := TFhirBundle.Create;
  try
    with bnd.link_List.Append do
    begin
      relation := 'next';
      url := 'http://test.fhir.org/r3/Patient?_format=text/xhtml&search-id=77c97e03-8a6c-415f-a63d-11c80cf73f&&active=true&_sort=_id&search-offset=50&_count=50';
    end;
    with bnd.link_List.Append do
    begin
      relation := 'self';
      url := 'http://test.fhir.org/r3/Patient?_format=text/xhtml&search-id=77c97e03-8a6c-415f-a63d-11c80cf73f&&active=true&_sort=_id&search-offset=0&_count=50';
    end;
    with bnd.entryList.Append do
    begin
      fullUrl := 'http://hl7.org/fhir/Patient/example';
      resource := TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), TestSettings.fhirTestFile(['r4', 'examples', 'patient-example.xml']));
    end;
    with bnd.entryList.Append do
    begin
      fullUrl := 'http://hl7.org/fhir/Patient/example';
      resource := TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), TestSettings.fhirTestFile(['r4', 'examples', 'patient-example-xds.xml']));
      search := TFhirBundleEntrySearch.Create;
      search.score := '0.5';
      search.mode := SearchEntryModeMatch;
    end;
    bnd.total := inttostr(50);
    f := TFHIRFactoryX.Create;
    try
      result := f.wrapBundle(bnd.Link);
    finally
      f.Free;
    end;
  finally
    bnd.Free;
  end;
end;

procedure TFHIRGraphQLTest.TestCase(name : String);
var
  parts : TArray<String>;
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
  ok : boolean;
  msg : String;
  filename : String;
begin
  filename := '';
  if context <> '' then
  begin
    parts := context.Split(['/']);
    if length(parts) <> 3 then
      raise ETestCase.create('not done yet '+source+' '+output+' '+context);
    if resource <> '' then
      filename := TestSettings.fhirTestFile(['r4', 'examples', resource+'.xml'])
    else
      filename := TestSettings.fhirTestFile(['r4', 'examples', parts[0].ToLower+'-'+parts[1].ToLower+'.xml'])
  end;

  gql := TFHIRGraphQLEngine.Create(TFHIRFactoryX.create);
  try
    gql.OnFollowReference := ResolveReference;
    gql.OnLookup := LookupResource;
    gql.OnListResources := ListResources;
    gql.OnSearch := Search;
    if (filename <> '') then
      gql.Focus := TFHIRParsers4.ParseFile(nil, ffXml, THTTPLanguages.create('en'), filename);
    gql.GraphQL := TGraphQLParser.parseFile(TestSettings.fhirTestFile(['r4', 'graphql', source]));
    gql.GraphQL.OperationName := opName;
    gql.GraphQL.variables.Add(TGraphQLArgument.Create('var', TGraphQLNameValue.Create('true')));
    try
      gql.execute;
      ok := true;
    except
      on e : Exception do
      begin
        ok := false;
        msg:= e.Message;
      end;
    end;
    if ok then
    begin
      assertTrue(output <> '$error', 'Expected to fail, but didn''t');
      str := TStringBuilder.create;
      try
        gql.output.write(str, 0);
        StringToFile(str.ToString, TestSettings.fhirTestFile(['r4', 'graphql', output+'.out']), TEncoding.UTF8);
        ok := CheckJsonIsSame(     TestSettings.fhirTestFile(['r4', 'graphql', output]),
                                   TestSettings.fhirTestFile(['r4', 'graphql', output+'.out']), msg);
        assertTrue(ok, msg);
      finally
        str.free;
      end;
    end
    else
      assertTrue(output = '$error', 'Error, but proper output was expected ('+msg+')');
  finally
    gql.Free;
  end;
end;

{ TFHIRGraphQLTests }

constructor TFHIRGraphQLTests.Create;
var
  tests : TMXmlElement;
  test : TMXmlElement;
begin
  inherited Create;

  tests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r4', 'graphql', 'manifest.xml']), [xpDropWhitespace]);
  try
    test := tests.document.first;

    while (test <> nil) and (test.Name = 'test') do
    begin
      AddTest(TFHIRGraphQLTest.create(test.attribute['name'], test.attribute['source'], test.attribute['output'], test.attribute['context'], test.attribute['resource'], test.attribute['operation']));
      test := test.Next;
    end;
  finally
    tests.Free;
  end;
end;

procedure registerTests;
begin
  registerTest('GraphQL.Tests.Parser', TFHIRGraphQLParserTests.create);
  registerTest('GraphQL.Tests.Engine', TFHIRGraphQLTests.create);
end;

end.
