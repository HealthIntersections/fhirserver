unit FHIR.Tests.GraphQL;

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
  SysUtils, Classes, DUnitX.TestFramework, Variants,
   FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Base, FHIR.Support.MXml,
  FHIR.Web.GraphQL, FHIR.Base.Objects, FHIR.Base.Common, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Parser, FHIR.Tools.GraphQL, FHIR.Version.Factory,
  FHIR.R4.Tests.Worker, FHIR.Support.Tests, FHIR.Support.Comparisons;

type
  GraphQLParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphQLParserTests = class (TObject)
  public
    [GraphQLParserTestCase]
    procedure TestCase(body : String);
  end;


  GraphQLTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphQLTests = class (TObject)
  private
    function ResolveReference(appInfo : TFslObject; context : TFHIRResourceV; reference : TFHIRObject; out targetContext, target : TFHIRResourceV) : boolean;
//    procedure ResolveReverseReference(appInfo : TFslObject; focusType, focusId, requestType, requestParam : String; params : TFslList<TGraphQLArgument>; start, limit : integer; list : TFslList<TFhirResource>);
    function LookupResource(appInfo : TFslObject; requestType, id : String; var res : TFHIRResourceV) : boolean;
    procedure ListResources(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>; list : TFslList<TFHIRResourceV>);
    function Search(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>) : TFHIRBundleW;
  public
    [GraphQLTestCase]
    procedure TestCase(source,output,context,resource,opName: String);
  end;


implementation

{ GraphQLParserTestCaseAttribute }

function GraphQLParserTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : String;
  test : String;
  i: integer;
  name,body : String;
begin
  tests := FileToString('C:\work\org.hl7.fhir\build\tests\graphql\parser-tests.gql', TEncoding.UTF8);
  i := 0;
  while (tests <> '') do
  begin
    StringSplit(tests, '####', test, tests);
    if (test.Trim <> '') then
      inc(i);
  end;
  setLength(result, i);
  i := 0;
  tests := FileToString('C:\work\org.hl7.fhir\build\tests\graphql\parser-tests.gql', TEncoding.UTF8);
  while (tests <> '') do
  begin
    StringSplit(tests, '####', test, tests);
    if (test.Trim <> '') then
    begin
      StringSplit(test, #13#10, name, body);

      result[i].Name := name;
      SetLength(result[i].Values, 1);
      result[i].Values[0] := body;
      inc(i);
    end;
  end;
end;

{ TFHIRGraphQLParserTests }

procedure TFHIRGraphQLParserTests.TestCase(body: String);
var
  doc : TGraphQLPackage;
begin
  doc := TGraphQLParser.parse(body);
  try
    Assert.IsNotNull(doc);
  finally
    doc.Free;
  end;
end;


{ GraphQLTestCaseAttribute }

function GraphQLTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : TMXmlElement;
  test : TMXmlElement;
  i: integer;
begin
  tests := TMXmlParser.ParseFile('C:\work\org.hl7.fhir\build\tests\graphql\manifest.xml', [xpDropWhitespace]);
  try
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
      SetLength(result[i].Values, 5);
      result[i].Values[0] := test.attribute['source'];
      result[i].Values[1] := test.attribute['output'];
      result[i].Values[2] := test.attribute['context'];
      result[i].Values[3] := test.attribute['resource'];
      result[i].Values[4] := test.attribute['operation'];
      inc(i);
      test := test.Next;
    end;
  finally
    tests.Free;
  end;
end;

{ TFHIRGraphQLTests }

procedure TFHIRGraphQLTests.ListResources(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>; list: TFslList<TFHIRResourceV>);
begin
  if requestType = 'Condition' then
    list.add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('condition-example.xml')))
  else if requestType = 'Patient' then
  begin
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example.xml')));
    list.Add(TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example-xds.xml')));
  end;
end;

function TFHIRGraphQLTests.LookupResource(appInfo: TFslObject; requestType, id: String; var res: TFHIRResourceV): boolean;
var
  filename : String;
begin
  filename := FHIR_PUB_FILE(requestType+'-'+id+'.xml');
  result := FileExists(filename);
  if result then
    res := TFHIRParsers.ParseFile(nil, ffXml, 'en', filename);
end;

function TFHIRGraphQLTests.ResolveReference(appInfo : TFslObject; context: TFHIRResourceV; reference: TFHIRObject; out targetContext, target: TFHIRResourceV): boolean;
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
    filename := FHIR_PUB_FILE(parts[0].ToLower+'-'+parts[1].ToLower+'.xml');
    result := FileExists(filename);
    if result then
      target := TFHIRParsers.ParseFile(nil, ffXml, 'en', filename);
  end;
end;

function TFHIRGraphQLTests.Search(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>): TFHIRBundleW;
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
      resource := TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example.xml'));
    end;
    with bnd.entryList.Append do
    begin
      fullUrl := 'http://hl7.org/fhir/Patient/example';
      resource := TFHIRParsers.ParseFile(nil, ffXml, 'en', FHIR_PUB_FILE('patient-example-xds.xml'));
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

procedure TFHIRGraphQLTests.TestCase(source, output, context, resource, opName: String);
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
      filename := FHIR_PUB_FILE(resource+'.xml')
    else
      filename := FHIR_PUB_FILE(parts[0].ToLower+'-'+parts[1].ToLower+'.xml');
  end;

  gql := TFHIRGraphQLEngine.Create(TFHIRFactoryX.create);
  try
    gql.OnFollowReference := ResolveReference;
    gql.OnLookup := LookupResource;
    gql.OnListResources := ListResources;
    gql.OnSearch := Search;
    if (filename <> '') then
      gql.Focus := TFHIRParsers.ParseFile(nil, ffXml, 'en', filename);
    gql.GraphQL := TGraphQLParser.parseFile('C:\work\org.hl7.fhir\build\tests\graphql\'+source);
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
      Assert.IsTrue(output <> '$error', 'Expected to fail, but didn''t');
      str := TStringBuilder.create;
      try
        gql.output.write(str, 0);
        StringToFile(str.ToString, 'C:\work\org.hl7.fhir\build\tests\graphql\'+output+'.out', TEncoding.UTF8);
        ok := CheckJsonIsSame('C:\work\org.hl7.fhir\build\tests\graphql\'+output+'.out', 'C:\work\org.hl7.fhir\build\tests\graphql\'+output, msg);
        assert.IsTrue(ok, msg);
      finally
        str.free;
      end;
    end
    else
      Assert.IsTrue(output = '$error', 'Error, but proper output was expected ('+msg+')');
  finally
    gql.Free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TFHIRGraphQLParserTests);
  TDUnitX.RegisterTestFixture(TFHIRGraphQLTests);
end.
