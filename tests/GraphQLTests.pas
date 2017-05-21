unit GraphQLTests;

interface

uses
  SysUtils, Classes, DUnitX.TestFramework,
  StringSupport, TextUtilities,
  AdvObjects,
  MsXml, MsXmlParser,
  GraphQL, FHIRBase, FHIRTypes, FHIRResources, FHIRParser, FHIRGraphQL,
  FHIRTestWorker, JsonTests;

type
  GraphQLParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphQLParserTests = class (TObject)
  public
    [GraphQLParserTestCase]
    procedure TestCase(body
    : String);
  end;


  GraphQLTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphQLTests = class (TObject)
  private
    function ResolveReference(appInfo : TAdvObject; context : TFHIRResource; reference : TFHIRReference; out targetContext, target : TFHIRResource) : boolean;
  public
    [GraphQLTestCase]
    procedure TestCase(source,output,context,resource: String);
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
  doc : TGraphQLDocument;
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
  tests : IXMLDomDocument2;
  test : IXmlDomElement;
  i: integer;
  s : String;
begin
  tests := TMsXmlParser.Parse('C:\work\org.hl7.fhir\build\tests\graphql\manifest.xml');
  test := TMsXmlParser.FirstChild(tests.documentElement);
  i := 0;
  while (test <> nil) and (test.nodeName = 'test') do
  begin
    inc(i);
    test := TMsXmlParser.NextSibling(test);
  end;
  setLength(result, i);
  i := 0;
  test := TMsXmlParser.FirstChild(tests.documentElement);
  while (test <> nil) and (test.nodeName = 'test') do
  begin
    result[i].Name := test.getAttribute('name');
    SetLength(result[i].Values, 4);
    s := test.getAttribute('source');
    result[i].Values[0] := s;
    s := test.getAttribute('output');
    result[i].Values[1] := s;
    s := test.getAttribute('context');
    result[i].Values[2] := s;
    s := test.getAttribute('resource');
    result[i].Values[3] := s;
    inc(i);
    test := TMsXmlParser.NextSibling(test);
  end;
end;

{ TFHIRGraphQLTests }

function TFHIRGraphQLTests.ResolveReference(appInfo : TAdvObject; context: TFHIRResource; reference: TFHIRReference; out targetContext, target: TFHIRResource): boolean;
var
  parts : TArray<String>;
  res : TFHIRResource;
  filename : String;
begin
  targetContext := nil;
  target := nil;
  if reference.reference.startsWith('#') then
  begin
    if not (context is TFhirDomainResource) then
      exit(false);
    for res in TFhirDomainResource(context).containedList do
      if '#'+res.id = reference.reference then
      begin
        targetContext := context.Link;
        target := res.Link;
        exit(true);
      end;
    result := false;
  end
  else
  begin
    parts := reference.reference.Split(['/']);
    filename := 'C:\work\org.hl7.fhir\build\publish\'+parts[0].ToLower+'-'+parts[1].ToLower+'.xml';
    result := FileExists(filename);
    if result then
      target := TFHIRXmlParser.ParseFile(nil, 'en', filename);
  end;
end;

procedure TFHIRGraphQLTests.TestCase(source, output, context, resource: String);
var
  parts : TArray<String>;
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
  ok : boolean;
  msg : String;
  filename : String;
begin
  parts := context.Split(['/']);
  if length(parts) <> 3 then
    raise Exception.Create('not done yet '+source+' '+output+' '+context);
  if resource <> '' then
    filename := 'C:\work\org.hl7.fhir\build\publish\'+resource+'.xml'
  else
    filename := 'C:\work\org.hl7.fhir\build\publish\'+parts[0].ToLower+'-'+parts[1].ToLower+'.xml';

  gql := TFHIRGraphQLEngine.Create;
  try
    gql.OnFollowReference := ResolveReference;
    gql.Focus := TFHIRXmlParser.ParseFile(nil, 'en', filename);
    gql.QueryDocument := TGraphQLParser.parseFile('C:\work\org.hl7.fhir\build\tests\graphql\'+source);
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
