unit FHIRGraphQL;

interface

uses
  SysUtils, Classes, DUnitX.TestFramework,
  AdvObjects, AdvGenerics,
  MsXml, MsXmlparser, TextUtilities,
  GraphQL,
  FHIRBase, FHIRTypes, FHIRResources, FHIRParser,
  FHIRTestWorker;

type
  TFHIRGraphQLEngineDereferenceEvent = function(context : TFHIRResource; reference : TFHIRReference; out resource : TFHIRResource) : boolean of Object;

  TFHIRGraphQLEngine = class (TAdvObject)
  private
    FFocus: TFHIRResource;
    FQueryName: String;
    FOutput: TGraphQLObjectValue;
    FDocument: TGraphQLDocument;
    FOnFollowReference: TFHIRGraphQLEngineDereferenceEvent;
    procedure SetDocument(const Value: TGraphQLDocument);
    procedure SetFocus(const Value: TFHIRResource);

    procedure processObject(source : TFHIRObject; target : TGraphQLObjectValue; selection : TAdvList<TGraphQLSelection>);
    procedure processPrimitive(arg : TGraphQLArgument; value : TFHIRObject);

  public
    Constructor Create; override;
    Destructor Destroy; override;

    // the focus resource - if there is one. If there isn't, then the focus is a collection
    property focus : TFHIRResource read FFocus write SetFocus;

    // the graphql document to execute
    property queryDocument : TGraphQLDocument read FDocument write SetDocument;

    // the name of the query (if there is more than one
    property queryName : String read FQueryName write FQueryName;

    // where output is going to go
    property output : TGraphQLObjectValue read FOutput;

    property OnFollowReference : TFHIRGraphQLEngineDereferenceEvent read FOnFollowReference write FOnFollowReference;

    procedure execute;
  end;

  GraphQLTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRGraphQLTests = class (TObject)
  public
    [GraphQLTestCase]
    procedure TestCase(source,output,context: String);
  end;

implementation

{ TFHIRGraphQLEngine }

constructor TFHIRGraphQLEngine.Create;
begin
  inherited;

end;

destructor TFHIRGraphQLEngine.Destroy;
begin
  FFocus.Free;
  FOutput.Free;
  FDocument.Free;
  inherited;
end;

procedure TFHIRGraphQLEngine.SetDocument(const Value: TGraphQLDocument);
begin
  FDocument.Free;
  FDocument := Value;
end;

procedure TFHIRGraphQLEngine.SetFocus(const Value: TFHIRResource);
begin
  FFocus.Free;
  FFocus := Value;
end;

procedure TFHIRGraphQLEngine.execute;
begin
  FOutput.Free;
  FOutput := TGraphQLObjectValue.Create;

  // todo: initial conditions
  if FDocument.Operations.Count <> 1 then
    raise Exception.Create('Unable to process graphql');

  processObject(FFocus, FOutput, FDocument.Operations[0].SelectionSet);
end;

procedure TFHIRGraphQLEngine.processObject(source: TFHIRObject; target: TGraphQLObjectValue; selection: TAdvList<TGraphQLSelection>);
var
  sel : TGraphQLSelection;
  new : TGraphQLObjectValue;
  prop : TFHIRProperty;
  arg : TGraphQLArgument;
  value : TFHIRObject;
begin
  for sel in selection do
  begin
    if (sel.Field <> nil) then
    begin
      prop := source.getPropertyValue(sel.field.Name);
      try
        if prop.hasValue then
        begin
          arg := TGraphQLArgument.Create;
          try
            arg.Name := sel.field.Alias;
            arg.list := prop.IsList;
            target.Fields.Add(arg.Link);
            for value in prop.Values do
            begin
              if value.isPrimitive then
                processPrimitive(arg, value)
              else
              begin
                if sel.field.SelectionSet.Empty then
                  raise EGraphQLException.Create('No Fields selected on a complex object');
                new := TGraphQLObjectValue.Create;
                try
                  arg.Values.Add(new.Link);
                  processObject(value, new, sel.field.SelectionSet);
                finally
                  new.Free;
                end;
              end;
            end;
          finally
            arg.Free;
          end;
        end;
      finally
        prop.Free;
      end;
    end;
  end;
end;

procedure TFHIRGraphQLEngine.processPrimitive(arg : TGraphQLArgument; value: TFHIRObject);
var
  s : String;
begin
  s := value.fhirType;
  if (s = 'integer') or (s = 'decimal') or (s = 'unsignedInt') or (s = 'positiveInt') then
    arg.values.add(TGraphQLNumberValue.Create(value.primitiveValue))
  else if (s = 'boolean') then
    arg.values.add(TGraphQLNameValue.Create(value.primitiveValue))
  else
    arg.values.add(TGraphQLStringValue.Create(value.primitiveValue));
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
    SetLength(result[i].Values, 3);
    s := test.getAttribute('source');
    result[i].Values[0] := s;
    s := test.getAttribute('output');
    result[i].Values[1] := s;
    s := test.getAttribute('context');
    result[i].Values[2] := s;
    inc(i);
    test := TMsXmlParser.NextSibling(test);
  end;
end;

{ TFHIRGraphQLTests }

procedure TFHIRGraphQLTests.TestCase(source, output, context: String);
var
  parts : TArray<String>;
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
begin
  parts := context.Split(['/']);
  if length(parts) <> 3 then
    raise Exception.Create('not done yet '+source+' '+output+' '+context);
  gql := TFHIRGraphQLEngine.Create;
  try
    gql.FFocus := TFHIRXmlParser.ParseFile(nil, 'en', 'C:\work\org.hl7.fhir\build\publish\'+parts[0].ToLower+'-'+parts[1].ToLower+'.xml');
    gql.FDocument := TGraphQLParser.parseFile('C:\work\org.hl7.fhir\build\tests\graphql\'+source);
    gql.execute;
    str := TStringBuilder.create;
    try
      gql.output.write(str, 0);
      StringToFile(str.ToString, 'C:\work\org.hl7.fhir\build\tests\graphql\'+output+'.out', TEncoding.UTF8);
    finally
      str.free;
    end;
  finally
    gql.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRGraphQLTests);
end.
