unit FHIRPathTests;

interface

uses
  SysUtils, classes,
  ActiveX, ComObj, Variants, StringSupport, AdvGenerics,
  FHIRTestWorker, FHIRResources, FHIRBase, FHIRParser, FHIRPath, FHIRTypes,
  MsXml, MsXmlParser, DUnitX.TestFramework;

Type
  [TextFixture]
  TFHIRPathTests = Class (TObject)
  Private
  Published
  End;

  FHIRPathTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TFHIRPathTest = Class (TObject)
  private
    engine : TFHIRExpressionEngine;

    function findTest(path : String) : IXMLDOMElement;
  Published
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

    [FHIRPathTestCase]
    procedure FHIRPathTest(Name : String);
  End;

implementation

var
  tests : IXMLDOMDocument2;

{ FHIRPathTestCaseAttribute }

function FHIRPathTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  group, test : IXMLDOMElement;
  i, g, t : integer;
  gn, s : String;
  v : OleVariant;
begin
  tests := TMsXmlParser.Parse('C:\work\fluentpath\tests\r4\tests-fhir-r4.xml');

  group := TMsXmlParser.FirstChild(tests.documentElement);
  i := 0;
  g := 0;
  while group <> nil do
  begin
    inc(g);
    test := TMsXmlParser.FirstChild(group);
    gn := group.getAttribute('name');
    t := 0;
    while test <> nil do
    begin
      inc(t);
      s := VarToStrDef(test.getAttribute('name'), '');
      if (s = '') then
        s := gn+' '+inttostr(t);

      s := s + ' ('+inttostr(g)+'.'+inttostr(t)+')';

      SetLength(result, i+1);
      result[i].Name := s;
      SetLength(result[i].Values, 1);
      result[i].Values[0] := s;
      inc(i);
      test := TMsXmlParser.NextSibling(test);
    end;
    group := TMsXmlParser.NextSibling(group);
  end;
end;


{ TFHIRPathTest }

function TFHIRPathTest.findTest(path: String): IXMLDOMElement;
var
  l, r : String;
  gs, ts, g, t : integer;
var
  group, test : IXMLDOMElement;
begin
  result := nil;
  StringSplit(path, '(', l, r);
  StringSplit(r, ')', l, r);
  StringSplit(l, '.', l, r);
  gs := StrToInt(l);
  ts := StrToInt(r);

  group := TMsXmlParser.FirstChild(tests.documentElement);
  g := 0;
  while group <> nil do
  begin
    inc(g);
    test := TMsXmlParser.FirstChild(group);
    t := 0;
    while test <> nil do
    begin
      inc(t);
      if (g = gs) and (t = ts) then
        result := test;
      test := TMsXmlParser.NextSibling(test);
    end;
    group := TMsXmlParser.NextSibling(group);
  end;
end;

procedure TFHIRPathTest.FHIRPathTest(Name: String);
var
  test : IXMLDOMElement;
  input, expression, s, tn : String;
  fail, ok : boolean;
  res : TFHIRResource;
  outcome : TFHIRSelectionList;
  node : TFHIRExpressionNode;
  p : TFHIRXmlParser;
  f :  TFileStream;
  expected : TInterfaceList;
  i : integer;
begin
  test := findTest(name);
  input := test.getAttribute('inputfile');
  expression := TMsXmlParser.NamedChild(test, 'expression').text;
  fail := TMsXmlParser.NamedChild(test, 'expression').getAttribute('invalid') = 'true';
  res := nil;

  outcome := nil;
  try
    node := engine.parse(expression);
    try
      try
        if (input = '') then
          engine.check(nil, '', '', '', node, false)
        else
        begin
          p := TFHIRXmlParser.create(TTestingWorkerContext.Use, 'en');
          try
            f := TFileStream.Create(IncludeTrailingBackslash('C:\\work\\org.hl7.fhir\\build\\publish')+input, fmOpenRead);
            try
              p.source := f;
              p.parse;
              res := p.resource.Link;
            finally
              f.Free;
            end;
          finally
            p.Free;
          end;

          engine.check(nil, res.fhirType, res.fhirType, res.fhirType, node, false).free;
        end;
        outcome := engine.evaluate(nil, res, res, node);
        Assert.IsTrue(not fail, StringFormat('Expected exception parsing %s', [expression]));
      except
        on e:Exception do
        begin
          Assert.IsTrue(fail, StringFormat('Unexpected exception parsing %s: %s', [expression, e.Message]));
          outcome := TFHIRSelectionList.create;
        end;
      end;
      if (TMsXmlParser.GetAttribute(test, 'predicate') = 'true') then
      begin
        ok := engine.convertToBoolean(outcome);
        outcome.clear();
        outcome.add(TFHIRBoolean.create(ok));
      end;
      s := engine.UseLog;
      if (s <> '') then
        writeln(s);

      expected := TInterfaceList.Create;
      try
        TMsXmlParser.getNamedChildrenWithWildcard(test, 'output', expected);
        Assert.isTrue(outcome.count = expected.count, StringFormat('Expected %d objects but found %d', [expected.count, outcome.count]));
        for i := 0 to outcome.count- 1 do
        begin
          tn := IXMLDomElement(expected[i]).getAttribute('type');
          if (tn <> '') then
            Assert.isTrue(tn = outcome[i].value.fhirType(), StringFormat('Outcome %d: Type should be %s but was %s', [i, tn, outcome[i].value.fhirType()]));
          s := IXMLDomElement(expected[i]).Text;
          if (s <> '') then
          begin
            Assert.isTrue(outcome[i].value is TFHIRPrimitiveType, StringFormat('Outcome %d: Value should be a primitive type but was %s', [i, outcome[i].value.fhirType()]));
            Assert.isTrue(s = TFHIRPrimitiveType(outcome[i].value).StringValue, StringFormat('Outcome %d: Value should be %s but was %s', [i, s, outcome[i].value.toString()]));
          end;
        end;
      finally
        expected.Free;
      end;
    finally
      node.Free;
    end;
  finally
    res.free;
    outcome.free;
  end;
end;
(*


    } catch (Exception e) {
    }


*)


(*
var
  test, target, patch, error, patched, outcome : IXMLDOMElement;
  s : String;
  ok : boolean;
begin
  test := TMsXmlParser.FirstChild(tests.documentElement);
  while test <> nil do
  begin
    if (test.nodeName = 'case') and (name = test.getAttribute('name')) then
    begin
      target := TMsXmlParser.NamedChild(test, 'target');
      patch := TMsXmlParser.NamedChild(test, 'patch');
      error := TMsXmlParser.NamedChild(test, 'error');
      patched := TMsXmlParser.NamedChild(test, 'patched');
      if patched <> nil then
        patched := TMsXmlParser.FirstChild(patched);

      if (error <> nil) then
        Assert.WillRaiseWithMessage(
          procedure begin
            //
          end, Exception, error.text)
      else
      begin
        // ok :=
        Assert.IsTrue(ok, s);
      end;
    end;
    test := TMsXmlParser.NextSibling(test);
  end;
end;
  *)
procedure TFHIRPathTest.setup;
begin
  engine := TFHIRExpressionEngine.Create(TTestingWorkerContext.Use);
end;

procedure TFHIRPathTest.teardown;
begin
  engine.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRPathTest);
  TDUnitX.RegisterTestFixture(TFHIRPathTests);
end.
