unit FHIR.R3.Tests.PathEngine;

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
  SysUtils, classes,
  ActiveX, ComObj, Variants, fsl_utilities, 
  fhir_objects, FHIR.Version.Parser,
  FHIR.R3.Tests.Worker, fhir3_resources, fhir3_pathnode, fhir3_pathengine, fhir3_types,
  fsl_msxml, DUnitX.TestFramework;

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
    engine : TFHIRPathEngine;

    function findTest(path : String) : IXMLDOMElement;
  Published
    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [FHIRPathTestCase]
    procedure FHIRPathTest(Name : String);
  protected
    function sizeInBytesV : cardinal; override;
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
  tests := TMsXmlParser.Parse('C:\work\fluentpath\tests\stu3\tests-fhir-r3.xml');

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
  node : TFHIRPathExpressionNode;
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
          p := TFHIRXmlParser.create(TTestingWorkerContext.Use, THTTPLanguages.create('en'));
          try
            f := TFileStream.Create(IncludeTrailingBackslash('C:\\work\\org.hl7.fhir\\build\\publish')+input, fmOpenRead);
            try
              p.source := f;
              p.parse;
              res := p.resource.Link as TFHIRResource;
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
  engine := TFHIRPathEngine.Create(TTestingWorkerContext.Use, nil);
end;

procedure TFHIRPathTest.TearDown;
begin
  engine.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRPathTest);
  TDUnitX.RegisterTestFixture(TFHIRPathTests);
function TFHIRPathTest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, engine.sizeInBytes);
end;

end.
