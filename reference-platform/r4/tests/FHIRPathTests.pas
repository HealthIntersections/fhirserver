unit FHIRPathTests;

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
  ActiveX, ComObj, Variants, StringSupport, FileSupport, AdvGenerics,
  FHIRTestWorker, FHIRResources, FHIRBase, FHIRParser, FHIRPath, FHIRTypes,
  MXML, DUnitX.TestFramework;

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
    tests : TMXmlElement;
    function findTest(path : String) : TMXmlElement;
  Published
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

    [FHIRPathTestCase]
    procedure FHIRPathTest(Name : String);
  End;

implementation


{ FHIRPathTestCaseAttribute }

function FHIRPathTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : TMXmlElement;
  group, test : TMXmlElement;
  i, g, t : integer;
  gn, s : String;
begin
  tests := TMXmlParser.ParseFile('C:\work\fluentpath\tests\r4\tests-fhir-r4.xml', [xpDropWhitespace, xpDropComments]);
  try
    group := tests.document.first;
    i := 0;
    g := 0;
    while group <> nil do
    begin
      inc(g);
      test := group.first;
      gn := group.attribute['name'];
      t := 0;
      while test <> nil do
      begin
        inc(t);
        s := VarToStrDef(test.attribute['name'], '');
        if (s = '') then
          s := gn+' '+inttostr(t);

        s := s + ' ('+inttostr(g)+'.'+inttostr(t)+')';

        SetLength(result, i+1);
        result[i].Name := s;
        SetLength(result[i].Values, 1);
        result[i].Values[0] := s;
        inc(i);
        test := test.Next;
      end;
      group := group.Next;
    end;
  finally
    tests.free;
  end;
end;


{ TFHIRPathTest }

function TFHIRPathTest.findTest(path: String): TMXmlElement;
var
  l, r : String;
  gs, ts, g, t : integer;
var
  group, test : TMXmlElement;
begin
  result := nil;
  StringSplit(path, '(', l, r);
  StringSplit(r, ')', l, r);
  StringSplit(l, '.', l, r);
  gs := StrToInt(l);
  ts := StrToInt(r);

  group := tests.document.first;
  g := 0;
  while group <> nil do
  begin
    inc(g);
    test := group.first;
    t := 0;
    while test <> nil do
    begin
      inc(t);
      if (g = gs) and (t = ts) then
        result := test;
      test := test.Next;
    end;
    group := group.Next;
  end;
end;

procedure TFHIRPathTest.FHIRPathTest(Name: String);
var
  test : TMXmlElement;
  input, expression, s, tn : String;
  fail, ok : boolean;
  res : TFHIRResource;
  outcome : TFHIRSelectionList;
  node : TFHIRPathExpressionNode;
  p : TFHIRXmlParser;
  f :  TFileStream;
  expected : TAdvList<TMXmlElement>;
  i : integer;
begin
  test := findTest(name);
  input := test.attribute['inputfile'];
  expression := test.element('expression').text;
  fail := test.element('expression').attribute['invalid'] = 'true';
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
            f := TFileStream.Create(Path([GBasePath, 'build' ,'publish', input]), fmOpenRead);
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
      if (test.attribute['predicate'] = 'true') then
      begin
        ok := engine.convertToBoolean(outcome);
        outcome.clear();
        outcome.add(TFHIRBoolean.create(ok));
      end;
      s := engine.UseLog;
      if (s <> '') then
        writeln(s);

      expected := TAdvList<TMXmlElement>.Create;
      try
        test.listElements('output', expected);
        Assert.isTrue(outcome.count = expected.count, StringFormat('Expected %d objects but found %d', [expected.count, outcome.count]));
        for i := 0 to outcome.count- 1 do
        begin
          tn := TMXmlElement(expected[i]).attribute['type'];
          if (tn <> '') then
            Assert.isTrue(tn = outcome[i].value.fhirType(), StringFormat('Outcome %d: Type should be %s but was %s', [i, tn, outcome[i].value.fhirType()]));
          s := TMXmlElement(expected[i]).Text;
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

procedure TFHIRPathTest.setup;
begin
  tests := TMXmlParser.ParseFile('C:\work\fluentpath\tests\r4\tests-fhir-r4.xml', [xpDropWhitespace, xpDropComments]);
  engine := TFHIRPathEngine.Create(TTestingWorkerContext.Use);
end;

procedure TFHIRPathTest.teardown;
begin
  tests.Free;
  engine.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRPathTest);
  TDUnitX.RegisterTestFixture(TFHIRPathTests);
end.
