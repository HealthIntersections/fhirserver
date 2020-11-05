unit FHIR.R4.Tests.PathEngine;

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
  SysUtils, classes,
  {$IFDEF WINDOWS}
  ActiveX, ComObj, Variants,
  {$ENDIF}
  FHIR.Support.Testing,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Web.Parsers,
  FHIR.Base.Objects, FHIR.R4.Parser, FHIR.R4.Xml,
  FHIR.R4.Tests.Worker, FHIR.R4.Resources, FHIR.R4.PathEngine, FHIR.R4.Types, FHIR.R4.PathNode,
  FHIR.Ucum.Services,
  FHIR.Support.MXml, FHIR.Support.Tests;

Type
  TFHIRPathTest4 = Class (TFslTestSuiteCase)
  private
    engine : TFHIRPathEngine;
    resources : TFslMap<TFHIRResource>;
    ucum : TUcumServices;
    function findTest(path : String) : TMXmlElement;
  Public
    Procedure SetUp; override;
    procedure TearDown; override;
    procedure TestCase(Name : String); override;
  End;

  TFHIRPathTest4Suite  = class (TFslTestSuite)
  public
    constructor Create; override;
  end;

procedure registerTests;

implementation

var
  gtests : TMXmlElement;

{ TFHIRPathTest4 }

procedure TFHIRPathTest4.setup;
begin
  resources := TFslMap<TFHIRResource>.create('resources');
  if gTests = nil then
    gTests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r5', 'fhirpath', 'tests-fhir-r4.xml']), [xpDropWhitespace, xpDropComments]);
  ucum := TUcumServices.Create;
  ucum.Import('ucum-essence.xml');
  engine := TFHIRPathEngine.Create(TTestingWorkerContext4.Use, TUcumServiceImplementation.Create(ucum.link));
end;

procedure TFHIRPathTest4.TearDown;
begin
  ucum.free;
  engine.Free;
  resources.Free;
end;

function TFHIRPathTest4.findTest(path: String): TMXmlElement;
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

  group := gTests.document.first;
  g := 0;
  while group <> nil do
  begin
    inc(g);
    if (g = gs) then
    begin
      test := group.first;
      t := 0;
      while test <> nil do
      begin
        inc(t);
        if (g = gs) and (t = ts) then
          result := test;
        test := test.Next;
      end;
    end;
    group := group.Next;
  end;
end;

procedure TFHIRPathTest4.TestCase(Name: String);
var
  test : TMXmlElement;
  input, expression, s, tn : String;
  fail, ok : boolean;
  res : TFHIRResource;
  outcome : TFHIRSelectionList;
  node : TFHIRPathExpressionNode;
  p : TFHIRXmlParser;
  f :  TFileStream;
  expected : TFslList<TMXmlElement>;
  i, j : integer;
  found : boolean;
begin
  test := findTest(name);
  input := test.attribute['inputfile'];
  expression := test.element('expression').text;
  fail :=  test.element('expression').HasAttribute['invalid']; // test.element('expression').attribute['invalid'] = 'true';
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
          if not resources.TryGetValue(input, res) then
          begin
            p := TFHIRXmlParser.create(TTestingWorkerContext4.Use, THTTPLanguages.create('en'));
            try
              f := TFileStream.Create(TestSettings.fhirTestFile(['r5', 'examples', input]), fmOpenRead);
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
            resources.Add(input, res.link);
          end
          else
            res.Link;

          engine.check(nil, res.fhirType, res.fhirType, res.fhirType, node, false).free;
        end;
        outcome := engine.evaluate(nil, res, res, node);
        ok := true;
      except
        on e:Exception do
        begin
          ok := false;
          assertTrue(fail, StringFormat('Unexpected exception parsing %s: %s', [expression, e.Message]));
          outcome := TFHIRSelectionList.create;
        end;
      end;
      if ok then
        assertTrue(not fail, StringFormat('Expected exception parsing %s', [expression]));
      if (test.attribute['predicate'] = 'true') then
      begin
        ok := engine.convertToBoolean(outcome);
        outcome.clear();
        outcome.add(TFHIRBoolean.create(ok));
      end;
      s := engine.UseLog;
      if (s <> '') then
        writeln(s);

      expected := TFslList<TMXmlElement>.Create;
      try
        test.listElements('output', expected);
        assertTrue(outcome.count = expected.count, StringFormat('Expected %d objects but found %d for %s', [expected.count, outcome.count, expression]));
        if test.attribute['ordered'] <> 'false' then
        begin
          for i := 0 to outcome.count- 1 do
          begin
            tn := TMXmlElement(expected[i]).attribute['type'];
            if (tn <> '') then
              assertTrue(tn = outcome[i].value.fhirType(), StringFormat('Outcome %d: Type should be %s but was %s for %s', [i, tn, outcome[i].value.fhirType(), expression]));
            s := TMXmlElement(expected[i]).Text;
            if (s <> '') then
            begin
              assertTrue((outcome[i].value is TFHIRPrimitiveType) or (outcome[i].value is TFhirQuantity), StringFormat('Outcome %d: Value should be a primitive type but was %s for %s', [i, outcome[i].value.fhirType(), expression]));
              if outcome[i].value is TFhirQuantity then
                assertTrue(s = engine.convertToString(outcome[i].value), StringFormat('Outcome %d: Value should be %s but was %s for %s', [i, s, outcome[i].value.toString(), expression]))
              else
                assertTrue(s = TFHIRPrimitiveType(outcome[i].value).StringValue, StringFormat('Outcome %d: Value should be %s but was %s for %s', [i, s, outcome[i].value.toString(), expression]));
            end;
          end;
        end
        else
        begin
          for i := 0 to outcome.count - 1 do
          begin
            tn := outcome[i].value.fhirType();
            if outcome[i].value is TFhirQuantity then
              s := engine.convertToString(outcome[i].value)
            else
              s := TFHIRPrimitiveType(outcome[i].value).StringValue;
            found := false;
            for j := 0 to expected.Count - 1 do
            begin
              if ((TMXmlElement(expected[j]).attribute['type'] = '') or (TMXmlElement(expected[j]).attribute['type'] = tn)) and
                ((TMXmlElement(expected[j]).Text = '') or (TMXmlElement(expected[j]).Text = s)) then
                found := true;
            end;
            assertTrue(found, StringFormat('Outcome %d: Value %s of type %s not expected for %s', [i, s, tn, expression]));
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

{ TFHIRPathTest4Suite }

constructor TFHIRPathTest4Suite.Create;
var
  group, test : TMXmlElement;
  g, t : integer;
  gn, s : String;
begin
  inherited create;
  if gTests = nil then
    gTests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r4', 'fhirpath', 'tests-fhir-r4.xml']), [xpDropWhitespace, xpDropComments]);
  group := gtests.document.first;
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
      s := test.attribute['name'];
      if (s = '') then
        s := gn+' '+inttostr(t);

      s := s + ' ('+inttostr(g)+'.'+inttostr(t)+')';
      AddTest(TFHIRPathTest4.Create(s));
      test := test.Next;
    end;
    group := group.Next;
  end;
end;

procedure registerTests;
begin
  RegisterTest('R4', TFHIRPathTest4Suite.Create);
end;

initialization
finalization
  gTests.Free;
end.

