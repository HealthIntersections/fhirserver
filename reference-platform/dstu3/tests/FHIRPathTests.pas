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

{$IFNDEF FHIR3}
// This is the dstu3 version of the FHIR code
{$ENDIF}


interface

uses
//  SysUtils, Classes, Math, RegExpr, Generics.Collections, Character,
//  StringSupport, TextUtilities, SystemSupport, MathSupport,
//  AdvObjects, AdvGenerics, DecimalSupport, DateAndTime,
//  XmlBuilder,
//
//  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRProfileUtilities, FHIRConstants,
//  FHIRParser;
  SysUtils, Classes,
  StringSupport,
  FHIRBase, FHIRTypes, FHIRResources, FHIRConstants, FHIRParser,
  FHIRContext,
  FHIRPath, FHIRTestWorker,
  DUnitX.TestFramework;


Type
  [TextFixture]
  TFHIRPathTests = class (TObject)
  private
    FServices : TWorkerContext;
//    FPatient, FObservation, FValueset, FQuestionnaire : TFhirResource;

//    procedure load(name : string; var resource : TFhirResource);
//    Function patient : TFhirResource;
//    Function observation : TFhirResource;
//    Function valueset : TFhirResource;
//    Function questionnaire : TFhirResource;
//
//    function FHIRPathTests : String;
//    procedure FHIRPathTest(b : TStringBuilder; s : String);
//    procedure test(resource : TFhirResource; expression : string; count : integer; typeName : String);
//    procedure testBoolean(resource : TFhirResource; expression : string; value : boolean);
//    procedure testWrong(resource : TFhirResource; expression : string; tested : boolean);
//    procedure testObject(resource : TFhirResource; context : String; focus : TFHIRBase; expression : string; count : integer; typeName : String);

  public

    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

//    [TestCase] procedure testSimple();
//    [TestCase] procedure testSimpleNone();
//    [TestCase] procedure testSimpleDoubleQuotes();
//    [TestCase] procedure testSimpleFail();
//    [TestCase] procedure testSimpleWithContext();
//    [TestCase] procedure testSimpleWithWrongContext();
//    [TestCase] procedure testPolymorphismA();
//    [TestCase] procedure testPolymorphismB();
//    [TestCase] procedure testPolymorphismIsA();
//    [TestCase] procedure testPolymorphismIsB();
//    [TestCase] procedure testPolymorphismAsA();
//    [TestCase] procedure testPolymorphismAsB();
//    [TestCase] procedure testPolymorphismAsC();
//    [TestCase] procedure testDollarThis1();
//    [TestCase] procedure testDollarThis2();
//    [TestCase] procedure testDollarOrderAllowed();
//    [TestCase] procedure testDollarOrderAllowedA();
//    [TestCase] procedure testDollarOrderNotAllowed();
//    [TestCase] procedure testLiteralTrue();
//    [TestCase] procedure testLiteralFalse();
//    [TestCase] procedure testLiteralString();
//    [TestCase] procedure testLiteralInteger();
//    [TestCase] procedure testLiteralDecimal();
//    [TestCase] procedure testLiteralDate();
//    [TestCase] procedure testLiteralUnicode();
//    [TestCase] procedure testLiteralEmptyCollection();
//    [TestCase] procedure testExpressions();
//    [TestCase] procedure testEmpty();
//    [TestCase] procedure testNot();
//    [TestCase] procedure testAll();
//    [TestCase] procedure testSubSetOf();
//    [TestCase] procedure testSuperSetOf();
//    [TestCase] procedure testDistinct();
//    [TestCase] procedure testCount();
//    [TestCase] procedure testWhere();
//    [TestCase] procedure testSelect();
//    [TestCase] procedure testRepeat();
//    [TestCase] procedure testIndexer();
//    [TestCase] procedure testSingle();
//    [TestCase] procedure testFirstLast();
//    [TestCase] procedure testTail();
//    [TestCase] procedure testSkip();
//    [TestCase] procedure testTake();
//    [TestCase] procedure testIif();
//    [TestCase] procedure testToInteger();
//    [TestCase] procedure testToDecimal();
//    [TestCase] procedure testToString();
//    [TestCase] procedure testSubstring();
//    [TestCase] procedure testStartsWith();
//    [TestCase] procedure testEndsWith();
//    [TestCase] procedure testContainsString();
//    [TestCase] procedure testLength();
//    [TestCase] procedure testTrace();
//    [TestCase] procedure testToday();
//    [TestCase] procedure testNow();
//    [TestCase] procedure testEquality();
//    [TestCase] procedure testNEquality();
//    [TestCase] procedure testEquivalent();
//    [TestCase] procedure testNotEquivalent();
//    [TestCase] procedure testLessThan();
//    [TestCase] procedure testLessOrEqual();
//    [TestCase] procedure testGreatorOrEqual();
//    [TestCase] procedure testGreatorThan();
//    [TestCase] procedure testUnion();
//    [TestCase] procedure testIn();
//    [TestCase] procedure testContainsCollection();
//    [TestCase] procedure testBooleanLogicAnd();
//    [TestCase] procedure testBooleanLogicOr();
//    [TestCase] procedure testBooleanLogicXOr();
//    [TestCase] procedure testBooleanImplies();
//    [TestCase] procedure testPlus();
//    [TestCase] procedure testMinus();
//    [TestCase] procedure testMultiply();
//    [TestCase] procedure testDivide();
//    [TestCase] procedure testDiv();
//    [TestCase] procedure testMod();
//    [TestCase] procedure testPrecedence();
//    [TestCase] procedure testVariables();
//    [TestCase] procedure testExtension();
//    [TestCase] procedure testDoubleEntryPoint();
//    [TestCase] procedure testExtensionContext();
//    [TestCase] procedure testDataElement();
  end;

implementation

{ TFHIRPathTests }

//procedure TFHIRPathTests.FHIRPathTest(b : TStringBuilder; s: String);
//var
//  l, r : String;
//  expr : TFHIRExpressionNode;
//  engine : TFHIRExpressionEngine;
//begin
//  StringSplit(s, ':', l, r);
//  r := r.trim;
//  if (r <> 'n/a') then
//  begin
//    engine := TFHIRExpressionEngine.create(FServices.link);
//    try
//      expr := engine.parse(r);
//      try
//        l := expr.Canonical;
//        if (l.replace(' ', '') <> r.replace(' ', '').replace('''', '"')) then
//        begin
//          b.Append('i: '+r);
//          b.Append(#13#10);
//          b.Append('o: '+l);
//          b.Append(#13#10);
//          b.Append(#13#10);
//        end;
//      finally
//        expr.free;
//      end;
//    finally
//      engine.free;
//    end;
//  end;
//end;
//
//function TFHIRPathTests.FHIRPathTests : String;
//begin
//  testSimple();
//  testSimpleNone();
//  testSimpleDoubleQuotes();
//  testSimpleFail();
//  testSimpleWithContext();
//  testSimpleWithWrongContext();
//  testPolymorphismA();
//  testPolymorphismB();
//  testPolymorphismIsA();
//  testPolymorphismIsB();
//  testPolymorphismAsA();
//  testPolymorphismAsB();
//  testPolymorphismAsC();
//  testDollarThis1();
//  testDollarThis2();
//  testDollarOrderAllowed();
//  testDollarOrderAllowedA();
//  testDollarOrderNotAllowed();
//  testLiteralTrue();
//  testLiteralFalse();
//  testLiteralString();
//  testLiteralInteger();
//  testLiteralDecimal();
//  testLiteralDate();
//  testLiteralUnicode();
//  testLiteralEmptyCollection();
//  testExpressions();
//  testEmpty();
//  testNot();
//  testAll();
//  testSubSetOf();
//  testSuperSetOf();
//  testDistinct();
//  testCount();
//  testWhere();
//  testSelect();
//  testRepeat();
//  testIndexer();
//  testSingle();
//  testFirstLast();
//  testTail();
//  testSkip();
//  testTake();
//  testIif();
//  testToInteger();
//  testToDecimal();
//  testToString();
//  testSubstring();
//  testStartsWith();
//  testEndsWith();
//  testContainsString();
//  testLength();
//  testTrace();
//  testToday();
//  testNow();
//  testEquality();
//  testNEquality();
//  testEquivalent();
//  testNotEquivalent();
//  testLessThan();
//  testLessOrEqual();
//  testGreatorOrEqual();
//  testGreatorThan();
//  testUnion();
//  testIn();
//  testContainsCollection();
//  testBooleanLogicAnd();
//  testBooleanLogicOr();
//  testBooleanLogicXOr();
//  testBooleanImplies();
//  testPlus();
//  testMinus();
//  testMultiply();
//  testDivide();
//  testDiv();
//  testMod();
//  testPrecedence();
//  testVariables();
//  testExtension();
//  testExtensionContext();
//  testDataElement();
//end;
//
//procedure TFHIRPathTests.load(name : string; var resource : TFhirResource);
//var
//  json : TFHIRJsonParser;
//  f : TFileStream;
//begin
//  f := TFileStream.Create(name, fmOpenRead + fmShareDenyWrite);
//  try
//    json := TFHIRJsonParser.Create(nil, 'en');
//    try
//      json.source := f;
//      json.Parse;
//      resource := json.resource.link;
//    finally
//      json.Free;
//    end;
//  finally
//    f.Free;
//  end;
//end;
//
//function TFHIRPathTests.observation: TFhirResource;
//begin
//  if (Fobservation = nil) then
//    load('C:/work/org.hl7.fhir/build/publish/observation-example.json', fobservation);
//  result := Fobservation;
//end;
//
//function TFHIRPathTests.patient: TFhirResource;
//begin
//  if (Fpatient = nil) then
//    load('C:/work/org.hl7.fhir/build/publish/patient-example.json', FPatient);
//  result := Fpatient;
//end;
//
//function TFHIRPathTests.questionnaire: TFhirResource;
//begin
//  if (Fquestionnaire = nil) then
//    load('C:/work/org.hl7.fhir/build/publish/questionnaire-example.json', FQuestionnaire);
//  result := Fquestionnaire;
//end;
//
procedure TFHIRPathTests.setup;
begin
  FServices := TTestingWorkerContext.Use;
end;

//function TFHIRPathTests.valueset: TFhirResource;
//begin
//  if (Fvalueset = nil) then
//    load('C:/work/org.hl7.fhir/build/publish/valueset-example-expansion.json', FValueset);
//  result := Fvalueset;
//end;
//
procedure TFHIRPathTests.teardown;
begin
  FServices.Free;
  FServices := nil;
//  FPatient.Free;
//  FPatient := nil;
//  FObservation.Free;
//  FObservation := nil;
//  FValueset.Free;
//  FValueset := nil;
//  FQuestionnaire.Free;
//  FQuestionnaire := nil;
end;

//procedure TFHIRPathTests.test(resource : TFhirResource; expression : string; count : integer; typeName : String);
//var
//  fp : TFHIRExpressionEngine;
//  node : TFHIRExpressionNode;
//  s : String;
//  outcome : TFHIRBaseList;
//  b : TFHIRBase;
//  found : boolean;
//  td : TFHIRTypeDetails;
//begin
//  fp := TFHIRExpressionEngine.Create(FServices.Link);
//  try
//    node := fp.parse(expression);
//    try
//      td := fp.check(nil, CODES_TFhirResourceType[resource.ResourceType], CODES_TFhirResourceType[resource.ResourceType], '', node, false);
//      try
//        Assert.isFalse(td.hasNoTypes);
//      finally
//        td.free;
//      end;
//      outcome := fp.evaluate(nil, resource, node);
//      try
//        s := fp.UseLog;
//        if s <> '' then
//            writeln(s);
//        Assert.AreEqual(outcome.count, count, StringFormat('Expected %d objects but found %d', [count, outcome.count]));
//        for b in outcome do
//          Assert.AreEqual(b.FhirType, typeName, StringFormat('Object type %s not ok, should be %s', [b.FhirType, typeName]));
//      finally
//         outcome.Free;
//      end;
//    finally
//      node.Free;
//    end;
//  finally
//    fp.Free;
//  end;
//end;
//
//procedure TFHIRPathTests.testBoolean(resource : TFhirResource; expression : string; value : boolean);
//var
//  fp : TFHIRExpressionEngine;
//  node : TFHIRExpressionNode;
//  s : String;
//  outcome : TFHIRBaseList;
//  td : TFHIRTypeDetails;
//begin
//  fp := TFHIRExpressionEngine.Create(FServices.Link);
//  try
//    node := fp.parse(expression);
//    try
//      td := fp.check(nil, CODES_TFhirResourceType[resource.ResourceType], CODES_TFhirResourceType[resource.ResourceType], '', node, false);
//      try
//        Assert.isFalse(td.hasNoTypes);
//      finally
//        td.free;
//      end;
//      outcome := fp.evaluate(nil, resource, node);
//      try
//        s := fp.UseLog;
//        if s <> '' then
//            writeln(s);
//        Assert.AreEqual(fp.convertToBoolean(outcome), value, 'Wrong boolean outcome');
//      finally
//         outcome.Free;
//      end;
//    finally
//      node.Free;
//    end;
//  finally
//    fp.Free;
//  end;
//end;
//
//procedure TFHIRPathTests.testWrong(resource : TFhirResource; expression : string; tested : boolean);
//var
//  fp : TFHIRExpressionEngine;
//  node : TFHIRExpressionNode;
//  s : String;
//  ok : boolean;
//begin
//  fp := TFHIRExpressionEngine.Create(FServices.Link);
//  try
//    node := fp.parse(expression);
//    try
//      Assert.WillRaise(
//        procedure begin
//          fp.check(nil, CODES_TFhirResourceType[resource.ResourceType], CODES_TFhirResourceType[resource.ResourceType], '', node, false).free;
//          fp.evaluate(nil, resource, node);
//        end, EFHIRPath);
//    finally
//      node.Free;
//    end;
//  finally
//    fp.Free;
//  end;
//end;
//
//
//procedure TFHIRPathTests.testSimple();
//begin
//  test(patient(), 'name.given', 3, 'string');
//end;
//
//procedure TFHIRPathTests.testSimpleNone();
//begin
//  test(patient(), 'name.period', 0, '');
//end;
//
//procedure TFHIRPathTests.testSimpleDoubleQuotes();
//begin
//  test(patient(), 'name."given"', 3, 'string');
//end;
//
//procedure TFHIRPathTests.testSimpleFail();
//begin
//  testWrong(patient(), 'name.given1', true);
//end;
//
//procedure TFHIRPathTests.testSimpleWithContext();
//begin
//  test(patient(), 'Patient.name.given', 3, 'string');
//end;
//
//procedure TFHIRPathTests.testSimpleWithWrongContext();
//begin
//  testWrong(patient(), 'Encounter.name.given', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismA();
//begin
//  test(observation(), 'Observation.value.unit', 1, 'string');
//end;
//
//procedure TFHIRPathTests.testPolymorphismB();
//begin
//  testWrong(observation(), 'Observation.valueQuantity.unit', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismIsA();
//begin
//  testBoolean(observation(), 'Observation.value.is(Quantity)', true);
//  testBoolean(observation(), 'Observation.value is Quantity', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismIsB();
//begin
//  testBoolean(observation(), 'Observation.value.is(Period).not()', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismAsA();
//begin
//  testBoolean(observation(), 'Observation.value.as(Quantity).unit', true);
//  testBoolean(observation(), '(Observation.value as Quantity).unit', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismAsB();
//begin
//  testWrong(observation(), '(Observation.value as Period).unit', true);
//end;
//
//procedure TFHIRPathTests.testPolymorphismAsC();
//begin
//  test(observation(), 'Observation.value.as(Period).start', 0, '');
//end;
//
//procedure TFHIRPathTests.testDollarThis1();
//begin
//  test(patient(), 'Patient.name.given.where(substring($this.length()-3) = ''out'')', 0, '');
//end;
//
//procedure TFHIRPathTests.testDollarThis2();
//begin
//  test(patient(), 'Patient.name.given.where(substring($this.length()-3) = ''ter'')', 1, 'string');
//end;
//
//procedure TFHIRPathTests.testDoubleEntryPoint;
//begin
//  testBoolean(patient(), '(Patient.name | Patient.address).count() = 3', true);
//end;
//
//procedure TFHIRPathTests.testExtensionContext();
//var
//  p : TFHIRPatient;
//  ext : TFhirExtension;
//begin
//  p := patient().Clone as TFhirPatient;
//  try
//    ext := p.extensionList.Append;
//    ext.url := 'http://something';
//    ext.value := TFhirQuantity.Create;
//    TFhirQuantity(ext.value).unit_ := 'g';
//    testObject(p, 'Extension.valueQuantity', ext.value, 'code.empty() or system', 1, 'boolean');
//  finally
//    p.Free;
//  end;
//end;
//
//
//procedure TFHIRPathTests.testDollarOrderAllowed();
//begin
//  test(patient(), 'Patient.name.skip(1).given', 1, 'string');
//end;
//
//procedure TFHIRPathTests.testDollarOrderAllowedA();
//begin
//  test(patient(), 'Patient.name.skip(3).given', 0, '');
//end;
//
//procedure TFHIRPathTests.testDollarOrderNotAllowed();
//begin
//  testWrong(patient(), 'Patient.children().skip(1)', true);
//end;
//
//procedure TFHIRPathTests.testLiteralTrue();
//begin
//  testBoolean(patient(), 'Patient.name.exists() = true', true);
//end;
//
//procedure TFHIRPathTests.testLiteralFalse();
//begin
//  testBoolean(patient(), 'Patient.name.empty() = false', true);
//end;
//
//procedure TFHIRPathTests.testLiteralString();
//begin
//  testBoolean(patient(), 'Patient.name.given.first() = ''Peter''', true);
//end;
//
//procedure TFHIRPathTests.testLiteralInteger();
//begin
//  testBoolean(patient(), '-3 != 3', true);
//  testBoolean(patient(), 'Patient.name.given.count() = 3', true);
//  testBoolean(patient(), 'Patient.name.given.count() > -3', true);
//  testBoolean(patient(), 'Patient.name.given.count() != 0', true);
//  testBoolean(patient(), '1 < 2', true);
//  testBoolean(patient(), '1 < -2', false);
//  testBoolean(patient(), '+1 < +2', true);
//  testBoolean(patient(), '-1 < 2', true);
//end;
//
//procedure TFHIRPathTests.testLiteralDecimal();
//begin
//  testBoolean(observation(), 'Observation.value.value > 180.0', true);
//  testBoolean(observation(), 'Observation.value.value > 0.0', true);
//  testBoolean(observation(), 'Observation.value.value > 0', true);
//  testBoolean(observation(), 'Observation.value.value < 190', true);
//  testBoolean(observation(), 'Observation.value.value < ''test''', false);
//end;
//
//procedure TFHIRPathTests.testLiteralDate();
//begin
//  testBoolean(patient(), 'Patient.birthDate = @1974-12-25', true);
//  testBoolean(patient(), 'Patient.birthDate != @1974-12-25T12:34:00', true);
//  testBoolean(patient(), 'Patient.birthDate != @1974-12-25T12:34:00-10:00', true);
//  testBoolean(patient(), 'Patient.birthDate != @1974-12-25T12:34:00+10:00', true);
//  testBoolean(patient(), 'Patient.birthDate != @1974-12-25T12:34:00Z', true);
//  testBoolean(patient(), 'Patient.birthDate != @T12:14:15', true);
//  testBoolean(patient(), 'Patient.birthDate != @T12:14', true);
//end;
//
//procedure TFHIRPathTests.testLiteralUnicode();
//begin
//  testBoolean(patient(), 'Patient.name.given.first() = ''P\u0065ter''', true);
//end;
//
//procedure TFHIRPathTests.testLiteralEmptyCollection();
//begin
//  testBoolean(patient(), 'Patient.name.given != {}', true);
//end;
//
//procedure TFHIRPathTests.testExpressions();
//begin
//  testBoolean(patient(), 'Patient.name.select(given | family).distinct()', true);
//  testBoolean(patient(), 'Patient.name.given.count() = 1 + 2', true);
//end;
//
//procedure TFHIRPathTests.testEmpty();
//begin
//  testBoolean(patient(), 'Patient.name.empty().not()', true);
//  testBoolean(patient(), 'Patient.link.empty()', true);
//end;
//
//procedure TFHIRPathTests.testNot();
//begin
//  testBoolean(patient(), 'true.not() = false', true);
//  testBoolean(patient(), 'false.not() = true', true);
//  testBoolean(patient(), '(0).not() = false', true);
//  testBoolean(patient(), '(1).not() = false', true);
//  testBoolean(patient(), '(1|2).not() = false', true);
//end;
//
//procedure TFHIRPathTests.testAll();
//begin
//  testBoolean(patient(), 'Patient.name.select(given.exists()).all()', true);
//  testBoolean(patient(), 'Patient.name.select(family.exists()).all()', false);
//end;
//
//procedure TFHIRPathTests.testSubSetOf();
//begin
//  testBoolean(patient(), 'Patient.name.first().subsetOf($this.name)', true);
//  testBoolean(patient(), 'Patient.name.subsetOf($this.name.first()).not()', true);
//end;
//
//procedure TFHIRPathTests.testSuperSetOf();
//begin
//  testBoolean(patient(), 'Patient.name.first().supersetOf($this.name).not()', true);
//  testBoolean(patient(), 'Patient.name.supersetOf($this.name.first())', true);
//end;
//
//procedure TFHIRPathTests.testDataElement;
//var
//  de : TFHIRDataElement;
//  ed : TFhirElementDefinition;
//begin
//  de := TFhirDataElement.Create;
//  try
//    ed := de.elementList.Append;
//    ed.slicing := TFhirElementDefinitionSlicing.create;
//    ed.slicing.ordered := true;
//    ed.max := '1a';
//    ed.min := '1';
//    testObject(de, 'DataElement.element.max', ed.maxElement, 'empty() or ($this = ''*'') or (toInteger() >= 0)', 0, 'boolean');
//  finally
//    de.Free;
//  end;
//
//end;
//
//procedure TFHIRPathTests.testDistinct();
//begin
//  testBoolean(patient(), '(1 | 2 | 3).isDistinct()', true);
//  testBoolean(questionnaire(), 'Questionnaire.descendents().linkId.isDistinct()', true);
////  testBoolean(questionnaire(), 'Questionnaire.descendents().linkId.select(substring(0,1)).isDistinct().not()', true);
////  test(patient(), '(1 | 2 | 3).distinct()', 3, 'integer');
////  test(questionnaire(), 'Questionnaire.descendents().linkId.distinct()', 9, 'string');
////  test(questionnaire(), 'Questionnaire.descendents().linkId.select(substring(0,1)).distinct()', 2, 'string');
//end;
//
//procedure TFHIRPathTests.testCount();
//begin
//  test(patient(), 'Patient.name.count()', 1, 'integer');
//  testBoolean(patient(), 'Patient.name.count() = 2', true);
//  test(patient(), 'Patient.name.first().count()', 1, 'integer');
//  testBoolean(patient(), 'Patient.name.first().count() = 1', true);
//end;
//
//procedure TFHIRPathTests.testWhere();
//begin
//  testBoolean(patient(), 'Patient.name.count() = 2', true);
//  testBoolean(patient(), 'Patient.name.where(given = ''Jim'').count() = 1', true);
//  testBoolean(patient(), 'Patient.name.where(given = ''X'').count() = 0', true);
//  testBoolean(patient(), 'Patient.name.where($this.given = ''Jim'').count() = 1', true);
//end;
//
//procedure TFHIRPathTests.testSelect();
//begin
//  testBoolean(patient(), 'Patient.name.select(given) = ''Peter'' | ''James'' | ''Jim''', true);
//  testBoolean(patient(), 'Patient.name.select(given | family) = ''Peter'' | ''James'' | ''Chalmers'' | ''Jim''', true);
//end;
//
//procedure TFHIRPathTests.testRepeat();
//begin
//  testBoolean(valueset(), 'ValueSet.expansion.repeat(contains).count() = 10', true);
//  testBoolean(questionnaire(), 'Questionnaire.repeat(item).concept.count() = 10', true);
//  testBoolean(questionnaire(), 'Questionnaire.descendants().concept.count() = 10', true);
//  testBoolean(questionnaire(), 'Questionnaire.children().concept.count() = 2', true);
//end;
//
//procedure TFHIRPathTests.testIndexer();
//begin
//  testBoolean(patient(), 'Patient.name[0].given = ''Peter'' | ''James''', true);
//  testBoolean(patient(), 'Patient.name[1].given = ''Jim''', true);
//end;
//
//procedure TFHIRPathTests.testSingle();
//begin
//  testBoolean(patient(), 'Patient.name.first().single().exists()', true);
//  testWrong(patient(), 'Patient.name.single().exists()', true);
//end;
//
//procedure TFHIRPathTests.testFirstLast();
//begin
//  testBoolean(patient(), 'Patient.name.first().given = ''Peter'' | ''James''', true);
//  testBoolean(patient(), 'Patient.name.last().given = ''Jim''', true);
//end;
//
//procedure TFHIRPathTests.testTail();
//begin
//  testBoolean(patient(), '(0 | 1 | 2).tail() = 1 | 2', true);
//  testBoolean(patient(), 'Patient.name.tail().given = ''Jim''', true);
//end;
//
//procedure TFHIRPathTests.testSkip();
//begin
//  testBoolean(patient(), '(0 | 1 | 2).skip(1) = 1 | 2', true);
//  testBoolean(patient(), '(0 | 1 | 2).skip(2) = 2', true);
//  testBoolean(patient(), 'Patient.name.skip(1).given = ''Jim''', true);
//  testBoolean(patient(), 'Patient.name.skip(2).given.exists() = false', true);
//end;
//
//procedure TFHIRPathTests.testTake();
//begin
//  testBoolean(patient(), '(0 | 1 | 2).take(1) = 0', true);
//  testBoolean(patient(), '(0 | 1 | 2).take(2) = 0 | 1', true);
//  testBoolean(patient(), 'Patient.name.take(1).given = ''Peter'' | ''James''', true);
//  testBoolean(patient(), 'Patient.name.take(2).given = ''Peter'' | ''James'' | ''Jim''', true);
//  testBoolean(patient(), 'Patient.name.take(3).given = ''Peter'' | ''James'' | ''Jim''', true);
//  testBoolean(patient(), 'Patient.name.take(0).given.exists() = false', true);
//end;
//
//procedure TFHIRPathTests.testIif();
//begin
//  testBoolean(patient(), 'iif(Patient.name.exists(), ''named'', ''unnamed'') = ''named''', true);
//  testBoolean(patient(), 'iif(Patient.name.empty(), ''unnamed'', ''named'') = ''named''', true);
//end;
//
//procedure TFHIRPathTests.testToInteger();
//begin
//  testBoolean(patient(), '''1''.toInteger() = 1', true);
//  testBoolean(patient(), '''-1''.toInteger() = -1', true);
//  testBoolean(patient(), '''0''.toInteger() = 0', true);
//  testBoolean(patient(), '''0.0''.toInteger().empty()', true);
//  testBoolean(patient(), '''st''.toInteger().empty()', true);
//end;
//
//procedure TFHIRPathTests.testToDecimal();
//begin
//  testBoolean(patient(), '''1''.toDecimal() = 1', true);
//  testBoolean(patient(), '''-1''.toInteger() = -1', true);
//  testBoolean(patient(), '''0''.toDecimal() = 0', true);
//  testBoolean(patient(), '''0.0''.toDecimal() = 0.0', true);
//  testBoolean(patient(), '''st''.toDecimal().empty()', true);
//end;
//
//procedure TFHIRPathTests.testToString();
//begin
//  testBoolean(patient(), '1.toString() = ''1''', true);
//  testBoolean(patient(), '''-1''.toInteger() = -1', true);
//  testBoolean(patient(), '0.toString() = ''0''', true);
//  testBoolean(patient(), '0.0.toString() = ''0.0''', true);
//  testBoolean(patient(), '@2014-12-14.toString() = ''2014-12-14''', true);
//end;
//
//procedure TFHIRPathTests.testSubstring();
//begin
//  testBoolean(patient(), '''12345''.substring(2) = ''345''', true);
//  testBoolean(patient(), '''12345''.substring(2,1) = ''3''', true);
//  testBoolean(patient(), '''12345''.substring(2,5) = ''345''', true);
//  testBoolean(patient(), '''12345''.substring(25).empty()', true);
//  testBoolean(patient(), '''12345''.substring(-1).empty()', true);
//end;
//
//procedure TFHIRPathTests.testStartsWith();
//begin
//  testBoolean(patient(), '''12345''.startsWith(''2'') = false', true);
//  testBoolean(patient(), '''12345''.startsWith(''1'') = true', true);
//  testBoolean(patient(), '''12345''.startsWith(''12'') = true', true);
//  testBoolean(patient(), '''12345''.startsWith(''13'') = false', true);
//  testBoolean(patient(), '''12345''.startsWith(''12345'') = true', true);
//  testBoolean(patient(), '''12345''.startsWith(''123456'') = false', true);
//  testBoolean(patient(), '''12345''.startsWith('''') = false', true);
//end;
//
//procedure TFHIRPathTests.testEndsWith();
//begin
//  testBoolean(patient(), '''12345''.endsWith(''2'') = false', true);
//  testBoolean(patient(), '''12345''.endsWith(''5'') = true', true);
//  testBoolean(patient(), '''12345''.endsWith(''45'') = true', true);
//  testBoolean(patient(), '''12345''.endsWith(''35'') = false', true);
//  testBoolean(patient(), '''12345''.endsWith(''12345'') = true', true);
//  testBoolean(patient(), '''12345''.endsWith(''012345'') = false', true);
//  testBoolean(patient(), '''12345''.endsWith('''') = false', true);
//end;
//
//procedure TFHIRPathTests.testContainsString();
//begin
//  testBoolean(patient(), '''12345''.contains(''6'') = false', true);
//  testBoolean(patient(), '''12345''.contains(''5'') = true', true);
//  testBoolean(patient(), '''12345''.contains(''45'') = true', true);
//  testBoolean(patient(), '''12345''.contains(''35'') = false', true);
//  testBoolean(patient(), '''12345''.contains(''12345'') = true', true);
//  testBoolean(patient(), '''12345''.contains(''012345'') = false', true);
//  testBoolean(patient(), '''12345''.contains('''') = false', true);
//end;
//
//procedure TFHIRPathTests.testLength();
//begin
//  testBoolean(patient(), '''123456''.length() = 6', true);
//  testBoolean(patient(), '''12345''.length() = 5', true);
//  testBoolean(patient(), '''123''.length() = 3', true);
//  testBoolean(patient(), '''1''.length() = 1', true);
//  testBoolean(patient(), '''''.length() = 0', true);
//end;
//
//procedure TFHIRPathTests.testTrace();
//begin
//  testBoolean(patient(), 'name.given.trace(''test'').count() = 3', true);
//end;
//
//procedure TFHIRPathTests.testToday();
//begin
//  testBoolean(patient(), 'Patient.birthDate < today()', true);
//  testBoolean(patient(), 'today().toString().length() = 10', true);
//end;
//
//procedure TFHIRPathTests.testNow();
//begin
//  testBoolean(patient(), 'Patient.birthDate < now()', true);
//  testBoolean(patient(), 'now().toString().length() > 10', true);
//end;
//
//procedure TFHIRPathTests.testObject(resource: TFhirResource; context: String; focus: TFHIRBase; expression: string; count: integer; typeName: String);
//var
//  fp : TFHIRExpressionEngine;
//  node : TFHIRExpressionNode;
//  s : String;
//  outcome : TFHIRBaseList;
//  b : TFHIRBase;
//  found : boolean;
//  td : TFHIRTypeDetails;
//begin
//  fp := TFHIRExpressionEngine.Create(FServices.Link);
//  try
//    node := fp.parse(expression);
//    try
//      td := fp.check(nil, CODES_TFhirResourceType[resource.ResourceType], context, '', node, false);
//      try
//        Assert.IsFalse(td.hasNoTypes);
//      finally
//        td.free;
//      end;
//      outcome := fp.evaluate(nil, resource, focus, node);
//      try
//        s := fp.UseLog;
//        if s <> '' then
//            writeln(s);
//        assert.AreEqual(outcome.count, count, StringFormat('Expected %d objects but found %d', [count, outcome.count]));
//        for b in outcome do
//          assert.AreEqual(b.FhirType, typeName, StringFormat('Object type %s not ok, should be %s', [b.FhirType, typeName]));
//      finally
//         outcome.Free;
//      end;
//    finally
//      node.Free;
//    end;
//  finally
//    fp.Free;
//  end;
//end;
//
//procedure TFHIRPathTests.testEquality();
//begin
//  testBoolean(patient(), '1 = 1', true);
//  testBoolean(patient(), '{} = {}', true);
//  testBoolean(patient(), '1 = 2', false);
//  testBoolean(patient(), '''a'' = ''a''', true);
//  testBoolean(patient(), '''a'' = ''A''', false);
//  testBoolean(patient(), '''a'' = ''b''', false);
//  testBoolean(patient(), '1.1 = 1.1', true);
//  testBoolean(patient(), '1.1 = 1.2', false);
//  testBoolean(patient(), '1.10 = 1.1', false);
//  testBoolean(patient(), '0 = 0', true);
//  testBoolean(patient(), '0.0 = 0', false);
//  testBoolean(patient(), '@2012-04-15 = @2012-04-15', true);
//  testBoolean(patient(), '@2012-04-15 = @2012-04-16', false);
//  testBoolean(patient(), '@2012-04-15 = @2012-04-15T10:00:00', false);
//  testBoolean(patient(), 'name = name', true);
//  testBoolean(patient(), 'name = name.first() | name.last()', true);
//  testBoolean(patient(), 'name = name.last() | name.first()', false);
//end;
//
//procedure TFHIRPathTests.testNEquality();
//begin
//  testBoolean(patient(), '1 != 1', false);
//  testBoolean(patient(), '{} != {}', false);
//  testBoolean(patient(), '1 != 2', true);
//  testBoolean(patient(), '''a'' != ''a''', false);
//  testBoolean(patient(), '''a'' != ''b''', true);
//  testBoolean(patient(), '1.1 != 1.1', false);
//  testBoolean(patient(), '1.1 != 1.2', true);
//  testBoolean(patient(), '1.10 != 1.1', true);
//  testBoolean(patient(), '0 != 0', false);
//  testBoolean(patient(), '0.0 != 0', true);
//  testBoolean(patient(), '@2012-04-15 != @2012-04-15', false);
//  testBoolean(patient(), '@2012-04-15 != @2012-04-16', true);
//  testBoolean(patient(), '@2012-04-15 != @2012-04-15T10:00:00', true);
//  testBoolean(patient(), 'name != name', false);
//  testBoolean(patient(), 'name != name.first() | name.last()', false);
//  testBoolean(patient(), 'name != name.last() | name.first()', true);
//end;
//
//procedure TFHIRPathTests.testEquivalent();
//begin
//  testBoolean(patient(), '1 ~ 1', true);
//  testBoolean(patient(), '{} ~ {}', true);
//  testBoolean(patient(), '1 ~ 2', false);
//  testBoolean(patient(), '''a'' ~ ''a''', true);
//  testBoolean(patient(), '''a'' ~ ''A''', true);
//  testBoolean(patient(), '''a'' ~ ''b''', false);
//  testBoolean(patient(), '1.1 ~ 1.1', true);
//  testBoolean(patient(), '1.1 ~ 1.2', false);
//  testBoolean(patient(), '1.10 ~ 1.1', true);
//  testBoolean(patient(), '0 ~ 0', true);
//  testBoolean(patient(), '0.0 ~ 0', true);
//  testBoolean(patient(), '@2012-04-15 ~ @2012-04-15', true);
//  testBoolean(patient(), '@2012-04-15 ~ @2012-04-16', false);
//  testBoolean(patient(), '@2012-04-15 ~ @2012-04-15T10:00:00', true);
//  //  testBoolean(patient(), 'name ~ name', true);
//  testBoolean(patient(), 'name.given ~ name.first().given | name.last().given', true);
//  testBoolean(patient(), 'name.given ~ name.last().given | name.first().given', true);
//end;
//
//procedure TFHIRPathTests.testNotEquivalent();
//begin
//  testBoolean(patient(), '1 !~ 1', false);
//  testBoolean(patient(), '{} !~ {}', false);
//  testBoolean(patient(), '1 !~ 2', true);
//  testBoolean(patient(), '''a'' !~ ''a''', false);
//  testBoolean(patient(), '''a'' !~ ''A''', false);
//  testBoolean(patient(), '''a'' !~ ''b''', true);
//  testBoolean(patient(), '1.1 !~ 1.1', false);
//  testBoolean(patient(), '1.1 !~ 1.2', true);
//  testBoolean(patient(), '1.10 !~ 1.1', false);
//  testBoolean(patient(), '0 !~ 0', false);
//  testBoolean(patient(), '0.0 !~ 0', false);
//  testBoolean(patient(), '@2012-04-15 !~ @2012-04-15', false);
//  testBoolean(patient(), '@2012-04-15 !~ @2012-04-16', true);
//  testBoolean(patient(), '@2012-04-15 !~ @2012-04-15T10:00:00', false);
//  //  testBoolean(patient(), 'name !~ name', true);
//  testBoolean(patient(), 'name.given !~ name.first().given | name.last().given', false);
//  testBoolean(patient(), 'name.given !~ name.last().given | name.first().given', false);
//end;
//
//procedure TFHIRPathTests.testLessThan();
//begin
//  testBoolean(patient(), '1 < 2', true);
//  testBoolean(patient(), '1.0 < 1.2', true);
//  testBoolean(patient(), '''a'' < ''b''', true);
//  testBoolean(patient(), '''A'' < ''a''', true);
//  testBoolean(patient(), '@2014-12-12 < @2014-12-13', true);
//  testBoolean(patient(), '@2014-12-13T12:00:00 < @2014-12-13T12:00:01', true);
//  testBoolean(patient(), '@T12:00:00 < @T14:00:00', true);
//
//  testBoolean(patient(), '1 < 1', false);
//  testBoolean(patient(), '1.0 < 1.0', false);
//  testBoolean(patient(), '''a'' < ''a''', false);
//  testBoolean(patient(), '''A'' < ''A''', false);
//  testBoolean(patient(), '@2014-12-12 < @2014-12-12', false);
//  testBoolean(patient(), '@2014-12-13T12:00:00 < @2014-12-13T12:00:00', false);
//  testBoolean(patient(), '@T12:00:00 < @T12:00:00', false);
//
//  testBoolean(patient(), '2 < 1', false);
//  testBoolean(patient(), '1.1 < 1.0', false);
//  testBoolean(patient(), '''b'' < ''a''', false);
//  testBoolean(patient(), '''B'' < ''A''', false);
//  testBoolean(patient(), '@2014-12-13 < @2014-12-12', false);
//  testBoolean(patient(), '@2014-12-13T12:00:01 < @2014-12-13T12:00:00', false);
//  testBoolean(patient(), '@T12:00:01 < @T12:00:00', false);
//end;
//
//procedure TFHIRPathTests.testLessOrEqual();
//begin
//  testBoolean(patient(), '1 <= 2', true);
//  testBoolean(patient(), '1.0 <= 1.2', true);
//  testBoolean(patient(), '''a'' <= ''b''', true);
//  testBoolean(patient(), '''A'' <= ''a''', true);
//  testBoolean(patient(), '@2014-12-12 <= @2014-12-13', true);
//  testBoolean(patient(), '@2014-12-13T12:00:00 <= @2014-12-13T12:00:01', true);
//  testBoolean(patient(), '@T12:00:00 <= @T14:00:00', true);
//
//  testBoolean(patient(), '1 <= 1', true);
//  testBoolean(patient(), '1.0 <= 1.0', true);
//  testBoolean(patient(), '''a'' <= ''a''', true);
//  testBoolean(patient(), '''A'' <= ''A''', true);
//  testBoolean(patient(), '@2014-12-12 <= @2014-12-12', true);
//  testBoolean(patient(), '@2014-12-13T12:00:00 <= @2014-12-13T12:00:00', true);
//  testBoolean(patient(), '@T12:00:00 <= @T12:00:00', true);
//
//  testBoolean(patient(), '2 <= 1', false);
//  testBoolean(patient(), '1.1 <= 1.0', false);
//  testBoolean(patient(), '''b'' <= ''a''', false);
//  testBoolean(patient(), '''B'' <= ''A''', false);
//  testBoolean(patient(), '@2014-12-13 <= @2014-12-12', false);
//  testBoolean(patient(), '@2014-12-13T12:00:01 <= @2014-12-13T12:00:00', false);
//  testBoolean(patient(), '@T12:00:01 <= @T12:00:00', false);
//end;
//
//procedure TFHIRPathTests.testGreatorOrEqual();
//begin
//  testBoolean(patient(), '1 >= 2', false);
//  testBoolean(patient(), '1.0 >= 1.2', false);
//  testBoolean(patient(), '''a'' >= ''b''', false);
//  testBoolean(patient(), '''A'' >= ''a''', false);
//  testBoolean(patient(), '@2014-12-12 >= @2014-12-13', false);
//  testBoolean(patient(), '@2014-12-13T12:00:00 >= @2014-12-13T12:00:01', false);
//  testBoolean(patient(), '@T12:00:00 >= @T14:00:00', false);
//
//  testBoolean(patient(), '1 >= 1', true);
//  testBoolean(patient(), '1.0 >= 1.0', true);
//  testBoolean(patient(), '''a'' >= ''a''', true);
//  testBoolean(patient(), '''A'' >= ''A''', true);
//  testBoolean(patient(), '@2014-12-12 >= @2014-12-12', true);
//  testBoolean(patient(), '@2014-12-13T12:00:00 >= @2014-12-13T12:00:00', true);
//  testBoolean(patient(), '@T12:00:00 >= @T12:00:00', true);
//
//  testBoolean(patient(), '2 >= 1', true);
//  testBoolean(patient(), '1.1 >= 1.0', true);
//  testBoolean(patient(), '''b'' >= ''a''', true);
//  testBoolean(patient(), '''B'' >= ''A''', true);
//  testBoolean(patient(), '@2014-12-13 >= @2014-12-12', true);
//  testBoolean(patient(), '@2014-12-13T12:00:01 >= @2014-12-13T12:00:00', true);
//  testBoolean(patient(), '@T12:00:01 >= @T12:00:00', true);
//end;
//
//procedure TFHIRPathTests.testGreatorThan();
//begin
//  testBoolean(patient(), '1 > 2', false);
//  testBoolean(patient(), '1.0 > 1.2', false);
//  testBoolean(patient(), '''a'' > ''b''', false);
//  testBoolean(patient(), '''A'' > ''a''', false);
//  testBoolean(patient(), '@2014-12-12 > @2014-12-13', false);
//  testBoolean(patient(), '@2014-12-13T12:00:00 > @2014-12-13T12:00:01', false);
//  testBoolean(patient(), '@T12:00:00 > @T14:00:00', false);
//
//  testBoolean(patient(), '1 > 1', false);
//  testBoolean(patient(), '1.0 > 1.0', false);
//  testBoolean(patient(), '''a'' > ''a''', false);
//  testBoolean(patient(), '''A'' > ''A''', false);
//  testBoolean(patient(), '@2014-12-12 > @2014-12-12', false);
//  testBoolean(patient(), '@2014-12-13T12:00:00 > @2014-12-13T12:00:00', false);
//  testBoolean(patient(), '@T12:00:00 > @T12:00:00', false);
//
//  testBoolean(patient(), '2 > 1', true);
//  testBoolean(patient(), '1.1 > 1.0', true);
//  testBoolean(patient(), '''b'' > ''a''', true);
//  testBoolean(patient(), '''B'' > ''A''', true);
//  testBoolean(patient(), '@2014-12-13 > @2014-12-12', true);
//  testBoolean(patient(), '@2014-12-13T12:00:01 > @2014-12-13T12:00:00', true);
//  testBoolean(patient(), '@T12:00:01 > @T12:00:00', true);
//end;
//
//procedure TFHIRPathTests.testUnion();
//begin
//  testBoolean(patient(), '(1 | 2 | 3).count() = 3', true);
//  testBoolean(patient(), '(1 | 2 | 2).count() = 2', true); // merge duplicates
//end;
//
//procedure TFHIRPathTests.testIn();
//begin
//  testBoolean(patient(), '1 in (1 | 2 | 3)', true);
//  testBoolean(patient(), '1 in (2 | 3)', false);
//  testBoolean(patient(), '''a'' in (''a'' | ''c'' | ''d'')', true);
//  testBoolean(patient(), '''b'' in (''a'' | ''c'' | ''d'')', false);
//end;
//
//procedure TFHIRPathTests.testContainsCollection();
//begin
//  testBoolean(patient(), '(1 | 2 | 3) contains 1', true);
//  testBoolean(patient(), '(2 | 3) contains 1 ', false);
//  testBoolean(patient(), '(''a'' | ''c'' | ''d'') contains ''a''', true);
//  testBoolean(patient(), '(''a'' | ''c'' | ''d'') contains ''b''', false);
//end;
//
//procedure TFHIRPathTests.testBooleanLogicAnd();
//begin
//  testBoolean(patient(), '(true and true) = true', true);
//  testBoolean(patient(), '(true and false) = false', true);
//  testBoolean(patient(), '(true and {}) = {}', true);
//
//  testBoolean(patient(), '(false and true) = false', true);
//  testBoolean(patient(), '(false and false) = false', true);
//  testBoolean(patient(), '(false and {}) = false', true);
//
//  testBoolean(patient(), '({} and true) = {}', true);
//  testBoolean(patient(), '({} and false) = false', true);
//  testBoolean(patient(), '({} and {}) = {}', true);
//end;
//
//procedure TFHIRPathTests.testBooleanLogicOr();
//begin
//  testBoolean(patient(), '(true or true) = true', true);
//  testBoolean(patient(), '(true or false) = true', true);
//  testBoolean(patient(), '(true or {}) = true', true);
//
//  testBoolean(patient(), '(false or true) = true', true);
//  testBoolean(patient(), '(false or false) = false', true);
//  testBoolean(patient(), '(false or {}) = {}', true);
//
//  testBoolean(patient(), '({} or true) = true', true);
//  testBoolean(patient(), '({} or false) = {}', true);
//  testBoolean(patient(), '({} or {}) = {}', true);
//end;
//
//procedure TFHIRPathTests.testBooleanLogicXOr();
//begin
//  testBoolean(patient(), '(true xor true) = false', true);
//  testBoolean(patient(), '(true xor false) = true', true);
//  testBoolean(patient(), '(true xor {}) = {}', true);
//
//  testBoolean(patient(), '(false xor true) = true', true);
//  testBoolean(patient(), '(false xor false) = false', true);
//  testBoolean(patient(), '(false xor {}) = {}', true);
//
//  testBoolean(patient(), '({} xor true) = {}', true);
//  testBoolean(patient(), '({} xor false) = {}', true);
//  testBoolean(patient(), '({} xor {}) = {}', true);
//end;
//
//procedure TFHIRPathTests.testBooleanImplies();
//begin
//  testBoolean(patient(), '(true implies true) = true', true);
//  testBoolean(patient(), '(true implies false) = false', true);
//  testBoolean(patient(), '(true implies {}) = {}', true);
//
//  testBoolean(patient(), '(false implies true) = true', true);
//  testBoolean(patient(), '(false implies false) = true', true);
//  testBoolean(patient(), '(false implies {}) = true', true);
//
//  testBoolean(patient(), '({} implies true) = true', true);
//  testBoolean(patient(), '({} implies false) = true', true);
//  testBoolean(patient(), '({} implies {}) = true', true);
//end;
//
//procedure TFHIRPathTests.testPlus();
//begin
//  testBoolean(patient(), '1 + 1 = 2', true);
//  testBoolean(patient(), '1 + 0 = 1', true);
//  testBoolean(patient(), '1.2 + 1.8 = 3.0', true);
//  testBoolean(patient(), '''a''+''b'' = ''ab''', true);
//end;
//
//procedure TFHIRPathTests.testMinus();
//begin
//  testBoolean(patient(), '1 - 1 = 0', true);
//  testBoolean(patient(), '1 - 0 = 1', true);
//  testBoolean(patient(), '1.8 - 1.2 = 0.6', true);
//  testWrong(patient(), '''a''-''b'' = ''ab''', true);
//end;
//
//procedure TFHIRPathTests.testMultiply();
//begin
//  testBoolean(patient(), '1 * 1 = 1', true);
//  testBoolean(patient(), '1 * 0 = 0', true);
//  testBoolean(patient(), '1.2 * 1.8 = 2.16', true);
//end;
//
//procedure TFHIRPathTests.testDivide();
//begin
//  testBoolean(patient(), '1 / 1 = 1', true);
//  testBoolean(patient(), '4 / 2 = 2', true);
//  testBoolean(patient(), '1 / 2 = 0.5', true);
//  testBoolean(patient(), '1.2 / 1.8 = 0.67', true);
//end;
//
//procedure TFHIRPathTests.testDiv();
//begin
//  testBoolean(patient(), '1 div 1 = 1', true);
//  testBoolean(patient(), '4 div 2 = 2', true);
//  testBoolean(patient(), '5 div 2 = 2', true);
//  testBoolean(patient(), '2.2 div 1.8 = 1', true);
//end;
//
//procedure TFHIRPathTests.testMod();
//begin
//  testBoolean(patient(), '1 mod 1 = 0', true);
//  testBoolean(patient(), '4 mod 2 = 0', true);
//  testBoolean(patient(), '5 mod 2 = 1', true);
//  testBoolean(patient(), '2.2 mod 1.8 = 0.4', true);
//end;
//
//procedure TFHIRPathTests.testPrecedence();
//begin
//  testBoolean(patient(), '1+2*3+4 = 11', true);
//end;
//
//procedure TFHIRPathTests.testVariables();
//begin
//  testBoolean(patient(), '%sct = ''http://snomed.info/sct''', true);
//  testBoolean(patient(), '%loinc = ''http://loinc.org''', true);
//  testBoolean(patient(), '%ucum = ''http://unitsofmeasure.org''', true);
//  testBoolean(patient(), '%"vs-administrative-gender" = ''http://hl7.org/fhir/ValueSet/administrative-gender''', true);
//end;
//
//procedure TFHIRPathTests.testExtension();
//begin
//  testBoolean(patient(), 'Patient.birthDate.extension(''http://hl7.org/fhir/StructureDefinition/patient-birthTime'').exists()', true);
//  testBoolean(patient(), 'Patient.birthDate.extension(%"ext-patient-birthTime").exists()', true);
//  testBoolean(patient(), 'Patient.birthDate.extension(''http://hl7.org/fhir/StructureDefinition/patient-birthTime1'').empty()', true);
//end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRPathTests);
end.

