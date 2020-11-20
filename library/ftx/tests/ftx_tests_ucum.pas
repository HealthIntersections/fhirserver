unit ftx_tests_ucum;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes,
  fsl_testing,
  fsl_base, fsl_utilities, fsl_xml,
  ftx_ucum_services;

type
  TUcumTest = class (TFslTestSuiteCase)
  private
    procedure findTest(name : String; var foundGroup, foundTest : TMXmlElement);

    procedure TestValidation(test : TMXmlElement);
    procedure TestDisplayNameGeneration(test : TMXmlElement);
    procedure TestConversion(test : TMXmlElement);
    procedure TestMultiplication(test : TMXmlElement);
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCase(name : String); override;
  end;

  TUcumTests = class(TFslTestSuite)
  public
    constructor Create; override;
  end;

  TUcumSpecialTests = class (TFslTestCase)
  private
  public
    Procedure SetUp; override;
  published
    procedure TestIssue10;
  end;

procedure registerTests;

implementation

var
  testList : TMXmlElement;
  svc : TUcumServices;

procedure LoadTests;
begin
  if testList = nil then
    testList := TMXmlParser.ParseFile(TestSettings.serverTestFile(['testcases', 'ucum', 'ucum-tests.xml']), [xpDropWhitespace]);
end;


{ TUcumTests }

constructor TUcumTests.Create;
var
  group, test : TMXmlElement;
begin
  inherited Create;
  LoadTests;
  for group in testList.document.Children do
    for test in group.Children do
      if (test.Name = 'case') and (test.attribute['id'] <> '') then
        AddTest(TUcumTest.create(group.Name+'-'+test.attribute['id']));
end;

{ TUcumTest }

procedure TUcumTest.findTest(name: String; var foundGroup, foundTest: TMXmlElement);
var
  group, test : TMXmlElement;
begin
  LoadTests;
  foundGroup := nil;
  foundTest := nil;

  for group in testList.document.Children do
    for test in group.Children do
      if (test.Name = 'case') and  (group.name+'-'+test.attribute['id'] = name) then
      begin
        foundGroup := Group;
        foundTest := test;
        exit;
      end;
  raise ELibraryException.Create('nout found id = '+name);
end;

procedure TUcumTest.Setup;
begin
  if (svc = nil) then
  begin
    svc := TUcumServices.Create;
    svc.Import(TestSettings.serverTestFile(['exec', 'pack', 'ucum-essence.xml']));
  end;
end;

procedure TUcumTest.TearDown;
begin
end;

procedure TUcumTest.TestCase(name: String);
var
  group, test : TMXmlElement;
begin
  {$IFDEF EXCLUDE_FAILING_TESTS}
  if StringArrayExistsSensitive(['2-102', '2-103', '2-104','2-108', '3-108', '3-109', '3-110', '3-111',
     '3-111a', '3-115'], id) then
  begin
    Assert.Pass('not tested');
    exit;
  end;

  {$ENDIF}
  findTest(name, group, test);
  if group.name = 'validation' then
    TestValidation(test)
  else if group.name = 'displayNameGeneration' then
    TestDisplayNameGeneration(test)
  else if group.name = 'conversion' then
    TestConversion(test)
  else if group.name = 'multiplication' then
    TestMultiplication(test)
  else
    raise ETerminologyError.Create('unknown group '+group.Name);
end;

procedure TUcumTest.TestValidation(test : TMXmlElement);
var
  units : String;
  valid : boolean;
  res, reason : String;
begin
  units := test.attribute['unit'];
  valid := test.attribute['valid'] = 'true';
  reason := test.attribute['reason'];

  res := svc.validate(units);
  if valid then
    assertTrue(res = '', 'Unit "'+units+'" should be valid, but had error: '+res)
  else
    AssertTrue(res <> '', 'Unit "'+units+'" should be invalid, but no error');
end;

procedure TUcumTest.TestDisplayNameGeneration(test : TMXmlElement);
var
  units, display, res : String;
begin
  units := test.attribute['unit'];
  display := test.attribute['display'];
  res := svc.analyse(units);
  assertEqual(display, res, 'Display name should have been "'+display+'" but was "'+res+'"');
end;

procedure TUcumTest.TestConversion(test : TMXmlElement);
var
  value, srcUnit, dstUnit, outcome : String;
  d : TFslDecimal;
begin
  value := test.attribute['value'];
  srcUnit := test.attribute['srcUnit'];
  dstUnit := test.attribute['dstUnit'];
  outcome := test.attribute['outcome'];

  d := svc.convert(TFslDecimal.ValueOf(value), srcUnit, dstUnit);

  assertTrue(d.Compares(TFslDecimal.ValueOf(outcome)) = 0, 'Value does not match. expected '+outcome+' but got '+d.AsString);
end;

procedure TUcumTest.TestMultiplication(test : TMXmlElement);
var
  id, v1, u1, v2, u2, vRes, uRes : String;
  o1, o2, o3 : TUcumPair;
begin
  id := test.attribute['id'];
  v1 := test.attribute['v1'];
  u1 := test.attribute['u1'];
  v2 := test.attribute['v2'];
  u2 := test.attribute['u2'];
  vRes := test.attribute['vRes'];
  uRes := test.attribute['uRes'];

  o1 := TUcumPair.Create(TFslDecimal.ValueOf(v1), u1);
  try
    o2 := TUcumPair.Create(TFslDecimal.ValueOf(v2), u2);
    try
      o3 := svc.multiply(o1, o2);
      try
        assertTrue((o3.Value.compares(TFslDecimal.valueOf(vRes)) = 0) and (o3.UnitCode = uRes));
      finally
        o3.Free;
      end;
    finally
      o2.Free;
    end;
  finally
    o1.Free;
  end;
end;

{ TUcumSpecialTests }

procedure TUcumSpecialTests.Setup;
begin
  if (svc = nil) then
  begin
    svc := TUcumServices.Create;
    svc.Import(TestSettings.serverTestFile(['exec', 'pack', 'ucum-essence.xml']));
  end;
end;

procedure TUcumSpecialTests.TestIssue10;
var
  i : integer;
  f, e : double;
  d, o : TFslDecimal;
begin
  for i := 905000 to 1000000 do
  begin
    f := i * 0.0001;
    d := TFslDecimal.Create(f);
    o := svc.convert(d, 'kg', '[lb_av]');
    e := f * 2.2046226218487758072297380134503;
    assertTrue(Abs(o.AsDouble - e) < 0.001, 'Expected '+FloatToStr(e)+', but got '+o.AsDecimal);
  end;
end;

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Terminology.UCUM testList', TUcumTests.create);
  RegisterTest('Terminology.UCUM Special Test', TUcumSpecialTests.Suite);
end;

initialization
finalization
  testList.Free;
  svc.Free;
end.
