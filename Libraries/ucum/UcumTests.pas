unit UcumTests;

// -tests  -definitions

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
interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  StringSupport, DecimalSupport,
  AdvObjects,
  MXML,
  UcumServices;

type
  UcumTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TUcumTests = class (TObject)
  private
    procedure findTest(id : String; var foundGroup, foundTest : TMXmlElement);

    procedure TestValidation(test : TMXmlElement);
    procedure TestDisplayNameGeneration(test : TMXmlElement);
    procedure TestConversion(test : TMXmlElement);
    procedure TestMultiplication(test : TMXmlElement);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [UcumTestCase]
    procedure TestCase(id : String);
  end;



implementation

var
  tests : TMXmlElement;
  svc : TUcumServices;

procedure LoadTests;
begin
  if tests = nil then
    tests := TMXmlParser.ParseFile('C:\work\fhir.org\Ucum-java\src\test\resources\UcumFunctionalTests.xml', [xpDropWhitespace]);
end;


{ UcumTestCaseAttribute }

function UcumTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  group, test : TMXmlElement;
  i: integer;
begin
  LoadTests;

  i := 0;
  for group in tests.document.Children do
    for test in group.Children do
      if (test.Name = 'case') and (test.attribute['id'] <> '') then
        inc(i);
  setLength(result, i);
  i := 0;
  for group in tests.document.Children do
    for test in group.Children do
      if (test.Name = 'case') and (test.attribute['id'] <> '') then
      begin
        result[i].Name := group.Name+'-'+test.attribute['id'];
        SetLength(result[i].Values, 1);
        result[i].Values[0] := test.attribute['id'];
        inc(i);
      end;
end;

{ TUcumTests }

procedure TUcumTests.findTest(id: String; var foundGroup, foundTest: TMXmlElement);
var
  group, test : TMXmlElement;
begin
  LoadTests;
  foundGroup := nil;
  foundTest := nil;

  for group in tests.document.Children do
    for test in group.Children do
      if (test.Name = 'case') and (test.attribute['id'] = id) then
      begin
        foundGroup := Group;
        foundTest := test;
        exit;
      end;
  raise Exception.Create('nout found id = '+id);
end;

procedure TUcumTests.Setup;
begin
  if (svc = nil) then
  begin
    svc := TUcumServices.Create;
    svc.Import('C:\work\fhir.org\Ucum-java\src\main\resources\ucum-essence.xml');
  end;
end;

procedure TUcumTests.TearDown;
begin
end;

procedure TUcumTests.TestCase(id: String);
var
  group, test : TMXmlElement;
begin
  findTest(id, group, test);
  if group.name = 'validation' then
    TestValidation(test)
  else if group.name = 'displayNameGeneration' then
    TestDisplayNameGeneration(test)
  else if group.name = 'conversion' then
    TestConversion(test)
  else if group.name = 'multiplication' then
    TestMultiplication(test)
  else
    raise Exception.Create('unknown group '+group.Name);
end;

procedure TUcumTests.TestValidation(test : TMXmlElement);
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
    Assert.IsEmpty(res, 'Unit "'+units+'" should be valid, but had error: '+res)
  else
    Assert.IsNotEmpty(res, 'Unit "'+units+'" should be invalid, but no error');
end;

procedure TUcumTests.TestDisplayNameGeneration(test : TMXmlElement);
var
  units, display, res : String;
begin
  units := test.attribute['unit'];
  display := test.attribute['display'];
  res := svc.analyse(units);
  Assert.AreEqual(display, res, 'Display name should have been "'+display+'" but was "'+res+'"');
end;

procedure TUcumTests.TestConversion(test : TMXmlElement);
var
  value, srcUnit, dstUnit, outcome : String;
  d : TSmartDecimal;
begin
  value := test.attribute['value'];
  srcUnit := test.attribute['srcUnit'];
  dstUnit := test.attribute['dstUnit'];
  outcome := test.attribute['outcome'];

  d := svc.convert(TSmartDecimal.ValueOf(value), srcUnit, dstUnit);

  Assert.IsTrue(d.Compares(TSmartDecimal.ValueOf(outcome)) = 0, 'Value does not match. expected '+outcome+' but got '+d.AsString);
end;

procedure TUcumTests.TestMultiplication(test : TMXmlElement);
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

  o1 := TUcumPair.Create(TSmartDecimal.ValueOf(v1), u1);
  try
    o2 := TUcumPair.Create(TSmartDecimal.ValueOf(v2), u2);
    try
	    o3 := svc.multiply(o1, o2);
      try
        Assert.IsTrue((o3.Value.compares(TSmartDecimal.valueOf(vRes)) = 0) and (o3.UnitCode = uRes));
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

initialization
  TDUnitX.RegisterTestFixture(TUcumTests);
finalization
  tests.Free;
  svc.Free;
end.
