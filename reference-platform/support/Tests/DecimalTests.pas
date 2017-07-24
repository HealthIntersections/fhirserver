Unit DecimalTests;

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


Interface

Uses
  SysUtils,
  StringSupport, DecimalSupport,
  AdvObjects,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TDecimalTests = Class (TObject)
  Private
    procedure testString(s, st, std : String);
    Procedure testTrunc(s1,s2 : String);
    procedure TestAdd(s1,s2,s3:String);
    procedure TestMultiply(s1,s2,s3:String);
    procedure TestSubtract(s1,s2,s3:String);
    procedure TestDivide(s1,s2,s3:String);
    procedure TestDivInt(s1,s2,s3:String);
    procedure TestModulo(s1,s2,s3:String);
    procedure TestInteger(i : integer);
    procedure TestCardinal(i : cardinal);
    procedure TestInt64(i : int64);
    procedure TestRoundTrip(n1, n2, n3, t : String);
    procedure TestBoundsCase(v, low, high, ilow, ihigh : String);
  Published
    [TestCase]
    Procedure TestIsDecimal;

    [TestCase]
    Procedure TestAsInteger;
    [TestCase]
    Procedure TestStringSupport;
    [TestCase]
    Procedure TestAddition;
    [TestCase]
    Procedure TestMultiplication;
    [TestCase]
    Procedure TestBounds;
  End;

Implementation


{ TDecimalTests }

procedure TDecimalTests.testString(s, st, std: String);
var
  dec : TSmartDecimal;
  s1, s2 : String;
begin
  dec := TSmartDecimal.valueOf(s);
  s1 := dec.AsString;
  s2 := dec.AsScientific;
  Assert.IsTrue(s1 = st);
  Assert.IsTrue(s2 = std);
  dec := TSmartDecimal.valueOf(std);
  s1 := dec.AsDecimal;
  Assert.IsTrue(s1 = st);
end;

procedure TDecimalTests.TestStringSupport;
begin
  testString('1', '1', '1e0');
  testString('0', '0', '0e0');
  testString('10', '10', '1.0e1');
  testString('99', '99', '9.9e1');
  testString('-1', '-1', '-1e0');
  testString('-0', '0', '0e0');
  testString('-10', '-10', '-1.0e1');
  testString('-99', '-99', '-9.9e1');

  testString('1.1', '1.1', '1.1e0');
  testString('-1.1', '-1.1', '-1.1e0');
  testString('11.1', '11.1', '1.11e1');
  testString('1.11', '1.11', '1.11e0');
  testString('1.111', '1.111', '1.111e0');
  testString('0.1', '0.1', '1e-1');
  testString('00.1', '0.1', '1e-1');
  testString('.1', '0.1', '1e-1');
  testString('1.0', '1.0', '1.0e0');
  testString('1.00', '1.00', '1.00e0');
  testString('1.000000000000000000000000000000000000000', '1.000000000000000000000000000000000000000', '1.000000000000000000000000000000000000000e0');

  testString('-11.1', '-11.1', '-1.11e1');
  testString('-1.11', '-1.11', '-1.11e0');
  testString('-1.111', '-1.111', '-1.111e0');
  testString('-0.1', '-0.1', '-1e-1');
  testString('-00.1', '-0.1', '-1e-1');
  testString('-.1', '-0.1', '-1e-1');
  testString('-1.0', '-1.0', '-1.0e0');
  testString('-1.00', '-1.00', '-1.00e0');
  testString('-1.000000000000000000000000000000000000000', '-1.000000000000000000000000000000000000000', '-1.000000000000000000000000000000000000000e0');

  testString('0.0', '0.0', '0.0e0');
  testString('0.0000', '0.0000', '0.0000e0');
  testString('0.1', '0.1', '1e-1');
  testString('00.1', '0.1', '1e-1');
  testString('0.100', '0.100', '1.00e-1');
  testString('100', '100', '1.00e2');
  testString('1.0', '1.0', '1.0e0');
  testString('1.1', '1.1', '1.1e0');
  testString('-0.1', '-0.1', '-1e-1');
  testString('0.01', '0.01', '1e-2');
  testString('0.001', '0.001', '1e-3');
  testString('0.0001', '0.0001', '1e-4');
  testString('00.0001', '0.0001', '1e-4');
  testString('000.0001', '0.0001', '1e-4');
  testString('-0.01', '-0.01', '-1e-2');
  testString('10.01', '10.01', '1.001e1');
  testString('0.0001', '0.0001', '1e-4');
  testString('0.00001', '0.00001', '1e-5');
  testString('0.000001', '0.000001', '1e-6');
  testString('0.0000001', '0.0000001', '1e-7');
  testString('0.000000001', '0.000000001', '1e-9');
  testString('0.00000000001', '0.00000000001', '1e-11');
  testString('0.0000000000001', '0.0000000000001', '1e-13');
  testString('0.000000000000001', '0.000000000000001', '1e-15');
  testString('0.00000000000000001', '0.00000000000000001', '1e-17');
  testString('10.1', '10.1', '1.01e1');
  testString('100.1', '100.1', '1.001e2');
  testString('1000.1', '1000.1', '1.0001e3');
  testString('10000.1', '10000.1', '1.00001e4');
  testString('100000.1', '100000.1', '1.000001e5');
  testString('1000000.1', '1000000.1', '1.0000001e6');
  testString('10000000.1', '10000000.1', '1.00000001e7');
  testString('100000000.1', '100000000.1', '1.000000001e8');
  testString('1000000000.1', '1000000000.1', '1.0000000001e9');
  testString('10000000000.1', '10000000000.1', '1.00000000001e10');
  testString('100000000000.1', '100000000000.1', '1.000000000001e11');
  testString('1000000000000.1', '1000000000000.1', '1.0000000000001e12');
  testString('10000000000000.1', '10000000000000.1', '1.00000000000001e13');
  testString('100000000000000.1', '100000000000000.1', '1.000000000000001e14');
//  testString('1e-3', '1e-3');   , '1e-3');  e0  }

  testTrunc('1', '1');
  testTrunc('1.01', '1');
  testTrunc('-1.01', '-1');
  testTrunc('0.01', '0');
  testTrunc('-0.01', '0');
  testTrunc('0.1', '0');
  testTrunc('0.0001', '0');
  testTrunc('100.000000000000000000000000000000000000000001', '100');
end;

procedure TDecimalTests.TestAddition;
begin
  TestAdd('1', '1', '2');
  TestAdd('0', '1', '1');
  TestAdd('0', '0', '0');
  TestAdd('5', '5', '10');
  TestAdd('10', '1', '11');
  TestAdd('11', '12', '23');
  TestAdd('15', '16', '31');
  TestAdd('150', '160', '310');
  TestAdd('153', '168', '321');
  TestAdd('15300000000000000000000000000000000001', '1680', '15300000000000000000000000000000001681');
  TestAdd('1', '.1', '1.1');
  TestAdd('1', '.001', '1.001');
  TestAdd('.1', '.1', '0.2');
  TestAdd('.1', '.01', '0.11');

  TestSubtract('2', '1', '1');
  TestSubtract('2', '0', '2');
  TestSubtract('0', '0', '0');
  TestSubtract('0', '2', '-2');
  TestSubtract('2', '2', '0');
  TestSubtract('1', '2', '-1');
  TestSubtract('20', '1', '19');
  TestSubtract('2', '.1', '1.9');
  TestSubtract('2', '.000001', '1.999999');
  TestSubtract('2', '2.000001', '-0.000001');
  TestSubtract('3.5', '35.5', '-32.0');

  TestAdd('5', '6', '11');
  TestAdd('5', '-6', '-1');
  TestAdd('-5', '6', '1');
  TestAdd('-5', '-6', '-11');

  TestSubtract('5', '6', '-1');
  TestSubtract('6', '5', '1');
  TestSubtract('5', '-6', '11');
  TestSubtract('6', '-5', '11');
  TestSubtract('-5', '6', '-11');
  TestSubtract('-6', '5', '-11');
  TestSubtract('-5', '-6', '1');
  TestSubtract('-6', '-5', '-1');

  TestAdd('2', '0.001', '2.001');
  TestAdd('2.0', '0.001', '2.001');
end;



procedure TDecimalTests.TestAdd(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
    o1 := TSmartDecimal.valueOf(s1);
    o2 := TSmartDecimal.valueOf(s2);
    o3 := o1.add(o2);
    Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestSubtract(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := TSmartDecimal.valueOf(s2);
  o3 := o1.Subtract(o2);
  Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestMultiplication;
begin
  TestMultiply('2', '2', '4');
  TestMultiply('2', '0.5', '1');
  TestMultiply('0', '0', '0');
  TestMultiply('0', '1', '0');
  TestMultiply('4', '4', '16');
  TestMultiply('20', '20', '400');
  TestMultiply('200', '20', '4000');
  TestMultiply('400', '400', '160000');
  TestMultiply('2.0', '2.0', '4.0');
  TestMultiply('2.00', '2.0', '4.0');
  TestMultiply('2.0', '0.2', '0.4');
  TestMultiply('2.0', '0.20', '0.40');
  TestMultiply('13', '13', '169');
  TestMultiply('12', '89', '1068');
  TestMultiply('1234', '6789', '8377626');

  TestMultiply('10000', '0.0001', '1');
  TestMultiply('10000', '0.00010', '1.0');
  TestMultiply('10000', '0.000100', '1.00');
  TestMultiply('10000', '0.0001000', '1.000');
  TestMultiply('10000', '0.00010000', '1.0000');
  TestMultiply('10000', '0.000100000', '1.00000');
  TestMultiply('10000.0', '0.000100000', '1.00000');
  TestMultiply('10000.0', '0.0001000000', '1.00000');
  TestMultiply('10000.0', '0.00010000000', '1.00000');

  TestMultiply('2', '-2', '-4');
  TestMultiply('-2', '2', '-4');
  TestMultiply('-2', '-2', '4');

  TestMultiply('35328734682734', '2349834295876423', '83016672387407213199375780482');
  TestMultiply('35328734682734000000000', '2349834295876423000000000', '83016672387407213199375780482000000000000000000');
  TestMultiply('3532873468.2734', '23498342958.76423', '83016672387407213199.375780482');

  TestDivide('500', '4', '125');
  TestDivide('1260257', '37', '34061');

  TestDivide('127', '4', '31.75');
  TestDivide('10', '10', '1');
  TestDivide('1', '1', '1');
  TestDivide('1', '3', '0.333333333333333333333333');
  TestDivide('1.0', '3', '0.33');
  TestDivide('10', '3', '3.33333333333333333333333');
  TestDivide('10.0', '3', '3.33');
  TestDivide('10.00', '3', '3.333');
  TestDivide('10.00', '3.0', '3.3');
  TestDivide('100', '1', '100');
  TestDivide('1000', '10', '100');
  TestDivide('100001', '10', '10000.1');
  TestDivide('100', '10', '10');
  TestDivide('1', '10', '0.1');
  TestDivide('1', '15', '0.0666666666666666666666667');
  TestDivide('1.0', '15', '0.067');
  TestDivide('1.00', '15.0', '0.0667');
  TestDivide('1', '0.1', '10');
  TestDivide('1', '0.10', '10');
  TestDivide('1', '0.010', '100');
  TestDivide('1', '1.5', '0.67');
  TestDivide('1.0', '1.5', '0.67');
  TestDivide('10', '1.5', '6.7');

  TestDivide('-1', '1', '-1');
  TestDivide('1', '-1', '-1');
  TestDivide('-1', '-1', '1');

  TestDivide('2', '2', '1');
  TestDivide('20', '2', '10');
  TestDivide('22', '2', '11');

  TestDivide('83016672387407213199375780482', '2349834295876423', '35328734682734');
  TestDivide('83016672387407213199375780482000000000000000000', '2349834295876423000000000', '35328734682734000000000');
  TestDivide('83016672387407213199.375780482', '23498342958.76423', '3532873468.2734');

  TestDivInt('500', '4', '125');
  TestDivInt('1260257', '37', '34061');
  TestDivInt('127', '4', '31');
  TestDivInt('10', '10', '1');
  TestDivInt('1', '1', '1');
  TestDivInt('100', '1', '100');
  TestDivInt('1000', '10', '100');
  TestDivInt('100001', '10', '10000');
  TestDivInt('1', '1.5', '0');
  TestDivInt('10', '1.5', '6');

  TestModulo('10', '1', '0');
  TestModulo('7', '4', '3');

  TestMultiply('2', '2', '4');
  TestMultiply('2.0', '2.0', '4.0');
  TestMultiply('2.00', '2.0', '4.0');

  TestDivide('10.0',  '3', '3.33');
  TestDivide('10.00',  '3', '3.333');
  TestDivide('10.00',  '3.0', '3.3');
  TestDivide('10',  '3.0', '3.3');

  TestRoundTrip('1','60', '60', '1');
end;

procedure TDecimalTests.TestMultiply(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := TSmartDecimal.valueOf(s2);
  o3 := o1.Multiply(o2);
  Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestRoundTrip(n1, n2, n3, t: String);
var
  o1, o2, o3, o4: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(n1);
  o2 := TSmartDecimal.valueOf(n2);
  o3 := o1.Divide(o2);
  o4 := o3.Multiply(TSmartDecimal.valueOf(n3));
  Assert.IsTrue(o4.AsDecimal = t);
end;

procedure TDecimalTests.TestDivide(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := TSmartDecimal.valueOf(s2);
  o3 := o1.Divide(o2);
  Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.testTrunc(s1, s2: String);
var
  o1, o2 : TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := o1.Trunc;
  Assert.IsTrue(o2.AsDecimal = s2);
end;

procedure TDecimalTests.TestDivInt(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := TSmartDecimal.valueOf(s2);
  o3 := o1.DivInt(o2);
  Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestModulo(s1, s2, s3: String);
var
  o1, o2, o3: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(s1);
  o2 := TSmartDecimal.valueOf(s2);
  o3 := o1.Modulo(o2);
  Assert.IsTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestAsInteger;
begin
  TestInteger(0);
  TestInteger(1);
  TestInteger(2);
  TestInteger(64);
  TestInteger(High(Integer));
  TestInteger(-1);
  TestInteger(-2);
  TestInteger(-64);
  TestInteger(Low(Integer));

  TestCardinal(0);
  TestCardinal(2);
  TestCardinal(High(Cardinal));

  TestInt64(0);
  TestInt64(1);
  testInt64(-1);
  TestInt64(High(integer));
  TestInt64(Low(integer));
  TestInt64(High(Cardinal));
  TestInt64(High(int64));
  TestInt64(Low(int64));
end;

procedure TDecimalTests.TestBounds;
begin
  TestBoundsCase('1',      '0.5',   '1.5',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('1.0',   '0.95',  '1.05',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('1.00', '0.995', '1.005',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('0',     '-0.5',   '0.5', '-0.000000000000000000000001',   '0.000000000000000000000001');
  TestBoundsCase('0.0',  '-0.05',  '0.05', '-0.000000000000000000000001',   '0.000000000000000000000001');
  TestBoundsCase('-1',    '-1.5',  '-0.5', '-1.000000000000000000000001',  '-0.99999999999999999999999');
end;

procedure TDecimalTests.TestBoundsCase(v, low, high, ilow, ihigh : String);
var
  o1: TSmartDecimal;
begin
  o1 := TSmartDecimal.valueOf(v);
  Assert.IsTrue(o1.upperBound.AsDecimal = high);
  Assert.IsTrue(o1.lowerBound.AsDecimal = low);
//    check(o1.immediateUpperBound.AsDecimal = ihigh);
//    check(o1.immediateLowerBound.AsDecimal = ilow);
end;

procedure TDecimalTests.TestInteger(i: integer);
var
  d : TSmartDecimal;
begin
  d := TSmartDecimal.valueOf(i);
  Assert.IsTrue(d.AsInteger = i);
end;

procedure TDecimalTests.TestIsDecimal;
begin
  Assert.IsTrue(StringIsDecimal('0'), '"0" is a decimal');
  Assert.IsTrue(StringIsDecimal('+0'), '"+0" is a decimal');
  Assert.IsFalse(StringIsDecimal('0+'), '"0+" is not a decimal');
  Assert.IsFalse(StringIsDecimal('+'), '"+" is not a decimal');
  Assert.IsTrue(StringIsDecimal('-0'), '"-0" is a decimal');
  Assert.IsFalse(StringIsDecimal('0-'), '"0-" is not a decimal');
  Assert.IsFalse(StringIsDecimal('-'), '"-" is not a decimal');
  Assert.IsTrue(StringIsDecimal('0e0'), '"0e0" is a decimal');
  Assert.IsTrue(StringIsDecimal('+0e+0'), '"+0e+0" is a decimal');
  Assert.IsTrue(StringIsDecimal('-0e-0'), '"-0e-0" is a decimal');
  Assert.IsFalse(StringIsDecimal('0e'), '"0e" is not a decimal');
  Assert.IsFalse(StringIsDecimal('e0'), '"e0" is not a decimal');
  Assert.IsTrue(StringIsDecimal('1.2'), '"1.2" is a decimal');
  Assert.IsTrue(StringIsDecimal('-1.2'), '"-1.2" is a decimal');
  Assert.IsTrue(StringIsDecimal('+1.2'), '"+1.2" is a decimal');
  Assert.IsFalse(StringIsDecimal('1. 2'), '"1. 2" is not a decimal');
  Assert.IsFalse(StringIsDecimal('1 .2'), '"1 .2" is not a decimal');
  Assert.IsFalse(StringIsDecimal(' 1.2'), '" 1.2" is not a decimal');
  Assert.IsFalse(StringIsDecimal('1.2 '), '"1.2 " is not a decimal');
  Assert.IsTrue(StringIsDecimal('1.2e2'), '"1.2e2" is a decimal');
  Assert.IsTrue(StringIsDecimal('1.2e-2'), '"1.2e2" is a decimal');
  Assert.IsTrue(StringIsDecimal('1.2e+2'), '"1.2e2" is a decimal');
  Assert.IsFalse(StringIsDecimal('1.2e2e3'), '"1.2e2e3" is not a decimal');
end;

procedure TDecimalTests.TestCardinal(i: cardinal);
var
  i64 : int64;
  d : TSmartDecimal;
begin
  i64 := i;
  d := TSmartDecimal.valueOf(i64);
  Assert.IsTrue(d.AsCardinal = i);
  //check(d.AsInteger = i);
end;

procedure TDecimalTests.TestInt64(i: int64);
var
  d : TSmartDecimal;
begin
  d := TSmartDecimal.valueOf(i);
  Assert.IsTrue(d.AsInt64 = i);
end;

initialization
  TDUnitX.RegisterTestFixture(TDecimalTests);
End.

