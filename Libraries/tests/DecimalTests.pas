{! 1 !}
{0.00-001  09 Nov 04 08:47  [21370]  User: Grahame Grieve    fix for new path}

Unit DecimalTests;

Interface

Uses
  SysUtils,
  StringSupport,
  DecimalSupport,
  AdvObjects;

Type
  TDecimalTests = Class (TAdvObject) // but can be used with DUnit
  Private
    procedure check(b : boolean);
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
  Published
    Class procedure runTests;

    Procedure TestAsInteger;
    Procedure TestStringSupport;
    Procedure TestAddition;
    Procedure TestMultiplication;
  End;

Implementation


{ TDecimalTests }

procedure TDecimalTests.testString(s, st, std: String);
var
  ctxt : TSmartDecimalContext;
  dec : TSmartDecimal;
  s1, s2 : String;
begin
  ctxt := TSmartDecimalContext.create;
  try
    dec := ctxt.value(s);
    s1 := dec.AsString;
    s2 := dec.AsScientific;
    check(s1 = st);
    check(s2 = std);
    dec := ctxt.value(std);
    s1 := dec.AsDecimal;
    check(s1 = st);
  finally
    ctxt.Free;
  end;
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


procedure TDecimalTests.check(b: boolean);
begin
  if not b then
    raise Exception.Create('test failed');
end;

class procedure TDecimalTests.runTests;
var
  this : TDecimalTests;
begin
  this := TDecimalTests.Create;
  try
    this.TestAsInteger;
    this.TestStringSupport;
    this.TestAddition;
    this.TestMultiplication;
  finally
    this.free;
  end;
end;

procedure TDecimalTests.TestAdd(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.Value(s1);
    o2 := ctxt.Value(s2);
    o3 := o1.add(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
end;

procedure TDecimalTests.TestSubtract(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.value(s1);
    o2 := ctxt.value(s2);
    o3 := o1.Subtract(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
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
  TestDivide('10', '3', '3.3');
  TestDivide('10.0', '3', '3.33');
  TestDivide('10.00', '3', '3.333');
  TestDivide('10.00', '3.0', '3.3');
  TestDivide('100', '1', '100');
  TestDivide('1000', '10', '100');
  TestDivide('100001', '10', '10000.1');
  TestDivide('100', '10', '10');
  TestDivide('1', '10', '0.1');
  TestDivide('1', '15', '0.067');
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

  TestDivide('10',  '3', '3.3');
  TestDivide('10.0',  '3', '3.33');
  TestDivide('10.00',  '3', '3.333');
  TestDivide('10.00',  '3.0', '3.3');
  TestDivide('10',  '3.0', '3.3');
end;

procedure TDecimalTests.TestMultiply(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.Value(s1);
    o2 := ctxt.Value(s2);
    o3 := o1.Multiply(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
end;

procedure TDecimalTests.TestDivide(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.value(s1);
    o2 := ctxt.value(s2);
    o3 := o1.Divide(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
end;

procedure TDecimalTests.testTrunc(s1, s2: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2 : TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.Create;
  try
    o1 := ctxt.value(s1);
    o2 := o1.Trunc;
    check(o2.AsDecimal = s2);
  Finally
    ctxt.Free;
  End;
end;

procedure TDecimalTests.TestDivInt(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.value(s1);
    o2 := ctxt.value(s2);
    o3 := o1.DivInt(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
end;

procedure TDecimalTests.TestModulo(s1, s2, s3: String);
var
  ctxt : TSmartDecimalContext;
  o1, o2, o3: TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  try
    o1 := ctxt.value(s1);
    o2 := ctxt.value(s2);
    o3 := o1.Modulo(o2);
    check(o3.AsDecimal = s3);
  Finally
    ctxt.Free;
  End;
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

procedure TDecimalTests.TestInteger(i: integer);
var
  ctxt : TSmartDecimalContext;
  d : TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.create;
  Try
    d := ctxt.value(i);
    check(d.AsInteger = i);
  Finally
    ctxt.free;
  End;
end;

procedure TDecimalTests.TestCardinal(i: cardinal);
var
  i64 : int64;
  ctxt : TSmartDecimalContext;
  d : TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.Create;
  try
    i64 := i;
    d := ctxt.value(i64);
    check(d.AsCardinal = i);
    //check(d.AsInteger = i);
  Finally
    ctxt.free;
  End;
end;

procedure TDecimalTests.TestInt64(i: int64);
var
  ctxt : TSmartDecimalContext;
  d : TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.Create;
  try
    d := ctxt.value(i);
    check(d.AsInt64 = i);
  Finally
    ctxt.free;
  End;
end;

End.

