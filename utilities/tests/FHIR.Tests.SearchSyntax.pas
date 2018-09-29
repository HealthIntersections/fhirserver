unit FHIR.Tests.SearchSyntax;

interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  FHIR.Support.Base,
  FHIR.Server.SearchSyntax;

type
  [TextFixture]
  TFSFilterParserTests = class (TObject)
  private
    procedure test(expression: String);
  public
    [TestCase] procedure testString;
    [TestCase] procedure testToken;
    [TestCase] procedure testURL;
    [TestCase] procedure testDate;
    [TestCase] procedure testSubsumes;
    [TestCase] procedure testSubsumesId;
    [TestCase] procedure testFilter;
    [TestCase] procedure testFilter2;
    [TestCase] procedure testParentheses;
    [TestCase] procedure testPrecedence;
  end;

implementation

{ TFSFilterParserTests }

procedure TFSFilterParserTests.test(expression: String);
var
  filter : TFSFilter;
begin
  filter := TFSFilterParser.parse(expression);
  Assert.IsNotNull(filter, 'parsing failed - returned nil');
  if (filter <> nil) then
    Assert.IsTrue(filter.ToString = expression, 'Expression mismatch: found "'+filter.ToString+'" expecting "'+expression+'"');
  filter.Free;
end;

procedure TFSFilterParserTests.testDate;
begin
  test('date ge 2010-10-10');
end;

procedure TFSFilterParserTests.testFilter;
begin
  test('related[type eq comp].target pr false');
end;

procedure TFSFilterParserTests.testFilter2;
begin
  test('related[type eq comp and this lt that].target pr false');
end;

procedure TFSFilterParserTests.testParentheses;
begin
  test('(userName eq "bjensen") or (code sb snomed|diabetes)');
end;

procedure TFSFilterParserTests.testPrecedence;
begin
  test('this eq that and this1 eq that1');
end;

procedure TFSFilterParserTests.testString;
begin
  test('userName eq "bjensen"');
end;

procedure TFSFilterParserTests.testSubsumes;
begin
  test('code sb snomed|diabetes');
end;

procedure TFSFilterParserTests.testSubsumesId;
begin
  test('code ss snomed|diabetes-NIDDM-stage-1');
end;

procedure TFSFilterParserTests.testToken;
begin
  test('name eq loinc|1234');
end;

procedure TFSFilterParserTests.testURL;
begin
  test('name in http://loinc.org/vs/LP234');
end;

initialization
  TDUnitX.RegisterTestFixture(TFSFilterParserTests);
end.
