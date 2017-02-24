unit IETFLangTests;

interface

uses
  Windows, Sysutils, DUnitX.TestFramework,
  TextUtilities,
  IETFLanguageCodeServices;


type
  [TextFixture]
  TIETFLangTests = Class (TObject)
  private
    FDefinitions : TIETFLanguageDefinitions;
    procedure pass(code : String);
    procedure fail(code : String);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [TestCase] Procedure TestSimple;
    [TestCase] Procedure TestWrong;
  end;

implementation

{ TIETFLangTests }

procedure TIETFLangTests.fail(code : String);
var
  msg : String;
  o : TIETFLanguageCodeConcept;
begin
  o := FDefinitions.parse(code, msg);
  try
    Assert.IsNull(o);
    Assert.IsNotEmpty(msg);
  finally
    o.Free;
  end;
end;

procedure TIETFLangTests.pass(code : String);
var
  msg : String;
  o : TIETFLanguageCodeConcept;
begin
  o := FDefinitions.parse(code, msg);
  try
    Assert.IsNotNull(o);
    Assert.IsEmpty(msg);
  finally
    o.Free;
  end;
end;

procedure TIETFLangTests.Setup;
begin
  FDefinitions := TIETFLanguageDefinitions.create(FileToString('C:\work\fhirserver\sql\lang.txt', TEncoding.ASCII));
end;

procedure TIETFLangTests.TearDown;
begin
  FDefinitions.Free;
end;

procedure TIETFLangTests.TestSimple;
begin
  pass('en');
  pass('en-AU');
  pass('en-Latn-AU');
end;

procedure TIETFLangTests.TestWrong;
begin
  fail('enAU');
  fail('en-AUA');
end;

initialization
  TDUnitX.RegisterTestFixture(TIETFLangTests);
end.
