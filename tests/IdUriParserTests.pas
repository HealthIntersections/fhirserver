unit IdUriParserTests;

interface

uses
  Windows, Sysutils, DUnitX.TestFramework,
  IdUri;

type
  [TextFixture]
  TIdUriParserTests = Class (TObject)
  private
    procedure ok(uri : String);
    procedure fail(uri : String);
  public
    [TestCase] Procedure TestOK;
    [TestCase] Procedure TestFail;
    [TestCase] Procedure TestUnicode1;
    [TestCase] Procedure TestUnicode2;
  end;


implementation


//

{ TIdUriParserTests }

procedure TIdUriParserTests.ok(uri: String);
var
  o : TIdUri;
begin
  o := TIdUri.create(uri);
  try
    Assert.IsTrue(o <> nil);
  finally
    o.free;
  end;
end;

procedure TIdUriParserTests.fail(uri: String);
var
  o : TIdUri;
begin
  o := TIdUri.create(uri);
  try
    Assert.IsTrue(false);
  finally
    o.free;
  end;
end;

procedure TIdUriParserTests.TestFail;
begin
  ok('http://foo@127.0.0.1 @google.com/');
end;

procedure TIdUriParserTests.TestOK;
begin
  ok('http://test.fhir.org/r3');
end;

procedure TIdUriParserTests.TestUnicode1;
begin
  ok('http://orange.tw/sandbox/ＮＮ/passwd');
end;

procedure TIdUriParserTests.TestUnicode2;
begin
  ok('http://orange.tw/sandbox/%EF%BC%AE%EF%BC%AE/passwd');
end;

initialization
  TDUnitX.RegisterTestFixture(TIdUriParserTests);
end.
