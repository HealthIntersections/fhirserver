unit CqlTests;

interface

uses
  SysUtils, Classes,
  DUnitX.TestFramework,
  TextUtilities,
  CqlModel, CqlParser;

Type
  CqlParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TCqlParserTests = Class (TObject)
  Published
    [CqlParserTestCase]
    procedure ParserTest(Name : String);
  End;


implementation

{ CqlParserTestCaseAttribute }

function CqlParserTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  sl : TStringlist;
  sr : TSearchRec;
  s : String;
  i : integer;
begin
  sl := TStringList.create;
  try
    if FindFirst('C:\work\fhirserver\Libraries\cql\samples\*.Cql', faAnyFile, SR) = 0 then
    repeat
      s := sr.Name;
      sl.Add(sr.Name);
    until FindNext(SR) <> 0;
    setLength(result, sl.Count);
    for i := 0 to sl.Count - 1 do
    begin
      result[i].Name := sl[i];
      SetLength(result[i].Values, 1);
      result[i].Values[0] := 'C:\work\fhirserver\Libraries\cql\samples\' + sl[i];
    end;
  finally
    sl.Free;
  end;
end;


{ TCqlParserTests }

procedure TCqlParserTests.ParserTest(Name: String);
var
  Cql : TCqlElement;
  parser : TCQLParser;
begin
  parser := TCQLParser.create;
  try
    Cql := parser.parseCql(FileToString(name, TEncoding.UTF8));
    try
      Assert.IsNotNull(cql);
    finally
      Cql.Free;
    end;
  finally
    parser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCqlParserTests);
end.
