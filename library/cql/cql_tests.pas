unit FHIR.Cql.Tests;

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
  {$IFDEF FPC}
  FPCUnit,
  {$ELSE}
  DUnitX.TestFramework,
  {$ENDIF}
  fsl_stream, fsl_tests,
  FHIR.Cql.Model, FHIR.Cql.Parser;

Type
  {$IFNDEF FPC}
  CqlParserTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  {$ENDIF}
   TCqlParserTest = Class (TFslTestCase)
   {$IFNDEF FPC}
   Published
    [CqlParserTestCase]
    {$ENDIF}
    procedure ParserTest(Name : String);
  End;


implementation

{$IFNDEF FPC}

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

{$ENDIF}
{  TCqlParserTest }

procedure  TCqlParserTest.ParserTest(Name: String);
var
  Cql : TCqlElement;
  parser : TCQLParser;
begin
  parser := TCQLParser.create;
  try
    Cql := parser.parseCql(FileToString(name, TEncoding.UTF8));
    try
      assertTrue(cql <> nil);
    finally
      Cql.Free;
    end;
  finally
    parser.Free;
  end;
end;

initialization
  {$IFNDEF FPC}
  {$IFNDEF EXCLUDE_FAILING_TESTS}
  TDUnitX.RegisterTestFixture( TCqlParserTest);
  {$ENDIF}
  {$ENDIF}
end.
