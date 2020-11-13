unit scim_search;

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

{$I fhir.inc}

interface

uses
  SysUtils,
   fsl_base;

Type
  TSCIMSearchItemType = (sitTest, sitCriteria, sitValuePath);

  TSCIMSearchFilterOperation = (sfoEq, sfoNe, sfoCo, sfoSw, sfoEw, sfoGt, sfoLt, sfoGe, sfoLe, sfoPr);
  TSCIMSearchFilterLogicalOperation = (sfloAnd, sfloOr, sfloNot);

  TSCIMSearchFilter = class (TFslObject)
  public
    function SearchItemType : TSCIMSearchItemType; virtual;
  end;

  TSCIMSearchFilterTest = class (TSCIMSearchFilter)
  private
    FAttributePath : String;
    FOperation : TSCIMSearchFilterOperation;
    FValue : String;
  public
    function SearchItemType : TSCIMSearchItemType; override;
    Property AttributePath : String read FAttributePath write FAttributePath;
    Property Operation : TSCIMSearchFilterOperation read FOperation write FOperation;
    Property Value : String read FValue write FValue;
  end;

  TSCIMSearchFilterCriteria = class (TSCIMSearchFilter)
  private
    FCriterion1 : TSCIMSearchFilter;
    FOperation : TSCIMSearchFilterLogicalOperation;
    FCriterion2 : TSCIMSearchFilter;
    procedure SetCriterion1(const Value: TSCIMSearchFilter);
    procedure SetCriterion2(const Value: TSCIMSearchFilter);
  public
    destructor Destroy; override;

    function SearchItemType : TSCIMSearchItemType; override;
    property Criterion1 : TSCIMSearchFilter read FCriterion1 write SetCriterion1;
    property Operation : TSCIMSearchFilterLogicalOperation read FOperation write FOperation;
    property Criterion2 : TSCIMSearchFilter read FCriterion2 write SetCriterion2;
  end;

  TSCIMSearchFilterValuePath = class (TSCIMSearchFilter)
  private
    FAttributePath : String;
    FFilter : TSCIMSearchFilter;
    procedure SetFilter(const Value: TSCIMSearchFilter);
  public
    destructor Destroy; override;
    function SearchItemType : TSCIMSearchItemType; override;
    Property AttributePath : String read FAttributePath write FAttributePath;
    property Filter : TSCIMSearchFilter read FFilter write SetFilter;
  end;

  TSCIMSearchLexType = (sltEnded, sltName, sltString, sltNUmber, sltOpen, sltClose, sltOpenSq, sltCloseSq);


  TSCIMSearchParser = class (TFslObject)
  private
    original : String;
    cursor: integer;
    filter : TSCIMSearchFilter;

    function peek : TSCIMSearchLexType;
    function ConsumeName : String;
    function ConsumeNumber : String;
    function ConsumeString : String;

    procedure parse; overload;
    function parseOpen : TSCIMSearchFilter;
    function parseLogical(test : TSCIMSearchFilter) : TSCIMSearchFilter;
    function parseValuePath(path : String) : TSCIMSearchFilter;

  public
    destructor Destroy; override;
    class procedure runTests;
    class procedure test(expression : String);
    class function parse(expression : String) : TSCIMSearchFilter; overload;
  end;

implementation

{ TSCIMSearchFilterCriteria }

destructor TSCIMSearchFilterCriteria.Destroy;
begin
  FCriterion1.Free;
  FCriterion2.Free;
  inherited;
end;

function TSCIMSearchFilterCriteria.SearchItemType: TSCIMSearchItemType;
begin
  result := sitCriteria
end;

procedure TSCIMSearchFilterCriteria.SetCriterion1(const Value: TSCIMSearchFilter);
begin
  FCriterion1.Free;
  FCriterion1 := Value;
end;

procedure TSCIMSearchFilterCriteria.SetCriterion2(const Value: TSCIMSearchFilter);
begin
  FCriterion2.Free;
  FCriterion2 := Value;
end;

{ TSCIMSearchFilterValuePath }

destructor TSCIMSearchFilterValuePath.Destroy;
begin
  FFilter.Free;
  inherited;
end;

function TSCIMSearchFilterValuePath.SearchItemType: TSCIMSearchItemType;
begin
  result := sitValuePath;
end;

procedure TSCIMSearchFilterValuePath.SetFilter(const Value: TSCIMSearchFilter);
begin
  FFilter.Free;
  FFilter := Value;
end;

{ TSCIMSearchFilter }

function TSCIMSearchFilter.SearchItemType: TSCIMSearchItemType;
begin
  result := sitTest;
end;

{ TSCIMSearchFilterTest }

function TSCIMSearchFilterTest.SearchItemType: TSCIMSearchItemType;
begin
  result := sitTest;
end;

{ TSCIMSearchParser }

destructor TSCIMSearchParser.Destroy;
begin
  filter.free;
  inherited;
end;

class procedure TSCIMSearchParser.runTests;
begin
  test('userName eq "bjensen"');
  test('name.familyName co "O''Malley"');
  test('userName sw "J"');
  test('title pr');
  test('meta.lastModified gt "2011-05-13T04:42:34Z"');
  test('meta.lastModified ge "2011-05-13T04:42:34Z"');
  test('meta.lastModified lt "2011-05-13T04:42:34Z"');
  test('meta.lastModified le "2011-05-13T04:42:34Z"');
  test('title pr and userType eq "Employee"');
  test('title pr or userType eq "Intern"');
  test('schemas eq "urn:scim:schemas:extension:enterprise:2.0:User"');
  test('userType eq "Employee" and (emails co "example.com" or emails co "example.org")');
  test('userType ne "Employee" and not (emails co "example.com" or emails co "example.org")');
  test('userType eq "Employee" and (emails.type eq "work")');
  test('userType eq "Employee" and emails[type eq "work" and value co "@example.com"]');
  test('emails[type eq "work" and value co "@example.com"] or ims[type eq "xmpp" and value co "@foo.com"]');
  test('addresses[state eq "CA" and rooms[type eq "bedroom" and number gt 2]]');
end;

class procedure TSCIMSearchParser.test(expression: String);
var
  filter : TSCIMSearchFilter;
begin
  filter := parse(expression);
  if filter = nil then
    raise ELibraryException.create('parsing failed - returned nil');
  filter.Free;
end;

class function TSCIMSearchParser.parse(expression: String): TSCIMSearchFilter;
var
  this : TSCIMSearchParser;
begin
  this := TSCIMSearchParser.Create;
  try
    this.original := expression;
    this.cursor := 1;
    this.parse;
    result := this.filter.link as TSCIMSearchFilter;
  finally
    this.Free;
  end;
end;


procedure TSCIMSearchParser.parse;
begin
  filter := parseOpen;
  if cursor <= length(original) then
    raise ELibraryException.create('Expression did not terminate at '+inttostr(cursor));
end;

function TSCIMSearchParser.peek: TSCIMSearchLexType;
begin
  while (cursor <= length(original)) and (original[cursor] = ' ') do
    inc(cursor);

  if cursor > length(original) then
    result := sltEnded
  else
   case original[cursor] of
     'a'..'z', 'A'..'Z' : result := sltName;
     '0'..'9' : result := sltNUmber;
     '"' : result := sltString;
     '(' : result := sltOpen;
     ')' : result := sltClose;
     '[' : result := sltOpenSq;
     ']' : result := sltCloseSq;
   else
     raise ELibraryException.create('Unknown Character at '+inttostr(cursor));
   end;
end;

function TSCIMSearchParser.parseOpen: TSCIMSearchFilter;
var
  ap, s : String;
  test : TSCIMSearchFilterTest;
begin
  if peek = sltOpen then
  begin
    inc(Cursor);
    result := parseOpen;
    try
      if peek <> sltClose then
        raise ELibraryException.create('Expected '')'' at '+inttostr(cursor));
      inc(cursor);
      result.Link;
    finally
      result.Free;
    end;
  end
  else
  begin
    ap := ConsumeName;
    if ap = 'not' then
      result := parseLogical(nil)
    else
    begin
      if peek  = sltOpenSq then
        result := parseValuePath(ap)
      else
      begin
        if peek <> sltName then
          raise ELibraryException.create('Unexpected Character at '+inttostr(cursor));

        test := TSCIMSearchFilterTest.Create;
        try
          test.FAttributePath := ap;
          s := ConsumeName;
          if s = 'eq' then test.FOperation := sfoEq
          else if s = 'ne' then test.FOperation := sfoNe
          else if s = 'co' then test.FOperation := sfoCo
          else if s = 'sw' then test.FOperation := sfoSw
          else if s = 'ew' then test.FOperation := sfoEw
          else if s = 'gt' then test.FOperation := sfoGt
          else if s = 'lt' then test.FOperation := sfoLt
          else if s = 'ge' then test.FOperation := sfoGe
          else if s = 'le' then test.FOperation := sfoLe
          else if s = 'pr' then test.FOperation := sfoPr
          else
            raise ELibraryException.create('Unknown operation '+s+' at '+inttostr(cursor));

          if test.FOperation <> sfoPr then
          begin
            case peek of
              sltName :
                begin
                test.FValue := ConsumeName;
                if (test.Value <> 'null') and (test.Value <> 'true')  and (test.Value <> 'false')  then
                  raise ELibraryException.create('Unexpected value '''+test.value+''' at '+inttostr(cursor));
                end;
              sltNumber : test.FValue := ConsumeNumber;
              sltString: test.FValue := ConsumeString;
            else
              raise ELibraryException.create('Unexpected Character at '+inttostr(cursor));
            end;
          end;

          case peek of
            sltName : result := parseLogical(test);
            sltEnded, sltClose, sltCloseSq : result := test.Link as TSCIMSearchFilterTest;
          else
            raise ELibraryException.create('Unexpected Character at '+inttostr(cursor));
          end;
        finally
          test.free;
        end;
      end;
    end;
  end;
end;

function TSCIMSearchParser.parseValuePath(path: String): TSCIMSearchFilter;
var
  vp : TSCIMSearchFilterValuePath;
begin
  vp := TSCIMSearchFilterValuePath.Create;
  try
    vp.FAttributePath := path;
    inc(cursor);
    vp.FFilter := parseOpen;
    if peek <> sltCloseSq then
      raise ELibraryException.create('Problem with filter termination at '+inttostr(cursor));
    inc(cursor);
    case peek of
      sltName : result := parseLogical(vp);
      sltEnded, sltClose, sltCloseSq : result := vp.Link as TSCIMSearchFilter;
    else
      raise ELibraryException.create('Unexpected Character at '+inttostr(cursor));
    end;
  finally
    vp.Free;
  end;
end;

function TSCIMSearchParser.ConsumeName: String;
var
  i : integer;
begin
  i := cursor;
  repeat
    inc(i);
  until (i > length(original)) or not CharInSet(original[i], ['a'..'z', 'A'..'Z', '.', '_', ':']);
  result := copy(original, cursor, i - cursor);
  cursor := i;
end;

function TSCIMSearchParser.ConsumeNumber: String;
var
  i : integer;
begin
  i := cursor;
  repeat
    inc(i);
  until (i > length(original)) or not CharInSet(original[i], ['0'..'9', '.']);
  result := copy(original, cursor, i - cursor);
  cursor := i;
end;

function TSCIMSearchParser.ConsumeString: String;
var
  l : integer;
begin
  inc(cursor);
  setLength(result, length(original)); // can't be longer than that
  l := 0;
  while (cursor <= length(original)) and (original[cursor] <> '"') do
  begin
    inc(l);
    if (cursor < length(original)) and (original[cursor] <> '\') then
      result[l] := original[cursor]
    else
    begin
      inc(cursor);
      if (original[cursor] = '"') then
        result[l] := '"'
      else if (original[cursor] = 't') then
        result[l] := #9
      else if (original[cursor] = 'r') then
        result[l] := #13
      else if (original[cursor] = 'n') then
        result[l] := #10
      else
        raise ELibraryException.create('Unknown escape sequence at '+inttostr(cursor));
    end;
    inc(cursor);
  end;
  SetLength(result, l);
  if (cursor > length(original)) or (original[cursor] <> '"') then
    raise ELibraryException.create('Problem with string termination at '+inttostr(cursor));
  inc(cursor);
end;

function TSCIMSearchParser.parseLogical(test: TSCIMSearchFilter): TSCIMSearchFilter;
var
  s : String;
  criteria : TSCIMSearchFilterCriteria;
begin
  if (test = nil) then
    s := 'not'
  else
    s := ConsumeName;
  if (s <> 'or') and (s <> 'and') and (s <> 'not') then
    raise ELibraryException.create('Unexpected Name at '+inttostr(cursor));
  criteria:= TSCIMSearchFilterCriteria.Create;
  try
    criteria.FCriterion1 := test.Link as TSCIMSearchFilter;
    if s = 'or' then
      criteria.FOperation := sfloOr
    else if s = 'not' then
      criteria.FOperation := sfloNot
    else
      criteria.FOperation := sfloAnd;
    criteria.FCriterion2 := parseOpen;

    result := criteria.Link as TSCIMSearchFilter;
  finally
    criteria.Free;
  end;
end;


end.
