unit search_syntax;

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


{$i fhir.inc}

interface

uses
  SysUtils, {$IFDEF DELPHI} RegularExpressions, {$ENDIF}
  fsl_utilities, fsl_base, fsl_fpc,
  fhir_objects;

Type
  TFSCompareOperation = (fscoEQ, fscoNE, fscoCO, fscoSW, fscoEW, fscoGT, fscoLT, fscoGE, fscoLE, fscoPR, fscoPO, fscoSS, fscoSB, fscoIN, fscoRE);
  TFSFilterLogicalOperation = (fsloAnd, fsloOr, fsloNot);

  TFSFilterItemType = (fsitParameter, fsitLogical, fsitGroup);
  TFSFilterValueType = (fvtToken, fvtString, fvtNumberOrDate);

  TFSFilter = class (TFslObject)
  public
    function FilterItemType : TFSFilterItemType; virtual; abstract;
  end;

  TFSFilterParameterPath = class (TFslObject)
  private
    FName : String;
    FFilter: TFSFilter;
    FNext: TFSFilterParameterPath;
    procedure SetFilter(const Value: TFSFilter);
    procedure SetNext(const Value: TFSFilterParameterPath);
  public
    destructor Destroy; override;

    Property Name : String read FName write FName;
    Property Filter : TFSFilter read FFilter write SetFilter;
    Property Next : TFSFilterParameterPath read FNext write SetNext;
    function ToString : String; override;
  end;

  TFSFilterParameterGroup = class (TFSFilter)
  private
    FContained: TFSFilter;
    procedure SetContained(const Value: TFSFilter);
  public
    destructor Destroy; override;
    property contained : TFSFilter read FContained write SetContained;
    function FilterItemType : TFSFilterItemType; override;
    function ToString : String; override;
  end;

  TFSFilterParameter = class (TFSFilter)
  private
    FParamPath : TFSFilterParameterPath;
    FOperation : TFSCompareOperation;
    FValue : String;
    FValueType : TFSFilterValueType;
    procedure SetParamPath(const Value: TFSFilterParameterPath);
  public
    destructor Destroy; override;
    function FilterItemType : TFSFilterItemType; override;

    Property ParamPath : TFSFilterParameterPath read FParamPath write SetParamPath;
    Property Operation : TFSCompareOperation read FOperation write FOperation;
    Property Value : String read FValue write FValue;
    function ToString : String; override;
  end;

  TFSFilterLogical = class (TFSFilter)
  private
    FFilter1 : TFSFilter;
    FOperation : TFSFilterLogicalOperation;
    FFilter2 : TFSFilter;
    procedure SetFilter1(const Value: TFSFilter);
    procedure SetFilter2(const Value: TFSFilter);
  public
    destructor Destroy; override;
    function FilterItemType : TFSFilterItemType; override;

    property Filter1 : TFSFilter read FFilter1 write SetFilter1;
    property Operation : TFSFilterLogicalOperation read FOperation write FOperation;
    property Filter2 : TFSFilter read FFilter2 write SetFilter2;
    function ToString : String; override;
  end;


  TFSFilterLexType = (fsltEnded, fsltName, fsltString, fsltNUmber, fsltDot, fsltOpen, fsltClose, fsltOpenSq, fsltCloseSq);

  TFSFilterParser = class (TFslObject)
  private
    original : String;
    cursor: integer;

    function IsDate(s : String): boolean;
    function peek : TFSFilterLexType;
    function peekCh : String;
    function ConsumeName : String;
    function ConsumeToken : String;
    function ConsumeNumberOrDate : String;
    function ConsumeString : String;

    function parse : TFSFilter; overload;
    function parseOpen : TFSFilter;
    function parseLogical(filter : TFSFilter) : TFSFilter;
    function parsePath(name : String) : TFSFilterParameterPath;
    function parseParameter(name: String): TFSFilter;

  public
    class function parse(expression : String) : TFSFilter; overload;
  end;

  TFSCharIssuer = class (TFslObject)
  private
    cursor : char;
  public
    constructor Create; override;
    function next : char;
  end;

const
  CODES_CompareOperation : array [TFSCompareOperation] of string = ('eq', 'ne', 'co', 'sw', 'ew', 'gt', 'lt', 'ge', 'le', 'pr', 'po', 'ss', 'sb', 'in', 're');
  CODES_LogicalOperation : array [TFSFilterLogicalOperation] of string = ('and', 'or', 'not');
  XML_DATE_PATTERN = '[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?';

implementation

{ TFSFilterParameterPath }

destructor TFSFilterParameterPath.Destroy;
begin
  FFilter.Free;
  FNext.Free;
  inherited;
end;

procedure TFSFilterParameterPath.SetFilter(const Value: TFSFilter);
begin
  FFilter.Free;
  FFilter := Value;
end;

procedure TFSFilterParameterPath.SetNext(const Value: TFSFilterParameterPath);
begin
  FNext.Free;
  FNext := Value;
end;

function TFSFilterParameterPath.ToString: String;
begin
  if Filter <> nil then
    result := Name+'['+Filter.ToString+']'
  else
    result := Name;
  if Next <> nil then
    result := result+'.'+Next.ToString;
end;

{ TFSFilterParameter }

destructor TFSFilterParameter.Destroy;
begin
  FParamPath.Free;
  inherited;
end;

function TFSFilterParameter.FilterItemType: TFSFilterItemType;
begin
  result := fsitParameter;
end;

procedure TFSFilterParameter.SetParamPath(const Value: TFSFilterParameterPath);
begin
  FParamPath.Free;
  FParamPath := Value;
end;

function TFSFilterParameter.ToString: String;
begin
  if FValueType = fvtString then
    result := ParamPath.ToString+' '+CODES_CompareOperation[Operation]+' "'+Value+'"'
  else
    result := ParamPath.ToString+' '+CODES_CompareOperation[Operation]+' '+Value;
end;

{ TFSFilterLogical }

destructor TFSFilterLogical.Destroy;
begin
  FFilter1.Free;
  FFilter2.Free;
  inherited;
end;

function TFSFilterLogical.FilterItemType: TFSFilterItemType;
begin
  result := fsitLogical;
end;

procedure TFSFilterLogical.SetFilter1(const Value: TFSFilter);
begin
  FFilter1.Free;
  FFilter1 := Value;
end;

procedure TFSFilterLogical.SetFilter2(const Value: TFSFilter);
begin
  FFilter2.Free;
  FFilter2 := Value;
end;

function TFSFilterLogical.ToString: String;
begin
  result := Filter1.ToString+' '+CODES_LogicalOperation[Operation]+' '+Filter2.ToString;
end;

{ TFSFilterParser }

class function TFSFilterParser.parse(expression: String): TFSFilter;
var
  this : TFSFilterParser;
begin
  this := TFSFilterParser.Create;
  try
    this.original := expression;
    this.cursor := 1;
    result := this.parse;
  finally
    this.Free;
  end;
end;

function TFSFilterParser.parse : TFSFilter;
begin
  result := parseOpen;
  if cursor <= length(original) then
  begin
    result.Free;
    raise EFHIRException.create('Expression did not terminate at '+inttostr(cursor));
  end;
end;

function TFSFilterParser.parseOpen: TFSFilter;
var
  s : String;
  grp : TFSFilterParameterGroup;
begin
  if peek = fsltOpen then
  begin
    inc(Cursor);
    grp := TFSFilterParameterGroup.Create;
    try
      grp.contained := parseOpen;
      if peek <> fsltClose then
        raise EFHIRException.create('Expected '')'' at '+inttostr(cursor)+' but found "'+peekCh+'"');
      inc(cursor);
      case peek of
        fsltName : result := parseLogical(grp);
        fsltEnded, fsltClose, fsltCloseSq : result := grp.Link as TFSFilter;
      else
        raise EFHIRException.create('Unexpected Character "'+PeekCh+'" at '+inttostr(cursor));
      end;
    finally
      grp.Free;
    end;
  end
  else
  begin
    s := ConsumeName;
    if s = 'not' then
      result := parseLogical(nil)
    else
      result := parseParameter(s);
  end;
end;

function TFSFilterParser.parseParameter(name : String): TFSFilter;
var
  s : String;
  i : integer;
  filter : TFSFilterParameter;
begin
  filter := TFSFilterParameter.Create;
  try
    // 1. the path
    filter.ParamPath := parsePath(name);

    if peek <> fsltName then
      raise EFHIRException.create('Unexpected Character "'+PeekCh+'" at '+inttostr(cursor));
    s := ConsumeName;
    i := StringArrayIndexOfSensitive(CODES_CompareOperation, s);
    if (i < 0) then
      raise EFHIRException.create('Unknown operation "'+s+'" at '+inttostr(cursor));
    filter.FOperation := TFSCompareOperation(i);

    case peek of
      fsltName :
        begin
        filter.FValue := ConsumeToken;
        filter.FValueType := fvtToken;
        end;
      fsltNumber :
        begin
        filter.FValue := ConsumeNumberOrDate;
        filter.FValueType := fvtNumberOrDate;
        end;
      fsltString :
        begin
        filter.FValue := ConsumeString;
        filter.FValueType := fvtString;
        end
    else
      raise EFHIRException.create('Unexpected Character "'+PeekCh+'" at '+inttostr(cursor));
    end;

    // check operation / value type results
    case Filter.FOperation of
      fscoPR: if (filter.FValue <> 'true') and (filter.FValue <> 'false') then raise EFHIRException.create('Value "'+filter.Value+'" not valid for Operation '+CODES_CompareOperation[filter.Operation]+' at '+inttostr(cursor));
      fscoPO: if not IsDate(filter.FValue) then raise EFHIRException.create('Value "'+filter.Value+'" not valid for Operation '+CODES_CompareOperation[filter.Operation]+' at '+inttostr(cursor));
    end;

    case peek of
      fsltName : result := parseLogical(filter);
      fsltEnded, fsltClose, fsltCloseSq : result := filter.Link as TFSFilter;
    else
      raise EFHIRException.create('Unexpected Character "'+PeekCh+'" at '+inttostr(cursor));
    end;
  finally
    filter.Free;
  end;
end;

function TFSFilterParser.parseLogical(filter: TFSFilter): TFSFilter;
var
  s : String;
  logical : TFSFilterLogical;
begin
  if (filter = nil) then
    s := 'not'
  else
    s := ConsumeName;
  if (s <> 'or') and (s <> 'and') and (s <> 'not') then
    raise EFHIRException.create('Unexpected Name "'+s+'" at '+inttostr(cursor));

  logical:= TFSFilterLogical.Create;
  try
    logical.FFilter1 := filter.Link as TFSFilter;
    if s = 'or' then
      logical.FOperation := fsloOr
    else if s = 'not' then
      logical.FOperation := fsloNot
    else
      logical.FOperation := fsloAnd;
    logical.FFilter2 := parseOpen;

    result := logical.Link as TFSFilter;
  finally
    logical.Free;
  end;
end;

function TFSFilterParser.parsePath(name: String): TFSFilterParameterPath;
begin
  result := TFSFilterParameterPath.Create;
  try
    result.Name := name;
    if peek = fsltOpenSq then
    begin
      inc(Cursor);
      result.FFilter := parseOpen;
      if peek <> fsltCloseSq then
        raise EFHIRException.create('Expected '']'' at '+inttostr(cursor)+' but found "'+peekCh+'"');
      inc(cursor);
    end;
    if peek = fsltDot then
    begin
      inc(Cursor);
      if peek <> fsltName then
        raise EFHIRException.create('Unexpected Character "'+PeekCh+'" at '+inttostr(cursor));
      result.FNext := parsePath(ConsumeName);
    end
    else if result.FFilter <> nil then
      raise EFHIRException.create('Expected ''.'' at '+inttostr(cursor)+' but found "'+peekCh+'"');

    result.Link;
  finally
    result.Free;
  end;

end;

function TFSFilterParser.peek: TFSFilterLexType;
begin
  while (cursor <= length(original)) and (original[cursor] = ' ') do
    inc(cursor);

  if cursor > length(original) then
    result := fsltEnded
  else
   case original[cursor] of
     'a'..'z', 'A'..'Z', '_' : result := fsltName;
     '0'..'9' : result := fsltNumber;
     '"' : result := fsltString;
     '.' : result := fsltDot;
     '(' : result := fsltOpen;
     ')' : result := fsltClose;
     '[' : result := fsltOpenSq;
     ']' : result := fsltCloseSq;
   else
     raise EFHIRException.create('Unknown Character "'+PeekCh+'"  at '+inttostr(cursor));
   end;
end;

function TFSFilterParser.peekCh: String;
begin
  if cursor > length(original) then
    result := '[end!]'
  else
    result := original[cursor];
end;

function TFSFilterParser.ConsumeName: String;
var
  i : integer;
begin
  i := cursor;
  repeat
    inc(i);
  until (i > length(original)) or not CharInSet(original[i], ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', ':']);
  result := copy(original, cursor, i - cursor);
  cursor := i;
end;

function TFSFilterParser.ConsumeNumberOrDate: String;
var
  i : integer;
begin
  i := cursor;
  repeat
    inc(i);
  until (i > length(original)) or not CharInSet(original[i], ['0'..'9', '.', '-', ':', '+', 'T']);
  result := copy(original, cursor, i - cursor);
  cursor := i;
end;

function TFSFilterParser.ConsumeString: String;
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
        raise EFHIRException.create('Unknown escape sequence at '+inttostr(cursor));
    end;
    inc(cursor);
  end;
  SetLength(result, l);
  if (cursor > length(original)) or (original[cursor] <> '"') then
    raise EFHIRException.create('Problem with string termination at '+inttostr(cursor));
  if result = '' then
    raise EFHIRException.create('Problem with string at '+inttostr(cursor)+': cannot be empty');

  inc(cursor);
end;

function TFSFilterParser.ConsumeToken: String;
var
  i : integer;
begin
  i := cursor;
  repeat
    inc(i);
  until (i > length(original)) or (Ord(original[i]) <= 32) or StringIsWhitespace(original[i]) or (original[i] = ')') or (original[i] = ']');
  result := copy(original, cursor, i - cursor);
  cursor := i;
end;


function TFSFilterParser.IsDate(s: String): boolean;
var
  reg :  TRegex;
begin
  reg := TRegex.Create(XML_DATE_PATTERN);
  result := reg.IsMatch(s);
end;

{ TFSCharIssuer }

constructor TFSCharIssuer.Create;
begin
  inherited;
  cursor := 'a';
end;

function TFSCharIssuer.next: char;
begin
  result := cursor;
  inc(cursor);
end;


{ TFSFilterParameterGroup }

destructor TFSFilterParameterGroup.Destroy;
begin
  FContained.Free;
  inherited;
end;

function TFSFilterParameterGroup.FilterItemType: TFSFilterItemType;
begin
  result := fsitGroup;
end;

procedure TFSFilterParameterGroup.SetContained(const Value: TFSFilter);
begin
  FContained.Free;
  FContained := Value;
end;

function TFSFilterParameterGroup.ToString: String;
begin
  result := '('+FContained.ToString+')';
end;

end.


