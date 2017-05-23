unit SnomedExpressions;

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


interface

uses
  SysUtils, Classes, AdvObjects, AdvGenerics, Generics.Defaults;

const
  MAX_TERM_LENGTH = 1024;
  NO_REFERENCE : cardinal = MAXINT;

Type
  ESnomedServices = class (Exception);
  ESnomedParser = class (Exception);

  TSnomedRefinementGroup = class;
  TSnomedRefinement = class;

  TSnomedExpressionBase = class (TAdvObject)
  private
    Fstop: integer;
    Fstart: integer;
  public
    Function Link : TSnomedExpressionBase; overload;
    property start : integer read Fstart write Fstart;
    property stop : integer read Fstop write Fstop;
  end;

  TSnomedConcept = class (TSnomedExpressionBase)
  private
    FReference : cardinal;
    Fcode: String;
    Fdescription: String;
    FLiteral: String;
    FDecimal: String;
  public
    Constructor Create; overload; override;
    Constructor Create(reference : cardinal); overload;

    Function Link : TSnomedConcept; overload;
    Property code : String read Fcode write Fcode;
    Property literal : String read FLiteral write FLiteral;
    property decimal : String read FDecimal write FDecimal;
    Property description : String read Fdescription write Fdescription;
    Property reference : cardinal read FReference write FReference;

    function matches(other : TSnomedConcept) : boolean;
    function describe : String;
    function compare(other : TSnomedConcept): integer;
    function canonical : TSnomedConcept;
    procedure copyFrom(other : TSnomedConcept);
  end;

  TSnomedExpressionStatus = (SnomedExpressionStatusUnknown, SnomedExpressionStatusEquivalent, SnomedExpressionStatusSubsumedBy);

  TSnomedExpression = class (TSnomedExpressionBase)
  private
    FrefinementGroups: TAdvList<TSnomedRefinementGroup>;
    Frefinements: TAdvList<TSnomedRefinement>;
    Fconcepts: TAdvList<TSnomedConcept>;
    FStatus: TSnomedExpressionStatus;
    function GetRefinementGroups: TAdvList<TSnomedRefinementGroup>;
    function GetRefinements: TAdvList<TSnomedRefinement>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedExpression; overload;
    Property status : TSnomedExpressionStatus read FStatus write FStatus;
    Property concepts : TAdvList<TSnomedConcept> read Fconcepts;
    Property refinements : TAdvList<TSnomedRefinement> read GetRefinements;
    Property refinementGroups : TAdvList<TSnomedRefinementGroup> read GetRefinementGroups;

    function HasRefinements : boolean;
    function HasRefinementGroups : boolean;
    function isSimple : boolean;
    function isComplex : boolean;
    function hasConcept(concept : TSnomedConcept) : boolean; overload;
    function hasConcept(concept : Cardinal) : boolean; overload;
    function hasRefinement(refinement : TSnomedRefinement) : boolean;
    function hasRefinementGroup(refinementGroup : TSnomedRefinementGroup) : boolean;
    function matches(other : TSnomedExpression) : string;
    function describe : String;
    function canonical : TSnomedExpression;
    procedure merge(exp : TSnomedExpression);
  end;

  TSnomedRefinement = class (TSnomedExpressionBase)
  private
    Fname: TSnomedConcept;
    Fvalue: TSnomedExpression;
    procedure SetName(const Value: TSnomedConcept);
    procedure SetValue(const Value: TSnomedExpression);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedRefinement; overload;
    Property name : TSnomedConcept read Fname write SetName;
    Property value : TSnomedExpression read Fvalue write SetValue;

    function matches(other : TSnomedRefinement) : boolean;
    function describe : String;
    function compare(other : TSnomedRefinement) : integer;
    function canonical : TSnomedRefinement;
  end;

  TSnomedRefinementGroup = class (TSnomedExpressionBase)
  private
    Frefinements: TAdvList<TSnomedRefinement>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedRefinementGroup; overload;
    Property refinements : TAdvList<TSnomedRefinement> read Frefinements;

    function matches(other : TSnomedRefinementGroup) : boolean;
    function hasRefinement(refinement : TSnomedRefinement) : boolean;
    function describe : String;
    function compare(other : TSnomedRefinementGroup) : integer;
    function canonical : TSnomedRefinementGroup;
  end;

  TSnomedConceptSorter = class (TAdvObject, IComparer<TSnomedConcept>)
  public
    function Compare(const left, right : TSnomedConcept) : Integer;
  end;
  TSnomedRefinementSorter = class (TAdvObject, IComparer<TSnomedRefinement>)
  public
    function Compare(const left, right : TSnomedRefinement) : Integer;
  end;
  TSnomedRefinementGroupSorter = class (TAdvObject, IComparer<TSnomedRefinementGroup>)
  public
    function Compare(const left, right : TSnomedRefinementGroup) : Integer;
  end;

  TSnomedExpressionParser = class (TAdvObject)
  private
    source : String;
    cursor : integer;
    function next : char;
    function peek : char;
    function peekDisp : String;
    procedure rule(test : boolean; message : String);
    procedure ws;
    function gchar(c : char) : boolean;
    procedure fixed(c : char);
    procedure prefix(c : char);
    function decimal : String;
    function stringConstant : String;
    Function concept : TSnomedConcept;
    function conceptId : String;
    function term : String;
    function attribute : TSnomedRefinement;
    function attributeValue : TSnomedExpression;
    function attributeName : TSnomedConcept;
    Function attributeGroup : TSnomedRefinementGroup;
    procedure refinements(expr : TSnomedExpression);
    function expression : TSnomedExpression;
  public
    function parse(source : String) : TSnomedExpression;
  end;

implementation

{ TSnomedExpressionBase }

function TSnomedExpressionBase.Link: TSnomedExpressionBase;
begin
  result := TSnomedExpressionBase(inherited Link);
end;

{ TSnomedConcept }

function TSnomedConcept.canonical: TSnomedConcept;
begin
  result := TSnomedConcept.Create;
  try
    result.copyFrom(self);
    result.Link;
  finally
    result.Free;
  end;
end;

function TSnomedConcept.compare(other: TSnomedConcept): integer;
begin
  if code <> '' then
    result := String.Compare(code, other.code)
  else if (decimal <> '') then
    result := String.Compare(decimal, other.decimal)
  else
    result := String.Compare(literal, other.literal);
end;

procedure TSnomedConcept.copyFrom(other: TSnomedConcept);
begin
  reference := other.reference;
  Fcode := other.Fcode;
  Fdescription := other.Fdescription;
  FLiteral := other.FLiteral;
  FDecimal := other.FDecimal;
end;

constructor TSnomedConcept.Create(reference: cardinal);
begin
  inherited Create;
  FReference := reference;
end;

function TSnomedConcept.describe: String;
begin
  if (code <> '') then
    result := code
  else if (decimal <> '') then
    result := '#'+decimal
  else if (literal <> '') then
    result := '"'+literal+'"'
  else
    result := ''
end;

constructor TSnomedConcept.Create;
begin
  inherited Create;
  reference := NO_REFERENCE
end;

function TSnomedConcept.Link: TSnomedConcept;
begin
  result := TSnomedConcept(Inherited Link);
end;

function TSnomedConcept.matches(other: TSnomedConcept): boolean;
begin
  if (other = nil) then
    result := false
  else if (reference <> NO_REFERENCE) then
    result := reference = other.reference
  else if (code <> '') then
    result := code = other.code
  else if (decimal <> '') then
    result := decimal = other.decimal
  else if (literal <> '') then
    result := literal = other.literal
  else
    result := false;
end;

{ TSnomedExpression }

function TSnomedExpression.canonical : TSnomedExpression;
var
  cs : TSnomedConceptSorter;
  rs : TSnomedRefinementSorter;
  rgs : TSnomedRefinementGroupSorter;
  concept : TSnomedConcept;
  refinement : TSnomedRefinement;
  group : TSnomedRefinementGroup;
begin
  result := TSnomedExpression.Create;
  try
    result.status := status;
    for concept in concepts do
      result.concepts.Add(concept.canonical);

    if HasRefinementGroups or HasRefinements then
    begin
      for group in refinementGroups do
      begin
        if group.refinements.Count > 0 then
          result.refinementGroups.Add(group.canonical)
      end;
      for refinement in refinements do
      begin
        group := TSnomedRefinementGroup.Create;
        result.refinementGroups.Add(group);
        group.refinements.Add(refinement.canonical);
      end;
    end;

    cs := TSnomedConceptSorter.Create;
    rs := TSnomedRefinementSorter.Create;
    rgs := TSnomedRefinementGroupSorter.Create;
    try
      result.concepts.Sort(cs);
      if HasRefinementGroups then
      begin
        for group in result.refinementGroups do
          group.refinements.Sort(rs);
        result.refinementGroups.Sort(rgs);
      end;
    finally
      cs.Free;
      rs.Free;
      rgs.Free;
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

constructor TSnomedExpression.Create;
begin
  inherited;
  Fconcepts := TAdvList<TSnomedConcept>.Create;
end;

function TSnomedExpression.describe: String;
var
  b : TStringBuilder;
  first : boolean;
  concept : TSnomedConcept;
  refinement : TSnomedRefinement;
  refinementGroup : TSnomedRefinementGroup;
begin
  b := TStringBuilder.Create;
  try
    case status of
      SnomedExpressionStatusEquivalent:b.Append('===');
      SnomedExpressionStatusSubsumedBy:b.Append('<<<');
    end;
    first := true;
    for concept in concepts do
    begin
      if first then first := false else b.Append(',');
      b.Append(concept.describe);
    end;
    for refinement in refinements do
    begin
      if first then first := false else b.Append(',');
      b.Append(refinement.describe);
    end;
    for refinementGroup in refinementGroups do
    begin
      if first then first := false else b.Append(',');
      b.Append(refinementGroup.describe);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

destructor TSnomedExpression.Destroy;
begin
  FrefinementGroups.Free;
  Frefinements.Free;
  Fconcepts.Free;
  inherited;
end;

function TSnomedExpression.GetRefinementGroups: TAdvList<TSnomedRefinementGroup>;
begin
  if FrefinementGroups = nil then
    FrefinementGroups := TAdvList<TSnomedRefinementGroup>.Create;
  result := FrefinementGroups;
end;

function TSnomedExpression.GetRefinements: TAdvList<TSnomedRefinement>;
begin
  if Frefinements = nil then
    Frefinements := TAdvList<TSnomedRefinement>.Create;
  result := Frefinements;
end;

function TSnomedExpression.hasConcept(concept: TSnomedConcept): boolean;
var
  test : TSnomedConcept;
begin
  for test in concepts do
    if test.matches(concept) then
      exit(true);
  result := false;
end;

function TSnomedExpression.hasConcept(concept: Cardinal): boolean;
var
  test : TSnomedConcept;
begin
  for test in concepts do
    if (test.reference = concept) then
      exit(true);
  result := false;
end;

function TSnomedExpression.hasRefinement(refinement: TSnomedRefinement): boolean;
var
  test : TSnomedRefinement;
begin
  for test in refinements do
    if test.matches(refinement) then
      exit(true);
  result := false;
end;

function TSnomedExpression.hasRefinementGroup(refinementGroup: TSnomedRefinementGroup): boolean;
var
  test : TSnomedRefinementGroup;
begin
  for test in refinementGroups do
    if test.matches(refinementGroup) then
      exit(true);
  result := false;
end;

function TSnomedExpression.HasRefinements: boolean;
begin
  result := (Frefinements <> nil) and (Frefinements.Count > 0);
end;

function TSnomedExpression.HasRefinementGroups: boolean;
begin
  result := (FrefinementGroups <> nil) and (FrefinementGroups.Count > 0);
end;

function TSnomedExpression.isComplex: boolean;
begin
  result := not isSimple;
end;

function TSnomedExpression.isSimple: boolean;
begin
  result := (concepts.Count = 1) and (concepts[0].reference <> NO_REFERENCE) and not HasRefinements and not HasRefinementGroups;
end;

function TSnomedExpression.Link: TSnomedExpression;
begin
  result := TSnomedExpression(Inherited Link);
end;

function TSnomedExpression.matches(other: TSnomedExpression): string;
var
  concept : TSnomedConcept;
  refinement : TSnomedRefinement;
  refinementGroup : TSnomedRefinementGroup;
begin
  if other = nil then
    exit('other is nil');

  for concept in concepts do
    if not other.hasConcept(concept) then
      exit('concept '+concept.describe+' not found in first expression');
  for concept in other.concepts do
    if not hasConcept(concept) then
      exit('concept '+concept.describe+' not found in second expression');

  for refinement in refinements do
    if not other.hasRefinement(refinement) then
      exit('refinement '+refinement.describe+' not found in first expression');
  for refinement in other.refinements do
    if not hasRefinement(refinement) then
      exit('refinement '+refinement.describe+' not found in second expression');

  for refinementGroup in refinementGroups do
    if not other.hasRefinementGroup(refinementGroup) then
      exit('refinement group '+refinementGroup.describe+' not found in first expression');
  for refinementGroup in other.refinementGroups do
    if not hasRefinementGroup(refinementGroup) then
      exit('refinement group '+refinementGroup.describe+' not found in second expression');

  result := '';
end;

procedure TSnomedExpression.merge(exp: TSnomedExpression);
begin
  if (exp <> nil) then
  begin
    concepts.AddAll(exp.concepts);
    refinements.AddAll(exp.refinements);
    refinementGroups.AddAll(exp.refinementGroups);
  end;
end;

{ TSnomedRefinement }

function TSnomedRefinement.canonical: TSnomedRefinement;
begin
  result := TSnomedRefinement.Create;
  try
    result.name := name.canonical;
    result.value := value.canonical;
    result.Link;
  finally
    result.free;
  end;
end;

function TSnomedRefinement.compare(other: TSnomedRefinement): integer;
begin
  result := name.compare(other.name);
end;

constructor TSnomedRefinement.Create;
begin
  inherited;
end;

function TSnomedRefinement.describe: String;
begin
  result := name.describe+'='+value.describe;
end;

destructor TSnomedRefinement.Destroy;
begin
  Fname.Free;
  Fvalue.Free;
  inherited;
end;

function TSnomedRefinement.Link: TSnomedRefinement;
begin
  result := TSnomedRefinement(inherited Link);
end;

function TSnomedRefinement.matches(other: TSnomedRefinement): boolean;
begin
  if other = nil then
    exit(false);
  if not name.matches(other.name) then
    exit(false);
  result := value.matches(other.value) = '';
end;

procedure TSnomedRefinement.SetName(const Value: TSnomedConcept);
begin
  Fname.Free;
  Fname := value;
end;

procedure TSnomedRefinement.SetValue(const Value: TSnomedExpression);
begin
  Fvalue.Free;
  Fvalue := value;
end;

{ TSnomedRefinementGroup }

function TSnomedRefinementGroup.canonical: TSnomedRefinementGroup;
var
  refinement : TSnomedRefinement;
begin
  result := TSnomedRefinementGroup.Create;
  try
    for refinement in refinements do
      result.refinements.Add(refinement.canonical);
    result.Link;
  finally
    result.Free;
  end;
end;

function TSnomedRefinementGroup.compare(other: TSnomedRefinementGroup): integer;
begin
  result := refinements[0].compare(other.refinements[0]);
end;

constructor TSnomedRefinementGroup.Create;
begin
  inherited;
  Frefinements := TAdvList<TSnomedRefinement>.Create;
end;

function TSnomedRefinementGroup.describe: String;
var
  b : TStringBuilder;
  first : boolean;
  refinement : TSnomedRefinement;
begin
  b := TStringBuilder.Create;
  try
    first := true;
    for refinement in refinements do
    begin
      if first then first := false else b.Append(',');
      b.Append(refinement.describe);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

destructor TSnomedRefinementGroup.Destroy;
begin
  Frefinements.Free;
  inherited;
end;

function TSnomedRefinementGroup.hasRefinement(refinement: TSnomedRefinement): boolean;
var
  test : TSnomedRefinement;
begin
  for test in refinements do
    if test.matches(refinement) then
      exit(true);
  result := false;
end;

function TSnomedRefinementGroup.Link: TSnomedRefinementGroup;
begin
  result := TSnomedRefinementGroup(inherited link);

end;

function TSnomedRefinementGroup.matches(other: TSnomedRefinementGroup): boolean;
var
  refinement : TSnomedRefinement;
begin
  if other = nil then
    exit(false);

  for refinement in refinements do
    if not other.hasRefinement(refinement) then
      exit(false);
  for refinement in other.refinements do
    if not hasRefinement(refinement) then
      exit(false);
  result := true;
end;

{ TSnomedConceptSorter }

function TSnomedConceptSorter.Compare(const left, right: TSnomedConcept): Integer;
begin
  result := left.compare(right);
end;

{ TSnomedRefinementSorter }

function TSnomedRefinementSorter.Compare(const left, right: TSnomedRefinement): Integer;
begin
  result := left.compare(right);
end;

{ TSnomedRefinementGroupSorter }

function TSnomedRefinementGroupSorter.Compare(const left, right: TSnomedRefinementGroup): Integer;
begin
  result := left.compare(right);
end;

{ TSnomedExpressionParser }

function TSnomedExpressionParser.concept: TSnomedConcept;
begin
  result := TSnomedConcept.Create;
  try
    result.start := cursor;
    ws;
    if peek = '#' then
      result.decimal := decimal
    else if peek = '"' then
      result.literal := stringConstant
    else
      result.code := conceptId;
    ws;
    if gchar('|') then
    begin
      ws;
      result.description := term.trim();
      ws;
      fixed('|');
      ws;
    end;
    result.stop := cursor;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TSnomedExpressionParser.refinements(expr : TSnomedExpression);
var
   n : boolean;
begin
  n := true;
  while n do
  begin
    if peek <> '{' then
      expr.refinements.add(attribute)
    else
      expr.refinementGroups.add(attributeGroup);
    ws;
    n := gchar(',');
    ws;
  end;
end;

function TSnomedExpressionParser.attributeGroup : TSnomedRefinementGroup;
begin
  result := TSnomedRefinementGroup.Create;
  try
    fixed('{');
    ws;
    result.start := cursor;
    result.refinements.add(attribute);
    while gchar(',') do
      result.refinements.add(attribute);
    result.stop := cursor;
    ws;
    fixed('}');
    ws;
    result.Link;
  finally
    result.free;
  end;
end;

function TSnomedExpressionParser.attribute: TSnomedRefinement;
begin
  result := TSnomedRefinement.Create;
  try
    result.start := cursor;
    result.name := attributeName;
    fixed('=');
    result.value := attributeValue;
    ws;
    result.stop := cursor;
    result.Link;
  finally
    result.free;
  end;
end;

function TSnomedExpressionParser.attributeName : TSnomedConcept;
begin
  result := TSnomedConcept.Create;
  try
    result.start := cursor;
    ws;
    result.code := conceptId;
    ws;
    if gchar('|') then
    begin
      ws;
      result.description := term;
      ws;
      fixed('|');
      ws;
    end;
    result.stop := cursor;
    result.Link;
  finally
    result.Free;
  end;
end;

function TSnomedExpressionParser.attributeValue : TSnomedExpression;
begin
  ws;
  if gchar('(') then
  begin
    result := expression;
    fixed(')');
  end
  else
  begin
    result := expression;
//    result := TSnomedExpression.create;
//    try
//      result.start := cursor;
//      result.concepts.Add(concept);
//      result.stop := cursor;
//      ws;
//      while peek = '+' do
//      begin
//        gchar('+');
//        result.concepts.Add(concept);
//      end;
//      result.link;
//    finally
//      result.free;
//    end;
  end;
end;

function TSnomedExpressionParser.expression : TSnomedExpression;
begin
  result := TSnomedExpression.Create;
  try
    result.start := cursor;
    ws;
    result.concepts.Add(concept);
    while gchar('+') do
      result.concepts.add(concept);
    if gchar(':') then
    begin
      ws;
      refinements(result);
    end;
    result.stop := cursor;
    result.link;
  finally
    result.free;
  end;
end;

function TSnomedExpressionParser.conceptId : String;
var
  i : integer;
begin
  SetLength(result, 18);
  i := 0;
  while charInSet(peek, ['0'..'9']) do
  begin
    inc(i);
    result[i] := next;
  end;
  SetLength(result, i);
  rule(i > 0, 'Concept not found (next char = "'+peekDisp+'")');

end;

function TSnomedExpressionParser.decimal: String;
var
  i : integer;
begin
  SetLength(result, MAX_TERM_LENGTH);
  i := 0;
  fixed('#');
  while CharInSet(peek, ['0'..'9', '.']) do
  begin
    inc(i);
    result[i] := next;
  end;
  SetLength(result, i);
end;

function TSnomedExpressionParser.term : String;
var
  i : integer;
begin
  SetLength(result, MAX_TERM_LENGTH);
  i := 0;
  while peek <> '|' do
  begin
    inc(i);
    result[i] := next;
  end;
  SetLength(result, i);
end;

procedure TSnomedExpressionParser.ws;
begin
  while CharInSet(peek, [' ', #9, #10, #13]) do
    next;
end;

function TSnomedExpressionParser.gchar(c : char) : boolean;
begin
  result := peek = c;
  if result then
    next;
end;

procedure TSnomedExpressionParser.fixed(c : char);
var
  b : boolean;
begin
  b := gchar(c);
  rule(b, 'Expected character "'+c+'" but found '+peek);
  ws;
end;

function TSnomedExpressionParser.parse(source: String): TSnomedExpression;
begin
  self.source := source;
  self.cursor := 1;
  result := TSnomedExpression.Create;
  try
    result.start := cursor;
    ws;
    if peek = '=' then
    begin
      result.status := SnomedExpressionStatusEquivalent;
      prefix('=');
    end
    else if peek = '<' then
    begin
      result.status := SnomedExpressionStatusSubsumedBy;
      prefix('<');
    end;

    result.concepts.Add(concept);
    while gchar('+') do
      result.concepts.add(concept);
    if gchar(':') then
    begin
      ws;
      refinements(result);
    end;
    result.stop := cursor;

    rule(peek = #0, 'Found content ("'+peekDisp+'") after end of expression');
    result.link;
  finally
    result.free;
  end;
end;

function TSnomedExpressionParser.peek : char;
begin
  if cursor > length(source) then
    result := #0
  else
    result := source[cursor];
end;

function TSnomedExpressionParser.peekDisp: String;
begin
  if cursor > length(source) then
    result := '[n/a: overrun]'
  else
    result := source[cursor];
end;

procedure TSnomedExpressionParser.prefix(c: char);
begin
  fixed(c);
  fixed(c);
  fixed(c);
  ws;
end;

function TSnomedExpressionParser.next : char;
begin
  result := peek;
  inc(cursor);
end;

procedure TSnomedExpressionParser.rule(test : boolean; message : String);
begin
  if not test then
    raise ESnomedParser.Create(message+' at character '+inttostr(cursor));
end;

function TSnomedExpressionParser.stringConstant: String;
var
  i : integer;
begin
  SetLength(result, MAX_TERM_LENGTH);
  fixed('"');
  i := 0;
  while peek <> '"' do
  begin
    inc(i);
    result[i] := next;
  end;
  fixed('"');
  SetLength(result, i);
end;


end.

