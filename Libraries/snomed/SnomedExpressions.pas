unit SnomedExpressions;

interface

uses
  SysUtils, Classes,AdvObjects, AdvObjectLists,
  SnomedServices;

const
  MAX_TERM_LENGTH = 1024;

Type
  TSnomedRefinementGroupList = class;
  TSnomedRefinementList = class;

  TSnomedExpressionBase = class (TAdvObject)
  private
    Fstop: integer;
    Fstart: integer;
  public
    Function Link : TSnomedExpressionBase; overload;
    property start : integer read Fstart write Fstart;
    property stop : integer read Fstop write Fstop;
  end;

  TSnomedExpressionBaseList = class (TAdvObjectList)
  private
    function getItem(i: integer): TSnomedExpressionBase; ReIntroduce;
  protected
    Function ItemClass : TAdvObjectClass; override;
  public
    Property Item[i : integer] : TSnomedExpressionBase read getItem; default;
  end;

  TSnomedConcept = class (TSnomedExpressionBase)
  private
    Fcode: String;
    Fdescription: String;
  public
    Function Link : TSnomedConcept; overload;
    Property code : String read Fcode write Fcode;
    Property description : String read Fdescription write Fdescription;
  end;

  TSnomedConceptList = class (TSnomedExpressionBaseList)
  private
    function getItem(i: integer): TSnomedConcept;
  protected
    Function ItemClass : TAdvObjectClass; override;
  public
    Property Item[i : integer] : TSnomedConcept read getItem; default;
  end;

  TSnomedExpression = class (TSnomedExpressionBase)
  private
    FrefinementGroups: TSnomedRefinementGroupList;
    Frefinements: TSnomedRefinementList;
    Fconcepts: TSnomedConceptList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedExpression; overload;
    Property concepts : TSnomedConceptList read Fconcepts;
    Property refinements : TSnomedRefinementList read Frefinements;
    Property refinementGroups : TSnomedRefinementGroupList read FrefinementGroups;
  end;

  TSnomedExpressionList = class (TSnomedExpressionBaseList)
  private
    function getItem(i: integer): TSnomedExpression;
  protected
    Function ItemClass : TAdvObjectClass; override;
  public
    Property Item[i : integer] : TSnomedExpression read getItem; default;
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
  end;

  TSnomedRefinementList = class (TSnomedExpressionBaseList)
  private
    function getItem(i: integer): TSnomedRefinement;
  protected
    Function ItemClass : TAdvObjectClass; override;
  public
    Property Item[i : integer] : TSnomedRefinement read getItem; default;
  end;

  TSnomedRefinementGroup = class (TSnomedExpressionBase)
  private
    Frefinements: TSnomedRefinementList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedRefinementGroup; overload;
    Property refinements : TSnomedRefinementList read Frefinements;
  end;

  TSnomedRefinementGroupList = class (TSnomedExpressionBaseList)
  private
    function getItem(i: integer): TSnomedRefinementGroup;
  protected
    Function ItemClass : TAdvObjectClass; override;
  public
    Property Item[i : integer] : TSnomedRefinementGroup read getItem; default;
  end;

  TSnomedExpressionParserRenderOption = (sroMinimal, sroAsIs, sroFillMissing, sroReplaceAll);

  TSnomedExpressionParser = class (TAdvObject)
  private
    FServices : TSnomedServices;
    source : String;
    cursor : integer;

    function next : char;
    function peek : char;
    procedure rule(test : boolean; message : String);
    procedure ws;
    function char(c : char) : boolean;
    procedure fixed(c : char);
    Function concept : TSnomedConcept;
    function conceptId : String;
    function term : String;
    function attribute : TSnomedRefinement;
    function attributeValue : TSnomedExpression;
    function attributeName : TSnomedConcept;
    procedure attributeSet(list : TSnomedRefinementList);
    Function attributeGroup : TSnomedRefinementGroup;
    procedure refinements(expr : TSnomedExpression);
    function expression : TSnomedExpression;

    procedure check(expression : TSnomedExpression); overload;
    procedure check(concept : TSnomedConcept); overload;
    procedure check(refinement : TSnomedRefinement); overload;

    procedure Render(b : TStringBuilder; expr : TSnomedExpression; option : TSnomedExpressionParserRenderOption); overload;
    procedure Render(b : TStringBuilder; expr : TSnomedConcept; option : TSnomedExpressionParserRenderOption); overload;
    procedure Render(b : TStringBuilder; expr : TSnomedRefinement; option : TSnomedExpressionParserRenderOption); overload;

    procedure Display(b : TStringBuilder; expr : TSnomedExpression); overload;
    procedure Display(b : TStringBuilder; expr : TSnomedConcept); overload;
    procedure Display(b : TStringBuilder; expr : TSnomedRefinement); overload;

  public
    constructor create(services : TSnomedServices);
    destructor destroy; override;

    Class Function Parse(services : TSnomedServices; source : String) : TSnomedExpression;
    Class Function Render(services : TSnomedServices; source : TSnomedExpression; option : TSnomedExpressionParserRenderOption) : String;  overload;
    Class Function Display(services : TSnomedServices; source : TSnomedExpression) : String;  overload;

  end;

implementation

{ TSnomedExpressionBaseList }

function TSnomedExpressionBaseList.getItem(i: integer): TSnomedExpressionBase;
begin
  result := TSnomedExpressionBase(ObjectByIndex[i]);
end;

function TSnomedExpressionBaseList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedExpressionBase;
end;

{ TSnomedConceptList }

function TSnomedConceptList.getItem(i: integer): TSnomedConcept;
begin
  result := TSnomedConcept(ObjectByIndex[i]);
end;

function TSnomedConceptList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedConcept;
end;

{ TSnomedExpressionList }

function TSnomedExpressionList.getItem(i: integer): TSnomedExpression;
begin
  result := TSnomedExpression(ObjectByIndex[i]);
end;

function TSnomedExpressionList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedExpression;
end;


{ TSnomedRefinementList }

function TSnomedRefinementList.getItem(i: integer): TSnomedRefinement;
begin
  result := TSnomedRefinement(ObjectByIndex[i]);
end;

function TSnomedRefinementList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedRefinement;
end;

{ TSnomedRefinementGroupList }

function TSnomedRefinementGroupList.getItem(i: integer): TSnomedRefinementGroup;
begin
  result := TSnomedRefinementGroup(ObjectByIndex[i]);
end;

function TSnomedRefinementGroupList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedRefinementGroup;
end;


{ TSnomedExpressionBase }

function TSnomedExpressionBase.Link: TSnomedExpressionBase;
begin
  result := TSnomedExpressionBase(inherited Link);
end;

{ TSnomedConcept }

function TSnomedConcept.Link: TSnomedConcept;
begin
  result := TSnomedConcept(Inherited Link);
end;

{ TSnomedExpression }

constructor TSnomedExpression.Create;
begin
  inherited;
  FrefinementGroups := TSnomedRefinementGroupList.Create;
  Frefinements := TSnomedRefinementList.Create;
  Fconcepts := TSnomedConceptList.Create;
end;

destructor TSnomedExpression.Destroy;
begin
  FrefinementGroups.Free;
  Frefinements.Free;
  Fconcepts.Free;
  inherited;
end;

function TSnomedExpression.Link: TSnomedExpression;
begin
  result := TSnomedExpression(Inherited Link);
end;

{ TSnomedRefinement }

constructor TSnomedRefinement.Create;
begin
  inherited;
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

constructor TSnomedRefinementGroup.Create;
begin
  inherited;
  Frefinements := TSnomedRefinementList.Create;
end;

destructor TSnomedRefinementGroup.Destroy;
begin
  Frefinements.Free;
  inherited;
end;

function TSnomedRefinementGroup.Link: TSnomedRefinementGroup;
begin
  result := TSnomedRefinementGroup(inherited link);

end;

{ TSnomedExpressionParser }

constructor TSnomedExpressionParser.create(services: TSnomedServices);
begin
  inherited Create;
  FServices := services;
end;

destructor TSnomedExpressionParser.destroy;
begin
  FServices.Free;
  inherited;
end;

class function TSnomedExpressionParser.Parse(services : TSnomedServices; source: String): TSnomedExpression;
var
  prsr : TSnomedExpressionParser;
begin
  prsr := TSnomedExpressionParser.create(services.Link);
  try
    prsr.source := source;
    prsr.cursor := 1;
    result := prsr.expression;
    try
      prsr.rule(prsr.next = #0, 'Found content after end of expression');
      prsr.check(result);
      result.Link;
    finally
      result.free;
    end;
  finally
    prsr.Free;
  end;
end;

function TSnomedExpressionParser.concept: TSnomedConcept;
begin
  result := TSnomedConcept.Create;
  try
    result.start := cursor;
    ws;
    result.code := conceptId;
    ws;
    if char('|') then
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
begin
  if peek <> '{' then
    attributeSet(expr.Frefinements);
  while char('{') do
    expr.refinementGroups.add(attributeGroup);
end;

function TSnomedExpressionParser.attributeGroup : TSnomedRefinementGroup;
begin
  result := TSnomedRefinementGroup.Create;
  try
    result.start := cursor;
    attributeSet(result.Frefinements);
    result.stop := cursor;
    result.Link;
  finally
    result.free;
  end;
  fixed('}');
end;

procedure TSnomedExpressionParser.attributeSet(list : TSnomedRefinementList);
begin
  list.add(attribute);
  while char(',') do
    list.add(attribute);
end;

function TSnomedExpressionParser.attribute: TSnomedRefinement;
begin
  result := TSnomedRefinement.Create;
  try
    result.start := cursor;
    result.name := attributeName;
    fixed('=');
    result.value := attributeValue;
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
    if char('|') then
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
  if char('(') then
  begin
    result := expression;
    fixed(')');
  end
  else
  begin
    result := TSnomedExpression.create;
    try
      result.start := cursor;
      result.concepts.Add(concept);
    result.stop := cursor;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TSnomedExpressionParser.expression : TSnomedExpression;
begin
  result := TSnomedExpression.Create;
  try
    result.start := cursor;
    result.concepts.Add(concept);
    while char('+') do
      result.concepts.add(concept);
    if char(':') then
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
  rule(i > 0, 'Concept not found');

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

function TSnomedExpressionParser.char(c : char) : boolean;
begin
  result := peek = c;
  if result then
    next;
end;

procedure TSnomedExpressionParser.fixed(c : char);
begin
  rule(char(c), 'Expected character "'+c+'"');
  ws;
end;

function TSnomedExpressionParser.peek : char;
begin
  if cursor > length(source) then
    result := #0
  else
    result := source[cursor];
end;

function TSnomedExpressionParser.next : char;
begin
  result := peek;
  inc(cursor);
end;

procedure TSnomedExpressionParser.rule(test : boolean; message : String);
begin
  if not test then
    raise Exception.Create(message+' at character '+inttostr(cursor));
end;


procedure TSnomedExpressionParser.check(expression: TSnomedExpression);
var
  i, j : integer;
begin
  for i := 0 to expression.concepts.count - 1 do
     check(expression.concepts[i]);
  for i := 0 to expression.refinements.count - 1 do
     check(expression.refinements[i]);
  for j := 0 to expression.refinementGroups.count - 1 do
    for i := 0 to expression.refinementGroups[j].refinements.count - 1 do
     check(expression.refinementGroups[j].refinements[i]);
end;

procedure TSnomedExpressionParser.check(concept: TSnomedConcept);
var
  list : TStringList;
  i : integer;
  ok : boolean;
begin
  list := TStringList.create;
  try
    FServices.ListDisplayNames(list, concept.code, '', $FF);
    if list.count = 0 then
      raise Exception.Create('Unknown code "'+concept.code+'" at character '+inttostr(cursor));
    ok := concept.description = '';
    for i := 0 to list.count - 1 do
      if SameText(list[i].trim, concept.description.trim) then
        ok := true;
    if not ok then
      raise Exception.Create('Term "'+concept.description+'" doesn''t match a defined term at '+inttostr(cursor)+' (valid terms would be from this list: "'+list.CommaText+'")');
  finally
    list.free;
  end;
end;

procedure TSnomedExpressionParser.check(refinement: TSnomedRefinement);
begin
  check(refinement.name);
  check(refinement.value);
end;

class function TSnomedExpressionParser.Render(services : TSnomedServices; source : TSnomedExpression; option : TSnomedExpressionParserRenderOption): String;
var
  b : TStringBuilder;
  prsr : TSnomedExpressionParser;
begin
  b := TStringBuilder.Create;
  try
    prsr := TSnomedExpressionParser.create(services.Link);
    try
      prsr.Render(b, source, option);
    finally
      prsr.Free;
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TSnomedExpressionParser.Render(b: TStringBuilder; expr: TSnomedExpression; option : TSnomedExpressionParserRenderOption);
var
  i, j : integer;
begin
  for i := 0 to expr.concepts.Count - 1 do
  begin
    if (i > 0) then
      b.Append('+');
    Render(b, expr.concepts[i], option);
  end;
  if (expr.refinements.Count > 0) or (expr.refinementGroups.Count > 0) then
  begin
    b.Append(':');
    for i := 0 to expr.refinements.Count - 1 do
    begin
      if (i > 0) then
        b.Append(',');
      Render(b, expr.refinements[i], option);
    end;
    for j := 0 to expr.refinementGroups.Count - 1 do
    begin
      b.Append('{');
      for i := 0 to expr.refinementGroups[j].refinements.Count - 1 do
      begin
        if (i > 0) then
          b.Append(',');
        Render(b, expr.refinementGroups[j].refinements[i], option);
      end;

      b.Append('}');
    end;
  end;
end;

procedure TSnomedExpressionParser.Render(b : TStringBuilder; expr : TSnomedConcept; option : TSnomedExpressionParserRenderOption);
var
  s : String;
begin
  b.Append(expr.code);
  case option of
    sroMinimal : s := '';
    sroAsIs : s := expr.Fdescription;
    sroFillMissing :
      begin
        s := expr.Fdescription;
        if (s = '') then
          s := FServices.GetDisplayName(expr.code, '');
      end;
    sroReplaceAll : s := FServices.GetDisplayName(expr.code, '');
  end;
  if s <> '' then
  begin
    b.Append(' | ');
    b.Append(expr.description);
    b.Append(' | ');
  end;
end;


procedure TSnomedExpressionParser.Render(b : TStringBuilder; expr : TSnomedRefinement; option : TSnomedExpressionParserRenderOption);
begin
  render(b, expr.name, option);
  b.Append('=');
  Render(b, expr.value, option);
end;


class function TSnomedExpressionParser.Display(services: TSnomedServices; source: TSnomedExpression): String;
var
  b : TStringBuilder;
  prsr : TSnomedExpressionParser;
begin
  b := TStringBuilder.Create;
  try
    prsr := TSnomedExpressionParser.create(services.Link);
    try
      prsr.Display(b, source);
    finally
      prsr.Free;
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;


procedure TSnomedExpressionParser.Display(b: TStringBuilder; expr: TSnomedExpression);
var
  i, j : integer;
  done : boolean;
begin
  for i := 0 to expr.concepts.Count - 1 do
  begin
    if (i > 0) then
      b.Append(', ');
    if (i > 0) and (i = expr.concepts.Count - 1) then
      b.Append('and ');
    Display(b, expr.concepts[i]);
  end;
  done := false;
  for i := 0 to expr.refinements.Count - 1 do
  begin
    if (i = 0) then
    begin
      b.Append(' where ');
      done := true;
    end;
    if (i > 0) then
      b.Append(', ');
    if (i > 0) and (i = expr.refinements.Count - 1) then
      b.Append('and ');
    Display(b, expr.refinements[i]);
  end;
  for j := 0 to expr.refinementGroups.Count - 1 do
  begin
    if not done and (j = 0) then
      b.Append(' where ');
    if (j > 0) then
      b.Append(', ');
    if (j > 0) and (j = expr.refinementGroups.Count - 1) then
      b.Append('and ');
    b.Append('(');
    for i := 0 to expr.refinementGroups[j].refinements.Count - 1 do
    begin
      if (i > 0) then
        b.Append(', ');
      if (i = expr.refinementGroups[j].refinements.Count - 1) then
        b.Append('and ');
      Display(b, expr.refinementGroups[j].refinements[i]);
    end;

    b.Append('}');
  end;
end;

procedure TSnomedExpressionParser.Display(b : TStringBuilder; expr : TSnomedConcept);
var
  s : String;
begin
  s := FServices.GetDisplayName(expr.code, '');
  if (s = '') then
    s := expr.Fdescription;
  b.Append(s);
end;


procedure TSnomedExpressionParser.Display(b : TStringBuilder; expr : TSnomedRefinement);
begin
  Display(b, expr.name);
  b.Append(' = ');
  Display(b, expr.value);
end;


end.

