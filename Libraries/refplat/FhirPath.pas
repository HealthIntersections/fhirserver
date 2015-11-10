unit FhirPath;

interface

uses
  SysUtils,
  StringSupport,
  AdvObjects, AdvGenerics,

  FHIRBase, FHIRTypes, FHIRUtilities;

type
  EFHIRPath = class (Exception)
  public
     constructor create(path : String; offset : integer; problem : String);
  end;

  TFHIRPathOperation = (poEquals, poNotEquals, poLessThen, poGreater, poLessOrEqual, poGreaterOrEqual, poIn, poPlus, poMinus, poDivide, poMultiply);

const
  CODES_TFHIRPathOperation : array [TFHIRPathOperation] of String = ('=', '!=', '>', '<', '>=', '<=', 'in', '+', '-', '/', '*');

type
  TFHIRPathLexer = class (TAdvObject)
  private
    FPath : String;
    FCursor : integer;
    FCurrent : String;
    FCurrentStart : integer;
  public
    constructor Create(path : String); overload;
    destructor Destroy; override;
    procedure next;
    property current : String read FCurrent;
    function done : boolean;
    function take : String;

    function error(msg : String) : Exception;
    function isToken : boolean;
    function isOp : boolean;
  end;

  TFHIRPathExpression = class (TAdvObject)
  private
    FName: String;
    FIsFunction : boolean;
    FParameters : TAdvList<TFHIRPathExpression>;
    FOperation : TFHIRPathOperation;
    FNext: TFHIRPathExpression;
    procedure SetNext(const Value: TFHIRPathExpression);
    procedure SetIsFunction(const Value: boolean);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : TFHIRPathExpression; overload;
    property name : String read FName write FName;
    property next : TFHIRPathExpression read FNext write SetNext;
    property isFunction : boolean read FIsFunction write SetIsFunction;
    property Parameters : TAdvList<TFHIRPathExpression> read FParameters;
    property Operation : TFHIRPathOperation read FOperation write FOperation;
  end;

  TFHIRPathEvaluator = class (TAdvObject)
  private
    function parseExpression(lexer: TFHIRPathLexer): TFHIRPathExpression;
    function parse(path : String) : TFHIRPathExpression;
    function execute(context : TFHIRBaseList; exp : TFHIRPathExpression; atEntry : boolean) : TFHIRBaseList; overload;
    function execute(item : TFHIRBase; exp : TFHIRPathExpression; atEntry : boolean) : TFHIRBaseList; overload;

    function funcEmpty(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcWhere(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAll(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAny(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcFirst(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcLast(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcTail(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcCount(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAsInteger(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcStartsWith(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcLength(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcMatches(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcDistinct(context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;

  public
    // syntax check and determine if the paths referred to in the path are valid
    procedure check(context : String; path : String);

    // evaluate a path and return the matching elements
    function evaluate(base : TFHIRBase; path : String) : TFHIRBaseList;

    // evaluate a path and return true or false
    function evaluateToBoolean(base : TFHIRBase; path : String) : boolean;

    // evaluate a path and return a string describing the outcome
    function evaluateToString(base : TFHIRBase; path : String) : string;

    // worker routine for converting a set of objects to a string representation
    function convertToString(items : TFHIRBaseList) : String;
  end;

implementation

{ TFHIRPathEvaluator }

procedure TFHIRPathEvaluator.check(context : String; path: String);
begin
  raise Exception.Create('Not Done Yet');
end;

function TFHIRPathEvaluator.convertToString(items: TFHIRBaseList): String;
var
  b : TStringBuilder;
  first : boolean;
  item : TFHIRBase;
begin
  b := TStringBuilder.Create;
  try
    first := true;
    for item in items do
    begin
      if (first) then
        first := false
      else
        b.Append(',');
      if item is TFHIRPrimitiveType then
        b.append((item as TFHIRPrimitiveType).StringValue)
      else if item is TFhirType then
        b.Append(gen(item as TFHIRType));
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPathEvaluator.evaluate(base: TFHIRBase; path: String): TFHIRBaseList;
var
  exp : TFHIRPathExpression;
  list : TFHIRBaseList;
begin
  exp := parse(path);
  try
    list := TFHIRBaseList.Create;
    try
      list.Add(base.Link);
      result := execute(list, exp, true);
    finally
      list.Free;
    end;
  finally
    exp.free;
  end;
end;

function TFHIRPathEvaluator.evaluateToBoolean(base: TFHIRBase; path: String): boolean;
begin
  raise Exception.Create('Not Done Yet');
end;

function TFHIRPathEvaluator.evaluateToString(base: TFHIRBase; path: String): string;
var
  res : TFHIRBaseList;
begin
  res := evaluate(base, path);
  try
    result := convertToString(res);
  finally
    res.Free;
  end;
end;



function TFHIRPathEvaluator.execute(item: TFHIRBase; exp: TFHIRPathExpression; atEntry : boolean): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  try
    if atEntry and (CharInSet(exp.name[1], ['A'..'Z'])) then // special case for start up
    begin
      if item.FhirType = exp.name then
        result.Add(item.Link);
    end
    else
      item.ListChildrenByName(exp.name, result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEvaluator.funcAll(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcAny(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcAsInteger(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcCount(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirInteger.Create(inttostr(context.Count)));
end;

function TFHIRPathEvaluator.funcDistinct(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcEmpty(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(context.Count = 0));
end;

function TFHIRPathEvaluator.funcFirst(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if context.Count > 0 then
    result.Add(context[0].Link);
end;

function TFHIRPathEvaluator.funcLast(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if context.Count > 0 then
    result.Add(context[context.Count - 1].Link);
end;

function TFHIRPathEvaluator.funcLength(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcMatches(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcStartsWith(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcTail(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
   i : integer;
begin
  result := TFHIRBaseList.Create;
  for i := 1 to Context.Count -1 do
    result.Add(context[i].Link);
end;

function TFHIRPathEvaluator.funcWhere(context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.execute(context: TFHIRBaseList; exp: TFHIRPathExpression; atEntry : boolean): TFHIRBaseList;
var
  work : TFHIRBaseList;
  item : TFHIRBase;
  outcome : TFHIRBaseList;
begin
  work := TFHIRBaseList.Create;
  try
    // functions are evaluated on the collection
    if exp.isFunction then
    begin
      if (exp.name = 'empty') then
        work.addAll(funcEmpty(context, exp))
      else if (exp.name = 'where') then
        work.addAll(funcWhere(context, exp))
      else if (exp.name = 'all') then
        work.addAll(funcAll(context, exp))
      else if (exp.name = 'any') then
        work.addAll(funcAny(context, exp))
      else if (exp.name = 'first') then
        work.addAll(funcFirst(context, exp))
      else if (exp.name = 'last') then
        work.addAll(funcLast(context, exp))
      else if (exp.name = 'tail') then
        work.addAll(funcTail(context, exp))
      else if (exp.name = 'count') then
        work.addAll(funcCount(context, exp))
      else if (exp.name = 'asInteger') then
        work.addAll(funcAsInteger(context, exp))
      else if (exp.name = 'startsWith') then
        work.addAll(funcStartsWith(context, exp))
      else if (exp.name = 'length') then
        work.addAll(funcLength(context, exp))
      else if (exp.name = 'matches') then
        work.addAll(funcMatches(context, exp))
      else if (exp.name = 'distinct') then
        work.addAll(funcDistinct(context, exp))
      else
        raise Exception.Create('Unknown Function '+exp.name);
    end
    else for item in context do
    begin
      outcome := execute(item, exp, atEntry);
      try
        work.AddAll(outcome);
      finally
        outcome.Free;
      end;
    end;

    if exp.next = nil then
      result := work.Link
    else
      result := execute(work, exp.next, false);
  finally
    work.Free;
  end;
end;

function TFHIRPathEvaluator.parseExpression(lexer : TFHIRPathLexer): TFHIRPathExpression;
begin
  result := TFHIRPathExpression.Create;
  try
    if not lexer.isToken then
      raise lexer.error('Found '+lexer.current+' expecting a token name');
    result.FName := lexer.take;
    if (lexer.current = '(') then
    begin
      result.IsFunction := true;
      lexer.next;
      while lexer.current <> ')' do
        result.Parameters.add(parseExpression(lexer));
      lexer.next;
    end;
    if lexer.isOp then
    begin
      result.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current));
      lexer.next;
      result.Next := parseExpression(lexer);
    end
    else if lexer.current = '.' then
    begin
      lexer.next;
      result.Next := parseExpression(lexer);
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.parse(path: String): TFHIRPathExpression;
var
  lexer : TFHIRPathLexer;
  focus : TFHIRPathExpression;
begin
  lexer := TFHIRPathLexer.Create(path);
  try
    if lexer.done then
      raise lexer.error('Path cannot be empty');
    result := parseExpression(lexer);
    try
      if not lexer.done then
        raise lexer.error('Premature expression termination at unexpected token');
      result.Link;
    finally
      result.free;
    end;
  finally
    lexer.Free;
  end;
end;

{ TFHIRPathExpression }

constructor TFHIRPathExpression.Create;
begin
  inherited;

end;

destructor TFHIRPathExpression.Destroy;
begin
  FParameters.free;
  FNext.Free;
  inherited;
end;

function TFHIRPathExpression.Link: TFHIRPathExpression;
begin
  result := TFHIRPathExpression(inherited Link);
end;

procedure TFHIRPathExpression.SetIsFunction(const Value: boolean);
begin
  FIsFunction := Value;
  if FParameters = nil then
    FParameters := TAdvList<TFHIRPathExpression>.create;
end;

procedure TFHIRPathExpression.SetNext(const Value: TFHIRPathExpression);
begin
  FNext.Free;
  FNext := Value;
end;

{ EFHIRPath }

constructor EFHIRPath.create(path: String; offset: integer; problem: String);
begin
  inherited create('FHIRPath error in "'+path+'" at position '+inttostr(offset)+': '+problem);
end;

{ TFHIRPathLexer }

constructor TFHIRPathLexer.Create(path: String);
begin
  inherited Create;
  FPath := path;
  FCursor := 1;
  next;
end;

destructor TFHIRPathLexer.Destroy;
begin

  inherited;
end;

function isWhitespace(ch : char) : Boolean;
begin
  result := CharInSet(ch, [#9, #10, #13, ' ']);
end;

procedure TFHIRPathLexer.next;
  procedure Grab(length : Integer);
  begin
    FCurrent := copy(FPath, FCurrentStart, length);
    inc(FCursor, length);
  end;
var
  ch : char;
begin
  FCurrent := '';
  while (FCursor <= FPath.Length) and isWhitespace(FPath[FCursor]) do
    inc(FCursor);
  FCurrentStart := FCursor;
  if (FCursor <= FPath.Length) then
  begin
    ch := FPath[FCursor];
    if charInSet(ch, ['!', '>', '<']) then
    begin
      if (FCursor < FPath.Length) and (FPath[FCursor+1] = '=') then
        Grab(2)
      else
        Grab(1);
    end
    else if ch = '*' then
    begin
      if (FCursor < FPath.Length) and (FPath[FCursor+1] = '*') then
        Grab(2)
      else
        Grab(1);
    end
    else if CharInSet(ch, ['0'..'9']) then
    begin
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['0'..'9', '.']) do
        inc(FCursor);
      FCurrent := FPath.Substring(FCurrentStart, FCursor);
    end
    else if CharInSet(ch, ['A'..'Z', 'a'..'z']) then
    begin
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', '[', ']', '*']) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else // if CharInSet(ch, ['.', ',', '(', ')', '=', '$']) then
      Grab(1);
  end;
end;


function TFHIRPathLexer.done: boolean;
begin
  result := FCurrentStart > FPath.Length;
end;

function TFHIRPathLexer.error(msg: String): Exception;
begin
  result := Exception.Create('Error in '+FPath+' at '+inttostr(FCurrentStart)+': '+msg);
end;

function TFHIRPathLexer.isOp: boolean;
begin
  result := StringArrayExistsSensitive(CODES_TFHIRPathOperation, current);
end;

function TFHIRPathLexer.isToken: boolean;
var
  i : integer;
begin
  if current = '' then
    result := false
  else if StringArrayExistsSensitive(['$', '*', '**'], current) then
    result := true
  else if CharInSet(current[1], ['A'..'Z', 'a'..'z']) then
  begin
    for i := 1 to length(current) do
      result := result and CharInSet(current[i], ['A'..'Z', 'a'..'z', '0'..'9', '[', ']']) or ((i = current.Length) and (current[i] = '*'));
  end
  else
    result := false;
end;

function TFHIRPathLexer.take: String;
begin
  result := current;
  next;
end;

end.

(*
{ TFHIRQueryProcessor }

constructor TFHIRQueryProcessor.Create;
begin
  inherited;
  FResults := TFHIRObjectList.Create;
  FSource := TFHIRObjectList.Create;
end;

destructor TFHIRQueryProcessor.Destroy;
begin
  FSource.Free;
  FResults.Free;
  inherited;
end;

procedure TFHIRQueryProcessor.execute;
var
  src, seg : String;
  i : integer;
  first : boolean;
  list : TFhirReferenceList;
begin
  src := FPath;
  if (src = '*') and (FSource[0] is TFHIRResource) then
  begin
    list := TFhirReferenceList.Create;
    try
      listReferences(FSource[0] as TFHIRResource, list);
      FResults.AddAll(list);
    finally
      list.Free;
    end;
  end
  else
begin
  first := true;
  while (src <> '') do
  begin
    StringSplit(src, '.', seg, src);
    if (not IsValidIdent(seg)) Then
      raise exception.create('unable to parse path "'+FPath+'"');
    FResults.clear;
    if first then
      for i := 0 to FSource.count - 1 Do
      begin
        if FSource[i].ClassName = 'TFhir'+seg then
          FResults.add(FSource[i].Link);
      end
    else
      for i := 0 to FSource.count - 1 Do
        FSource[i].GetChildrenByName(seg, FResults);
    first := false;
    for i := FResults.count- 1 downto 0 do
      if (FResults[i] = nil) then
        FResults.DeleteByIndex(i);
    if src <> '' then
    begin
      FSource.Free;
      FSource := FResults;
      FResults := TFHIRObjectList.Create;
      end;
    end;
  end;
end;


*)
