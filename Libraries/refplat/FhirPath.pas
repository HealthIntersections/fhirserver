unit FhirPath;

interface

uses
  SysUtils, Math, RegExpr, Generics.Collections,
  StringSupport,
  AdvObjects, AdvGenerics,

  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRProfileUtilities;

type
  EFHIRPath = class (Exception)
  public
     constructor create(path : String; offset : integer; problem : String);
  end;

  TFHIRPathOperation = (opNull, poEquals, poEquivalent, poNotEquals, poNotEquivalent, poLessThen, poGreater, poLessOrEqual, poGreaterOrEqual,
     poUnion, poIn, poAnd, poOr, poXor, poPlus, poMinus, poConcatenate);

  TFHIRPathFunction = (pfNull, pfEmpty, pfNot, pfWhere, pfAll, pfAny, pfItem, pfFirst, pfLast, pfTail, pfCount, pfAsInteger, pfStartsWith, pfSubString, pfLength, pfMatches, pfDistinct, pfResolve, pfContains);

const
  CODES_TFHIRPathOperation : array [TFHIRPathOperation] of String = ('', '=' , '~' , '!=' , '!~' , '>' , '<' , '<=' , '>=' , '|' , 'in' , 'and' , 'or' , 'xor' , '+' , '-' , '&');
  CODES_TFHIRPathFunctions : array [TFHIRPathFunction] of String = ('', 'empty' , 'not' , 'where' , 'all' , 'any' , 'item' , 'first' , 'last' , 'tail' , 'count' , 'asInteger' , 'startsWith' , 'substring', 'length' , 'matches' , 'distinct' , 'resolve' , 'contains');

type
  TFHIRPathLexer = class (TAdvObject)
  private
    FPath : String;
    FCursor : integer;
    FCurrent : String;
    FCurrentStart : integer;
    function replaceFixedConstant(const s : String) : String;
  public
    constructor Create(path : String); overload;
    destructor Destroy; override;
    procedure next;
    property current : String read FCurrent;
    property CurrentStart : integer read FCurrentStart;
    function done : boolean;
    function take : String;

    function error(msg : String) : Exception; overload;
    function error(msg : String; offset : integer) : Exception; overload;
    function isConstant : boolean;
    function isToken : boolean;
    function isOp : boolean;
  end;

  TFHIRPathExpression = class (TAdvObject)
  private
    FName: String;
    FConstant : string;
    FFunctionId : TFHIRPathFunction;
    FParameters : TAdvList<TFHIRPathExpression>;
    FInner: TFHIRPathExpression;
    FOperation : TFHIRPathOperation;
    FProximal : boolean;
    FOpNext: TFHIRPathExpression;

    procedure SetOpNext(const Value: TFHIRPathExpression);
    procedure SetInner(const Value: TFHIRPathExpression);
    procedure SetFunctionId(const Value: TFHIRPathFunction);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : TFHIRPathExpression; overload;
    function checkName : boolean;

    property name : String read FName write FName;
    property constant : String read FConstant;
    property FunctionId : TFHIRPathFunction read FFunctionId write SetFunctionId;
    property Parameters : TAdvList<TFHIRPathExpression> read FParameters;
    property Inner : TFHIRPathExpression read FInner write SetInner;
    property Operation : TFHIRPathOperation read FOperation write FOperation;
    property Proximal : boolean read FProximal write FProximal;
    property OpNext : TFHIRPathExpression read FOpNext write SetOpNext;
  end;

  TFHIRPathEvaluator = class (TAdvObject)
  private
    worker : TValidatorServiceProvider;
    function parseExpression(lexer: TFHIRPathLexer; proximal : boolean): TFHIRPathExpression;
    procedure checkParameters(lexer : TFHIRPathLexer; offset : Integer; exp : TFHIRPathExpression);

    function execute(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression; atEntry : boolean) : TFHIRBaseList; overload;
    function execute(originalContext : TFHIRBaseList; item : TFHIRBase; exp : TFHIRPathExpression; atEntry : boolean) : TFHIRBaseList; overload;
    function evaluateFunction(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
    function operate(left : TFHIRBaseList; op : TFHIRPathOperation; right : TFHIRBaseList) : TFHIRBaseList;
    function readConstant(constant : String) : TFHIRType;

    function executeType(resource : string; originalContext, context : TAdvStringSet; exp : TFHIRPathExpression; atEntry : boolean) : TAdvStringSet; overload;
    function executeType(originalContext : TAdvStringSet; item : string; exp : TFHIRPathExpression; atEntry : boolean) : TAdvStringSet; overload;
    function evaluateFunctionType(resource : string; originalContext, context: TAdvStringSet; exp: TFHIRPathExpression): TAdvStringSet;
    function operateType(left : TAdvStringSet; op : TFHIRPathOperation; right : TAdvStringSet) : TAdvStringSet;
    function readConstantType(constant : String) : string;

    procedure ListChildTypesByName(item, name : string; result : TAdvStringSet);
    function getElementDefinition(sd : TFHIRStructureDefinition; path : String; var specifiedType : String) : TFHIRElementDefinition;
    function getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
    function hasDataType(ed : TFhirElementDefinition) : boolean;
    function primitives(collection : TAdvStringSet) : TAdvStringSet;
    function isPrimitiveType(s : String) : boolean;

    function funcEmpty(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcItem(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcWhere(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAll(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAny(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcFirst(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcLast(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcTail(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcCount(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcAsInteger(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcStartsWith(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcLength(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcMatches(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcDistinct(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcNot(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcResolve(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcContains(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;
    function funcSubString(resource : TFHIRResource; originalContext, context : TFHIRBaseList; exp : TFHIRPathExpression) : TFHIRBaseList;

    function opEquals(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opNotEquals(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opLessThen(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opGreater(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opLessOrEqual(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opGreaterOrEqual(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opIn(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opPlus(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opMinus(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opEquivalent(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opNotEquivalent(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opUnion(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opAnd(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opOr(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opXor(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opConcatenate(left, right : TFHIRBaseList) : TFHIRBaseList;

  public
    constructor create(context : TValidatorServiceProvider);
    destructor destroy; override;

    // Parse a path for later use using execute
    function parse(path : String) : TFHIRPathExpression;

    // check that paths referred to in the expression are valid
    function check(resourceType, context : String; path : String) : TArray<String>;

    // evaluate a path and return the matching elements
    function evaluate(base : TFHIRBase; path : String) : TFHIRBaseList; overload;
    function evaluate(base : TFHIRBase; expr : TFHIRPathExpression) : TFHIRBaseList; overload;
    function evaluate(resource : TFHIRResource; base : TFHIRBase; path : String) : TFHIRBaseList; overload;
    function evaluate(resource : TFHIRResource; base : TFHIRBase; expr : TFHIRPathExpression) : TFHIRBaseList; overload;

    // evaluate a path and return true or false
    function evaluateToBoolean(resource : TFHIRResource; base : TFHIRBase; path : String) : boolean;

    // evaluate a path and return a string describing the outcome
    function evaluateToString(base : TFHIRBase; path : String) : string;

    // worker routine for converting a set of objects to a string representation
    function convertToString(items : TFHIRBaseList) : String; overload;
    function convertToString(item : TFHIRBase) : String; overload;

    // worker routine for converting a set of objects to a boolean representation
    function convertToBoolean(items : TFHIRBaseList) : boolean;
  end;

  TFHIRPathTests = class (TAdvObject)
  private
    class procedure test(expr : String);
  public
    class procedure runTests;
  end;
implementation

{ TFHIRPathEvaluator }

function TFHIRPathEvaluator.check(resourceType, context : String; path: String) : TArray<String>;
var
  expr : TFHIRPathExpression;
begin
  expr := parse(path);
  try

  finally
    expr.free;
  end;

end;

procedure TFHIRPathEvaluator.checkParameters(lexer: TFHIRPathLexer; offset: Integer; exp: TFHIRPathExpression);
  procedure CheckNoParams;
  begin
    if exp.Parameters.Count > 0 then
      raise lexer.error('The function "'+exp.name+'" can not have any parameters', offset);
  end;
  procedure CheckParamCount(c : integer);
  begin
    if exp.Parameters.Count <> c then
      raise lexer.error('The function "'+exp.name+'" requires '+inttostr(c)+' parameters', offset);
  end;
  procedure CheckParamRange(c1, c2 : integer);
  begin
    if (exp.Parameters.Count < c1) or (exp.Parameters.Count > c2) then
      raise lexer.error('The function "'+exp.name+'" requires between '+inttostr(c1)+' and '+inttostr(c2)+' parameters', offset);
  end;
begin
  case exp.FFunctionId of
    pfEmpty: CheckNoParams;
    pfItem: CheckParamCount(1);
    pfWhere: CheckParamCount(1);
    pfAll: CheckParamCount(1);
    pfAny: CheckParamCount(1);
    pfFirst: CheckNoParams;
    pfLast: CheckNoParams;
    pfTail: CheckNoParams;
    pfCount: CheckNoParams;
    pfAsInteger: CheckNoParams;
    pfStartsWith: CheckParamCount(1);
    pfSubString: CheckParamRange(1, 2);
    pfLength: CheckNoParams;
    pfMatches: CheckParamCount(1);
    pfNot: CheckNoParams;
    pfResolve: CheckNoParams;
    pfContains: CheckParamCount(1);
    pfDistinct: {no chcek};
  end;
end;

function TFHIRPathEvaluator.convertToBoolean(items: TFHIRBaseList): boolean;
begin
  if (items = nil) then
    result := false
  else if (items.count = 1) and (items[0] is TFHIRBoolean) then
    result := TFHIRBoolean(items[0]).value
  else
    result := items.count > 0;
end;

function TFHIRPathEvaluator.convertToString(item: TFHIRBase): String;
begin
  if item is TFHIRPrimitiveType then
    result := (item as TFHIRPrimitiveType).StringValue
  else if item is TFhirType then
    result := gen(item as TFHIRType)
  else
    result := '';
end;

constructor TFHIRPathEvaluator.create(context: TValidatorServiceProvider);
begin
  inherited Create;
  worker := context;
end;

destructor TFHIRPathEvaluator.destroy;
begin
  worker.Free;
  inherited;
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
      b.Append(convertToString(item));
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
      result := execute(nil, list, list, exp, true);
    finally
      list.Free;
    end;
  finally
    exp.free;
  end;
end;

function TFHIRPathEvaluator.evaluate(base: TFHIRBase; expr : TFHIRPathExpression): TFHIRBaseList;
var
  list : TFHIRBaseList;
begin
  list := TFHIRBaseList.Create;
  try
    list.Add(base.Link);
    result := execute(nil, list, list, expr, true);
  finally
    list.Free;
  end;
end;

function TFHIRPathEvaluator.evaluate(resource : TFHIRResource; base: TFHIRBase; path: String): TFHIRBaseList;
var
  exp : TFHIRPathExpression;
  list : TFHIRBaseList;
begin
  exp := parse(path);
  try
    list := TFHIRBaseList.Create;
    try
      list.Add(base.Link);
      result := execute(resource, list, list, exp, true);
    finally
      list.Free;
    end;
  finally
    exp.free;
  end;
end;

function TFHIRPathEvaluator.evaluate(resource : TFHIRResource; base: TFHIRBase; expr : TFHIRPathExpression): TFHIRBaseList;
var
  list : TFHIRBaseList;
begin
  list := TFHIRBaseList.Create;
  try
    list.Add(base.Link);
    result := execute(resource, list, list, expr, true);
  finally
    list.Free;
  end;
end;

function TFHIRPathEvaluator.evaluateToBoolean(resource : TFHIRResource; base: TFHIRBase; path: String): boolean;
var
  res : TFHIRBaseList;
begin
  res := evaluate(base, path);
  try
    result := convertToBoolean(res);
  finally
    res.Free;
  end;
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

function TFHIRPathEvaluator.execute(originalContext : TFHIRBaseList; item: TFHIRBase; exp: TFHIRPathExpression; atEntry : boolean): TFHIRBaseList;
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

function TFHIRPathEvaluator.executeType(originalContext : TAdvStringSet; item: string; exp: TFHIRPathExpression; atEntry : boolean): TAdvStringSet;
begin
  result := TAdvStringSet.Create;
  try
    if atEntry and (CharInSet(exp.name[1], ['A'..'Z'])) then // special case for start up
    begin
      if item = exp.name then
        result.Add(item);
    end
    else
      ListChildTypesByName(item, exp.name, result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEvaluator.funcAll(resource : TFHIRResource; originalContext : TFHIRBaseList; context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  all : boolean;
begin
  all := true;
  pc := TFHIRBaseList.Create;
  try
    for item in context do
    begin
      pc.Clear;
      pc.Add(item.Link);
      res := execute(resource, originalContext, pc, exp.Parameters[0], false);
      try
        if not convertToBoolean(res) then
        begin
          all := false;
          break;
        end;
      finally
        res.Free;
      end;
    end;
  finally
    pc.Free;
  end;

  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(all));
end;

function TFHIRPathEvaluator.funcAny(resource : TFHIRResource; originalContext : TFHIRBaseList; context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  all : boolean;
begin
  all := false;
  pc := TFHIRBaseList.Create;
  try
    for item in context do
    begin
      pc.Clear;
      pc.Add(item.Link);
      res := execute(resource, originalContext, pc, exp.Parameters[0], false);
      try
        if convertToBoolean(res) then
        begin
          all := true;
          break;
        end;
      finally
        res.Free;
      end;
    end;
  finally
    pc.Free;
  end;

  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(all));
end;

function TFHIRPathEvaluator.funcAsInteger(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  s : String;
begin
  s := convertToString(context);
  result := TFHIRBaseList.Create;
  if StringIsInteger32(s) then
    result.Add(TFhirInteger.Create(s));
end;

function TFHIRPathEvaluator.funcContains(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcCount(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirInteger.Create(inttostr(context.Count)));
end;

function TFHIRPathEvaluator.funcDistinct(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcEmpty(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(context.Count = 0));
end;

function TFHIRPathEvaluator.funcFirst(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if context.Count > 0 then
    result.Add(context[0].Link);
end;

function TFHIRPathEvaluator.funcItem(resource : TFHIRResource; originalContext : TFHIRBaseList; context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  s : String;
  res : TFHIRBaseList;
begin
  res := execute(resource, originalContext, context, exp.Parameters[0], false);
  try
		s := convertToString(res);
  finally
    res.Free;
  end;
  result := TFHIRBaseList.Create;
  if StringIsInteger16(s) and (context.Count > StrToInt(s)) then
    result.Add(context[StrToInt(s)].Link);
end;

function TFHIRPathEvaluator.funcLast(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if context.Count > 0 then
    result.Add(context[context.Count - 1].Link);
end;

function TFHIRPathEvaluator.funcLength(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  s : String;
  l : integer;
begin
  l := 0;
  for item in context do
  begin
    s := convertToString(item);
    l := max(l, s.Length);
  end;
  result := TFHIRBaseList.Create;
  result.Add(TFhirInteger.Create(inttostr(l)));
end;

function TFHIRPathEvaluator.funcMatches(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  s, p : String;
  reg : TRegExpr;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(resource, originalContext, context, exp.Parameters[0], false);
    try
      p := convertToString(res);
    finally
      res.free;
    end;
    reg := TRegExpr.Create;
    try
      reg.Expression := p;
      for item in context do
      begin
        s := convertToString(item);
        if (reg.Exec(s)) then
          result.Add(item.Link);
      end;
    finally
      reg.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.funcNot(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(not convertToBoolean(context)));
end;

function TFHIRPathEvaluator.funcResolve(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcStartsWith(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  s, sw : String;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(resource, originalContext, context, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    for item in context do
    begin
      s := convertToString(item);
      if (s.StartsWith(sw)) then
        result.Add(item.Link);
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.funcSubString(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  raise Exception.Create('The function '+exp.name+' is not done yet');
end;

function TFHIRPathEvaluator.funcTail(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
   i : integer;
begin
  result := TFHIRBaseList.Create;
  for i := 1 to Context.Count -1 do
    result.Add(context[i].Link);
end;

function TFHIRPathEvaluator.funcWhere(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  try
    pc := TFHIRBaseList.Create;
    try
      for item in context do
      begin
        pc.Clear;
        pc.Add(item.Link);
        res := execute(resource, originalContext, pc, exp.Parameters[0], false);
        try
          if convertToBoolean(res) then
            result.Add(item.Link);
        finally
          res.Free;
        end;
      end;
    finally
      pc.Free;
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.operate(left: TFHIRBaseList; op: TFHIRPathOperation; right: TFHIRBaseList): TFHIRBaseList;
begin
  case op of
    opNull: raise Exception.create('An internal error has occurred');
    poEquals: result := opEquals(left, right);
    poNotEquals: result := opNotEquals(left, right);
    poLessThen: result := opLessThen(left, right);
    poGreater: result := opGreater(left, right);
    poLessOrEqual: result := opLessOrEqual(left, right);
    poGreaterOrEqual: result := opGreaterOrEqual(left, right);
    poIn: result := opIn(left, right);
    poPlus: result := opPlus(left, right);
    poMinus: result := opMinus(left, right);
    poEquivalent: result := opEquivalent(left, right);
    poNotEquivalent: result := opNotEquivalent(left, right);
    poUnion: result := opUnion(left, right);
    poAnd: result := opAnd(left, right);
    poOr: result := opOr(left, right);
    poXor: result := opXor(left, right);
    poConcatenate: result := opConcatenate(left, right);

  else
    result := nil;
  end;
end;


function TFHIRPathEvaluator.operateType(left: TAdvStringSet; op: TFHIRPathOperation; right: TAdvStringSet): TAdvStringSet;
begin
  case op of
    poEquals: result := TAdvStringSet.Create('boolean');
    poEquivalent: result := TAdvStringSet.Create('boolean');
    poNotEquals: result := TAdvStringSet.Create('boolean');
    poNotEquivalent: result := TAdvStringSet.Create('boolean');
    poLessThen: result := TAdvStringSet.Create('boolean');
    poGreater: result := TAdvStringSet.Create('boolean');
    poLessOrEqual: result := TAdvStringSet.Create('boolean');
    poGreaterOrEqual: result := TAdvStringSet.Create('boolean');
    poIn: result := TAdvStringSet.Create('boolean');
    poPlus: result := TAdvStringSet.Create('string');
    poMinus: result := TAdvStringSet.Create('string');
    poConcatenate: result := TAdvStringSet.Create('boolean');
    poOr: result := TAdvStringSet.Create('boolean');
    poAnd: result := TAdvStringSet.Create('boolean');
    poXor: result := TAdvStringSet.Create('boolean');
    poUnion: result := TAdvStringSet.Create(left, right);
  else
    raise Exception.Create('not done yet');
  end;
end;

function TFHIRPathEvaluator.opAnd(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(convertToBoolean(left) and convertToBoolean(right)));
end;

function TFHIRPathEvaluator.opConcatenate(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Multiply is not done yet');
end;

function TFHIRPathEvaluator.opEquals(left, right: TFHIRBaseList): TFHIRBaseList;
var
  sl, sr : String;
  found : boolean;
  item : TFHIRBase;
begin
  found := false;
  sr := convertToString(right);
  for item in left do
  begin
    sl := convertToString(item);
    found := found or (sl = sr);
  end;
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(found));
end;

function TFHIRPathEvaluator.opEquivalent(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Multiply is not done yet');
end;

function TFHIRPathEvaluator.opGreater(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRPrimitiveType;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0] is TFHIRPrimitiveType) and (right[0] is TFHIRPrimitiveType) then
    begin
      l := left[0] as TFHIRPrimitiveType;
      r := right[0] as TFHIRPrimitiveType;
      if (l is TFhirString) and (r is TFhirString) then
        result.Add(TFhirBoolean.Create(l.StringValue > r.StringValue))
      else if ((l is TFhirInteger) or (l is TFhirDecimal)) and ((r is TFhirInteger) or (r is TFhirDecimal)) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.stringvalue) > StrToFloat(r.stringvalue)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.opGreaterOrEqual(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRPrimitiveType;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0] is TFHIRPrimitiveType) and (right[0] is TFHIRPrimitiveType) then
    begin
      l := left[0] as TFHIRPrimitiveType;
      r := right[0] as TFHIRPrimitiveType;
      if (l is TFhirString) and (r is TFhirString) then
        result.Add(TFhirBoolean.Create(l.StringValue >= r.StringValue))
      else if ((l is TFhirInteger) or (l is TFhirDecimal)) and ((r is TFhirInteger) or (r is TFhirDecimal)) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.stringvalue) >= StrToFloat(r.stringvalue)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.opIn(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation In is not done yet');
end;

function TFHIRPathEvaluator.opLessOrEqual(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRPrimitiveType;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0] is TFHIRPrimitiveType) and (right[0] is TFHIRPrimitiveType) then
    begin
      l := left[0] as TFHIRPrimitiveType;
      r := right[0] as TFHIRPrimitiveType;
      if (l is TFhirString) and (r is TFhirString) then
        result.Add(TFhirBoolean.Create(l.StringValue <= r.StringValue))
      else if ((l is TFhirInteger) or (l is TFhirDecimal)) and ((r is TFhirInteger) or (r is TFhirDecimal)) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.stringvalue) <= StrToFloat(r.stringvalue)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.opLessThen(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRPrimitiveType;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0] is TFHIRPrimitiveType) and (right[0] is TFHIRPrimitiveType) then
    begin
      l := left[0] as TFHIRPrimitiveType;
      r := right[0] as TFHIRPrimitiveType;
      if (l is TFhirString) and (r is TFhirString) then
        result.Add(TFhirBoolean.Create(l.StringValue < r.StringValue))
      else if ((l is TFhirInteger) or (l is TFhirDecimal)) and ((r is TFhirInteger) or (r is TFhirDecimal)) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.stringvalue) < StrToFloat(r.stringvalue)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.opMinus(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Minus is not done yet');
end;

function TFHIRPathEvaluator.opNotEquals(left, right: TFHIRBaseList): TFHIRBaseList;
var
  sl, sr : String;
  found : boolean;
  item : TFHIRBase;
begin
  found := false;
  sr := convertToString(right);
  for item in left do
  begin
    sl := convertToString(item);
    found := found or (sl = sr);
  end;
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(not found));
end;

function TFHIRPathEvaluator.opNotEquivalent(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Multiply is not done yet');
end;

function TFHIRPathEvaluator.opOr(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(convertToBoolean(left) or convertToBoolean(right)));
end;

function TFHIRPathEvaluator.opPlus(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Plus is not done yet');
end;

function TFHIRPathEvaluator.opUnion(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  raise Exception.Create('The operation Multiply is not done yet');
end;

function TFHIRPathEvaluator.opXor(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(convertToBoolean(left) xor convertToBoolean(right)));
end;

function TFHIRPathEvaluator.execute(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression; atEntry : boolean): TFHIRBaseList;
var
  work, work2 : TFHIRBaseList;
  item : TFHIRBase;
  outcome : TFHIRBaseList;
  next, last : TFHIRPathExpression;
begin
  work := TFHIRBaseList.Create;
  try
    // functions are evaluated on the collection
    if exp.FunctionId <> pfNull then
    begin
      work2 := evaluateFunction(resource, originalContext, context, exp);
      try
        work.addAll(work2);
      finally
        work2.Free;
      end;
    end
    else if exp.constant <> '' then
      work.Add(readConstant(exp.constant))
    else if (exp.name = '$resource') then
      work.add(resource.Link)
    else if (exp.name = '$context') then
      work.addAll(originalContext)
    else
      for item in context do
      begin
        outcome := execute(originalContext, item, exp, atEntry);
        try
          work.AddAll(outcome);
        finally
          outcome.Free;
        end;
      end;

    if (exp.Inner <> nil) then
    begin
      result := execute(resource, originalContext, work, exp.Inner, false);
      work.Free;
      work := result;
    end;

    if (exp.proximal and (exp.Operation <> opNull)) then
    begin
      next := exp.OpNext;
      last := exp;
      while (next <> nil) do
      begin
        work2 := execute(resource, originalContext, context, next, false);
        try
          result := operate(work, last.Operation, work2);
          work.Free;
          work := result;
        finally
          work2.Free;
        end;
        last := next;
        next := next.OpNext;
      end;
    end;
    result := work.Link;
  finally
    work.Free;
  end;
end;

function TFHIRPathEvaluator.executeType(resource : string; originalContext, context: TAdvStringSet; exp: TFHIRPathExpression; atEntry : boolean): TAdvStringSet;
var
  work, work2 : TAdvStringSet;
  item : string;
  outcome : TAdvStringSet;
  next, last : TFHIRPathExpression;
begin
  work := TAdvStringSet.Create;
  try
    // functions are evaluated on the collection
    if exp.FunctionId <> pfNull then
    begin
      work2 := evaluateFunctionType(resource, originalContext, context, exp);
      try
        work.addAll(work2);
      finally
        work2.Free;
      end;
    end
    else if exp.constant <> '' then
      work.add(readConstantType(exp.constant))
    else if (exp.name = '$resource') then
    begin
      if (resource <> '') then
        work.add(resource)
      else
        work.add('DomainResource');
    end
    else if (exp.name = '$context') then
      work.addAll(originalContext)
    else
      for item in context do
      begin
        outcome := executeType(originalContext, item, exp, atEntry);
        try
          work.AddAll(outcome);
        finally
          outcome.Free;
        end;
      end;

    if (exp.Inner <> nil) then
    begin
      result := executeType(resource, originalContext, work, exp.Inner, false);
      work.Free;
      work := result;
    end;

    if (exp.proximal and (exp.Operation <> opNull)) then
    begin
      next := exp.OpNext;
      last := exp;
      while (next <> nil) do
      begin
        work2 := executeType(resource, originalContext, context, next, false);
        try
          result := operateType(work, last.Operation, work2);
          work.Free;
          work := result;
        finally
          work2.Free;
        end;
        last := next;
        next := next.OpNext;
      end;
    end;
    result := work.Link;
  finally
    work.Free;
  end;
end;

function TFHIRPathEvaluator.evaluateFunction(resource : TFHIRResource; originalContext, context: TFHIRBaseList; exp: TFHIRPathExpression): TFHIRBaseList;
begin
  case exp.FFunctionId of
    pfEmpty : result := funcEmpty(resource, originalContext, context, exp);
    pfItem : result := funcItem(resource, originalContext, context, exp);
    pfWhere : result := funcWhere(resource, originalContext, context, exp);
    pfAll : result := funcAll(resource, originalContext, context, exp);
    pfAny : result := funcAny(resource, originalContext, context, exp);
    pfFirst : result := funcFirst(resource, originalContext, context, exp);
    pfLast : result := funcLast(resource, originalContext, context, exp);
    pfTail : result := funcTail(resource, originalContext, context, exp);
    pfCount : result := funcCount(resource, originalContext, context, exp);
    pfAsInteger : result := funcAsInteger(resource, originalContext, context, exp);
    pfStartsWith : result := funcStartsWith(resource, originalContext, context, exp);
    pfLength : result := funcLength(resource, originalContext, context, exp);
    pfMatches : result := funcMatches(resource, originalContext, context, exp);
    pfDistinct : result := funcDistinct(resource, originalContext, context, exp);
    pfNot : result := funcNot(resource, originalContext, context, exp);
    pfResolve : result := funcResolve(resource, originalContext, context, exp);
    pfContains : result := funcContains(resource, originalContext, context, exp);
    pfSubString : result := funcSubString(resource, originalContext, context, exp);
  else
    raise Exception.Create('Unknown Function '+exp.name);
  end;
end;

function TFHIRPathEvaluator.primitives(collection : TAdvStringSet) : TAdvStringSet;
var
  s : String;
begin
  result := TAdvStringSet.create;
  try
    for s in collection do
      if (isPrimitiveType(s)) then
        result.add(s);
  finally
    result.free;
  end;
end;

function TFHIRPathEvaluator.isPrimitiveType(s : String) : boolean;
begin
  result := (s = 'boolean') or (s = 'integer') or (s = 'decimal') or (s = 'base64Binary') or (s = 'instant') or (s = 'string') or (s = 'uri') or (s = 'date') or (s = 'dateTime') or (s = 'time') or (s = 'code') or (s = 'oid') or (s = 'id') or (s = 'unsignedInt') or (s = 'positiveInt') or (s = 'markdown');
end;


function TFHIRPathEvaluator.evaluateFunctionType(resource: string; originalContext, context: TAdvStringSet; exp: TFHIRPathExpression): TAdvStringSet;
var
  expr : TFHIRPathExpression;
begin
  for expr in exp.parameters do
    executeType(resource, originalContext, context, expr, false).Free; // just checking...

  case exp.FunctionId of
    pfEmpty : result := TAdvStringSet.create('boolean');
    pfItem : result := context.Link;
    pfWhere : result := context.Link;
    pfAll : result := TAdvStringSet.create('boolean');
    pfAny : result := TAdvStringSet.create('boolean');
    pfFirst : result := context.Link;
    pfLast : result := context.Link;
    pfTail : result := context.Link;
    pfCount : result := TAdvStringSet.create('integer');
    pfAsInteger : result := TAdvStringSet.create('integer');
    pfStartsWith : result := primitives(context);
    pfLength : result := TAdvStringSet.create('integer');
    pfMatches : result := primitives(context);
    pfContains : result := primitives(context);
    pfSubstring : result := TAdvStringSet.create('integer');
    pfDistinct : result := TAdvStringSet.create('boolean');
    pfNot : result := TAdvStringSet.create('boolean');
    pfResolve : result := TAdvStringSet.create('DomainResource');
  end;
end;


function cleanConstant(s: String; lexer : TFHIRPathLexer) : String;
var
  b : TStringBuilder;
  e : boolean;
  i : integer;
  ch : char;
begin
  if (s.StartsWith('"') and s.EndsWith('"')) then
  begin
    b := TStringBuilder.Create;
    try
      e := false;
      for i := 2 to length(s)-1 do
      begin
        ch := s[i];
        if (e) then
        begin
          case ch of
            't': b.Append(#9);
            'r': b.Append(#13);
            'n': b.Append(#10);
            '\': b.Append('\');
            '''': b.Append('''');
            '"': b.Append('"');
          else
            raise lexer.error('Unknown character escape \\'+ch);
          end;
        end
        else if ch = '\' then
          e := true
        else
          b.Append(ch);
      end;
      result := b.toString;
    finally
      b.Free;
    end;
  end
  else
    result := s;
end;


function TFHIRPathEvaluator.parseExpression(lexer : TFHIRPathLexer; proximal : boolean): TFHIRPathExpression;
var
  c : Integer;
begin
  result := TFHIRPathExpression.Create;
  try
    c := lexer.CurrentStart;
    if lexer.isConstant then
      result.FConstant := cleanConstant(lexer.take, lexer)
    else
    begin
      if lexer.current = '(' then
      begin
        lexer.next;
        result.Free;
        result := nil;
        result := parseExpression(lexer, true);
        if lexer.current <> ')' then
          raise lexer.error('Found '+lexer.current+' expecting a ")"');
        lexer.next;
      end
      else
      begin
        if not lexer.isToken then
          raise lexer.error('Found '+lexer.current+' expecting a token name');
        result.FName := lexer.take;
        if not result.checkName then
          raise lexer.error('Found '+lexer.current+' expecting a valid token name');
        if (lexer.current = '(') then
        begin
          if not StringArrayExistsSensitive(CODES_TFHIRPathFunctions, result.FName) then
            raise lexer.error('The name '+result.FName+' is not a valid function name');
          result.FunctionId := TFHIRPathFunction(StringArrayIndexOfSensitive(CODES_TFHIRPathFunctions, result.FName));
          lexer.next;
          while lexer.current <> ')' do
          begin
            result.Parameters.add(parseExpression(lexer, true));
            if lexer.current = ',' then
              lexer.next
            else if lexer.current <> ')' then
              raise lexer.error('The token '+lexer.current+' is not expected here - either a "," or a ")" expected');
          end;
          lexer.next;
          checkParameters(lexer, c, result);
        end;
      end;
      if lexer.current = '.' then
      begin
        lexer.next;
        result.Inner := parseExpression(lexer, false);
      end;
    end;
    if (proximal) then
    begin
      while lexer.isOp do
      begin
        result.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current));
        lexer.next;
        result.opNext := parseExpression(lexer, false);
      end;
    end;
    result.Proximal := proximal;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEvaluator.readConstant(constant: String): TFHIRType;
begin
  if (constant = 'true') then
    result := TFhirBoolean.Create(true)
  else if (constant = 'false') then
    result := TFhirBoolean.Create(false)
  else if StringIsInteger32(constant) then
    result := TFhirInteger.Create(constant)
  else if IsNumericString(constant) then
    result := TFhirDecimal.Create(constant)
  else
    result := TFhirString.Create(constant);
end;

function TFHIRPathEvaluator.readConstantType(constant: String): string;
begin
  if (constant = 'true') then
    result := 'boolean'
  else if (constant = 'false') then
    result := 'boolean'
  else if StringIsInteger32(constant) then
    result := 'integer'
  else if IsNumericString(constant) then
    result := 'decimal'
  else
    result := 'string';
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
    result := parseExpression(lexer, true);
    try
      if not lexer.done then
        raise lexer.error('Premature expression termination at unexpected token "'+lexer.current+'"');
      result.Link;
    finally
      result.free;
    end;
  finally
    lexer.Free;
  end;
end;

procedure TFHIRPathEvaluator.ListChildTypesByName(item, name : String; result : TAdvStringSet);
var
  url, tail, specifiedType, path, tn : String;
  sd, dt, sdi : TFhirStructureDefinition;
  sdl : TAdvList<TFhirStructureDefinition>;
  ed : TFhirElementDefinition;
  t : TFhirElementDefinitionType;
begin
  if (item = '') then
    raise Exception.create('No type provided in BuildToolPathEvaluator.getChildTypesByName');
  if (item.equals('xhtml')) then
    exit;
  if (item.contains('.')) then
    url := 'http://hl7.org/fhir/StructureDefinition/'+item.substring(0, item.indexOf('.'))
  else
    url := 'http://hl7.org/fhir/StructureDefinition/'+item;
  sd := worker.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
  if (sd = nil) then
    raise Exception.create('Unknown item '+item); // this really is an error, because we can only get to here if the internal infrastrucgture is wrong
  sdl := TAdvList<TFhirStructureDefinition>.create;
  try
    if (item.contains('.')) then
      ed := getElementDefinition(sd, item, specifiedType);
    if ((ed <> nil) and hasDataType(ed)) then
    begin
      if specifiedType <> '' then
      begin
        dt := worker.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+specifiedType) as TFhirStructureDefinition;
        if (dt = nil) then
          raise Exception.create('unknown data type '+specifiedType);
        sdl.add(dt.Link);
      end
      else
        for t in ed.type_List do
        begin
          dt := worker.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+t.Code) as TFhirStructureDefinition;
          if (dt = nil) then
            raise Exception.create('unknown data type '+t.code);
          sdl.add(dt.Link);
        end;
    end
    else
    begin
      sdl.add(sd.Link);
      if (item.contains('.')) then
        tail := item.substring(item.indexOf('.'));
    end;

    for sdi in sdl do
    begin
      path := sdi.snapshot.elementList[0].path+tail+'.';
      if (name = '**') then
      begin
        for ed in sdi.snapshot.elementList do
        begin
          if (ed.path.startsWith(path)) then
            for t in ed.type_List do
            begin
              if (t.code.equals('Element') or t.code.equals('BackboneElement')) then
                tn := ed.path
              else
                tn := t.code;
              if (not result.contains(tn)) then
              begin
                result.add(tn);
                ListChildTypesByName(tn, '**', result);
              end;
            end;
        end;
      end
      else if (name.equals('*')) then
      begin
        for ed in sdi.snapshot.elementList do
        begin
          if (ed.path.startsWith(path) and not ed.path.substring(path.length).contains('.')) then
            for t in ed.type_List do
              if (t.code.equals('Element') or t.code.equals('BackboneElement')) then
                result.add(ed.path)
              else if (t.code.equals('Resource')) then
                result.addAll(worker.getResourceNames())
              else
                result.add(t.code);
        end;
      end
      else
      begin
        if (name.endsWith('*')) then
          path := sdi.snapshot.elementList[0].path+tail+'.'+name.substring(0, name.length-1)
        else
          path := sdi.snapshot.elementList[0].path+tail+'.'+name;

        ed := getElementDefinition(sdi, path, specifiedType);
        if (ed <> nil) then
        begin
          if (specifiedType <> '') then
            result.add(specifiedType)
          else
          begin
            for t in ed.type_list do
            begin
              if (t.code = '') then
                raise Exception.create('Illegal reference to primative value attribute @ '+path);

              if (t.code.equals('Element') or t.code.equals('BackboneElement')) then
                result.add(path)
              else if (t.code.equals('Resource')) then
                result.addAll(worker.getResourceNames())
              else
                result.add(t.code);
            end;
          end;
        end;
      end;
    end;
  finally
    sdl.Free;
  end;
end;

function hasType(ed : TFhirElementDefinition; s : String) : boolean;
var
  t : TFhirElementDefinitionType;
begin
	result := false;
	for t in ed.type_List do
		if (s.equals(t.code)) then
			exit(true);
end;

function TFHIRPathEvaluator.getElementDefinition(sd : TFHIRStructureDefinition; path : String; var specifiedType : String) : TFHIRElementDefinition;
var
  ed, m : TFhirElementDefinition;
begin
  specifiedType := '';
  result := nil;
  for ed in sd.snapshot.elementList do
  begin
    if (ed.path.equals(path)) then
    begin
      if (ed.NameReference <> '') then
        exit(getElementDefinitionByName(sd, ed.NameReference))
      else
        exit(ed);
    end;

      if (ed.path.endsWith('[x]') and path.startsWith(ed.path.substring(0, ed.path.length-3)) and hasType(ed, path.Substring(ed.path.length-3))) then
      begin
        specifiedType := path.Substring(ed.path.length-3);
        exit(ed);
      end;
      if ((ed.NameReference <> '') and path.startsWith(ed.path+'.')) then
      begin
        m := getElementDefinitionByName(sd, ed.NameReference);
        exit(getElementDefinition(sd, m.path+path.substring(ed.path.length), specifiedType));
      end;
  end;
end;

function TFHIRPathEvaluator.hasDataType(ed : TFhirElementDefinition) : boolean;
begin
  result := (ed.type_List.Count > 0) and not (ed.type_list[0].code.equals('Element') or ed.type_list[0].code.equals('BackboneElement'));
end;

function TFHIRPathEvaluator.getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
var
  ed : TFhirElementDefinition;
begin
  for ed in sd.snapshot.elementList do
    if (name.equals(ed.name)) then
      exit(ed);
  result := nil;
end;

{ TFHIRPathExpression }

function TFHIRPathExpression.checkName: boolean;
begin
  if (name.StartsWith('$')) then
    result := StringArrayExistsSensitive(['$context', '$parent', '$resource'], name)
  else
    result := true;
end;

constructor TFHIRPathExpression.Create;
begin
  inherited;

end;

destructor TFHIRPathExpression.Destroy;
begin
  FParameters.free;
  FOpNext.Free;
  FInner.Free;
  inherited;
end;

function TFHIRPathExpression.Link: TFHIRPathExpression;
begin
  result := TFHIRPathExpression(inherited Link);
end;

procedure TFHIRPathExpression.SetFunctionId(const Value: TFHIRPathFunction);
begin
  FFunctionId := Value;
  if FParameters = nil then
    FParameters := TAdvList<TFHIRPathExpression>.create;
end;

procedure TFHIRPathExpression.SetOpNext(const Value: TFHIRPathExpression);
begin
  FOpNext.Free;
  FOpNext := Value;
end;

procedure TFHIRPathExpression.SetInner(const Value: TFHIRPathExpression);
begin
  FInner.Free;
  FInner := Value;
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
  escape : boolean;
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
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if CharInSet(ch, ['A'..'Z', 'a'..'z']) then
    begin
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', '[', ']', '*']) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '%') then
    begin
      inc(FCursor);
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', ':', '-']) do
        inc(FCursor);
      FCurrent := replaceFixedConstant(copy(FPath, FCurrentStart, FCursor-FCurrentStart));
    end
    else if (ch = '$') then
    begin
      inc(FCursor);
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['a'..'z']) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '"') or (ch = '''') then
    begin
      inc(FCursor);
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> ch)) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        inc(FCursor);
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
      if (ch = '''') then
        FCurrent := '"'+copy(FCurrent, 2, FCurrent.Length - 2)+'"';
    end
    else // if CharInSet(ch, ['.', ',', '(', ')', '=']) then
      Grab(1);
  end;
end;


function TFHIRPathLexer.replaceFixedConstant(const s: String): String;
begin
  if s = '%sct' then
    result := '"http://snomed.info/sct"'
  else if s = '%loinc' then
    result := '"http://loinc.org"'
  else if s = '%ucum' then
    result := '"http://unitsofmeasure.org"'
  else if s = '%uz-zip' then
    result := '"[0-9]{5}(-[0-9]{4}){0,1}"'
  else if s.StartsWith('%vs-') then
    result := '"http://hl7.org/fhir/ValueSet/'+s.Substring(4)+'"'
  else if s.StartsWith('%ext-') then
    result := '"http://hl7.org/fhir/StructureDefinition/'+s.Substring(5)+'"'
  else
    raise error('Unknown fixed constant '+s);
end;

function TFHIRPathLexer.done: boolean;
begin
  result := FCurrentStart > FPath.Length;
end;

function TFHIRPathLexer.error(msg: String; offset: integer): Exception;
begin
  result := Exception.Create('Error in '+FPath+' at '+inttostr(offset)+': '+msg);
end;

function TFHIRPathLexer.error(msg: String): Exception;
begin
  result := error(msg, FCurrentStart);
end;

function TFHIRPathLexer.isConstant: boolean;
begin
  result := (FCurrent <> '') and (CharInSet(FCurrent[1], ['"', '0'..'9']) or (FCurrent = 'true') or (FCurrent = 'false'));
end;

function TFHIRPathLexer.isOp: boolean;
begin
  result := (current <> '') and StringArrayExistsSensitive(CODES_TFHIRPathOperation, current);
end;

function TFHIRPathLexer.isToken: boolean;
var
  i : integer;
begin
  if current = '' then
    result := false
  else if current.StartsWith('$') then
    result := true
  else if StringArrayExistsSensitive(['*', '**'], current) then
    result := true
  else if CharInSet(current[1], ['A'..'Z', 'a'..'z']) then
  begin
    result := true;
    for i := 1 to length(current) do
      result := result and (CharInSet(current[i], ['A'..'Z', 'a'..'z', '0'..'9', '[', ']']) or ((i = current.Length) and (current[i] = '*')));
  end
  else
    result := false;
end;

function TFHIRPathLexer.take: String;
begin
  result := current;
  next;
end;

{ TFHIRPathTests }

class procedure TFHIRPathTests.runTests;
begin
//  test('aggregation.empty() or (code = "Reference")');
//  test('binding.empty() or type.code.empty() or type.any((code = ''code'') or (code = ''Coding'') or (code=''CodeableConcept'') or (code = ''Quantity'') or (code = ''Extension'') or (code = ''string'') or (code = ''uri''))');
//  test('(low.empty() or ((low.code = "%") and (low.system = %ucum))) and (high.empty() or ((high.code = "%") and (high.system = %ucum)))');
//  test('kind != ''root'' or uniqueId in (''uuid'' | ''ruid'')');
  test('reference.startsWith("#").not() or $resource.contained.where(id = $context.reference.substring(1))');
  test('(name.item(1).family | name.item(2).family).count() < 4');
end;

class procedure TFHIRPathTests.test(expr: String);
var
  parser : TFHIRPathEvaluator;
begin
  parser := TFHIRPathEvaluator.create(nil);
  try
    parser.parse(expr).Free;
  finally
    parser.Free;
  end;
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
