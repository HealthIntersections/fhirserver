unit CqlParser;

interface

uses
  SysUtils, Generics.Collections,
  StringSupport, ParserSupport,
  AdvObjects, AdvGenerics,
  FHIRBase, FHIRPath,
  CQLModel;

Type
  TCqlLexer = class (TFHIRPathLexer)
  private
  public
    procedure next; override;
  end;

  TCqlFunction = class (TAdvObject)
  private
    FName: String;
    FIndex: integer;
    FMaxParamCount: integer;
    FTypeName: String;
    FMinParamCount: integer;
  public
    constructor Create(name, typeName : String; minParamCount, maxParamCount : Integer);
    function Link : TCqlFunction;

    property name : String read FName write FName;
    property typeName : String read FTypeName write FTypeName;
    property minParamCount : integer read FMinParamCount write FMinParamCount;
    property maxParamCount : integer read FMaxParamCount write FMaxParamCount;

    property index : integer read FIndex write FIndex;
  end;

  TCqlFunctionRegistry = class (TAdvObject)
  private
    FMap : TAdvMap<TCqlFunction>;
    FList : TAdvList<TCqlFunction>;
    procedure add(func : TCqlFunction);
    procedure populate;
    function GetDefinition(i: integer): TCqlFunction;
  public
    constructor Create; override;
    destructor Destroy; override;
    property definition[i : integer] : TCqlFunction read GetDefinition; default;
    function IndexOf(name : String) : integer;
  end;

  TCqlParserState = (cpsProximal, cpsCheckAlias, cpsAllowInner);
  TCqlParserStateSet = set of TCqlParserState;

  TCqlParser = class (TFHIRPathParser)
  private
    FRegistry : TCqlFunctionRegistry;
    function isReservedWord(word : String) : boolean;

    procedure readVersion(lexer : TFHIRPathLexer; item : TCqlVersioned);
    function readContext(lexer : TFHIRPathLexer) : TCqlContextType;

    procedure organisePrecedence(lexer : TFHIRPathLexer; var node: TCqlExpressionNode);

    procedure readConstant(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readGroup(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readRetrieve(lexer: TFHIRPathLexer; expression : TCqlExpressionNode; states : TCqlParserStateSet);
    procedure checkForAlias(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readTuple(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readFunction(lexer : TFHIRPathLexer; expression : TCqlExpressionNode; name : String);
    procedure readCode(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readInterval(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
    procedure readIf(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
    function parseExpression(lexer : TFHIRPathLexer; states : TCqlParserStateSet) : TCQLExpressionNode;

    function parseListType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
    function parseIntervalType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
    function parseTupleType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
    function parseChoiceType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
    function parseSimpleType(lexer : TFHIRPathLexer; token : string) : TCqlTypeSpecifier;
    function parseTypeDetails(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
    function parseDefine(lexer : TFHIRPathLexer; context : TCqlContextType; access : TCqlAccessLevel) : TCqlExpressionDefinition;
    function parseFunction(lexer : TFHIRPathLexer; context : TCqlContextType; access : TCqlAccessLevel) : TCqlFunctionDefinition;
    function parseValueSet(lexer : TFHIRPathLexer; access : TCqlAccessLevel) : TCqlValueSetReference;
    function parseCodeSystem(lexer : TFHIRPathLexer; access : TCqlAccessLevel) : TCqlCodeSystemReference;
    function parseParameter(lexer : TFHIRPathLexer) : TCqlParameterDefinition;
    function parseInclude(lexer : TFHIRPathLexer) : TCqlInclude;
    function parseUsing(lexer : TFHIRPathLexer) : TCqlUsing;
    function parseLibrary(lexer : TFHIRPathLexer) : TCqlLibrary;

  public
    constructor Create; override;
    destructor Destroy; override;
    function parseCql(lib : String) : TCqlLibrary;
  end;

implementation

{ TCqlParser }

function TCqlParser.parseCodeSystem(lexer: TFHIRPathLexer; access : TCqlAccessLevel): TCqlCodeSystemReference;
begin
  result := TCqlCodeSystemReference.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.readConstant('codesystem name');
    result.AccessLevel := access;
    lexer.token(':');
    result.URL := lexer.readConstant('url');
    readVersion(lexer, result);

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseCql(lib: String): TCqlLibrary;
var
  lexer : TCqlLexer;
begin
  lexer := TCqlLexer.Create(lib);
  try
    result := parseLibrary(lexer);
  finally
    lexer.Free;
  end;
end;

function TCqlParser.parseFunction(lexer: TFHIRPathLexer; context: TCqlContextType; access: TCqlAccessLevel): TCqlFunctionDefinition;
var
  first : boolean;
  s : String;
  param : TCqlFunctionParameterDefinition;
begin
  lexer.Next;
  result := TCqlFunctionDefinition.create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Context := context;

    result.Name := lexer.readIdentifier('define');
    lexer.token('(');
    first := true;
    while not lexer.hasToken(')') do
    begin
      if first then
        first := false
      else
        lexer.token(',');
      param := TCqlFunctionParameterDefinition.create;
      try
        param.name := lexer.readIdentifier('parameter name');
        param.typeDetails := parseTypeDetails(lexer);
        result.parameters.add(param.link);
      finally
        param.Free;
      end;
    end;
    lexer.token(')');

    lexer.token(':');
    result.body := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.Free;
  end;
end;

function TCqlParser.parseDefine(lexer: TFHIRPathLexer; context: TCqlContextType; access: TCqlAccessLevel): TCqlExpressionDefinition;
begin
  result := TCqlExpressionDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Context := context;

    result.Name := lexer.readIdentifier('define');
    lexer.token(':');
    result.expression := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;


function TCqlParser.parseInclude(lexer: TFHIRPathLexer): TCqlInclude;
begin
  result := TCqlInclude.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    readVersion(lexer, result);
    if lexer.hasToken('called') then
    begin
      lexer.next;
      result.Alias := lexer.take;
    end
    else
      result.Alias := Result.Name;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseLibrary(lexer: TFHIRPathLexer): TCqlLibrary;
var
  token : String;
  access : TCqlAccessLevel;
  context : TCqlContextType;
begin
  context := CqlContextPatient;
  result := TCqlLibrary.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    lexer.token('library');
    result.Name := lexer.take;
    readVersion(lexer, result);
    access := CqlAccessDefault;

    while not lexer.done do
    begin
      token := lexer.take;
      if (token = 'public') then
        access := CqlAccessPublic
      else if (token = 'private') then
        access := CqlAccessPrivate
      else
      begin
        if (token = 'codesystem') then
          result.CodeSystems.Add(parseCodeSystem(lexer, access))
        else if (token = 'valueset') then
          result.ValueSets.Add(parseValueSet(lexer, access))
        else if access <> CqlAccessDefault then
          raise Exception.Create('Unexpected token '+CODES_AccessLevel[access])
        else if (token = 'using') then
          result.Using.Add(parseUsing(lexer))
        else if (token = 'include') then
          result.Includes.Add(parseInclude(lexer))
        else if (token = 'parameter') then
          result.Parameters.Add(parseParameter(lexer))
        else if (token = 'context') then
          context := readContext(lexer)
        else if (token = 'define') then
          if lexer.hasToken('function') then
            result.Functions.add(parseFunction(lexer, context, access))
          else
            result.Definitions.add(parseDefine(lexer, context, access))
        else
          raise lexer.error('not done yet: '+token);
        access := CqlAccessDefault;
      end
    end;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;


function TCqlParser.parseParameter(lexer: TFHIRPathLexer): TCqlParameterDefinition;
begin
  result := TCqlParameterDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    // what comes next may be a type
    if not isReservedWord(lexer.current) then
    begin
      if not lexer.hasToken('default') then
        result.TypeDetails := parseTypeDetails(lexer);
      if lexer.hasToken('default') then
      begin
        lexer.Next;
        result.DefaultValue := parseExpression(lexer, [cpsProximal]);
      end;
    end;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseListType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeList;
    result.Id := 'List';
    lexer.Next;
    result.Parameters.Add(parseTypeDetails(lexer));
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseIntervalType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
var
  param : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeInterval;
    result.Id := 'Interval';
    lexer.Next;
    param := TCqlTypeSpecifier.Create;
    try
      param.Id := lexer.readIdentifier('Interval Type');
      result.Parameters.Add(param.link);
    finally
      param.free;
    end;
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseTupleType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
var
  loc : TSourceLocation;
  s : String;
  td : TCqlTypeSpecifier;
  first : boolean;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeTuple;
    result.Id := 'Tuple';
    lexer.Next;
    first := true;
    while not lexer.hasToken('}') do
    begin
      if not first and lexer.hasToken(',') then
        lexer.Next;
      first := false;
      loc := lexer.CurrentLocation;
      s := lexer.take;
      td := parseTypeDetails(lexer);
      td.StartPosition := loc;
      result.Elements.Add(s, td);
    end;
    lexer.next;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.isReservedWord(word: String): boolean;
begin
  result := StringArrayExistsSensitive(['define', 'function', 'context', 'parameter', 'valueset', 'codesystem', 'concept', 'code', 'public', 'private', 'then', 'else'], word)
    or StringArrayExistsSensitive(CODES_CqlOperationId, word);
end;


procedure TCqlParser.organisePrecedence(lexer: TFHIRPathLexer; var node: TCqlExpressionNode);
begin
  // nothing yet
end;

function TCqlParser.parseChoiceType(lexer : TFHIRPathLexer) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeChoice;
    result.Id := 'Choice';
    lexer.Next;
    result.Parameters.Add(parseTypeDetails(lexer));
    while (lexer.hasToken(',')) do
    begin
      lexer.Next;
      result.Parameters.Add(parseTypeDetails(lexer));
    end;
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseSimpleType(lexer : TFHIRPathLexer; token : string) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeSimple;
    result.Id := token;
    if lexer.hasToken('.') then
    begin
      result.LibraryName := result.Id;
      lexer.take;
      result.Id := lexer.take;
    end;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseTypeDetails(lexer: TFHIRPathLexer): TCqlTypeSpecifier;
var
  token : string;
begin
  token := lexer.readIdentifier('Type Details');
  if (token = 'List') and lexer.hasToken('<') then
    result := parseListType(lexer)
  else if (token = 'Interval') and lexer.hasToken('<') then
    result := parseIntervalType(lexer)
  else if (token = 'Tuple') and lexer.hasToken('{') then
    result := parseTupleType(lexer)
  else if (token = 'Choice') and lexer.hasToken('{') then
    result := parseChoiceType(lexer)
  else
    result := parseSimpleType(lexer, token);
end;

function TCqlParser.parseUsing(lexer: TFHIRPathLexer): TCqlUsing;
begin
  result := TCqlUsing.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    readVersion(lexer, result);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseValueSet(lexer: TFHIRPathLexer; access : TCqlAccessLevel): TCqlValueSetReference;
var
  ref : TCqlScopedIdReference;
begin
  result := TCqlValueSetReference.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Name := lexer.readConstant('valueset name');
    lexer.token(':');
    result.URL := lexer.readConstant('url');
    readVersion(lexer, result);
    if lexer.hasToken('codesystems') then
    begin
      lexer.take;
      lexer.token('{');
      while not lexer.hasToken('}') do
      begin
        ref := TCqlScopedIdReference.Create;
        try
          ref.StartPosition := lexer.CurrentLocation;
          ref.Id := lexer.take;
          if lexer.hasToken('.') then
          begin
            ref.LibraryName := ref.Id;
            lexer.take;
            ref.Id := lexer.take;
          end;
          result.CodeSystems.Add(ref);
          ref.EndPosition := lexer.CurrentLocation;
        finally
          ref.Free;
        end;
      end;
    end;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.readContext(lexer: TFHIRPathLexer): TCqlContextType;
var
  token : String;
begin
  result := CqlContextPatient;
  token := lexer.take;
  if token = 'Patient' then
    result := CqlContextPatient
  else if token = 'Population' then
    result := CqlContextPopulation
  else
    raise lexer.error('Unknown value for context: "'+token+'"');
end;

procedure TCqlParser.readVersion(lexer: TFHIRPathLexer; item: TCqlVersioned);
begin
  if lexer.hasToken('version') then
  begin
    lexer.next;
    item.Version := lexer.readConstant('version');
  end;
end;

constructor TCqlParser.Create;
begin
  inherited;
  FRegistry := TCqlFunctionRegistry.create;

end;

destructor TCqlParser.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

{ TCqlLexer }

procedure TCqlLexer.next;
begin
  inherited next;
  while hasComment do
    inherited next;
end;

{ TCqlFunction }

constructor TCqlFunction.Create(name, typeName: String; minParamCount, maxParamCount: Integer);
begin
  inherited Create;
  FName := name;
  FTypeName := typeName;
  FMinParamCount := minParamCount;
  FMaxParamCount := maxParamCount;
end;

function TCqlFunction.Link: TCqlFunction;
begin
  result := TCqlFunction(inherited Link);
end;

{ TCqlFunctionRegistry }

constructor TCqlFunctionRegistry.Create;
begin
  inherited;
  FMap := TAdvMap<TCqlFunction>.create;
  FList := TAdvList<TCqlFunction>.create;
  Populate;
end;

destructor TCqlFunctionRegistry.Destroy;
begin
  FMap.Free;
  FList.Free;
  inherited;
end;

procedure TCqlFunctionRegistry.populate;
begin
  add(TCqlFunction.Create('retrieve', 'List', 3, 3));
  add(TCqlFunction.Create('query', 'List', 6, 6));
  add(TCqlFunction.Create('Code', 'Concept', 2, 3));
  add(TCqlFunction.Create('queryRef', 'List', 3, 4));
  add(TCqlFunction.Create('AgeInYears', 'integer', 0, 0));
  add(TCqlFunction.Create('AgeInYearsAt', 'integer', 1, 1));
  add(TCqlFunction.Create('Today', 'dateTime', 0, 0));
  add(TCqlFunction.Create('DateTime', 'dateTime', 3, 7));
  add(TCqlFunction.Create('Last', 'object', 1, 1));
  add(TCqlFunction.Create('Count', 'integer', 1, 1));
end;

procedure TCqlFunctionRegistry.add(func: TCqlFunction);
begin
  func.index := FList.Add(func)+1; // note one offset
  FMap.Add(func.name, func.Link);
end;

function TCqlFunctionRegistry.GetDefinition(i: integer): TCqlFunction;
begin
  result := FList[i-1];
end;

function TCqlFunctionRegistry.IndexOf(name: String): integer;
var
  func : TCqlFunction;
begin
  if FMap.TryGetValue(name, func) then
    result := func.index
  else
    result := 0;
end;

procedure TCqlParser.readConstant(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
begin
  if lexer.current.startsWith('''') then
    lexer.processConstant(lexer.current);
  expression.Constant := lexer.take;
  expression.kind := enkConstant;
  expression.SourceLocationEnd := lexer.CurrentLocation;
  if lexer.isUnit or lexer.isConstant then
    expression.units := lexer.take;
end;

procedure TCqlParser.readGroup(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
begin
  lexer.next;
  expression.kind := enkGroup;
  expression.group := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  if lexer.current <> ')' then
    raise lexer.error('Found '+lexer.current+' expecting a ")"');
  expression.SourceLocationEnd := lexer.CurrentLocation;
  lexer.next;
end;

procedure TCqlParser.readRetrieve(lexer: TFHIRPathLexer; expression : TCqlExpressionNode; states : TCqlParserStateSet);
var
  s : String;
begin
  expression.name := 'Retrieve';
  expression.kind := enkStructure;
  expression.StructureType := cstRetrieve;
  lexer.token('[');
  s := lexer.readIdentifier('Retrieve source');
  if (lexer.hasToken('.')) then
  begin
    expression.LibraryName := s;
    lexer.Next;
    expression.Name := lexer.readIdentifier('scoped id');
  end
  else
    expression.Name := s;

  if not lexer.takeToken(']') then
  begin
    lexer.token(':');
    s := lexer.readIdentifier('retrieve rule');
    if (lexer.hasToken('in')) then
    begin
      expression.codePath := s;
      lexer.Next;
      s := lexer.readIdentifier('retrieve rule');
    end;
    if lexer.hasToken('.') then
    begin
      lexer.next;
      expression.Terminology := s+'.'+lexer.readIdentifier('terminology id');
    end
    else
      expression.Terminology := s;
    lexer.token(']');
  end;
  if (cpsCheckAlias in states) then
    checkForAlias(lexer, expression);
end;

procedure TCqlParser.checkForAlias(lexer: TFHIRPathLexer; expression : TCqlExpressionNode);
begin
  if not lexer.done and not isReservedWord(lexer.current) and not lexer.endingToken(lexer.current) and
      not StringArrayExistsSensitive(CODES_TFHIRPathOperation, lexer.current) and
      not StringArrayExistsSensitive(CODES_CqlOperationId, lexer.current) then
  begin
    if (expression.kind = enkName) then
    begin
      expression.kind := enkStructure;
      expression.StructureType := cstRetrieve;
      if (expression.Inner <> nil) then
      begin
        expression.LibraryName := expression.name;
        expression.name := expression.Inner.name;
        expression.Inner := nil;
      end;
    end;
    expression.alias := lexer.readIdentifier('alias');
    while (lexer.hasToken('with')) do
    begin
      lexer.token('with');
      expression.withStmt := parseExpression(lexer, [cpsProximal]);
      expression.withStmt.alias := lexer.readIdentifier('With Alias');
      lexer.token('such');
      lexer.token('that');
      expression.suchThat := parseExpression(lexer, [cpsProximal]);
    end;
    if (lexer.hasToken('where')) then
    begin
      lexer.token('where');
      expression.where := parseExpression(lexer, [cpsProximal]);
    end;
    if (lexer.hasToken('return')) then
    begin
      lexer.token('return');
      if (lexer.takeToken('all')) then
        expression.returnType := cqlReturnAll
      else if (lexer.takeToken('distinct')) then
        expression.returnType := cqlReturnDistinct
      else
        expression.returnType := cqlReturnUnspecified;
      expression.return := parseExpression(lexer, [cpsProximal]);
    end;
  end;
end;

procedure TCqlParser.readTuple(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
var
  names, first : boolean;
  s : String;
begin
  expression.name := 'Tuple';
  expression.kind := enkStructure;
  expression.StructureType := cstTuple;

  if lexer.current = 'Tuple' then
  begin
    names := true;
    lexer.token('Tuple');
  end
  else
    names := false; // check this with Bryn
  lexer.token('{');
  first := true;
  while not lexer.hasToken('}') do
  begin
    if (first) then
      first := false
    else
      lexer.token(',');
    if names then
    begin
      s := lexer.readIdentifier('Tuple Field Name');
      lexer.token(':');
    end
    else
      s := 'param'+inttostr(expression.elements.count+1);
    expression.elements.Add(s, parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
  end;
  lexer.token('}');
end;

procedure TCqlParser.readFunction(lexer : TFHIRPathLexer; expression : TCqlExpressionNode; name : String);
var
  first : boolean;
begin
  expression.Name := name;
  if StringArrayExistsSensitive(CODES_TFHIRPathFunctions, expression.Name) then
    expression.FunctionId := TFHIRPathFunction(StringArrayIndexOfSensitive(CODES_TFHIRPathFunctions, expression.Name))
  else
  begin
    expression.FunctionId := pfCustom;
    if StringArrayExistsSensitive(NAMES_CqlFunctions, expression.Name) then
      expression.CqlFunctionId := TCqlFunctionDefinitionId(StringArrayIndexOfSensitive(NAMES_CqlFunctions, expression.Name))
    else
      expression.CqlFunctionId := cfNull; // try to resolve later after parsing completes
  end;
  expression.kind := enkFunction;
  lexer.next;
  first := true;
  while lexer.current <> ')' do
  begin
    if first then
      first := false
    else
      lexer.token(',');
    expression.Parameters.add(parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
  end;
  lexer.next;
  checkParameters(lexer, expression.SourceLocationStart, expression);
end;

procedure TCqlParser.readCode(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
begin
  expression.kind := enkFunction;
  expression.FunctionId := pfCustom;
  expression.CqlFunctionId := cfCode;
  expression.Parameters.Add(parse(lexer));
  lexer.token('from');
  expression.Parameters.Add(parse(lexer));
  if (lexer.hasToken('display')) then
  begin
    lexer.token('display');
    expression.Parameters.Add(parse(lexer));
  end;
end;

procedure TCqlParser.readIf(lexer: TFHIRPathLexer; expression: TCqlExpressionNode);
begin
  lexer.token('if');
  expression.name := 'if';
  expression.kind := enkStructure;
  expression.StructureType := cstIf;

  expression.ifTest := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  lexer.token('then');
  expression.thenStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  if lexer.takeToken('else') then
    expression.elseStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
end;

procedure TCqlParser.readInterval(lexer : TFHIRPathLexer; expression : TCqlExpressionNode);
begin
  expression.kind := enkFunction;
  expression.FunctionId := pfCustom;
  expression.CqlFunctionId := cfInterval;
  lexer.token('[');
  expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
  lexer.token(',');
  expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
  lexer.token(']');
end;

function TCqlParser.parseExpression(lexer: TFHIRPathLexer; states : TCqlParserStateSet): TCQLExpressionNode;
var
  c : Integer;
  s : String;
  focus : TCQLExpressionNode;
  localStates : TCqlParserStateSet;
begin
  localStates := states;
  result := TCQLExpressionNode.Create(lexer.nextId);
  try
    result.SourceLocationStart := lexer.CurrentLocation;
    c := lexer.CurrentStart;
    lexer.checkArithmeticPrefixes;

    if lexer.current = '(' then
    begin
      readGroup(lexer, result);
      localStates := localStates + [cpsAllowInner];
    end
    else if (lexer.current = '[') then
    begin
      readRetrieve(lexer, result, localStates);
      localStates := localStates - [cpsCheckAlias];
    end
    else if (lexer.current = 'Tuple') or (lexer.current = '{')  then
      readTuple(lexer, result)
    else if (lexer.current = 'if') then
      readIf(lexer, result)
    else if (cpsProximal in localStates) and (StringArrayExistsSensitive(NAMES_UNPREFIXED_OPERATORS, lexer.current)) then
    begin
      result.kind := enkStructure;
      result.StructureType := cstPlaceHolder;
      localStates := localStates - [cpsCheckAlias];
    end
    else
    begin
      if not lexer.isConstant and not lexer.isToken and not lexer.current.startsWith('"') then
        raise lexer.error('Found '+lexer.current+' expecting a constant or a token name');
      if not lexer.isConstant and StringArrayExistsSensitive(CODES_CqlModifier, lexer.Current) then
      begin
        result.Modifier := TCqlModifier(StringArrayIndexOfSensitive(CODES_CqlModifier, result.Name));
        lexer.Next;
      end;
      if lexer.isConstant then
        readConstant(lexer, result)
      else
      begin
        s := lexer.readIdentifier('Expression Token');
        if (lexer.current = '(') then
        begin
          readFunction(lexer, result, s);
          localStates := localStates + [cpsAllowInner];
        end
        else if s = 'Code' then
          readCode(lexer, result)
        else if s = 'Interval' then
          readInterval(lexer, result)
        else
        begin
          result.Name := s;
          localStates := localStates + [cpsAllowInner];
        end;
      end;
    end;
    result.SourceLocationEnd := lexer.CurrentLocation;

    {
    if (lexer.current = '[') then
    begin
      lexer.next();
      item := TFHIRExpressionNode.Create(lexer.nextId);
      item.Kind := enkFunction;
      item.Functionid := pfItem;
      item.Parameters.add(parseExpression(lexer, true, extensions));
      if (lexer.current <> ']') then
        raise lexer.error('The token '+lexer.Current+' is not expected here - a "]" expected');
      lexer.next;
      result.inner := item;
      focus := item;
    end;
    }
    if (cpsAllowInner in localStates) and (lexer.current = '.') then
    begin
      lexer.next;
      result.Inner := parseExpression(lexer, [cpsAllowInner]);
    end;

    if cpsCheckAlias in localStates then
      checkForAlias(lexer, result);

    result.Proximal := cpsProximal in states;
    if (result.proximal) then
    begin
      focus := result;
      while not lexer.done and (lexer.isOp or StringArrayExistsSensitive(CODES_CqlOperationId, lexer.current) or lexer.isNumericalConstant) do
      begin
        if lexer.isOp then
          focus.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current))
        else if lexer.isNumericalConstant then
        begin
          focus.Operation := popCustom;
          focus.CqlOperation := copUnnamedWhen;
        end
        else
        begin
          focus.Operation := popCustom;
          focus.CqlOperation := TCqlOperationId(StringArrayIndexOfSensitive(CODES_CqlOperationId, lexer.current))
        end;
        focus.OpSourceLocationStart := lexer.CurrentStartLocation;
        focus.OpSourceLocationEnd := lexer.CurrentLocation;
        if not lexer.isNumericalConstant then
          lexer.next;
        if (focus.CqlOperation <> copNull) and (lexer.current = FOLLOWING_WORDS_CqlOperationId[focus.CqlOperation]) then
          lexer.Next;
        focus.opNext := parseExpression(lexer, states - [cpsProximal]);
        focus := focus.OpNext as TCQLExpressionNode;
      end;
      organisePrecedence(lexer, result);
    end;
    result.link;
  finally
    result.Free;
  end;
end;

end.


