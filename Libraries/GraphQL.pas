unit GraphQL;

interface

uses
  SysUtils, Classes, DUnitX.TestFramework,
  StringSupport,
  AdvObjects, AdvGenerics, AdvTextExtractors, AdvStringStreams, XmlBuilder;

Type
  EGraphQLException = class (Exception);

  TGraphQLSelection = class;
  TGraphQLFragment = class;
  TGraphQLArgument = class;

  TGraphQLValue = class (TAdvObject)
  public
    Function Link : TGraphQLValue; overload;
  end;

  TGraphQLVariableValue = class (TGraphQLValue)
  private
    FValue : String;
  public
    Constructor Create(value : String);
    Function Link : TGraphQLVariableValue; overload;
    property Value : String read FValue write FValue;
  end;

  TGraphQLNumberValue = class (TGraphQLValue)
  private
    FValue : String;
  public
    Constructor Create(value : String);
    Function Link : TGraphQLNumberValue; overload;
    property Value : String read FValue write FValue;
  end;

  TGraphQLNameValue = class (TGraphQLValue)
  private
    FValue : String;
  public
    Constructor Create(value : String);
    Function Link : TGraphQLValue; overload;
    property Value : String read FValue write FValue;
  end;

  TGraphQLStringValue = class (TGraphQLValue)
  private
    FValue : String;
  public
    Constructor Create(value : String);
    Function Link : TGraphQLStringValue; overload;
    property Value : String read FValue write FValue;
  end;

  TGraphQLObjectValue = class (TGraphQLValue)
  private
    FFields : TAdvList<TGraphQLArgument>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLObjectValue; overload;
    property Fields : TAdvList<TGraphQLArgument> read FFields;
  end;

  TGraphQLArgument = class (TAdvObject)
  private
    FName: String;
    FValues: TAdvList<TGraphQLValue>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLArgument; overload;
    property Name : String read FName write FName;
    property Values : TAdvList<TGraphQLValue> read FValues;
  end;

  TGraphQLDirective = class (TAdvObject)
  private
    FName: String;
    FArguments: TAdvList<TGraphQLArgument>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLDirective; overload;
    property Name : String read FName write FName;
    Property Arguments : TAdvList<TGraphQLArgument> read FArguments;
  end;

  TGraphQLField = class (TAdvObject)
  private
    FName: String;
    FSelectionSet: TAdvList<TGraphQLSelection>;
    FAlias: String;
    FArguments: TAdvList<TGraphQLArgument>;
    FDirectives: TAdvList<TGraphQLDirective>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLField; overload;
    property Alias : String read FAlias write FAlias;
    Property Name : String read FName write FName;
    Property Arguments : TAdvList<TGraphQLArgument> read FArguments;
    property Directives : TAdvList<TGraphQLDirective> read FDirectives;
    property SelectionSet : TAdvList<TGraphQLSelection> read FSelectionSet;
  end;

  TGraphQLFragmentSpread = class (TAdvObject)
  private
    FName: String;
    FDirectives: TAdvList<TGraphQLDirective>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLFragmentSpread; overload;
    property Name : String read FName write FName;
    property Directives : TAdvList<TGraphQLDirective> read FDirectives;
  end;

  TGraphQLSelection = class (TAdvObject)
  private
    FField : TGraphQLField;
    FInlineFragment: TGraphQLFragment;
    FFragmentSpread: TGraphQLFragmentSpread;
    procedure SetField(const Value: TGraphQLField);
    procedure SetFragmentSpread(const Value: TGraphQLFragmentSpread);
    procedure SetInlineFragment(const Value: TGraphQLFragment);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLSelection; overload;
    property field : TGraphQLField read FField write SetField;
    property FragmentSpread : TGraphQLFragmentSpread read FFragmentSpread write SetFragmentSpread;
    property InlineFragment : TGraphQLFragment read FInlineFragment write SetInlineFragment;
  end;

  TGraphQLVariable = class (TAdvObject)
  private
    FName: String;
    FDefaultValue: TGraphQLValue;
    FTypeName: String;
    procedure SetDefaultValue(const Value: TGraphQLValue);
  public
    Destructor Destroy; override;
    Function Link : TGraphQLVariable; overload;
    property Name : String read FName write FName;
    property TypeName : String read FTypeName write FTypeName;
    property DefaultValue : TGraphQLValue read FDefaultValue write SetDefaultValue;
  end;

  TGraphQLOperationType = (qglotQuery, qglotMutation);
  TGraphQLOperation = class (TAdvObject)
  private
    FName: String;
    FoperationType: TGraphQLOperationType;
    FSelectionSet: TAdvList<TGraphQLSelection>;
    FVariables: TAdvList<TGraphQLVariable>;
    FDirectives: TAdvList<TGraphQLDirective>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLOperation; overload;
    property operationType : TGraphQLOperationType read FoperationType write FoperationType;
    property Name : String read FName write FName;
    property Variables : TAdvList<TGraphQLVariable> read FVariables;
    property Directives : TAdvList<TGraphQLDirective> read FDirectives;
    property SelectionSet : TAdvList<TGraphQLSelection> read FSelectionSet;
  end;

  TGraphQLFragment = class (TAdvObject)
  private
    FName: String;
    FTypeCondition: String;
    FSelectionSet: TAdvList<TGraphQLSelection>;
    FDirectives: TAdvList<TGraphQLDirective>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLFragment; overload;
    property Name : String read FName write FName;
    property TypeCondition : String read FTypeCondition write FTypeCondition;
    property Directives : TAdvList<TGraphQLDirective> read FDirectives;
    property SelectionSet : TAdvList<TGraphQLSelection> read FSelectionSet;

  end;

  TGraphQLDocument = class (TAdvObject)
  private
    FFragments: TAdvList<TGraphQLFragment>;
    FOperations: TAdvList<TGraphQLOperation>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TGraphQLDocument; overload;
    property Operations : TAdvList<TGraphQLOperation> read FOperations;
    property Fragments : TAdvList<TGraphQLFragment> read FFragments;
  end;

  TGraphQLPunctuator = (gqlpBang, gqlpDollar, gqlpOpenBrace, gqlpCloseBrace, gqlpEllipse, gqlpColon, gqlpEquals, gqlpAt, gqlpOpenSquare, gqlpCloseSquare, gqlpOpenCurly, gqlpVertical, gqlpCloseCurly);

const
  LITERALS_TGraphQLPunctuator : array [TGraphQLPunctuator] of String = ('!', '$', '(', ')', '...', ':', '=', '@', '[', ']', '{', '|', '}');

type
  TGraphQLLexType = (gqlltNull, gqlltName, gqlltPunctuation, gqlltString, gqlltNumber);

// graphql documents are in unicode
// ignore: BOM, tab, space, #10,#13, comma, comment
  TGraphQLParser = class (TAdvTextExtractor)
  private
    // lexer
    FToken : TStringBuilder;
    FPeek : String;
    FValue: String;
    FLexType: TGraphQLLexType;
    FLocation : TSourceLocation;
    Function getNextChar : Char;
    Procedure PushChar(ch : Char);
    procedure skipIgnore;
    procedure Next;

    function hasPunctuation(punc : String) : boolean;
    procedure consumePunctuation(punc : String);
    function hasName : boolean; overload;
    function hasName(name : String) : boolean; overload;
    function consumeName : String; overload;
    procedure consumeName(name : String); overload;

    Function Link : TGraphQLParser; overload;
    function parseValue : TGraphQLValue;
    procedure parseFragmentInner(fragment: TGraphQLFragment);
    function parseFragmentSpread : TGraphQLFragmentSpread;
    function parseInlineFragment : TGraphQLFragment;
    function parseArgument : TGraphQLArgument;
    function parseVariable : TGraphQLVariable;
    function parseDirective : TGraphQLDirective;
    function parseField : TGraphQLField;
    function parseSelection : TGraphQLSelection;
    procedure parseOperationInner(op : TGraphQLOperation);
    function parseOperation(name : String) : TGraphQLOperation;
    function parseFragment: TGraphQLFragment;
    procedure parseDocument(doc : TGraphQLDocument);
  public
    Constructor Create; overload; override;
    Destructor Destroy; override;
    class function parse(source : String) : TGraphQLDocument;
  end;

  [TextFixture]
  TGraphQLTests = Class (TObject)
  private
  public
    [TestCase] Procedure TestSimple;
    [TestCase] Procedure TestComment;
    [TestCase] Procedure TestMutation;
    [TestCase] Procedure TestShort;
    [TestCase] Procedure TestSelectionSet;
    [TestCase] Procedure TestNested;
    [TestCase] Procedure TestMe;
    [TestCase] Procedure TestUser;
    [TestCase] Procedure TestArguments1;
    [TestCase] Procedure TestArguments2;
    [TestCase] Procedure TestArguments3;
    [TestCase] Procedure TestArguments4;
    [TestCase] Procedure TestAlias1;
    [TestCase] Procedure TestAlias2;
    [TestCase] Procedure TestFragments0;
    [TestCase] Procedure TestFragments1;
    [TestCase] Procedure TestFragments2;
    [TestCase] Procedure TestFragments3;
    [TestCase] Procedure TestFragments4;
    [TestCase] Procedure TestFragments5;
    [TestCase] Procedure TestObject1;
    [TestCase] Procedure TestObject2;
    [TestCase] Procedure TestVariable;
  end;

implementation

{ TGraphQLArgument }

constructor TGraphQLArgument.Create;
begin
  inherited;
  FValues := TAdvList<TGraphQLValue>.create;
end;

destructor TGraphQLArgument.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TGraphQLArgument.Link: TGraphQLArgument;
begin
 result := TGraphQLArgument(inherited Link);
end;

{ TGraphQLDirective }

constructor TGraphQLDirective.Create;
begin
  inherited;
  FArguments := TAdvList<TGraphQLArgument>.create;
end;

destructor TGraphQLDirective.Destroy;
begin
  FArguments.Free;
  inherited;
end;

function TGraphQLDirective.Link: TGraphQLDirective;
begin
  result := TGraphQLDirective(inherited Link);
end;

{ TGraphQLField }

constructor TGraphQLField.Create;
begin
  inherited;
  FSelectionSet := TAdvList<TGraphQLSelection>.create;
  FArguments := TAdvList<TGraphQLArgument>.create;
  FDirectives := TAdvList<TGraphQLDirective>.create;
end;

destructor TGraphQLField.Destroy;
begin
  FSelectionSet.Free;
  FArguments.Free;
  FDirectives.Free;
  inherited;
end;

function TGraphQLField.Link: TGraphQLField;
begin
  result := TGraphQLField(inherited Link);
end;

{ TGraphQLFragmentSpread }

constructor TGraphQLFragmentSpread.Create;
begin
  inherited;
  FDirectives := TAdvList<TGraphQLDirective>.create;
end;

destructor TGraphQLFragmentSpread.Destroy;
begin
  FDirectives.Free;
  inherited;
end;

function TGraphQLFragmentSpread.Link: TGraphQLFragmentSpread;
begin
  result := TGraphQLFragmentSpread(inherited Link);
end;

{ TGraphQLSelection }

constructor TGraphQLSelection.Create;
begin
  inherited;
end;

destructor TGraphQLSelection.Destroy;
begin
  FField.Free;
  FInlineFragment.Free;
  FFragmentSpread.Free;
  inherited;
end;

function TGraphQLSelection.Link: TGraphQLSelection;
begin
  result := TGraphQLSelection(inherited Link);
end;

procedure TGraphQLSelection.SetField(const Value: TGraphQLField);
begin
  FField.Free;
  FField := Value;
end;

procedure TGraphQLSelection.SetFragmentSpread(const Value: TGraphQLFragmentSpread);
begin
  FFragmentSpread.Free;
  FFragmentSpread := Value;
end;

procedure TGraphQLSelection.SetInlineFragment(const Value: TGraphQLFragment);
begin
  FInlineFragment.Free;
  FInlineFragment := Value;
end;

{ TGraphQLVariable }

destructor TGraphQLVariable.Destroy;
begin
  FDefaultValue.Free;
  inherited;
end;

function TGraphQLVariable.Link: TGraphQLVariable;
begin
  result := TGraphQLVariable(inherited Link);
end;

procedure TGraphQLVariable.SetDefaultValue(const Value: TGraphQLValue);
begin
  FDefaultValue.Free;
  FDefaultValue := Value;
end;

{ TGraphQLOperation }

constructor TGraphQLOperation.Create;
begin
  inherited;
  FSelectionSet := TAdvList<TGraphQLSelection>.create;
  FVariables := TAdvList<TGraphQLVariable>.create;
  FDirectives := TAdvList<TGraphQLDirective>.create;
end;

destructor TGraphQLOperation.Destroy;
begin
  FSelectionSet.Free;
  FVariables.Free;
  FDirectives.Free;
  inherited;
end;

function TGraphQLOperation.Link: TGraphQLOperation;
begin
  result := TGraphQLOperation(inherited Link);
end;


{ TGraphQLFragment }

constructor TGraphQLFragment.Create;
begin
  inherited;
  FSelectionSet := TAdvList<TGraphQLSelection>.create;
  FDirectives := TAdvList<TGraphQLDirective>.create;
end;

destructor TGraphQLFragment.Destroy;
begin
  FSelectionSet.Free;
  FDirectives.free;
  inherited;
end;

function TGraphQLFragment.Link: TGraphQLFragment;
begin
  result := TGraphQLFragment(inherited Link);
end;

{ TGraphQLDocument }

constructor TGraphQLDocument.Create;
begin
  inherited;
  FFragments := TAdvList<TGraphQLFragment>.create;
  FOperations := TAdvList<TGraphQLOperation>.create;
end;

destructor TGraphQLDocument.Destroy;
begin
  FFragments.Free;
  FOperations.Free;
  inherited;
end;

function TGraphQLDocument.Link: TGraphQLDocument;
begin
  result := TGraphQLDocument(inherited Link);
end;

{ TGraphQLParser }

function TGraphQLParser.getNextChar: Char;
begin
  if FPeek <> '' Then
  Begin
    result := FPeek[1];
    Delete(FPeek, 1, 1);
  End
  Else
  begin
    result := ConsumeCharacter;
    if result = #10 then
    begin
      inc(FLocation.line);
      FLocation.col := 1;
    end
    else
      inc(FLocation.col);
  end;
end;

function TGraphQLParser.hasName: boolean;
begin
  result := (FLexType = gqlltName) and (FToken.ToString <> '');
end;

function TGraphQLParser.hasName(name: String): boolean;
begin
  result := (FLexType = gqlltName) and (FToken.ToString = name);
end;

function TGraphQLParser.hasPunctuation(punc: String): boolean;
begin
  result := (FLexType = gqlltPunctuation) and (FToken.ToString = punc);
end;

function TGraphQLParser.Link: TGraphQLParser;
begin
  result := TGraphQLParser(inherited Link);
end;

function TGraphQLParser.consumeName: String;
begin
  if FLexType <> gqlltName then
    raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting a name');
  result := FToken.ToString;
  next;
end;

procedure TGraphQLParser.Next;
var
  ch : Char;
  hex : String;
begin
  skipIgnore;
  FToken.Clear;
  if (not more and (FPeek = '')) then
    FLexType := gqlltNull
  else
  begin
    ch := getNextChar();
    if StringArrayExistsSensitive(LITERALS_TGraphQLPunctuator, ch) then
    begin
      FLexType := gqlltPunctuation;
      FToken.Append(ch);
    end
    else if ch = '.' then
    begin
      repeat
        FToken.Append(ch);
        ch := getNextChar;
      until ch <> '.';
      PushChar(ch);
      if (FToken.Length <> 3) then
        raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting "..."');
    end
    else if charInSet(ch, ['A'..'Z', 'a'..'z', '_']) then
    begin
      FLexType := gqlltName;
      repeat
        FToken.Append(ch);
        ch := getNextChar;
      until not charInSet(ch, ['A'..'Z', 'a'..'z', '_', '0'..'9']);
      pushChar(ch);
    end
    else if charInSet(ch, ['0'..'9', '-']) then
    begin
      FLexType := gqlltNumber;
      repeat
        FToken.Append(ch);
        ch := getNextChar;
      until not (charInSet(ch, ['0'..'9']) or ((ch = '.') and not FToken.ToString.Contains('.'))or ((ch = 'e') and not FToken.ToString.Contains('e')));
      pushChar(ch);
    end
    else if (ch = '"') then
    begin
      FLexType := gqlltString;
      repeat
        ch := getNextChar;
        if (ch = '\') Then
        Begin
          if not More then
            EGraphQLException.Create('premature termination of GraphQL during a string constant');
          ch := getNextChar;
          case ch of
            '"':FToken.Append('"');
            '\':FToken.Append('\');
            '/':FToken.Append('/');
            'n':FToken.Append(#10);
            'r':FToken.Append(#13);
            't':FToken.Append(#09);
            'u':
              begin
              setLength(hex, 4);
              hex[1] := getNextChar;
              hex[2] := getNextChar;
              hex[3] := getNextChar;
              hex[4] := getNextChar;
              FToken.Append(chr(StrToInt('$'+hex)));
              end
          Else
            EGraphQLException.Create('not supported in GraphQL: \'+ch);
          End;
          ch := #0;
        End
        Else if (ch <> '"') then
          FToken.Append(ch);
      until not More or (ch = '"');
      if ch <> '"' Then
        EGraphQLException.Create('premature termination of GraphQL during a string constant');
    end
    else
      raise EGraphQLException.Create('Not done yet: '+ch); // syntax error?
  end;
end;

class function TGraphQLParser.parse(source: String): TGraphQLDocument;
var
  this : TGraphQLParser;
  stream : TAdvStringStream;
begin
  stream := TAdvStringStream.Create;
  try
    stream.Bytes := TENcoding.UTF8.GetBytes(source);
    this := TGraphQLParser.Create(stream.link);
    try
      this.next;
      result := TGraphQLDocument.Create;
      try
        this.parseDocument(result);
        result.Link;
      finally
        result.free;
      end;
    finally
      this.free;
    end;
  finally
    stream.Free;
  end;
end;

function TGraphQLParser.parseArgument: TGraphQLArgument;
begin
  result := TGraphQLArgument.Create;
  try
    result.Name := consumeName;
    consumePunctuation(':');
    if hasPunctuation('[') then
    begin
      consumePunctuation('[');
      while not hasPunctuation(']') do
        result.Values.Add(parseValue);
      consumePunctuation(']');
    end
    else
      result.Values.Add(parseValue);
    result.Link;
  finally
    result.Free;
  end;
end;

function TGraphQLParser.parseDirective: TGraphQLDirective;
begin
  result := TGraphQLDirective.Create;
  try
    consumePunctuation('@');
    result.Name := consumeName;
    if hasPunctuation('(') then
    begin
      consumePunctuation('(');
      repeat
        result.Arguments.Add(parseArgument);
      until hasPunctuation(')');
      consumePunctuation(')');
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

procedure TGraphQLParser.parseDocument(doc: TGraphQLDocument);
var
  s : String;
  op : TGraphQLOperation;
begin
  if not hasName then
  begin
    op := TGraphQLOperation.Create;
    try
      parseOperationInner(op);
      doc.Operations.Add(op.Link);
    finally
      op.Free;
    end;

//    consumePunctuation('{');
//    if FLexType <> gqlltName then
//      raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting a name"');
//    s := consumeName;
//    if (s = 'fragment') then
//       raise Exception.Create('Not done yet')
//    else
//      doc.Operations.Add(parseOperation(s));
  end
  else
  begin
    while more or (FPeek <> '') do
    begin
      s := consumeName;
      if (s = 'mutation') or (s = 'query') then
        doc.Operations.Add(parseOperation(s))
      else if (s = 'fragment') then
        doc.Fragments.Add(parseFragment)
      else
        raise Exception.Create('Not done yet'); // doc.Operations.Add(parseOperation(s))?
    end;
  end;
end;

function TGraphQLParser.parseField: TGraphQLField;
begin
  result := TGraphQLField.Create;
  try
    result.Name := consumeName;
    if hasPunctuation(':') then
    begin
      result.Alias := result.Name;
      consumePunctuation(':');
      result.Name := consumeName;
    end;
    if hasPunctuation('(') then
    begin
      consumePunctuation('(');
      repeat
        result.Arguments.Add(parseArgument);
      until hasPunctuation(')');
      consumePunctuation(')');
    end;
    while hasPunctuation('@') do
      result.Directives.Add(parseDirective);
    if hasPunctuation('{') then
    begin
      consumePunctuation('{');
      repeat
        result.SelectionSet.Add(parseSelection);
      until hasPunctuation('}');
      consumePunctuation('}');
    end;
    result.link;
  finally
    result.Free;
  end;
end;

procedure TGraphQLParser.parseFragmentInner(fragment: TGraphQLFragment);
begin
  while hasPunctuation('@') do
    fragment.Directives.Add(parseDirective);
  consumePunctuation('{');
  repeat
    fragment.SelectionSet.Add(parseSelection);
  until hasPunctuation('}');
  consumePunctuation('}');
end;

function TGraphQLParser.parseFragment: TGraphQLFragment;
begin
  result := TGraphQLFragment.Create;
  try
    result.Name := consumeName;
    consumeName('on');
    result.TypeCondition := consumeName;
    parseFragmentInner(result);
    result.Link;
  finally
    result.free;
  end;
end;

function TGraphQLParser.parseFragmentSpread: TGraphQLFragmentSpread;
begin
  result := TGraphQLFragmentSpread.Create;
  try
    result.Name := consumeName;
    while hasPunctuation('@') do
      result.Directives.Add(parseDirective);
    result.Link;
  finally
    result.Free;
  end;
end;

function TGraphQLParser.parseInlineFragment: TGraphQLFragment;
begin
  result := TGraphQLFragment.Create;
  try
    if hasName('on') then
    begin
      consumeName('on');
      result.FTypeCondition := consumeName;
    end;
    parseFragmentInner(result);
    result.Link;
  finally
    result.Free;
  end;
end;

function TGraphQLParser.parseOperation(name : String) : TGraphQLOperation;
begin
  result := TGraphQLOperation.Create;
  try
    if name = 'mutation' then
    begin
      result.operationType := qglotMutation;
      if hasName then
        result.Name := consumeName;
    end
    else if name = 'query' then
    begin
      result.operationType := qglotQuery;
      if hasName then
        result.Name := consumeName;
    end
    else
      result.Name := name;
    parseOperationInner(result);
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TGraphQLParser.parseOperationInner(op: TGraphQLOperation);
begin
  if hasPunctuation('(') then
  begin
    consumePunctuation('(');
    repeat
      op.Variables.Add(parseVariable);
    until hasPunctuation(')');
    consumePunctuation(')');
  end;
  while hasPunctuation('@') do
    op.Directives.Add(parseDirective);
  if hasPunctuation('{') then
  begin
    consumePunctuation('{');
    repeat
      op.SelectionSet.Add(parseSelection);
    until hasPunctuation('}');
    consumePunctuation('}');
  end;
end;

function TGraphQLParser.parseSelection: TGraphQLSelection;
var
  name : String;
begin
  result := TGraphQLSelection.Create;
  try
    if hasPunctuation('...') then
    begin
      consumePunctuation('...');
      if hasName and (FToken.ToString <> 'on') then
        result.FragmentSpread := parseFragmentSpread
      else
        result.InlineFragment := parseInlineFragment;
    end
    else
      result.field := parseField;
    result.Link;
  finally
    result.Free;
  end;
end;

function TGraphQLParser.parseValue: TGraphQLValue;
begin
  result := nil;
  try
    case FLexType of
      gqlltNull: raise EGraphQLException.Create('Attempt to read a value after reading off the end of the GraphQL statement');
      gqlltName: result := TGraphQLNameValue.Create(FToken.ToString);
      gqlltPunctuation:
        if hasPunctuation('$') then
        begin
          consumePunctuation('$');
          result := TGraphQLVariableValue.Create(FToken.ToString);
        end else if hasPunctuation('{') then
        begin
          consumePunctuation('{');
          result := TGraphQLObjectValue.Create;
          while not hasPunctuation('}') do
            TGraphQLObjectValue(result).Fields.Add(parseArgument);
        end
        else
          raise EGraphQLException.Create('Attempt to read a value at "'+FToken.ToString+'"');
      gqlltString: result := TGraphQLStringValue.Create(FToken.ToString);
      gqlltNumber: result := TGraphQLNumberValue.Create(FToken.ToString);
    end;
    next;
    result.Link;
  finally
    result.Free;
  end;
end;

function TGraphQLParser.parseVariable: TGraphQLVariable;
begin
  result := TGraphQLVariable.Create;
  try
    consumePunctuation('$');
    result.Name := consumeName;
    consumePunctuation(':');
    result.TypeName := consumeName;
    if hasPunctuation('=') then
    begin
      consumePunctuation('=');
      result.DefaultValue := parseValue;

    end;


    result.Link;
  finally
    result.Free;
  end;
end;

procedure TGraphQLParser.consumeName(name: String);
begin
  if FLexType <> gqlltName then
    raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting a name');
  if FToken.ToString <> name then
    raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting "'+name+'"');
  next;
end;

procedure TGraphQLParser.consumePunctuation(punc: String);
begin
  if FLexType <> gqlltPunctuation then
    raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting "'+punc+'"');
  if FToken.ToString <> punc then
    raise EGraphQLException.Create('Found "'+FToken.ToString+'" expecting "'+punc+'"');
  next;
end;

constructor TGraphQLParser.Create;
begin
  inherited;
  FToken := TStringBuilder.Create;
end;

destructor TGraphQLParser.Destroy;
begin
  FToken.Free;
  inherited;
end;

procedure TGraphQLParser.PushChar(ch: Char);
begin
  if (ch <> #0) then
    insert(ch, FPeek, 1);
end;

procedure TGraphQLParser.skipIgnore;
var
  ch : char;
begin
  ch := getNextChar;
  while CharInSet(ch, [' ', #9, #13, #10, ',', #$FE, #$FF]) do
    ch := getNextChar;
  if (ch = '#') then
  begin
    while not CharInSet(ch, [#13, #10]) do
      ch := getNextChar;
    pushChar(ch);
    skipIgnore;
  end
  else
    pushChar(ch);
end;


{ TGraphQLTests }

procedure TGraphQLTests.TestSelectionSet;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  id'+#13#10+
'  firstName'+#13#10+
'  lastName'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestShort;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  field'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestSimple;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  user(id: 4) {'+#13#10+
'    name'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestUser;
begin
  TGraphQLParser.parse(
'# `user` represents one of many users in a graph of data, referred to by a'+#13#10+
'# unique identifier.'+#13#10+
'{'+#13#10+
'  user(id: 4) {'+#13#10+
'    name'+#13#10+
'  }'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestVariable;
begin
  TGraphQLParser.parse(
'query getZuckProfile($devicePicSize: Int) {'+#13#10+
'  user(id: 4) {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'    profilePic(size: $devicePicSize)'+#13#10+
'  }'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestAlias1;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  user(id: 4) {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'    smallPic: profilePic(size: 64)'+#13#10+
'    bigPic: profilePic(size: 1024)'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestAlias2;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  zuck: user(id: 4) {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestArguments1;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  user(id: 4) {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'    profilePic(size: 100)'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestArguments2;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  user(id: 4) {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'    profilePic(width: 100, height: 50)'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestArguments3;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  picture(width: 200, height: 100)'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestArguments4;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  picture(height: 100, width: 200)'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestComment;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  # test this'+#13#10+
'  user(id: 4) {'+#13#10+
'    name'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestFragments0;
begin
  TGraphQLParser.parse(
'query noFragments {'+#13#10+
'  user(id: 4) {'+#13#10+
'    friends(first: 10) {'+#13#10+
'      id'+#13#10+
'      name'+#13#10+
'      profilePic(size: 50)'+#13#10+
'    }'+#13#10+
'    mutualFriends(first: 10) {'+#13#10+
'      id'+#13#10+
'      name'+#13#10+
'      profilePic(size: 50)'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestFragments1;
begin
  TGraphQLParser.parse(
'query withFragments {'+#13#10+
'  user(id: 4) {'+#13#10+
'    friends(first: 10) {'+#13#10+
'      ...friendFields'+#13#10+
'    }'+#13#10+
'    mutualFriends(first: 10) {'+#13#10+
'      ...friendFields'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10+
''+#13#10+
'fragment friendFields on User {'+#13#10+
'  id'+#13#10+
'  name'+#13#10+
'  profilePic(size: 50)'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestFragments2;
begin
  TGraphQLParser.parse(
'query withNestedFragments {'+#13#10+
'  user(id: 4) {'+#13#10+
'    friends(first: 10) {'+#13#10+
'      ...friendFields'+#13#10+
'    }'+#13#10+
'    mutualFriends(first: 10) {'+#13#10+
'      ...friendFields'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10+
''+#13#10+
'fragment friendFields on User {'+#13#10+
'  id'+#13#10+
'  name'+#13#10+
'  ...standardProfilePic'+#13#10+
'}'+#13#10+
''+#13#10+
'fragment standardProfilePic on User {'+#13#10+
'  profilePic(size: 50)'+#13#10+
'}'+#13#10).Free;
end;


procedure TGraphQLTests.TestFragments3;
begin
  TGraphQLParser.parse(
'query FragmentTyping {'+#13#10+
'  profiles(handles: ["zuck", "cocacola"]) {'+#13#10+
'    handle'+#13#10+
'    ...userFragment'+#13#10+
'    ...pageFragment'+#13#10+
'  }'+#13#10+
'}'+#13#10+
''+#13#10+
'fragment userFragment on User {'+#13#10+
'  friends {'+#13#10+
'    count'+#13#10+
'  }'+#13#10+
'}'+#13#10+
''+#13#10+
'fragment pageFragment on Page {'+#13#10+
'  likers {'+#13#10+
'    count'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestFragments4;
begin
  TGraphQLParser.parse(
'query inlineFragmentTyping {'+#13#10+
'  profiles(handles: ["zuck", "cocacola"]) {'+#13#10+
'    handle'+#13#10+
'    ... on User {'+#13#10+
'      friends {'+#13#10+
'        count'+#13#10+
'      }'+#13#10+
'    }'+#13#10+
'    ... on Page {'+#13#10+
'      likers {'+#13#10+
'        count'+#13#10+
'      }'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestFragments5;
begin
  TGraphQLParser.parse(
'query inlineFragmentNoType($expandedInfo: Boolean) {'+#13#10+
'  user(handle: "zuck") {'+#13#10+
'    id'+#13#10+
'    name'+#13#10+
'    ... @include(if: $expandedInfo) {'+#13#10+
'      firstName'+#13#10+
'      lastName'+#13#10+
'      birthday'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10).Free;
end;

procedure TGraphQLTests.TestMe;
begin
  TGraphQLParser.parse(
'# `me` could represent the currently logged in viewer.'+#13#10+
'{'+#13#10+
'  me {'+#13#10+
'    name'+#13#10+
'  }'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestMutation;
begin
  TGraphQLParser.parse(
'mutation {'+#13#10+
'  likeStory(storyID: 12345) {'+#13#10+
'    story {'+#13#10+
'      likeCount'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestNested;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  me {'+#13#10+
'    id'+#13#10+
'    firstName'+#13#10+
'    lastName'+#13#10+
'    birthday {'+#13#10+
'      month'+#13#10+
'      day'+#13#10+
'    }'+#13#10+
'    friends {'+#13#10+
'      name'+#13#10+
'    }'+#13#10+
'  }'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestObject1;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  nearestThing(location: { lon: 12.43, lat: -53.211 })'+#13#10+
'}'+#13#10
).Free;
end;

procedure TGraphQLTests.TestObject2;
begin
  TGraphQLParser.parse(
'{'+#13#10+
'  nearestThing(location: { lat: -53.211, lon: 12.43 })'+#13#10+
'}'+#13#10
).Free;
end;

{ TGraphQLValue }

function TGraphQLValue.Link: TGraphQLValue;
begin
  result := TGraphQLValue(inherited Link);
end;

{ TGraphQLNumberValue }

constructor TGraphQLNumberValue.Create(value: String);
begin
  Inherited Create;
  FValue := value;
end;

function TGraphQLNumberValue.Link: TGraphQLNumberValue;
begin
  result := TGraphQLNumberValue(inherited Link);
end;

{ TGraphQLVariableValue }

constructor TGraphQLVariableValue.Create(value: String);
begin
  Inherited Create;
  FValue := value;
end;

function TGraphQLVariableValue.Link: TGraphQLVariableValue;
begin
  result := TGraphQLVariableValue(inherited Link);
end;

{ TGraphQLNameValue }

constructor TGraphQLNameValue.Create(value: String);
begin
  Inherited Create;
  FValue := value;
end;

function TGraphQLNameValue.Link: TGraphQLValue;
begin
  result := TGraphQLNameValue(inherited Link);
end;

{ TGraphQLStringValue }

constructor TGraphQLStringValue.Create(value: String);
begin
  Inherited Create;
  FValue := value;
end;

function TGraphQLStringValue.Link: TGraphQLStringValue;
begin
  result := TGraphQLStringValue(inherited Link);
end;

{ TGraphQLObjectValue }

constructor TGraphQLObjectValue.Create;
begin
  inherited;
  FFields := TAdvList<TGraphQLArgument>.create;
end;

destructor TGraphQLObjectValue.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TGraphQLObjectValue.Link: TGraphQLObjectValue;
begin
  result := TGraphQLObjectValue(inherited Link);
end;

initialization
  TDUnitX.RegisterTestFixture(TGraphQLTests);
end.
