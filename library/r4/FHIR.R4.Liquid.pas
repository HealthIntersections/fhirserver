unit FHIR.R4.Liquid;

interface

uses
  SysUtils, Classes, System.Character,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Lang,
  FHIR.R4.Resources, FHIR.R4.PathNode, FHIR.R4.PathEngine, FHIR.R4.Context, FHIR.R4.Base;

type
  TEvaluationContext = class (TObject);

  TFHIRLiquidEngine = class;
  TFHIRLiquidDocument = class;

  TFHIRLiquidEngineContext = class (TFslObject)
  private
    FExternalContext : TFSLObject;
    FVars : TFslMap<TFHIRObject>;
    FEngine: TFHIRLiquidEngine;
    FDocument : TFHIRLiquidDocument;
  public
    constructor Create(engine: TFHIRLiquidEngine; document : TFHIRLiquidDocument; externalContext : TFSLObject); overload;
    constructor Create(existing : TFHIRLiquidEngineContext); overload;
    destructor Destroy; override;
    function link : TFHIRLiquidEngineContext; overload;
  end;

  TFHIRLiquidNode = class abstract (TFSLObject)
  protected
    procedure closeUp(); virtual;
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); virtual; abstract;
  public
    function link : TFHIRLiquidNode; overload;
  end;

  TFHIRLiquidConstant = class (TFHIRLiquidNode)
  private
    FConstant : String;
    b : TStringBuilder;
  protected
    procedure closeUp; override;
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure addChar(ch : char);
    function link : TFHIRLiquidConstant; overload;
    function toString : String; override;
  end;

  TFHIRLiquidStatement = class (TFHIRLiquidNode)
  private
    FStatement : String;
    FCompiled : TFHIRPathExpressionNode;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    destructor Destroy; override;
    function link : TFHIRLiquidStatement; overload;
    function toString : String; override;

    property statement : string read FStatement write FStatement;
  end;

  TFHIRLiquidIf = class (TFHIRLiquidNode)
  private
    FCondition : String ;
    FCompiled : TFHIRPathExpressionNode;
    FThenBody : TFSLList<TFHIRLiquidNode>;
    FElseBody : TFSLList<TFHIRLiquidNode>;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidIf; overload;
    function toString : String; override;

    property condition : string read FCondition write FCOndition;
    property thenBody : TFSLList<TFHIRLiquidNode> read FThenBody;
    property elseBody : TFSLList<TFHIRLiquidNode> read FElseBody;
  end;

  TFHIRLiquidLoop = class (TFHIRLiquidNode)
  private
    FVarName : String;
    FCondition : String;
    FCompiled : TFHIRPathExpressionNode;
    FBody : TFSLList<TFHIRLiquidNode>;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidLoop; overload;
    function toString : String; override;

    property varName : String read FVarName write FVarName;
    property condition : string read FCondition write FCOndition;
    property body : TFSLList<TFHIRLiquidNode> read FBody;
  end;

  // we keep all the comment content in order to reproduce the original
  TFHIRLiquidComment = class (TFHIRLiquidNode)
  private
    FBody : TFSLList<TFHIRLiquidNode>;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidComment; overload;
    function toString : String; override;

    property body : TFSLList<TFHIRLiquidNode> read FBody;
  end;

  TFHIRLiquidInclude = class (TFHIRLiquidNode)
  private
    FPage : String;
    FParams : TFslMap<TFHIRPathExpressionNode>;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidInclude; overload;
    function toString : String; override;

    property page : String read FPage write FPage;
    property params : TFslMap<TFHIRPathExpressionNode> read FParams;
  end;

  TFHIRLiquidDocument = class (TFslObject)
  private
    FBody : TFSLList<TFHIRLiquidNode>;
    FSource: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidDocument; overload;

    property body : TFSLList<TFHIRLiquidNode> read FBody;
    property source : String read FSource write FSource;

    function toString : String; override;
  end;

  TFHIRLiquidParser = class (TFslObject)
  private
    source : String;
    cursor : integer;
    FCurrent, FLast : TSourceLocation;
    fpe : TFHIRPathEngine;
    FSourceName : String;
    function next1 : char;
    function next2 : char;
    function grab : char;
    function parseTag(ch : char) : String ;
    function parseIf(cnt : String) : TFHIRLiquidNode;
    function parseLoop(cnt : String) : TFHIRLiquidNode;
    function parseInclude(cnt : String) : TFHIRLiquidNode;
    function parseComment(cnt : String) : TFHIRLiquidNode;
    function parseStatement() : TFHIRLiquidStatement;
    function parseList(list : TFslList<TFHIRLiquidNode>; terminators : Array of String) : String;
    function parse : TFHIRLiquidDocument;
  public
    constructor Create(source : String);
    property sourceName : String read FSourceName  write FSourceName ;
  end;

  TFHIRLiquidEngineFetchIncludeEvent = function (sender : TFHIRLiquidEngine; name : String; var content : String) : boolean of object;

  TFHIRLiquidEngine = class (TFslObject)
  private
    fpe : TFHIRPathEngine;
    FOnFetchInclude: TFHIRLiquidEngineFetchIncludeEvent;

    function resolveConstant(source : TFHIRPathEngine; appInfo : TFslObject; name : String; beforeContext : boolean) : TFHIRObject;
    function findInclude(page, source : String) : String;
  public
    constructor Create(fpe : TFHIRPathEngine);
    destructor Destroy; override;
    function parse(source : String; sourceName : String) : TFHIRLiquidDocument;
    function evaluate(document : TFHIRLiquidDocument; resource : TFHIRResource;  appContext : TFslObject) : String;

    property OnFetchInclude : TFHIRLiquidEngineFetchIncludeEvent read FOnFetchInclude write FOnFetchInclude;
  end;

implementation

{ TFHIRLiquidEngineContext }

constructor TFHIRLiquidEngineContext.Create(existing: TFHIRLiquidEngineContext);
begin
  inherited create;
  FEngine := existing.FEngine;
  FDocument := existing.FDocument;
  FExternalContext := existing.FExternalContext.Link;
  FVars := TFslMap<TFHIRObject>.create;
  FVars.addAll(existing.Fvars);
end;

constructor TFHIRLiquidEngineContext.Create(engine: TFHIRLiquidEngine; document : TFHIRLiquidDocument; externalContext: TFSLObject);
begin
  inherited create;
  FEngine := engine;
  FDocument := document;
  FexternalContext := externalContext;
  FVars := TFslMap<TFHIRObject>.create;
end;

destructor TFHIRLiquidEngineContext.Destroy;
begin
  FVars.Free;
  FexternalContext.Free;
  inherited;
end;

function TFHIRLiquidEngineContext.link: TFHIRLiquidEngineContext;
begin
  result := TFHIRLiquidEngineContext(inherited Link);
end;

{ TFHIRLiquidNode }

procedure TFHIRLiquidNode.closeUp;
begin
end;

function TFHIRLiquidNode.link: TFHIRLiquidNode;
begin
  result := TFHIRLiquidNode(inherited Link);
end;

{ TFHIRLiquidConstant }

procedure TFHIRLiquidConstant.addChar(ch: char);
begin
  b.append(ch);
end;

procedure TFHIRLiquidConstant.closeUp;
begin
  FConstant := b.toString();
  b.Free;
  b := nil;
end;

constructor TFHIRLiquidConstant.Create;
begin
  inherited;
  b := TStringBuilder.create;
end;

destructor TFHIRLiquidConstant.Destroy;
begin
  b.Free;
  inherited;
end;

procedure TFHIRLiquidConstant.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
begin
  b.append(FConstant);
end;

function TFHIRLiquidConstant.link: TFHIRLiquidConstant;
begin
  result := TFHIRLiquidConstant(inherited Link);
end;

function TFHIRLiquidConstant.toString: String;
begin
  result := FConstant;
end;

{ TFHIRLiquidStatement }

destructor TFHIRLiquidStatement.Destroy;
begin
  FCompiled.Free;
  inherited;
end;

procedure TFHIRLiquidStatement.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
begin
  if (FCompiled = nil) then
    FCompiled := ctxt.FEngine.fpe.parse(FStatement);
  b.append(ctxt.FEngine.fpe.evaluateToString(ctxt, resource, FCompiled));
end;

function TFHIRLiquidStatement.link: TFHIRLiquidStatement;
begin
  result := TFHIRLiquidStatement(inherited Link);
end;

function TFHIRLiquidStatement.toString: String;
begin
  result := '{{ '+FStatement+' }}';
end;

{ TFHIRLiquidIf }

constructor TFHIRLiquidIf.Create;
begin
  inherited;
  FThenBody := TFSLList<TFHIRLiquidNode>.create;
  FElseBody := TFSLList<TFHIRLiquidNode>.create;
end;

destructor TFHIRLiquidIf.Destroy;
begin
  FThenBody.Free;
  FElseBody.Free;
  FCompiled.Free;
  inherited;
end;


procedure TFHIRLiquidIf.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
var
  ok : boolean;
  list : TFSLList<TFHIRLiquidNode>;
  n : TFHIRLiquidNode;
begin
  if (FCompiled = nil) then
    FCompiled := ctxt.Fengine.fpe.parse(FCondition);
  ok := ctxt.Fengine.fpe.evaluateToBoolean(ctxt, resource, resource, FCompiled);
  if ok then
    list := FThenBody
  else
    list := FElseBody;
  for n in list do
    n.evaluate(b, resource, ctxt);
end;

function TFHIRLiquidIf.link: TFHIRLiquidIf;
begin
  result := TFHIRLiquidIf(inherited Link);
end;

function TFHIRLiquidIf.toString: String;
var
  b : TStringBuilder;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.Create;
  try
    b.Append('{% if '+condition+' %}');
    for n in FThenBody do
      b.Append(n.ToString);
    if not FElseBody.Empty then
    begin
      b.Append('{% else %}');
      for n in FElseBody do
        b.Append(n.ToString);
    end;
    b.Append('{% endif %}');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TFHIRLiquidLoop }

constructor TFHIRLiquidLoop.Create;
begin
  inherited;
  FBody := TFSLList<TFHIRLiquidNode>.create;
end;

destructor TFHIRLiquidLoop.Destroy;
begin
  FBody.Free;
  FCompiled.Free;
  inherited;
end;

procedure TFHIRLiquidLoop.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
var
  list : TFHIRSelectionList;
  n : TFHIRLiquidNode;
  o : TFHIRSelection;
  lctxt : TFHIRLiquidEngineContext;
begin
  if (FCompiled = nil) then
    FCompiled := ctxt.Fengine.fpe.parse(FCondition);
  list := ctxt.Fengine.fpe.evaluate(ctxt, resource, resource, FCompiled);
  try
    lctxt := TFHIRLiquidEngineContext.create(ctxt);
    try
      for o in list do
      begin
       lctxt.Fvars.AddOrSetValue(FVarName, o.value.Link);
       for n in FBody do
        n.evaluate(b, resource, lctxt);
      end;
    finally
      lctxt.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRLiquidLoop.link: TFHIRLiquidLoop;
begin
  result := TFHIRLiquidLoop(inherited Link);
end;

function TFHIRLiquidLoop.toString: String;
var
  b : TStringBuilder;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.Create;
  try
    b.Append('{% loop '+condition+' %}');
    for n in FBody do
      b.Append(n.ToString);
    b.Append('{% endloop %}');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TFHIRLiquidDocument }

constructor TFHIRLiquidDocument.Create;
begin
  inherited;
  FBody := TFSLList<TFHIRLiquidNode>.create;
end;

destructor TFHIRLiquidDocument.Destroy;
begin
  FBody.Free;
  inherited;
end;

function TFHIRLiquidDocument.link: TFHIRLiquidDocument;
begin
  result := TFHIRLiquidDocument(inherited Link);
end;

function TFHIRLiquidDocument.toString: String;
var
  b : TStringBuilder;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.Create;
  try
    for n in FBody do
      b.Append(n.ToString);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TFHIRLiquidParser }

constructor TFHIRLiquidParser.Create(source: String);
begin
  inherited Create;
  self.source := source;
  cursor := 1;
  FCurrent.line := 1;
  FCurrent.col := 1;
end;

function TFHIRLiquidParser.grab: char;
begin
  inc(cursor);
  result := source[cursor-1];
  if result = #10 then
  begin
    inc(FCurrent.Line);
    FCurrent.Col := 1;
  end
  else if result <> #13 then
    inc(FCurrent.Col);
end;

function TFHIRLiquidParser.next1: char;
begin
  if (cursor > source.length) then
    result := #0
  else
    result := source[cursor];
end;

function TFHIRLiquidParser.next2: char;
begin
  if (cursor > source.length-1) then
    result := #0
  else
    result := source[cursor+1];
end;

function TFHIRLiquidParser.parse: TFHIRLiquidDocument;
begin
  result := TFHIRLiquidDocument.Create;
  try
    parseList(result.body, []);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRLiquidParser.parseComment(cnt: String): TFHIRLiquidNode;
var
  res : TFHIRLiquidComment;
begin
  res := TFHIRLiquidComment.create();
  try
    parseList(res.body, ['endcomment']);
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TFHIRLiquidParser.parseList(list: TFslList<TFHIRLiquidNode>; terminators: array of String): String;
var
  cnt : String;
  n : TFHIRLiquidNode;
begin
  result := '';
  while (cursor <= source.length) do
  begin
    if (next1() = '{') and ((next2() = '%') or (next2() = '{')) then
    begin
      if (next2() = '%') then
      begin
        cnt := parseTag('%');
        if StringArrayExistsSensitive(terminators, cnt) then
        begin
          result := cnt;
          break;
        end
        else if (cnt.startsWith('if ')) then
          list.add(parseIf(cnt))
        else if (cnt.startsWith('loop ')) then
          list.add(parseLoop(cnt.substring(4).trim()))
        else if (cnt.startsWith('include ')) then
          list.add(parseInclude(cnt.substring(7).trim()))
        else if (cnt.Trim= 'comment') then
          list.add(parseComment(cnt.substring(7).trim()))
        else
          raise EParserException.create(sourceName+': Unknown flow control statement '+cnt, FLast.line, FLast.col);
      end
      else // next2() == '{'
      begin
        list.add(parseStatement());
      end
    end
    else
    begin
      if (list.count = 0) or (not (list[list.count-1] is TFHIRLiquidConstant)) then
        list.add(TFHIRLiquidConstant.create());
      TFHIRLiquidConstant(list[list.count-1]).addChar(grab());
    end
  end;
  for n in list do
    n.closeUp();
  if (length(terminators) > 0) then
    if not StringArrayExistsSensitive(terminators, result) then
      raise EFHIRException.create(sourceName+': Found end of script looking for '+StringArrayToString(terminators));
end;

function TFHIRLiquidParser.parseIf(cnt: String): TFHIRLiquidNode;
var
  term : String;
  res : TFHIRLiquidIf;
begin
  res := TFHIRLiquidIf.Create;
  try
    res.condition := cnt.substring(3).trim();
    term := parseList(res.thenBody, ['else', 'endif']);
    if ('else' = term) then
      term := parseList(res.elseBody, ['endif']);
    result := res.link;
  finally
    res.Free;
  end;
end;

function TFHIRLiquidParser.parseInclude(cnt: String): TFHIRLiquidNode;
var
  i, j : integer;
  s, n : String;
  res : TFHIRLiquidInclude;
begin
  i := 1;
  while (i <= cnt.length) and (not cnt[i].isWhitespace) do
    inc(i);
  if (i > cnt.Length) or (i = 0) then
    raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast.line, FLast.col);
  res := TFHIRLiquidInclude.create();
  try
    res.page := cnt.substring(0, i-1);
    while (i <= cnt.length) and (cnt[i].isWhitespace) do
      inc(i);
    while (i <= cnt.length) do
    begin
      j := i;
      while (i <= cnt.length) and (cnt[i] <> '=') do
        inc(i);
      if (i > cnt.Length) or (j = i) then
        raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast.line, FLast.col);
      n := cnt.substring(j-1, i-j);
      if (res.params.ContainsKey(n)) then
        raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast.line, FLast.col);
      inc(i);
      res.params.AddOrSetValue(n, fpe.parse(cnt, i));
      while (i <= cnt.length) and (cnt[i].isWhitespace) do
        inc(i);
    end;
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TFHIRLiquidParser.parseLoop(cnt: String): TFHIRLiquidNode;
var
  i, j : integer;
  s : String;
  res : TFHIRLiquidLoop;
begin
  i := 1;
  while (i <= cnt.length) and (not cnt[i].isWhitespace) do
    inc(i);
  res := TFHIRLiquidLoop.create();
  try
    res.varName := cnt.substring(0, i-1);
    while (i <= cnt.length) and (cnt[i].isWhitespace) do
      inc(i);
    j := i;
    while (i <= cnt.length) and (not cnt[i].isWhitespace) do
      inc(i);
    s := cnt.substring(j-1, i-j);
    if ('in' <> s) then
      raise EParserException.create(sourceName+': Error reading loop: '+cnt, FLast.line, FLast.col);
    res.condition := cnt.substring(i).trim();
    parseList(res.body, ['endloop']);
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TFHIRLiquidParser.parseStatement(): TFHIRLiquidStatement;
var
  b : TStringBuilder;
  res : TFHIRLiquidStatement;
begin
  grab();
  grab();
  b := TStringBuilder.create();
  try
    while (cursor <= source.length) and not ((next1() = '}') and (next2() = '}')) do
      b.append(grab());
    if not ((next1() = '}') and (next2() = '}')) then
      raise EParserException.create(sourceName+': Unterminated Liquid statement {{ '+b.toString(), FCurrent.line, FCurrent.col);
    grab();
    grab();
    res := TFHIRLiquidStatement.create();
    try
      res.statement := b.toString().trim();
      result := res.link;
    finally
      res.Free;
    end;
  finally
    b.Free;
  end;
end;

function TFHIRLiquidParser.parseTag(ch: char): String;
var
  b : TStringBuilder;
begin
  FLast := FCurrent;
  grab();
  grab();
  b := TStringBuilder.create();
  try
    while (cursor <= source.length) and not ((next1() = '%') and(next2() = '}')) do
      b.append(grab());
    if not ((next1() = '%') and (next2() = '}')) then
      raise EParserException.create(sourceName+': Unterminated Liquid statement {% '+b.toString(), FCurrent.line, FCurrent.col);
    grab();
    grab();
    result := b.toString().trim();
  finally
    b.Free;
  end;
end;

{ TFHIRLiquidEngine }

constructor TFHIRLiquidEngine.Create(fpe: TFHIRPathEngine);
begin
  Inherited Create;
  self.fpe := fpe;
  self.fpe.OnResolveConstant := resolveConstant;
end;

destructor TFHIRLiquidEngine.Destroy;
begin
  fpe.Free;
  inherited;
end;

function TFHIRLiquidEngine.evaluate(document: TFHIRLiquidDocument; resource: TFHIRResource; appContext: TFslObject): String;
var
  b : TStringBuilder;
  ctxt : TFHIRLiquidEngineContext;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.create();
  try
    ctxt := TFHIRLiquidEngineContext.create(self, document, appContext.link);
    try
      for n in document.body do
        n.evaluate(b, resource, ctxt);
    finally
      ctxt.free;
    end;
    result := b.toString();
  finally
    b.free;
  end;
end;

function TFHIRLiquidEngine.findInclude(page, source: String): String;
begin
  if not assigned(FOnFetchInclude) then
    raise Exception.Create('Liquid Engine does not support includes (including "'+page+'" from '+source);
  if not FOnFetchInclude(self, page, result) then
    raise Exception.Create('Unable to find file "'+page+'" included from '+source);
end;

function TFHIRLiquidEngine.parse(source: String; sourceName : String): TFHIRLiquidDocument;
var
  parser : TFHIRLiquidParser;
begin
  parser := TFHIRLiquidParser.Create(source);
  try
    parser.fpe := fpe;
    parser.sourceName := sourceName;
    result := parser.parse;
    result.source := sourceName;
  finally
    parser.Free;
  end;
end;

function TFHIRLiquidEngine.resolveConstant(source: TFHIRPathEngine; appInfo: TFslObject; name: String; beforeContext: boolean): TFHIRObject;
var
  ctxt : TFHIRLiquidEngineContext;
begin
  ctxt := appInfo as TFHIRLiquidEngineContext;
  if (ctxt.Fvars.containsKey(name)) then
    result := ctxt.Fvars[name].Link
  else
    result := nil;
//  else if (externalHostServices == null)
//      return null;
//    return externalHostServices.resolveConstant(ctxt.externalContext, name, beforeContext);

end;

{ TFHIRLiquidInclude }

constructor TFHIRLiquidInclude.Create;
begin
  inherited;
  FParams := TFslMap<TFHIRPathExpressionNode>.create;
end;

destructor TFHIRLiquidInclude.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TFHIRLiquidInclude.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
var
  src : String;
  doc : TFHIRLiquidDocument;
  nctxt : TFHIRLiquidEngineContext;
  incl : TFHIRTuple;
  s : String;
  n : TFHIRLiquidNode;
begin
  src := ctxt.FEngine.findInclude(page, ctxt.FDocument.source);
  doc := ctxt.FEngine.parse(src, page);
  try
    nctxt := TFHIRLiquidEngineContext.Create(ctxt.FEngine, doc, ctxt.FExternalContext);
    try
      incl := TFHIRTuple.create;
      try
        nctxt.FVars.Add('include', incl.link);
        for s in FParams.Keys do
          incl.addProperty(s, ctxt.Fengine.fpe.evaluate(ctxt, resource, resource, FParams[s]));
        for n in doc.body do
          n.evaluate(b, resource, nctxt);
      finally
        incl.free;
      end;
    finally
      nctxt.Free;
    end;
  finally
    doc.Free;
  end;
end;

function TFHIRLiquidInclude.link: TFHIRLiquidInclude;
begin
  result := TFHIRLiquidInclude(inherited link);
end;

function TFHIRLiquidInclude.toString: String;
var
  b : TStringBuilder;
  s : String;
begin
  b := TStringBuilder.Create;
  try
    b.Append('{% include '+FPage+' %}');
    for s in FParams.Keys do
      b.Append(s+' = '+FParams[s].ToString);
    b.Append('{% endloop %}');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TFHIRLiquidComment }

constructor TFHIRLiquidComment.Create;
begin
  inherited;
  FBody := TFSLList<TFHIRLiquidNode>.create;
end;

destructor TFHIRLiquidComment.Destroy;
begin
  FBody.Free;
  inherited;
end;

procedure TFHIRLiquidComment.evaluate(b: TStringBuilder;
  resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
begin

end;

function TFHIRLiquidComment.link: TFHIRLiquidComment;
begin
  result := TFHIRLiquidComment(inherited Link);
end;

function TFHIRLiquidComment.toString: String;
var
  b : TStringBuilder;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.Create;
  try
    b.Append('{% comment %}');
    for n in FBody do
      b.Append(n.ToString);
    b.Append('{% endcomment %}');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

end.




