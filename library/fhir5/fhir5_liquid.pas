unit fhir5_liquid;

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
  SysUtils, Classes, Character,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc,
  fhir_objects, 
  fhir5_resources, fhir5_pathnode, fhir5_pathengine, fhir5_context, fhir5_base;

type
  TEvaluationContext = class (TObject);

  TFHIRLiquidEngine = class;
  TFHIRLiquidDocument = class;
  TFHIRLiquidNode = class;
  TFHIRLiquidEngineContext = class;

  TFHIRLiquidEngineDebuggingStatus = (ldsRunToBreakpoint, ldsStepOut, ldsStepOver, ldsStepIn);
  TFHIRLiquidEngineDebugEvent = procedure (engine : TFHIRLiquidEngine; info : TFHIRLiquidEngineContext) of object;

  TFHIRLiquidEngineContext = class (TFslObject)
  private
    FParent: TFHIRLiquidEngineContext;
    FExternalContext : TFslObject;
    FVars : TFslMap<TFHIRObject>;
    FEngine: TFHIRLiquidEngine;
    FDocument : TFHIRLiquidDocument;
    FOnDebug : TFHIRLiquidEngineDebugEvent;
    FNode : TFHIRLiquidNode;
    FStatus: TFHIRLiquidEngineDebuggingStatus;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(parent : TFHIRLiquidEngineContext; node : TFHIRLiquidNode);
    destructor Destroy; override;
    function link : TFHIRLiquidEngineContext; overload;
    property parent : TFHIRLiquidEngineContext read FParent;
    property vars : TFslMap<TFHIRObject> read FVars;
    property node : TFHIRLiquidNode read FNode;
    property status : TFHIRLiquidEngineDebuggingStatus read FStatus write FStatus;
  end;

  TFHIRLiquidNode = class abstract (TFslObject)
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
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure addChar(ch : char);
    function link : TFHIRLiquidConstant; overload;
    function ToString : String; override;
  end;

  TFHIRLiquidStatement = class (TFHIRLiquidNode)
  private
    FStatement : String;
    FCompiled : TFHIRPathExpressionNode;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function link : TFHIRLiquidStatement; overload;
    function ToString : String; override;

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
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidIf; overload;
    function ToString : String; override;

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
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidLoop; overload;
    function ToString : String; override;

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
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidComment; overload;
    function ToString : String; override;

    property body : TFSLList<TFHIRLiquidNode> read FBody;
  end;

  TFHIRLiquidInclude = class (TFHIRLiquidNode)
  private
    FPage : String;
    FParams : TFslMap<TFHIRPathExpressionNode>;
  protected
    procedure evaluate(b : TStringBuilder; resource : TFHIRResource; ctxt : TFHIRLiquidEngineContext); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidInclude; overload;
    function ToString : String; override;

    property page : String read FPage write FPage;
    property params : TFslMap<TFHIRPathExpressionNode> read FParams;
  end;

  TFHIRLiquidDocument = class (TFslObject)
  private
    FBody : TFSLList<TFHIRLiquidNode>;
    FSource: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRLiquidDocument; overload;

    property body : TFSLList<TFHIRLiquidNode> read FBody;
    property source : String read FSource write FSource;

    function ToString : String; override;
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
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(source : String);
    property sourceName : String read FSourceName  write FSourceName ;
  end;

  TFHIRLiquidEngineFetchIncludeEvent = function (sender : TFHIRLiquidEngine; name : String; var content : String) : boolean of object;

  TFHIRLiquidEngine = class (TFslObject)
  private
    fpe : TFHIRPathEngine;
    FOnFetchInclude: TFHIRLiquidEngineFetchIncludeEvent;

    function debug(dbgContext : TFHIRLiquidEngineContext; node : TFHIRLiquidNode) : TFHIRLiquidEngineContext; overload;
    function debug(dbgContext : TFHIRLiquidEngineContext; node : TFHIRLiquidNode; name : String; value : TFHIRObject) : TFHIRLiquidEngineContext; overload;
    function resolveConstant(source : TFHIRPathEngine; appInfo : TFslObject; name : String; beforeContext : boolean) : TFHIRObject;
    function findInclude(page, source : String) : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(fpe : TFHIRPathEngine);
    destructor Destroy; override;
    function parse(source : String; sourceName : String) : TFHIRLiquidDocument;
    function evaluate(document : TFHIRLiquidDocument; resource : TFHIRResource;  appContext : TFslObject) : String; overload;
    function evaluate(document : TFHIRLiquidDocument; resource : TFHIRResource;  appContext : TFslObject; OnDebug : TFHIRLiquidEngineDebugEvent) : String; overload;

    property OnFetchInclude : TFHIRLiquidEngineFetchIncludeEvent read FOnFetchInclude write FOnFetchInclude;
  end;

implementation

{ TFHIRLiquidEngineContext }

constructor TFHIRLiquidEngineContext.Create(parent : TFHIRLiquidEngineContext; node : TFHIRLiquidNode);
begin
  inherited create;
  FVars := TFslMap<TFHIRObject>.create('liquid.context');

  FParent := parent.link;
  FNode := node.Link;
  if FParent = nil then
    status := ldsRunToBreakpoint
  else
  begin
    FEngine := parent.FEngine;
    FDocument := parent.FDocument;
    FOnDebug := parent.FOnDebug;
    FExternalContext := parent.FExternalContext.Link;
    FVars.addAll(parent.Fvars);
    case FParent.status of
      ldsRunToBreakpoint: status := ldsRunToBreakpoint;
      ldsStepOut: status := ldsRunToBreakpoint;
      ldsStepOver: status := ldsRunToBreakpoint;
      ldsStepIn: status := ldsStepOver;
    end;
  end;
end;

destructor TFHIRLiquidEngineContext.Destroy;
begin
  FVars.Free;
  FExternalContext.Free;
  inherited;
end;

function TFHIRLiquidEngineContext.link: TFHIRLiquidEngineContext;
begin
  result := TFHIRLiquidEngineContext(inherited Link);
end;

function TFHIRLiquidEngineContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FExternalContext.sizeInBytes);
  inc(result, FVars.sizeInBytes);
  inc(result, FEngine.sizeInBytes);
  inc(result, FDocument.sizeInBytes);
  inc(result, FNode.sizeInBytes);
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
  FConstant := b.ToString();
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

function TFHIRLiquidConstant.ToString: String;
begin
  result := FConstant;
end;

function TFHIRLiquidConstant.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FConstant.length * sizeof(char)) + 12);
end;

{ TFHIRLiquidStatement }

destructor TFHIRLiquidStatement.Destroy;
begin
  FCompiled.Free;
  inherited;
end;

procedure TFHIRLiquidStatement.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
var
  c : TFHIRLiquidEngineContext;
begin
  c := ctxt.FEngine.debug(ctxt, self);
  try
    b.append(c.FEngine.fpe.evaluateToString(ctxt, resource, FCompiled));
  finally
    c.free;
  end;
end;

function TFHIRLiquidStatement.link: TFHIRLiquidStatement;
begin
  result := TFHIRLiquidStatement(inherited Link);
end;

function TFHIRLiquidStatement.ToString: String;
begin
  result := '{{ '+FStatement+' }}';
end;

function TFHIRLiquidStatement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FStatement.length * sizeof(char)) + 12);
  inc(result, FCompiled.sizeInBytes);
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
  c : TFHIRLiquidEngineContext;
begin
  c := ctxt.FEngine.debug(ctxt, self);
  try
    ok := ctxt.Fengine.fpe.evaluateToBoolean(ctxt, resource, resource, FCompiled);
    if ok then
      list := FThenBody
    else
      list := FElseBody;
    for n in list do
      n.evaluate(b, resource, ctxt);
  finally
    c.Free;
  end;
end;

function TFHIRLiquidIf.link: TFHIRLiquidIf;
begin
  result := TFHIRLiquidIf(inherited Link);
end;

function TFHIRLiquidIf.ToString: String;
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

function TFHIRLiquidIf.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCondition.length * sizeof(char)) + 12);
  inc(result, FCompiled.sizeInBytes);
  inc(result, FThenBody.sizeInBytes);
  inc(result, FElseBody.sizeInBytes);
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
  c : TFHIRLiquidEngineContext;
begin
  list := ctxt.Fengine.fpe.evaluate(ctxt, resource, resource, FCompiled);
  try
    for o in list do
    begin
      c := ctxt.FEngine.debug(ctxt, self, FVarName, o.value);
      try
        for n in FBody do
          n.evaluate(b, resource, c);
      finally
        c.Free;
      end;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRLiquidLoop.link: TFHIRLiquidLoop;
begin
  result := TFHIRLiquidLoop(inherited Link);
end;

function TFHIRLiquidLoop.ToString: String;
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

function TFHIRLiquidLoop.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FVarName.length * sizeof(char)) + 12);
  inc(result, (FCondition.length * sizeof(char)) + 12);
  inc(result, FCompiled.sizeInBytes);
  inc(result, FBody.sizeInBytes);
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

function TFHIRLiquidDocument.ToString: String;
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

function TFHIRLiquidDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBody.sizeInBytes);
  inc(result, (FSource.length * sizeof(char)) + 12);
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
          raise EParserException.create(sourceName+': Unknown flow control statement '+cnt, FLast);
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
    res.FCompiled := fpe.parse(res.Condition);
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
  n : String;
  res : TFHIRLiquidInclude;
begin
  i := 1;
  while (i <= cnt.length) and (not cnt[i].isWhitespace) do
    inc(i);
  if (i > cnt.Length) or (i = 0) then
    raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast);
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
        raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast);
      n := cnt.substring(j-1, i-j);
      if (res.params.ContainsKey(n)) then
        raise EParserException.create(sourceName+': Error reading include: '+cnt, FLast);
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
      raise EParserException.create(sourceName+': Error reading loop: '+cnt, FLast);
    res.condition := cnt.substring(i).trim();
    res.FCompiled := fpe.parse(res.condition);
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
      raise EParserException.create(sourceName+': Unterminated Liquid statement {{ '+b.ToString(), FCurrent);
    grab();
    grab();
    res := TFHIRLiquidStatement.create();
    try
      res.statement := b.ToString().trim();
      res.FCompiled := fpe.parse(res.Statement);
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
      raise EParserException.create(sourceName+': Unterminated Liquid statement {% '+b.ToString(), FCurrent);
    grab();
    grab();
    result := b.ToString().trim();
  finally
    b.Free;
  end;
end;

function TFHIRLiquidParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (source.length * sizeof(char)) + 12);
  inc(result, fpe.sizeInBytes);
  inc(result, (FSourceName.length * sizeof(char)) + 12);
end;

{ TFHIRLiquidEngine }

constructor TFHIRLiquidEngine.Create(fpe: TFHIRPathEngine);
begin
  Inherited Create;
  self.fpe := fpe;
  self.fpe.OnResolveConstant := resolveConstant;
end;

function TFHIRLiquidEngine.debug(dbgContext: TFHIRLiquidEngineContext; node: TFHIRLiquidNode; name: String; value: TFHIRObject): TFHIRLiquidEngineContext;
begin
  result := TFHIRLiquidEngineContext.create(dbgContext, node);
  try
    result.vars.Add(name, value.Link);
    if assigned(result.FOnDebug) then
      result.FOnDebug(self, result);
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRLiquidEngine.debug(dbgContext: TFHIRLiquidEngineContext; node: TFHIRLiquidNode): TFHIRLiquidEngineContext;
begin
  result := TFHIRLiquidEngineContext.create(dbgContext, node);
  try
    if assigned(result.FOnDebug) then
      result.FOnDebug(self, result);
    result.link;
  finally
    result.Free;
  end;
end;

destructor TFHIRLiquidEngine.Destroy;
begin
  fpe.Free;
  inherited;
end;

function TFHIRLiquidEngine.evaluate(document: TFHIRLiquidDocument; resource: TFHIRResource; appContext: TFslObject; OnDebug: TFHIRLiquidEngineDebugEvent): String;
var
  b : TStringBuilder;
  ctxt : TFHIRLiquidEngineContext;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.create();
  try
    ctxt := TFHIRLiquidEngineContext.create(nil, nil);
    try
      ctxt.FExternalContext := appContext.Link;
      ctxt.FEngine := self;
      ctxt.FDocument := document;
      ctxt.FOnDebug := OnDebug;
      for n in document.body do
        n.evaluate(b, resource, ctxt);
    finally
      ctxt.free;
    end;
    result := b.ToString();
  finally
    b.free;
  end;
end;

function TFHIRLiquidEngine.evaluate(document: TFHIRLiquidDocument; resource: TFHIRResource; appContext: TFslObject): String;
var
  b : TStringBuilder;
  ctxt : TFHIRLiquidEngineContext;
  n : TFHIRLiquidNode;
begin
  b := TStringBuilder.create();
  try
    ctxt := TFHIRLiquidEngineContext.create(nil, nil);
    try
      ctxt.FExternalContext := appContext.Link;
      ctxt.FEngine := self;
      ctxt.FDocument := document;
      for n in document.body do
        n.evaluate(b, resource, ctxt);
    finally
      ctxt.free;
    end;
    result := b.ToString();
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

function TFHIRLiquidEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, fpe.sizeInBytes);
end;

{ TFHIRLiquidInclude }

constructor TFHIRLiquidInclude.Create;
begin
  inherited;
  FParams := TFslMap<TFHIRPathExpressionNode>.create('liquid.include');
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
  c : TFHIRLiquidEngineContext;
  incl : TFHIRTuple;
  s : String;
  n : TFHIRLiquidNode;
begin
  src := ctxt.FEngine.findInclude(page, ctxt.FDocument.source);
  doc := ctxt.FEngine.parse(src, page);
  try
    incl := TFHIRTuple.create;
    try
      for s in FParams.Keys do
        incl.addProperty(s, ctxt.Fengine.fpe.evaluate(ctxt, resource, resource, FParams[s]));
      c := ctxt.FEngine.debug(ctxt, self, 'include', incl);
      try
        for n in doc.body do
          n.evaluate(b, resource, c);
      finally
        c.Free;
      end;
    finally
      incl.free;
    end;
  finally
    doc.Free;
  end;
end;

function TFHIRLiquidInclude.link: TFHIRLiquidInclude;
begin
  result := TFHIRLiquidInclude(inherited link);
end;

function TFHIRLiquidInclude.ToString: String;
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

function TFHIRLiquidInclude.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPage.length * sizeof(char)) + 12);
  inc(result, FParams.sizeInBytes);
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

procedure TFHIRLiquidComment.evaluate(b: TStringBuilder; resource: TFHIRResource; ctxt: TFHIRLiquidEngineContext);
var
  c : TFHIRLiquidEngineContext;
begin
  c := ctxt.FEngine.debug(ctxt, self);
  try
    // nothing
  finally
    c.Free;
  end;
end;

function TFHIRLiquidComment.link: TFHIRLiquidComment;
begin
  result := TFHIRLiquidComment(inherited Link);
end;

function TFHIRLiquidComment.ToString: String;
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

function TFHIRLiquidComment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBody.sizeInBytes);
end;

end.

