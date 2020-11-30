unit fhir3_liquid;

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  fsl_base, fsl_utilities, fsl_fpc,
  fhir_objects, 
  fhir3_resources, fhir3_pathnode, fhir3_pathengine, fhir3_context, fhir3_base;

type
  TEvaluationContext = class (TObject);

  TFHIRLiquidEngine = class;
  TFHIRLiquidDocument = class;

  TFHIRLiquidEngineContext = class (TFslObject)
  private
    FExternalContext : TFslObject;
    FVars : TFslMap<TFHIRObject>;
    FEngine: TFHIRLiquidEngine;
    FDocument : TFHIRLiquidDocument;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(engine: TFHIRLiquidEngine; document : TFHIRLiquidDocument; externalContext : TFslObject); overload;
    constructor Create(existing : TFHIRLiquidEngineContext); overload;
    destructor Destroy; override;
    function link : TFHIRLiquidEngineContext; overload;
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

    property varName : String read FVarName write FVarName;
    property condition : string read FCondition write FCOndition;
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
  end;

  TFHIRLiquidParser = class (TFslObject)
  private
    source : String;
    cursor : integer;
    fpe : TFHIRPathEngine;
    FSourceName : String;
    function next1 : char;
    function next2 : char;
    function grab : char;
    function parseTag(ch : char) : String ;
    function parseIf(cnt : String) : TFHIRLiquidNode;
    function parseLoop(cnt : String) : TFHIRLiquidNode;
    function parseInclude(cnt : String) : TFHIRLiquidNode;
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

    function resolveConstant(source : TFHIRPathEngine; appInfo : TFslObject; name : String; beforeContext : boolean) : TFHIRObject;
    function findInclude(page, source : String) : String;
  protected
    function sizeInBytesV : cardinal; override;
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
  FVars := TFslMap<TFHIRObject>.create('liquid.context');
  FVars.addAll(existing.Fvars);
end;

constructor TFHIRLiquidEngineContext.Create(engine: TFHIRLiquidEngine; document : TFHIRLiquidDocument; externalContext: TFslObject);
begin
  inherited create;
  FEngine := engine;
  FDocument := document;
  FexternalContext := externalContext;
  FVars := TFslMap<TFHIRObject>.create('Liquid.vars');
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

function TFHIRLiquidEngineContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FExternalContext.sizeInBytes);
  inc(result, FVars.sizeInBytes);
  inc(result, FEngine.sizeInBytes);
  inc(result, FDocument.sizeInBytes);
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
begin
  if (FCompiled = nil) then
    FCompiled := ctxt.FEngine.fpe.parse(FStatement);
  b.append(ctxt.FEngine.fpe.evaluateToString(ctxt, resource, FCompiled));
end;

function TFHIRLiquidStatement.link: TFHIRLiquidStatement;
begin
  result := TFHIRLiquidStatement(inherited Link);
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
end;

function TFHIRLiquidParser.grab: char;
begin
  inc(cursor);
  result := source[cursor-1];
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
        else
          raise EFHIRException.create(sourceName+': Unknown flow control statement '+cnt);
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
  n : String;
  res : TFHIRLiquidInclude;
begin
  i := 1;
  while (i <= cnt.length) and (not cnt[i].isWhitespace) do
    inc(i);
  if (i > cnt.Length) or (i = 0) then
    raise EFHIRException.create(sourceName+': Error reading include: '+cnt);
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
        raise EFHIRException.create(sourceName+': Error reading include: '+cnt);
      n := cnt.substring(j-1, i-j);
      if (res.params.ContainsKey(n)) then
        raise EFHIRException.create(sourceName+': Error reading include: '+cnt);
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
      raise EFHIRException.create(sourceName+': Error reading loop: '+cnt);
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
      raise EFHIRException.create(sourceName+': Unterminated Liquid statement {{ '+b.toString());
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
  grab();
  grab();
  b := TStringBuilder.create();
  try
    while (cursor <= source.length) and not ((next1() = '%') and(next2() = '}')) do
      b.append(grab());
    if not ((next1() = '%') and (next2() = '}')) then
      raise EFHIRException.create(sourceName+': Unterminated Liquid statement {% '+b.toString());
    grab();
    grab();
    result := b.toString().trim();
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

function TFHIRLiquidEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, fpe.sizeInBytes);
end;

{ TFHIRLiquidInclude }

constructor TFHIRLiquidInclude.Create;
begin
  inherited;
  FParams := TFslMap<TFHIRPathExpressionNode>.create('liquid.includes');
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

function TFHIRLiquidInclude.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPage.length * sizeof(char)) + 12);
  inc(result, FParams.sizeInBytes);
end;

end.

