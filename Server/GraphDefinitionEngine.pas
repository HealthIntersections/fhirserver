unit GraphDefinitionEngine;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils,
  StringSupport, FileSupport, MathSupport, ParseMap,
  AdvObjects, AdvGenerics, AdvJson,
  GraphQL,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRContext, FHIRPath,
  FHIRGraphQL;

type
  EGraphDefinitionEngine = class (Exception);

  TFHIRGraphDefinitionParser = class (TAdvObject)
  private
    FLexer : TFHIRPathLexer;

    procedure readHeader(gd :  TFhirGraphDefinition);
    function readResourceType : TFhirResourceTypesEnum;
    function readProfile : String;
    function readSearchLink : TFhirGraphDefinitionLink;
    function readCompartmentRule(use : TFhirGraphCompartmentUseEnum) : TFhirGraphDefinitionLinkTargetCompartment;
    function readPathLink : TFhirGraphDefinitionLink;

    procedure readLinkList(list : TFhirGraphDefinitionLinkList);
    function readDefinition : TFhirGraphDefinition;

    procedure writeCompartment(b : TStringBuilder; cr : TFhirGraphDefinitionLinkTargetCompartment);
    procedure writePathItem(b : TStringBuilder; item : TFhirGraphDefinitionLink; indent : integer);
    procedure writeSearchItem(b : TStringBuilder; item : TFhirGraphDefinitionLink; indent : integer);
    procedure writeLinklist(b : TStringBuilder; list : TFhirGraphDefinitionLinkList; indent : integer);
    procedure writeHeader(b : TStringBuilder; definition : TFhirGraphDefinition);
    procedure writeDefinition(b : TStringBuilder; definition : TFhirGraphDefinition);
  public
    destructor Destroy; override;

    class function parse(source : String) : TFhirGraphDefinition;
    class function asString(definition : TFhirGraphDefinition; header : boolean) : String;
  end;

  TFHIRGraphDefinitionEngine = class (TAdvObject)
  private
    FContext : TFHIRWorkerContext;
    FPathEngine : TFHIRPathEngine;

    FOnFollowReference : TFHIRGraphQLEngineDereferenceEvent;
    FOnListResources : TFHIRGraphQLEngineListResourcesEvent;
    FOnLookup : TFHIRGraphQLEngineLookupEvent;

    FBundle: TFHIRBundle;
    FStart: TFHIRResource;
    FDefinition: TFhirGraphDefinition;
    FDepthLimit: integer;
    FBaseUrl: String;
    FValidating: boolean;
    FAppinfo: TAdvObject;
    procedure SetBundle(const Value: TFHIRBundle);
    procedure SetDefinition(const Value: TFhirGraphDefinition);
    procedure SetStart(const Value: TFHIRResource);

    function check(test : boolean; msg : String) : boolean;
    function isInBundle(resource :  TFHIRResource) : boolean;
    procedure addToBundle(resource : TFHIRResource);
    procedure processLink(focusPath : String; focus : TFHIRResource; link : TFhirGraphDefinitionLink; depth : integer);
    procedure processLinkPath(focusPath: String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
    procedure processLinkTarget(focusPath: String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
    procedure parseParams(params : TAdvList<TGraphQLArgument>; value : String; res : TFHIRResource);

    procedure SetAppInfo(const Value: TAdvObject);
  public
    constructor Create(context : TFHIRWorkerContext); virtual;
    destructor Destroy; override;

    property appInfo : TAdvObject read FAppinfo write SetAppInfo;
    property baseURL : String read FBaseUrl write FBaseURL;
    property definition : TFhirGraphDefinition read FDefinition write SetDefinition;
    property start : TFHIRResource read FStart write SetStart;
    property bundle : TFHIRBundle read FBundle write SetBundle;
    property depthLimit : integer read FDepthLimit write FDepthLimit; // 0 = no limit
    property validating : boolean read FValidating write FValidating;

    property OnFollowReference : TFHIRGraphQLEngineDereferenceEvent read FOnFollowReference write FOnFollowReference;
    property OnListResources : TFHIRGraphQLEngineListResourcesEvent read FOnListResources write FOnListResources;
    property OnLookup : TFHIRGraphQLEngineLookupEvent read FOnLookup write FOnLookup;

    property PathEngine : TFHIRPathEngine read FPathEngine;
    procedure execute;
  end;

implementation

{ TFHIRGraphDefinitionEngine }

constructor TFHIRGraphDefinitionEngine.Create(context : TFHIRWorkerContext);
begin
  inherited Create;
  FContext := context;
  FPathEngine := TFHIRPathEngine.Create(context.link);
end;

destructor TFHIRGraphDefinitionEngine.Destroy;
begin
  FAppinfo.Free;
  FBundle.Free;
  FDefinition.Free;
  FStart.Free;
  FPathEngine.Free;
  FContext.Free;
  inherited;
end;

procedure TFHIRGraphDefinitionEngine.SetAppInfo(const Value: TAdvObject);
begin
  FAppinfo.Free;
  FAppinfo := Value;
end;

procedure TFHIRGraphDefinitionEngine.SetBundle(const Value: TFHIRBundle);
begin
  FBundle.Free;
  FBundle := Value;
end;

procedure TFHIRGraphDefinitionEngine.SetDefinition(const Value: TFhirGraphDefinition);
begin
  FDefinition.Free;
  FDefinition := Value;
end;

procedure TFHIRGraphDefinitionEngine.SetStart(const Value: TFHIRResource);
begin
  FStart.Free;
  FStart := Value;
end;

procedure TFHIRGraphDefinitionEngine.execute;
var
  l : TFhirGraphDefinitionLink;
begin
  check(assigned(OnFollowReference), 'Reference Resolution services not available');
  check(assigned(FOnListResources), 'Resource search services not available');

  check(Start <> nil, 'A focus resource is needed');
  check(Bundle <> nil, 'An output bundle is needed');
  check(definition <> nil, 'a definition is needed');
  check(FBaseUrl <> '', 'a baseURL is needed');
  FDefinition.checkNoModifiers('GraphDefinitionEngine.execute', 'definition');

  check(Start.fhirType = CODES_TFhirResourceTypesEnum[definition.start], 'Definition requires that Start is '+CODES_TFhirResourceTypesEnum[definition.start]+', but found '+Start.fhirType);
  if not isInBundle(start) then
    addToBundle(start);
  for l in definition.link_List do
    processLink(Start.fhirType, FStart, l, 1);
end;

function TFHIRGraphDefinitionEngine.isInBundle(resource: TFHIRResource): boolean;
var
  be : TFhirBundleEntry;
begin
  result := false;
  for be in FBundle.entryList do
    if (be.resource <> nil) and (be.resource.fhirType = resource.fhirType) and (be.resource.id = resource.id) then
      exit(true);
end;

procedure TFHIRGraphDefinitionEngine.addToBundle(resource: TFHIRResource);
var
  be : TFhirBundleEntry;
begin
  be := FBundle.entryList.Append;
  be.fullUrl := URLPath([FBaseUrl, resource.fhirType, resource.id]);
  be.resource := resource.Link;
end;

function TFHIRGraphDefinitionEngine.check(test: boolean; msg: String): boolean;
begin
  if not test then
    raise EGraphDefinitionEngine.Create(msg);
  result := test;
end;

procedure TFHIRGraphDefinitionEngine.parseParams(params: TAdvList<TGraphQLArgument>; value: String; res: TFHIRResource);
var
  p : TParseMap;
  i, j : integer;
  n, v : String;
  refed : boolean;
begin
  refed := false;
  p := TParseMap.create(value, false);
  try
    for i := 0 to p.getItemCount -1 do
    begin
      n := p.VarName(i);
      for j := 0 to p.getValueCount(i) - 1 do
      begin
        p.retrieveNumberedItem(i,j, v);
        if (v = '{ref}') then
        begin
          refed := true;
          v := res.fhirType+'/'+res.id;
        end;
        params.Add(TGraphQLArgument.Create(n, TGraphQLStringValue.Create(v)));
      end;
    end;
  finally
    p.Free;
  end;
  check(refed, 'no use of {ref} found');
end;

procedure TFHIRGraphDefinitionEngine.processLink(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
begin
  if link.path <> '' then
    processLinkPath(focusPath, focus, link, depth)
  else
    processLinkTarget(focusPath, focus, link, depth);
end;

procedure TFHIRGraphDefinitionEngine.processLinkPath(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
var
  path : String;
  node : TFHIRPathExpressionNode;
  matches : TFHIRSelectionList;
  sel : TFHIRSelection;
  tl : TFhirGraphDefinitionLinkTarget;
  res, tgtCtxt : TFhirResource;
  l : TFhirGraphDefinitionLink;
begin
  check(link.path <> '', 'Path is needed at '+path);
  check(link.sliceName = '', 'SliceName is not yet supported at '+path);

  path := focusPath+' -> '+link.path;
  if link.pathElement.Tag <> nil then
    node := link.pathElement.Tag.Link as TFHIRPathExpressionNode
  else
    node := FPathEngine.parse(link.path);
  try
    matches := FPathEngine.evaluate(nil, focus, focus, node);
    try
      check(not validating or (matches.Count >= StrToIntDef(link.min, 0)), 'Link at path '+path+' requires at least '+link.min+' matches, but only found '+inttostr(matches.Count));
      check(not validating or (matches.Count <= StrToIntDef(link.max, MAXINT)), 'Link at path '+path+' requires at most '+link.max+' matches, but found '+inttostr(matches.Count));
      for sel in matches do
      begin
        check(sel.value.fhirType = 'Reference', 'Selected node from an expression must be a Reference'); // todo: should a URL be ok?
        OnFollowReference(appInfo, focus, sel.value as TFhirReference, tgtCtxt, res);
        if (res <> nil) then
        begin
          try
            check(tgtCtxt <> focus, 'how to handle contained resources is not yet resolved'); // todo
            for tl in link.targetList do
            begin
              if CODES_TFhirResourceTypesEnum[tl.type_] = res.fhirType then
              begin
                if not isInBundle(res) then
                begin
                  addToBundle(res);
                  for l in definition.link_List do
                    processLink(Start.fhirType, res, l, depth+1);
                end;
              end;
            end;
          finally
            tgtCtxt.Free;
            res.Free;
          end;
        end;
      end;
    finally
      matches.Free;
    end;
  finally
    node.Free;
  end;
end;

procedure TFHIRGraphDefinitionEngine.processLinkTarget(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
var
  path : String;
  list : TAdvList<TFHIRResource>;
  params : TAdvList<TGraphQLArgument>;
  res : TFhirResource;
  l : TFhirGraphDefinitionLink;
begin
  check(link.targetList.Count = 1, 'If there is no path, there must be one and only one target at '+focusPath);
  check(link.targetList[0].type_ <> ResourceTypesNull, 'If there is no path, there must be type on the target at '+focusPath);
  check(link.targetList[0].params.Contains('{ref}'), 'If there is no path, the target must have parameters that include a parameter using {ref} at '+focusPath);
  path := focusPath+' -> '+CODES_TFhirResourceTypesEnum[link.targetList[0].type_]+'?'+link.targetList[0].params;

  list := TAdvList<TFHIRResource>.create;
  try
    params := TAdvList<TGraphQLArgument>.create;
    try
      parseParams(params, link.targetList[0].params, focus);
      FOnListResources(appInfo, CODES_TFhirResourceTypesEnum[link.targetList[0].type_], params, list);
    finally
      params.free;
    end;
    check(not validating or (list.Count >= StrToIntDef(link.min, 0)), 'Link at path '+path+' requires at least '+link.min+' matches, but only found '+inttostr(list.Count));
    check(not validating or (list.Count <= StrToIntDef(link.max, MAXINT)), 'Link at path '+path+' requires at most '+link.max+' matches, but found '+inttostr(list.Count));
    for res in list do
    begin
      if not isInBundle(res) then
      begin
        addToBundle(res);
        for l in definition.link_List do
          processLink(Start.fhirType, FStart, l, depth+1);
      end;
    end;
  finally
    list.Free;
  end;



end;

{ TFHIRGraphDefinitionParser }

destructor TFHIRGraphDefinitionParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

class function TFHIRGraphDefinitionParser.parse(source: String): TFhirGraphDefinition;
var
  this : TFHIRGraphDefinitionParser;
begin
  this := TFHIRGraphDefinitionParser.Create;
  try
    this.FLexer := TFHIRPathLexer.Create(source);
    result := this.readDefinition;
  finally
    this.Free;
  end;
end;

function TFHIRGraphDefinitionParser.readCompartmentRule(use: TFhirGraphCompartmentUseEnum): TFhirGraphDefinitionLinkTargetCompartment;
var
  i : integer;
  expr: TFHIRPathExpressionNode;
  fpp : TFHIRPathParser;
begin
  result := TFhirGraphDefinitionLinkTargetCompartment.Create;
  try
    result.use := use;
    i := StringArrayIndexOfSensitive(CODES_TFhirGraphCompartmentRuleEnum, FLexer.current);
    if i < 1 then
      raise FLexer.error('Unexpected token "'+FLexer.current+'" expecting a compartment rule');
    result.rule := TFhirGraphCompartmentRuleEnum(i);
    FLexer.next;
    i := StringArrayIndexOfSensitive(CODES_TFhirCompartmentTypeEnum, FLexer.current);
    if i < 1 then
      raise FLexer.error('Unexpected token "'+FLexer.current+'" expecting a compartment type');
    result.code := TFhirCompartmentTypeEnum(i);
    FLexer.next;
    if FLexer.takeToken('=') then
    begin
      fpp := TFHIRPathParser.Create;
      try
        expr := fpp.parse(FLexer);
        try
          result.expression := expr.ToString;
        finally
          expr.free;
        end;
      finally
        fpp.Free;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRGraphDefinitionParser.readDefinition: TFhirGraphDefinition;
begin
  result := TFhirGraphDefinition.Create;
  try
    if FLexer.takeToken('graph') then
      readHeader(result);
    result.start := readResourceType;
    result.profile := readProfile;
    readLinkList(result.link_List);
    if (not FLexer.done) then
      raise FLexer.error('Unexpected content');
    result.Link;
  finally
    result.Free;
  end;

end;

procedure TFHIRGraphDefinitionParser.readHeader(gd: TFhirGraphDefinition);
begin
  raise Exception.Create('Not done yet');
end;

procedure TFHIRGraphDefinitionParser.readLinkList(list: TFhirGraphDefinitionLinkList);
var
  first : boolean;
begin
  if FLexer.takeToken('{') then
  begin
    first := true;
    while first or FLexer.takeToken(',') do
    begin
      first := false;
      if (FLexer.takeToken('search')) then
        list.Add(readSearchLink)
      else
        list.Add(readPathLink);
    end;
    FLexer.token('}');
  end;
end;

function TFHIRGraphDefinitionParser.readPathLink: TFhirGraphDefinitionLink;
var
  fpp : TFHIRPathParser;
  first : boolean;
  tgt : TFhirGraphDefinitionLinkTarget;
  expr : TFHIRPathExpressionNode;
begin
  result := TFhirGraphDefinitionLink.Create;
  try
    fpp := TFHIRPathParser.Create;
    try
      expr := fpp.parse(FLexer);
      try
        result.path := expr.ToString;
      finally
        expr.free;
      end;
    finally
      fpp.Free;
    end;
    if (FLexer.takeToken('cardinality')) then
    begin
      result.min := FLexer.take;
      FLexer.token('.');
      FLexer.token('.');
      result.max := FLexer.take;
    end;
    if FLexer.isConstant then
      result.description := FLexer.readConstant('description');
    first := true;
    FLexer.token(':');
    repeat
      if first then
        first := false
      else
        FLexer.takeToken(';');
      tgt := result.targetList.Append;
      tgt.type_ := readResourceType;
      tgt.profile := readProfile;
      while FLexer.takeToken('where') do
        tgt.compartmentList.Add(readCompartmentRule(GraphCompartmentUseCondition));
      while FLexer.takeToken('require') do
        tgt.compartmentList.Add(readCompartmentRule(GraphCompartmentUseRequirement));
      readLinkList(tgt.link_List);
    until not FLexer.hasToken(';');
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRGraphDefinitionParser.readProfile: String;
begin
  if FLexer.takeToken('(') then
    result := FLexer.readTo(')', false)
  else
    result := '';
end;

function TFHIRGraphDefinitionParser.readResourceType: TFhirResourceTypesEnum;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceTypesEnum, FLexer.current);
  if i < 1 then
    raise FLexer.error('Unexpected token "'+FLexer.current+'" expecting a resource type');
  FLexer.next;
  result := TFhirResourceTypesEnum(i);
end;

function TFHIRGraphDefinitionParser.readSearchLink: TFhirGraphDefinitionLink;
var
  tgt : TFhirGraphDefinitionLinkTarget;
begin
  result := TFhirGraphDefinitionLink.Create;
  try
    tgt := result.targetList.Append;
    tgt.type_ := readResourceType;
    FLexer.token('?');
    tgt.params := FLexer.readToWS;
    while FLexer.takeToken('where') do
      tgt.compartmentList.Add(readCompartmentRule(GraphCompartmentUseCondition));
    if (FLexer.takeToken('cardinality')) then
    begin
      result.min := FLexer.take;
      FLexer.token('.');
      FLexer.token('.');
      result.max := FLexer.take;
    end;
    if FLexer.isConstant then
      result.description := FLexer.readConstant('description');
      while FLexer.takeToken('require') do
        tgt.compartmentList.Add(readCompartmentRule(GraphCompartmentUseRequirement));
    readLinkList(tgt.link_List);
    result.link;
  finally
    result.Free;
  end;
end;

class function TFHIRGraphDefinitionParser.asString(definition: TFhirGraphDefinition; header: boolean): String;
var
  this : TFHIRGraphDefinitionParser;
  b : TStringBuilder;
begin
  this := TFHIRGraphDefinitionParser.Create;
  try
    b := TStringBuilder.Create;
    try
      if header then
        this.writeHeader(b, definition);
      this.writeDefinition(b, definition);
      result := b.ToString;
    finally
      b.Free;
    end;
  finally
    this.Free;
  end;
end;

procedure TFHIRGraphDefinitionParser.writeCompartment(b: TStringBuilder; cr: TFhirGraphDefinitionLinkTargetCompartment);
begin
  if cr.use = GraphCompartmentUseCondition then
    b.Append('where ')
  else
    b.Append('require ');
  b.Append(CODES_TFhirGraphCompartmentRuleEnum[cr.rule]);
  b.Append(' ');
  b.Append(CODES_TFhirCompartmentTypeEnum[cr.code]);
  if cr.rule = GraphCompartmentRuleCustom then
  begin
    b.Append(' = ');
    b.Append(cr.expression)
  end;
end;

procedure TFHIRGraphDefinitionParser.writeDefinition(b: TStringBuilder; definition: TFhirGraphDefinition);
begin
  b.Append(CODES_TFhirResourceTypesEnum[definition.start]);
  if definition.profile <> '' then
  begin
    b.Append('(');
    b.Append(definition.profile);
    b.Append(')');
  end;
  writeLinklist(b, definition.link_List, 2);
end;

procedure TFHIRGraphDefinitionParser.writeHeader(b: TStringBuilder; definition: TFhirGraphDefinition);
begin

end;

procedure TFHIRGraphDefinitionParser.writeLinklist(b: TStringBuilder; list: TFhirGraphDefinitionLinkList; indent : integer);
var
  i : integer;
begin
  if list.Count > 0 then
  begin
    b.Append(' {');
    for i := 0 to list.Count - 1 do
    begin
      b.Append(#13#10);
      b.Append(StringPadLeft('', ' ', indent));
      if list[i].path <> '' then
        writePathItem(b, list[i], indent)
      else
        writeSearchItem(b, list[i], indent);
      if i < list.Count - 1 then
        b.Append(',')
    end;
    b.Append(#13#10);
    b.Append(StringPadLeft('', ' ', indent-2));
    b.Append('}');
  end;
end;

procedure TFHIRGraphDefinitionParser.writePathItem(b: TStringBuilder; item: TFhirGraphDefinitionLink; indent: integer);
var
  i : integer;
  cr : TFhirGraphDefinitionLinkTargetCompartment;
begin
  b.Append(item.path);
  if (item.min <> '') or (item.max <> '') then
  begin
    b.Append(' cardinality ');
    if item.min <> '' then
      b.Append(item.min)
    else
      b.Append('0');
    b.Append('..');
    if item.max <> '' then
      b.Append(item.max)
    else
      b.Append('*');
  end;
  if item.description <> '' then
  begin
    b.Append(' ''');
    b.Append(jsonEscape(item.description, true));
    b.Append('''');
  end;
  b.Append(' : ');

  for i := 0 to item.targetList.Count - 1 do
  begin
    if (item.targetList.count > 1) then
    begin
      b.Append(#13#10);
      b.Append(StringPadLeft('', ' ', indent+2));
    end;
    b.Append(CODES_TFhirResourceTypesEnum[item.targetList[i].type_]);
    if item.targetList[i].profile <> '' then
    begin
      b.Append('(');
      b.Append(item.targetList[i].profile);
      b.Append(')');
    end;
    for cr in item.targetList[i].compartmentList do
      if (cr.use = GraphCompartmentUseCondition) then
        writeCompartment(b, cr);
    for cr in item.targetList[i].compartmentList do
      if (cr.use = GraphCompartmentUseRequirement) then
        writeCompartment(b, cr);

    if (item.targetList.count = 1) then
      writeLinklist(b, item.targetList[i].link_List, indent+2)
    else
      writeLinklist(b, item.targetList[i].link_List, indent+4);
    if i < item.targetList.Count - 1 then
      b.Append(';')
  end;
end;

procedure TFHIRGraphDefinitionParser.writeSearchItem(b: TStringBuilder; item: TFhirGraphDefinitionLink; indent: integer);
begin
  b.Append('search ');
  b.Append(CODES_TFhirResourceTypesEnum[item.targetList[0].type_]);
  b.Append('?');
  b.Append(item.targetList[0].params);
  if (item.min <> '') or (item.max <> '') then
  begin
    b.Append(' cardinality ');
    if item.min <> '' then
      b.Append(item.min)
    else
      b.Append('0');
    b.Append('..');
    if item.max <> '' then
      b.Append(item.max)
    else
      b.Append('*');
  end;
  if item.description <> '' then
  begin
    b.Append(' ''');
    b.Append(jsonEscape(item.description, true));
    b.Append('''');
  end;
  writeLinklist(b, item.targetList[0].link_List, indent+2);
end;

end.
