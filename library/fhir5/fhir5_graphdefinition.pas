unit fhir5_graphdefinition;

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
  SysUtils,
  fsl_base, fsl_utilities,
  fsl_graphql, fsl_http,
  fhir_objects, fhir_graphdefinition, fhir_pathengine,  fhir_factory,
  fhir_graphql,
  fhir5_resources, fhir5_enums, fhir5_types, fhir5_pathengine, fhir5_pathnode, fhir5_context, fhir5_utilities;

type
  TFHIRGraphDefinitionParser5 = class (TFHIRGraphDefinitionParser)
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
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    function parseV(source : String) : TFhirResourceV; override;
    function asString(definition : TFhirResourceV; header : boolean) : String; override;
  end;

  TFHIRGraphDefinitionEngine4 = class (TFHIRGraphDefinitionEngine)
  private
    FContext : TFHIRWorkerContextWithFactory;
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
    FAppinfo: TFslObject;
    procedure SetBundle(const Value: TFHIRBundle);
    procedure SetDefinition(const Value: TFhirGraphDefinition);
    procedure SetStart(const Value: TFHIRResource);

    function check(test : boolean; msg : String) : boolean;
    function isInBundle(resource :  TFHIRResource) : boolean;
    procedure addToBundle(resource : TFHIRResource);
    procedure processLink(focusPath : String; focus : TFHIRResource; link : TFhirGraphDefinitionLink; depth : integer);
    procedure processLinkPath(focusPath: String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
    procedure processLinkTarget(focusPath: String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
    procedure parseParams(params : TFslList<TGraphQLArgument>; value : String; res : TFHIRResource);

    procedure SetAppInfo(const Value: TFslObject);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(context : TFHIRWorkerContextWithFactory); virtual;
    destructor Destroy; override;

    property appInfo : TFslObject read FAppinfo write SetAppInfo;
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

{ TFHIRGraphDefinitionParser }

destructor TFHIRGraphDefinitionParser5.Destroy;
begin
  FLexer.Free;
  inherited;
end;

function TFHIRGraphDefinitionParser5.parseV(source: String): TFhirResourceV;
begin
  FLexer := TFHIRPathLexer5.Create(fpV2, source);
  result := readDefinition;
end;

function TFHIRGraphDefinitionParser5.readCompartmentRule(use: TFhirGraphCompartmentUseEnum): TFhirGraphDefinitionLinkTargetCompartment;
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

function TFHIRGraphDefinitionParser5.readDefinition: TFhirGraphDefinition;
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

procedure TFHIRGraphDefinitionParser5.readHeader(gd: TFhirGraphDefinition);
begin
  raise EFHIRTodo.create('TFHIRGraphDefinitionParser5.readHeader');
end;

procedure TFHIRGraphDefinitionParser5.readLinkList(list: TFhirGraphDefinitionLinkList);
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

function TFHIRGraphDefinitionParser5.readPathLink: TFhirGraphDefinitionLink;
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

function TFHIRGraphDefinitionParser5.readProfile: String;
begin
  if FLexer.takeToken('(') then
    result := FLexer.readTo(')', false)
  else
    result := '';
end;

function TFHIRGraphDefinitionParser5.readResourceType: TFhirResourceTypesEnum;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceTypesEnum, FLexer.current);
  if i < 1 then
    raise FLexer.error('Unexpected token "'+FLexer.current+'" expecting a resource type');
  FLexer.next;
  result := TFhirResourceTypesEnum(i);
end;

function TFHIRGraphDefinitionParser5.readSearchLink: TFhirGraphDefinitionLink;
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

function TFHIRGraphDefinitionParser5.asString(definition: TFhirResourceV; header: boolean): String;
var
  t : TFhirGraphDefinition;
  b : TStringBuilder;
begin
  t := definition as TFhirGraphDefinition;
  b := TStringBuilder.Create;
  try
    if header then
      writeHeader(b, t);
    writeDefinition(b, t);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure TFHIRGraphDefinitionParser5.writeCompartment(b: TStringBuilder; cr: TFhirGraphDefinitionLinkTargetCompartment);
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

procedure TFHIRGraphDefinitionParser5.writeDefinition(b: TStringBuilder; definition: TFhirGraphDefinition);
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

procedure TFHIRGraphDefinitionParser5.writeHeader(b: TStringBuilder; definition: TFhirGraphDefinition);
begin

end;

procedure TFHIRGraphDefinitionParser5.writeLinklist(b: TStringBuilder; list: TFhirGraphDefinitionLinkList; indent : integer);
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

procedure TFHIRGraphDefinitionParser5.writePathItem(b: TStringBuilder; item: TFhirGraphDefinitionLink; indent: integer);
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

procedure TFHIRGraphDefinitionParser5.writeSearchItem(b: TStringBuilder; item: TFhirGraphDefinitionLink; indent: integer);
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


function TFHIRGraphDefinitionParser5.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLexer.sizeInBytes);
end;

{ TFHIRGraphDefinitionEngine4 }

constructor TFHIRGraphDefinitionEngine4.Create(context : TFHIRWorkerContextWithFactory);
begin
  inherited Create;
  FContext := context;
  FPathEngine := TFHIRPathEngine.Create((context as TFHIRWorkerContext).link, nil);
end;

destructor TFHIRGraphDefinitionEngine4.Destroy;
begin
  FAppinfo.Free;
  FBundle.Free;
  FDefinition.Free;
  FStart.Free;
  FPathEngine.Free;
  FContext.Free;
  inherited;
end;

procedure TFHIRGraphDefinitionEngine4.SetAppInfo(const Value: TFslObject);
begin
  FAppinfo.Free;
  FAppinfo := Value;
end;

procedure TFHIRGraphDefinitionEngine4.SetBundle(const Value: TFHIRBundle);
begin
  FBundle.Free;
  FBundle := Value;
end;

procedure TFHIRGraphDefinitionEngine4.SetDefinition(const Value: TFhirGraphDefinition);
begin
  FDefinition.Free;
  FDefinition := Value;
end;

procedure TFHIRGraphDefinitionEngine4.SetStart(const Value: TFHIRResource);
begin
  FStart.Free;
  FStart := Value;
end;

procedure TFHIRGraphDefinitionEngine4.execute;
var
  l : TFhirGraphDefinitionLink;
begin
  check(assigned(OnFollowReference), 'Reference Resolution services not available');
  check(assigned(FOnListResources), 'Resource search services not available');

  check(Start <> nil, 'A focus resource is needed');
  check(Bundle <> nil, 'An output bundle is needed');
  check(definition <> nil, 'a definition is needed');
  check(FBaseUrl <> '', 'a baseURL is needed');
  FDefinition.checkNoModifiers('FHIR.Server.GraphDefinition.execute', 'definition');

  check(Start.fhirType = CODES_TFhirResourceTypesEnum[definition.start], 'Definition requires that Start is '+CODES_TFhirResourceTypesEnum[definition.start]+', but found '+Start.fhirType);
  if not isInBundle(start) then
    addToBundle(start);
  for l in definition.link_List do
    processLink(Start.fhirType, FStart, l, 1);
end;

function TFHIRGraphDefinitionEngine4.isInBundle(resource: TFHIRResource): boolean;
var
  be : TFhirBundleEntry;
begin
  result := false;
  for be in FBundle.entryList do
    if (be.resource <> nil) and (be.resource.fhirType = resource.fhirType) and (be.resource.id = resource.id) then
      exit(true);
end;

procedure TFHIRGraphDefinitionEngine4.addToBundle(resource: TFHIRResource);
var
  be : TFhirBundleEntry;
begin
  be := FBundle.entryList.Append;
  be.fullUrl := URLPath([FBaseUrl, resource.fhirType, resource.id]);
  be.resource := resource.Link;
end;

function TFHIRGraphDefinitionEngine4.check(test: boolean; msg: String): boolean;
begin
  if not test then
    raise EFHIRException.Create(msg);
  result := test;
end;

procedure TFHIRGraphDefinitionEngine4.parseParams(params: TFslList<TGraphQLArgument>; value: String; res: TFHIRResource);
var
  p : THTTPParameters;
  i, j : integer;
  n, v : String;
  refed : boolean;
begin
  refed := false;
  p := THTTPParameters.create(value, false);
  try
    for i := 0 to p.Count -1 do
    begin
      n := p.Name[i];
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

procedure TFHIRGraphDefinitionEngine4.processLink(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
begin
  if link.path <> '' then
    processLinkPath(focusPath, focus, link, depth)
  else
    processLinkTarget(focusPath, focus, link, depth);
end;

procedure TFHIRGraphDefinitionEngine4.processLinkPath(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
var
  path : String;
  node : TFHIRPathExpressionNode;
  matches : TFHIRSelectionList;
  sel : TFHIRSelection;
  tl : TFhirGraphDefinitionLinkTarget;
  res : TFhirResourceV;
  tgtCtxt : TFHIRResourceV;
  l : TFhirGraphDefinitionLink;
begin
  path := focusPath+' -> '+link.path;
  check(link.path <> '', 'Path is needed at '+focusPath);
  check(link.sliceName = '', 'SliceName is not yet supported at '+focusPath);

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
                if not isInBundle(res as TFhirResource) then
                begin
                  addToBundle(res as TFhirResource);
                  for l in definition.link_List do
                    processLink(Start.fhirType, res as TFhirResource, l, depth+1);
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

procedure TFHIRGraphDefinitionEngine4.processLinkTarget(focusPath : String; focus: TFHIRResource; link: TFhirGraphDefinitionLink; depth: integer);
var
  path : String;
  list : TFslList<TFHIRResourceV>;
  params : TFslList<TGraphQLArgument>;
  res : TFhirResourceV;
  l : TFhirGraphDefinitionLink;
begin
  check(link.targetList.Count = 1, 'If there is no path, there must be one and only one target at '+focusPath);
  check(link.targetList[0].type_ <> ResourceTypesNull, 'If there is no path, there must be type on the target at '+focusPath);
  check(link.targetList[0].params.Contains('{ref}'), 'If there is no path, the target must have parameters that include a parameter using {ref} at '+focusPath);
  path := focusPath+' -> '+CODES_TFhirResourceTypesEnum[link.targetList[0].type_]+'?'+link.targetList[0].params;

  list := TFslList<TFHIRResourceV>.create;
  try
    params := TFslList<TGraphQLArgument>.create;
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
      if not isInBundle(res as TFhirResource) then
      begin
        addToBundle(res as TFhirResource);
        for l in definition.link_List do
          processLink(Start.fhirType, FStart, l, depth+1);
      end;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRGraphDefinitionEngine4.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContext.sizeInBytes);
  inc(result, FPathEngine.sizeInBytes);
  inc(result, FBundle.sizeInBytes);
  inc(result, FStart.sizeInBytes);
  inc(result, FDefinition.sizeInBytes);
  inc(result, (FBaseUrl.length * sizeof(char)) + 12);
  inc(result, FAppinfo.sizeInBytes);
end;

end.
