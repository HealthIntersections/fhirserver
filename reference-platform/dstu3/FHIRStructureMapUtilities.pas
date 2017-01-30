unit FHIRStructureMapUtilities;

interface

uses
  SysUtils, Generics.Collections,
  StringSupport, DecimalSupport, GUIDSupport, TextUtilities,
  AdvObjects, AdvGenerics,
  FHIRBase, FHIRTypes, FHIRResources, FHIRContext, FHIRPath, FHIRXhtml;

type
  TVariableMode = (vmINPUT, vmOUTPUT);

const
  CODES_TVariableMode : array [TVariableMode] of string = ('input', 'output');

type
  TVariable = class (TAdvObject)
  private
    Fname: String;
    Fmode: TVariableMode;
    Fobj: TFHIRBase;
  public
    Destructor Destroy; override;

    function link : TVariable; overload;
    function copy : TVariable;

    property mode : TVariableMode read Fmode write Fmode;
    property name : String read Fname write Fname;
    property obj : TFHIRBase read Fobj write Fobj;
  end;

  TVariables = class (TAdvObject)
  private
		list : TAdvList<TVariable>;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Link : TVariables; overload;
		procedure add(mode : TVariableMode; name : String; obj : TFHIRBase);
		function copy : TVariables;
		function get(mode : TVariableMode; name : String) : TFHIRBase;
  end;

  TTransformerServices = class abstract (TAdvObject)
  private
  public
    function oid2Uri(oid : String) : String; virtual; abstract;
    function translate(appInfo : TAdvObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; virtual; abstract;
    procedure log(s : String); virtual; abstract;
  end;

  TFHIRStructureMapUtilities = class (TAdvObject)
  private
	  FWorker : TWorkerContext;
   	fpe : TFHIRExpressionEngine;
  	FLib : TAdvMap<TFHIRStructureMap>;
  	FServices : TTransformerServices;
    procedure renderContained(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderUses(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderImports(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderGroup(b : TStringBuilder; g : TFHIRStructureMapGroup);
    procedure renderDoco(b : TStringBuilder; doco : String);
    procedure RenderRule(b : TStringBuilder; r : TFHIRStructureMapGroupRule; indent : integer);
    procedure RenderSource(b : TStringBuilder; rs : TFHIRStructureMapGroupRuleSource);
    procedure renderTarget(b : TStringBuilder; rt : TFHIRStructureMapGroupRuleTarget);
    procedure renderTransformParam(b : TStringBuilder; rtp : TFHIRStructureMapGroupRuleTargetParameter);
    procedure renderConceptMap(b : TStringBuilder; map : TFHIRConceptMap);

    function getGroup(map : TFHIRConceptMap; source, target : String) : TFHIRConceptMapGroup;
    function fromEnum(s : String; codes : Array of String) : integer;
    function readPrefix(prefixes : TDictionary<String, String>; lexer : TFHIRPathLexer) : String;
    function readEquivalence(lexer : TFHIRPathLexer) : TFhirConceptMapEquivalenceEnum;
    function readConstant(s : String; lexer : TFHIRPathLexer) : TFHIRType;
    procedure parseConceptMap(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseUses(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseImports(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseGroup(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseInput(group : TFHIRStructureMapGroup; lexer : TFHIRPathLexer);
    procedure parseRule(list : TFhirStructureMapGroupRuleList; lexer : TFHIRPathLexer);
    procedure parseSource(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseTarget(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseRuleReference(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseParameter(target : TFHIRStructureMapGroupRuleTarget; lexer : TFHIRPathLexer);

    procedure log(s : String);
    procedure getChildrenByName(item : TFHIRBase; name : String; result : TAdvList<TFHIRBase>);
    function runTransform(appInfo : TAdvObject; map : TFHIRStructureMap; tgt : TFHIRStructureMapGroupRuleTarget; vars : TVariables) : TFHIRBase;
    procedure executeGroup(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup);
    procedure executeRule(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule);
    function analyseSource(appInfo : TAdvObject; vars : TVariables; src : TFHIRStructureMapGroupRuleSource) : TAdvList<TVariables>;
    procedure processTarget(appInfo : TAdvObject; vars : TVariables; map : TFHIRStructureMap; tgt : TFHIRStructureMapGroupRuleTarget);
    procedure executeDependency(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vin : TVariables; group : TFHIRStructureMapGroup; dependent : TFHIRStructureMapGroupRuleDependent);
    function getParamString(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : String;
    function getParam(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : TFHIRBase;
    function translate(appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameterList) : TFHIRBase; overload;
    function translate(appInfo : TAdvObject; map : TFHIRStructureMap; source : TFHIRBase; conceptMapUrl : String; fieldToReturn : String): TFHIRBase; overload;
  public
    constructor Create(context : TWorkerContext; lib : TAdvMap<TFHIRStructureMap>; services : TTransformerServices);
    destructor Destroy; override;

  	property Lib : TAdvMap<TFHIRStructureMap> read FLib;

    function render(map : TFHIRStructureMap) : String; overload;
    function render(map : TFHIRConceptMap) : String; overload;
	  function parse(text : String) : TFHIRStructureMap;
    procedure transform(appInfo : TAdvObject; source : TFHIRBase; map : TFHIRStructureMap; target : TFHIRBase);
  end;


implementation

{ TFHIRStructureMapUtilities }

constructor TFHIRStructureMapUtilities.Create(context: TWorkerContext;
  lib: TAdvMap<TFHIRStructureMap>; services: TTransformerServices);
begin
  inherited Create;
	FWorker := context;
  FLib := lib;
  FServices := services;
  fpe := TFHIRExpressionEngine.Create(context.link);
end;

destructor TFHIRStructureMapUtilities.destroy;
begin
  FWorker.Free;
  fpe.Free;
	FLib.Free;
	FServices.Free;
  inherited;
end;

function TFHIRStructureMapUtilities.render(map : TFHIRStructureMap) : String;
var
  b : TStringBuilder;
  g : TFhirStructureMapGroup;
begin
  b := TStringBuilder.create();
  try
		b.append('map "');
		b.append(map.Url);
		b.append('" = "');
		b.append(jsonEscape(map.Name, true));
		b.append('"'#13#10#13#10);

    renderContained(b, map);
		renderUses(b, map);
		renderImports(b, map);
		for g in map.groupList do
			renderGroup(b, g);
		result := b.toString();
  finally
    b.free;
	end;
end;

procedure TFHIRStructureMapUtilities.renderUses(b : TStringBuilder; map : TFHIRStructureMap);
var
  s : TFHIRStructureMapStructure;
begin
  for s in map.structureList do
  begin
    b.append('uses "');
    b.append(s.url);
    b.append('" as ');
    b.append(CODES_TFhirMapModelModeEnum[s.Mode]);
    b.append(#13#10);
    renderDoco(b, s.Documentation);
  end;
  if (map.structureList.Count > 0) then
    b.append(#13#10);
end;

procedure TFHIRStructureMapUtilities.renderImports(b : TStringBuilder; map : TFHIRStructureMap);
var
  s : TFHIRUri;
begin
  for s in map.importList do
  begin
    b.append('imports "');
    b.append(s.Value);
    b.append('"'#13#10);
  end;
  if (map.importList.Count > 0) then
    b.append(#13#10);
end;

procedure TFHIRStructureMapUtilities.renderGroup(b : TStringBuilder; g : TFHIRStructureMapGroup);
var
  gi : TFHIRStructureMapGroupInput;
  r : TFHIRStructureMapGroupRule;
begin
		b.append('group ');
		b.append(g.Name);
		if (g.extends <> '') then
    begin
			b.append(' extends ');
			b.append(g.Extends);
		end;
		if (g.Documentation <> '') then
			renderDoco(b, g.Documentation);
		b.append(#13#10);
		for gi in g.inputList do
    begin
			b.append('  input ');
			b.append(gi.Name);
			if (gi.type_ <> '') then
      begin
				b.append(' : ');
				b.append(gi.Type_);
			end;
			b.append(' as ');
			b.append(CODES_TFhirMapInputModeEnum[gi.Mode]);
			b.append(';'#13#10);
		end;
		if (g.inputList.Count > 0) then
			b.append(#13#10);
		for r in g.ruleList do
			renderRule(b, r, 2);
		b.append(#13#10'endgroup'#13#10);
	end;

procedure TFHIRStructureMapUtilities.RenderRule(b : TStringBuilder; r : TFHIRStructureMapGroupRule; indent : integer);
var
  first, ifirst : boolean;
  rs : TFHIRStructureMapGroupRuleSource;
  rt : TFHIRStructureMapGroupRuleTarget;
  rd : TFHIRStructureMapGroupRuleDependent;
  rdp : TFHIRString;
begin
  b.append(StringPadLeft('', ' ', indent));
  b.append(r.Name);
  b.append(': for ');
  first := true;
  for rs in r.sourceList do
  begin
    if (first) then
      first := false
    else
      b.append(', ');
    renderSource(b, rs);
  end;
  if (r.targetList.count > 1) then
  begin
    b.append(' make ');
    first := true;
    for rt in r.targetList do
    begin
      if (first) then
        first := false
      else
        b.append(', ');
      b.append(#13#10);
      b.append(StringPadLeft('', ' ', indent+4));
      renderTarget(b, rt);
    end;
  end
  else if (r.targetList.count > 0) then
  begin
    b.append(' make ');
    renderTarget(b, r.TargetList[0]);
  end;
  if (r.ruleList.Count > 0) then
  begin
    b.append(' then {');
    renderDoco(b, r.Documentation);
    b.append(StringPadLeft('', ' ', indent));
    b.append('}'#13#10);
  end
  else
  begin
    if (r.dependentList.Count > 0) then
    begin
      first := true;
      for rd in r.dependentList do
      begin
        if (first) then
          first := false
        else
          b.append(', ');
        b.append(rd.Name);
        b.append('(');
        ifirst := true;
        for rdp in rd.variableList do
        begin
          if (ifirst) then
            ifirst := false
          else
            b.append(', ');
          b.append(rdp.value);
        end;
      end;
    end;
    renderDoco(b, r.Documentation);
    b.append(#13#10);
  end;
end;

procedure TFHIRStructureMapUtilities.RenderSource(b : TStringBuilder; rs : TFHIRStructureMapGroupRuleSource);
begin
  if (not rs.Required) then
    b.append('optional ');
  b.append(rs.Context);
  if (rs.Element <> '') then
  begin
    b.append('.');
    b.append(rs.Element);
  end;
  if (rs.ListMode <> MapListModeNull) then
  begin
    b.append(' ');
    if (rs.ListMode = MapListModeShare) then
      b.append('only_one')
    else
      b.append(CODES_TFhirMapListModeEnum[rs.ListMode]);
  end;
  if (rs.Variable <> '') then
  begin
    b.append(' as ');
    b.append(rs.Variable);
  end;
  if (rs.Condition <> '') then
  begin
    b.append(' where ');
    b.append(rs.Condition);
  end;
  if (rs.Check <> '')  then
  begin
    b.append(' check ');
    b.append(rs.Check);
  end;
end;

procedure TFHIRStructureMapUtilities.renderTarget(b : TStringBuilder; rt : TFHIRStructureMapGroupRuleTarget);
var
  first : boolean;
  rtp : TFHIRStructureMapGroupRuleTargetParameter;
  lm : TFhirMapListModeEnum;
begin
  b.append(rt.Context);
  if (rt.Element <> '')  then
  begin
    b.append('.');
    b.append(rt.Element);
  end;
  if (rt.Transform <> MapTransformNull) then
  begin
    b.append(' = ');
    if (rt.Transform = MapTransformCopy) and (rt.parameterList.count = 1) then
    begin
      renderTransformParam(b, rt.ParameterList[0]);
    end
    else if (rt.Transform = MapTransformEVALUATE) and (rt.ParameterList.count = 2) then
    begin
      b.append(CODES_TFhirMapTransformEnum[rt.Transform]);
      b.append('(');
      b.append(TFHIRId(rt.ParameterList[0].Value).StringValue);
      b.append(TFHIRString(rt.ParameterList[1].Value).StringValue);
      b.append(')');
    end
    else
    begin
      b.append(CODES_TFhirMapTransformEnum[rt.Transform]);
      b.append('(');
      first := true;
      for rtp in rt.ParameterList do
      begin
        if (first) then
          first := false
        else
          b.append(', ');
        renderTransformParam(b, rtp);
      end;
      b.append(')');
    end;
  end;
  if (rt.Variable <> '') then
  begin
    b.append(' as ');
    b.append(rt.Variable);
  end;
  for lm := low(TFhirMapListModeEnum) to high(TFhirMapListModeEnum) do
    if lm in rt.listMode then
    begin
      b.append(' ');
      b.append(CODES_TFhirMapListModeEnum[lm]);
      if (lm = MapListModeShare) then
      begin
        b.append(' ');
        b.append(rt.ListRuleId);
      end;
    end;
end;

procedure TFHIRStructureMapUtilities.renderTransformParam(b : TStringBuilder; rtp : TFHIRStructureMapGroupRuleTargetParameter);
begin
  if (rtp.Value is TFHIRBoolean) then
    b.append((rtp.Value as TFHIRBoolean).StringValue)
  else if (rtp.Value is TFHIRDecimal) then
    b.append((rtp.Value as TFHIRDecimal).StringValue)
  else if (rtp.Value is TFHIRId) then
    b.append((rtp.Value as TFHIRId).StringValue)
  else if (rtp.Value is TFHIRDecimal) then
    b.append((rtp.Value as TFHIRDecimal).StringValue)
  else if (rtp.Value is TFHIRInteger) then
    b.append((rtp.Value as TFHIRInteger).StringValue)
  else
  begin
    b.append('"');
    b.append(jsonEscape((rtp.Value as TFHIRString).StringValue, true));
    b.append('"');
  end;
end;

function TFHIRStructureMapUtilities.render(map: TFHIRConceptMap): String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.create();
  try
    renderConceptMap(b, map);
		result := b.toString();
  finally
    b.free;
	end;
end;

procedure TFHIRStructureMapUtilities.renderConceptMap(b: TStringBuilder; map: TFHIRConceptMap);
const
  CHARS_EQUIVALENCE : array [TFhirConceptMapEquivalenceEnum] of string = ('??', ':', '==', '=', '<-', '<=', '>-', '>=', '~', '||', '--');
var
  prefixes : TDictionary<String, String>;
  g : TFhirConceptMapGroup;
  e : TFhirConceptMapGroupElement;
  t : TFhirConceptMapGroupElementTarget;
  d : TFhirConceptMapGroupElementTargetDependsOn;
  s : String;
  f : boolean;
  procedure seeSystem(base, url : String);
  var
    i : integer;
  begin
    if not prefixes.ContainsKey(url) then
    begin
      if not prefixes.ContainsValue(base) then
        prefixes.Add(url, base)
      else
      begin
        i := 1;
        while prefixes.ContainsValue(base+inttostr(i)) do
          inc(i);
        prefixes.Add(url, base+inttostr(i));
      end;
    end;
  end;
  procedure app(system, code : String);
  begin
    b.append(prefixes[system]);
    b.append(':');
    if code.Contains(' ') then
      b.append('"'+jsonEscape(code, true)+'"')
    else
      b.append(jsonEscape(code, false));
  end;
begin
  prefixes := TDictionary<String, String>.create;
  try
    b.append('conceptmap "');
    b.append(map.Url);
    b.append('" = "');
    b.append(jsonEscape(map.Name, true));
    b.append('" {'#13#10#13#10);

    if (map.source is TFhirUri) then
    begin
      b.Append('source "');
      b.append(jsonEscape(TFhirUri(map.source).value, true));
      b.append('"'#13#10);
    end;
    if (map.target is TFhirUri) then
    begin
      b.Append('target "');
      b.append(jsonEscape(TFhirUri(map.target).value, true));
      b.append('"'#13#10);
    end;

    // first pass: determine prefixes:
    for g in map.groupList do
      for e in g.elementList do
      begin
        seeSystem('s', g.source);
        for t in e.targetList do
        begin
          seeSystem('t', g.target);
          for d in t.dependsOnList do
           seeSystem('d', d.system);
          for d in t.productList do
           seeSystem('p', d.system);
        end;
      end;
    for s in prefixes.Keys do
    begin
      b.Append('prefix "');
      b.append(prefixes[s]);
      b.Append(' = "');
      b.append(jsonEscape(s, true));
      b.append('"'#13#10);
    end;
    b.append(#13#10);

    // now render
    for g in map.groupList do
      for e in g.elementList do
      begin
        for t in e.targetList do
        begin
          app(g.source, e.code);
          b.Append(' ');
          if (t.dependsOnList.Count > 0) then
          begin
            b.Append('[');
            f := true;
            for d in t.dependsOnList do
            begin
              if f then f := false else b.Append(', ');
              app(d.system, d.code);
            end;
            b.Append(']');
          end;
          b.Append(CHARS_EQUIVALENCE[t.equivalence]);
          b.Append(' ');
          if (t.code <> '') then
          begin
            app(g.target, t.code);
            if (t.productList.Count > 0) then
            begin
              b.Append('[');
              f := true;
              for d in t.productList do
              begin
                if f then f := false else b.Append(', ');
                app(d.system, d.code);
              end;
              b.Append(']');
            end;
          end;
          b.Append(#13#10);
        end;
      end;
    b.append('}'#13#10);
  finally
    prefixes.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.renderContained(b: TStringBuilder; map: TFHIRStructureMap);
var
  r : TFHIRResource;
begin
  for r in map.containedList do
    if r is TFhirConceptMap then
      renderConceptMap(b, r as TFhirConceptMap);
end;

procedure TFHIRStructureMapUtilities.renderDoco(b : TStringBuilder; doco : String);
begin
  if (doco <> '') then
  begin
    b.append(' // ');
    b.append(doco.replace(#13#10, ' ').replace(#13, ' ').replace(#10, ' '));
  end;
end;

function TFHIRStructureMapUtilities.parse(text : String) : TFHIRStructureMap;
var
  lexer : TFHIRPathLexer;
begin
  lexer := TFHIRPathLexer.Create(text);
  try
		if (lexer.done()) then
			raise Exception.create('Map Input cannot be empty');
		lexer.skipComments();
		lexer.token('map');
		result := TFHIRStructureMap.Create;
    try
      result.Url := lexer.readConstant('url');
      result.id := result.url.Substring(result.url.LastIndexOf('/')+1);
      lexer.token('=');
      result.Name := lexer.readConstant('name');
      lexer.skipComments();
      result.status := PublicationStatusDraft;

      while (lexer.hasToken('conceptmap')) do
        parseConceptMap(result, lexer);

      while (lexer.hasToken('uses')) do
        parseUses(result, lexer);
      while (lexer.hasToken('imports')) do
        parseImports(result, lexer);

      parseGroup(result, lexer);

      while (not lexer.done()) do
        parseGroup(result, lexer);

      result.text := TFhirNarrative.Create;
      result.text.status := NarrativeStatusGenerated;
      result.text.div_ := TFHIRXhtmlParser.parse('en', xppReject, [], '<div><pre>'+FormatTextToXML(text)+'</pre></div>');
      result.link;
    finally
      result.free;
    end;
  finally
    lexer.Free;
  end;
end;


procedure TFHIRStructureMapUtilities.parseConceptMap(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
var
  map : TFhirConceptMap;
  id, n, v : String;
  prefixes : TDictionary<String, String>;
  e : TFhirConceptMapGroupElement;
  tgt : TFhirConceptMapGroupElementTarget;
  vs, vc, vt : String;
  eq : TFhirConceptMapEquivalenceEnum;
begin
  tgt := nil;
  lexer.token('conceptmap');
  map := TFhirConceptMap.create;
  result.ContainedList.add(map);
  id := lexer.readConstant('map id');
  if (not id.startsWith('#')) then
    raise lexer.error('Concept Map identifier must start with #');
  map.Id := id.substring(1);
  lexer.token('{');
  lexer.skipComments();
  //	  lexer.token('source');
  //	  map.Source := new UriType(lexer.readConstant('source')));
  //	  lexer.token('target');
  //	  map.Source := new UriType(lexer.readConstant('target')));
  prefixes := TDictionary<String, String>.create;
  try
    while (lexer.hasToken('prefix')) do
    begin
      lexer.token('prefix');
      n := lexer.take();
      lexer.token('=');
      v := lexer.readConstant('prefix url');
      prefixes.add(n, v);
    end;
    while (not lexer.hasToken('}')) do
    begin
      vs := readPrefix(prefixes, lexer);
      lexer.token(':');
      vc := lexer.take();
      eq := readEquivalence(lexer);
      if (tgt.Equivalence <> ConceptMapEquivalenceUNMATCHED) then
        vt := readPrefix(prefixes, lexer)
      else
        vt := '';

      e := getGroup(map, vs, vt).elementList.Append;
      e.Code := vc;
      tgt := e.targetList.Append;
      tgt.Equivalence := eq;
      if (tgt.Equivalence <> ConceptMapEquivalenceUNMATCHED) then
      begin
        lexer.token(':');
        tgt.Code := lexer.take();
      end;
      if (lexer.hasComment())  then
        tgt.Comments := lexer.take().substring(2).trim();
    end;
    lexer.token('}');
  finally
    prefixes.Free;
  end;
end;


function TFHIRStructureMapUtilities.readPrefix(prefixes : TDictionary<String, String>; lexer : TFHIRPathLexer) : String;
var
  prefix : String;
begin
  prefix := lexer.take();
  if (not prefixes.containsKey(prefix)) then
    raise Exception.create('Unknown prefix "'+prefix+'"');
  result := prefixes[prefix];
end;

function TFHIRStructureMapUtilities.readEquivalence(lexer : TFHIRPathLexer) : TFHIRConceptMapEquivalenceEnum;
var
  token : string;
begin
  token := lexer.take();
  if (token = '=') then
    result := ConceptMapEquivalenceEQUAL
  else if (token = '==') then
    result := ConceptMapEquivalenceEQUIVALENT
  else if (token = '!=') then
    result := ConceptMapEquivalenceDISJOINT
  else if (token = '--') then
    result := ConceptMapEquivalenceUNMATCHED
  else if (token = '<=') then
    result := ConceptMapEquivalenceWIDER
  else if (token = '<-') then
    result := ConceptMapEquivalenceSUBSUMES
  else if (token = '>=') then
    result := ConceptMapEquivalenceNARROWER
  else if (token = '>-') then
    result := ConceptMapEquivalenceSPECIALIZES
  else if (token = '~') then
    result := ConceptMapEquivalenceINEXACT
  else
    raise Exception.create('Unknown equivalence token "'+token+'"');
end;

function TFHIRStructureMapUtilities.fromEnum(s : String; codes : Array of String) : integer;
begin
  result := StringArrayIndexOfSensitive(codes, s);
  if result = -1 then
    raise Exception.Create('the code "'+s+'" is not known');
end;


procedure TFHIRStructureMapUtilities.parseUses(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
var
  st : TFHIRStructureMapStructure;
begin
  lexer.token('uses');
  st := result.StructureList.Append;
  st.Url := lexer.readConstant('url');
  lexer.token('as');
  st.Mode := TFhirMapModelModeEnum(fromEnum(lexer.take(), CODES_TFhirMapModelModeEnum));
  lexer.skiptoken(';');
  if (lexer.hasComment()) then
    st.Documentation := lexer.take().substring(2).trim();
  lexer.skipComments();
end;

procedure TFHIRStructureMapUtilities.parseImports(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
begin
  lexer.token('imports');
  result.ImportList.add(TFHIRUri.Create(lexer.readConstant('url')));
  lexer.skiptoken(';');
  if (lexer.hasComment()) then
    lexer.next();
  lexer.skipComments();
end;

procedure TFHIRStructureMapUtilities.parseGroup(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
var
  group : TFHIRStructureMapGroup;
begin
  lexer.token('group');
  group := result.GroupList.Append;
  group.Name := lexer.take();
  if (lexer.hasToken('extends')) then
  begin
    lexer.next();
    group.Extends := lexer.take();
  end;
  lexer.skipComments();
  while lexer.hasToken('input') do
    parseInput(group, lexer);
  while (not lexer.hasToken('endgroup')) do
  begin
    if (lexer.done()) then
      raise Exception.create('premature termination expecting "endgroup"');
    parseRule(group.ruleList, lexer);
  end;
  lexer.token('endgroup');
  lexer.skipComments();
end;

procedure TFHIRStructureMapUtilities.parseInput(group : TFHIRStructureMapGroup; lexer : TFHIRPathLexer);
var
  input : TFHIRStructureMapGroupInput;
begin
  lexer.token('input');
  input := group.inputList.Append;
  input.Name := lexer.take();
  if (lexer.hasToken(':')) then
  begin
    lexer.token(':');
    input.Type_ := lexer.take();
  end;
  lexer.token('as');
  input.Mode := TFhirMapInputModeEnum(fromEnum(lexer.take(), CODES_TFhirMapInputModeEnum));
  if (lexer.hasComment()) then
    input.Documentation := lexer.take().substring(2).trim();
  lexer.skipToken(';');
  lexer.skipComments();
end;

procedure TFHIRStructureMapUtilities.parseRule(list : TFhirStructureMapGroupRuleList; lexer : TFHIRPathLexer);
var
  rule : TFhirStructureMapGroupRule;
  done : boolean;
begin
  rule := list.Append;
  rule.Name := lexer.takeDottedToken();
  lexer.token(':');
  lexer.token('for');
  done := false;
  while (not done) do
  begin
    parseSource(rule, lexer);
    done := not lexer.hasToken(',');
    if (not done) then
      lexer.next();
  end;
  if (lexer.hasToken('make')) then
  begin
    lexer.token('make');
    done := false;
    while (not done) do
    begin
      parseTarget(rule, lexer);
      done := not lexer.hasToken(',');
      if (not done) then
        lexer.next();
    end;
  end;
  if (lexer.hasToken('then')) then
  begin
    lexer.token('then');
    if (lexer.hasToken('{')) then
    begin
      lexer.token('{');
      if (lexer.hasComment()) then
        rule.Documentation := lexer.take().substring(2).trim();
      lexer.skipComments();
      while (not lexer.hasToken('}')) do
      begin
        if (lexer.done()) then
          raise Exception.create('premature termination expecting "}" in nested group');
        parseRule(rule.ruleList, lexer);
      end;
      lexer.token('}');
    end
    else
    begin
      done := false;
      while (not done) do
      begin
        parseRuleReference(rule, lexer);
        done := not lexer.hasToken(',');
        if (not done) then
          lexer.next();
      end;
    end;
  end
  else if (lexer.hasComment()) then
    rule.Documentation := lexer.take().substring(2).trim();
  lexer.skipComments();
end;

procedure TFHIRStructureMapUtilities.parseRuleReference(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
var
  ref : TFHIRStructureMapGroupRuleDependent;
  done : boolean;
begin
  ref := rule.dependentList.Append;
  ref.Name := lexer.take();
  lexer.token('(');
  done := false;
  while (not done) do
  begin
    ref.variableList.add(TFhirString.Create(lexer.take()));
    done := not lexer.hasToken(',');
    if (not done) then
      lexer.next();
  end;
  lexer.token(')');
end;

procedure TFHIRStructureMapUtilities.parseSource(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
var
  source : TFHIRStructureMapGroupRuleSource;
  node : TFHIRExpressionNode;
begin
  source := rule.sourceList.Append;
  if (lexer.hasToken('optional')) then
    lexer.next()
  else
    source.Required := true;
  source.Context := lexer.take();
  source.contextType := MapContextTypeVariable;
  if (lexer.hasToken('.')) then
  begin
    lexer.token('.');
    source.Element := lexer.take();
  end;
  if StringArrayExistsInsensitive(['first', 'last', 'only_one'], lexer.current) then
    if (lexer.current = 'only_one') then
    begin
      source.ListMode := MapListModeShare;
      lexer.take();
    end
    else
      source.ListMode := TFhirMapListModeEnum(fromEnum(lexer.take(), CODES_TFhirMapListModeEnum));
  if (lexer.hasToken('as')) then
  begin
    lexer.take();
    source.Variable := lexer.take();
  end;
  if (lexer.hasToken('where')) then
  begin
    lexer.take();
    node := fpe.parse(lexer);
    source.Condition := node.toString();
    source.conditionElement.Tag := node;
  end;
  if (lexer.hasToken('check')) then
  begin
    lexer.take();
    node := fpe.parse(lexer);
    source.check := node.toString();
    source.checkElement.Tag := node;
  end;
end;

procedure TFHIRStructureMapUtilities.parseTarget(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
var
  target : TFHIRStructureMapGroupRuleTarget;
  isConstant : boolean;
  name, id : String;
  node : TFHIRExpressionNode;
  p : TFhirStructureMapGroupRuleTargetParameter;
begin
  target := rule.targetList.Append;
  target.Context := lexer.take();
  target.contextType := MapContextTypeVariable;
  if (lexer.hasToken('.')) then
  begin
    lexer.token('.');
    target.Element := lexer.take();
  end;
  if (lexer.hasToken('=')) then
  begin
    lexer.token('=');
    isConstant := lexer.isConstant(true);
    name := lexer.take();
    if (lexer.hasToken('(')) then
    begin
      target.Transform := TFhirMapTransformEnum(fromEnum(name, CODES_TFhirMapTransformEnum));
      lexer.token('(');
      if (target.Transform = MapTransformEVALUATE) then
      begin
        parseParameter(target, lexer);
        lexer.token(',');
        p := target.parameterList.Append;
        node := fpe.parse(lexer);
        p.tag := node;
        p.Value := TFHIRString.create(node.toString());
      end
      else
      begin
        while (not lexer.hasToken(')')) do
        begin
          parseParameter(target, lexer);
          if (not lexer.hasToken(')')) then
            lexer.token(',');
        end;
      end;
      lexer.token(')');
    end
    else
    begin
      target.Transform := MapTransformCopy;
      if (not isConstant) then
      begin
        id := name;
        while (lexer.hasToken('.')) do
        begin
          id := id + lexer.take() + lexer.take();
        end;
        target.parameterList.Append.Value := TFHIRId.create(id);
      end
      else
        target.parameterList.Append.Value := readConstant(name, lexer);
    end;
  end;
  if (lexer.hasToken('as')) then
  begin
    lexer.take();
    target.Variable := lexer.take();
  end;
  if StringArrayExistsInsensitive(['first', 'last', 'share', 'only_one'], lexer.current) then
  begin
    if (lexer.current = 'share') then
    begin
      target.listMode := target.listMode + [MapListModeSHARE];
      lexer.next();
      target.ListRuleId := lexer.take();
    end
    else if (lexer.current = 'first') then
      target.listMode := target.listMode + [MapListModeFirst]
    else
      target.listMode := target.listMode + [MapListModeLAST];
    lexer.next();
  end;
end;


procedure TFHIRStructureMapUtilities.parseParameter(target : TFHIRStructureMapGroupRuleTarget; lexer : TFHIRPathLexer);
begin
  if not lexer.isConstant(true) then
    target.parameterList.Append.Value := TFHIRId.create(lexer.take())
  else if (lexer.isStringConstant()) then
    target.parameterList.Append.Value := TFHIRString.create(lexer.readConstant('??'))
  else
    target.parameterList.Append.Value := readConstant(lexer.take(), lexer);
end;

function TFHIRStructureMapUtilities.readConstant(s : String; lexer : TFHIRPathLexer) : TFHIRType;
begin
  if (StringIsInteger32(s)) then
    result := TFHIRInteger.create(s)
  else if (StringIsDecimal(s)) then
    result := TFHIRDecimal.create(s)
  else if (s = 'true') or (s = 'false') then
    result := TFHIRBoolean.create(s = 'true')
  else
    result := TFHIRString.create(lexer.processConstant(s));
end;


procedure TFHIRStructureMapUtilities.transform(appInfo : TAdvObject; source : TFHIRBase; map : TFHIRStructureMap; target : TFHIRBase);
var
  vars : TVariables;
begin
  vars := TVariables.create;
  try
    vars.add(vmINPUT, 'src', source.Link);
    vars.add(vmOUTPUT, 'tgt', target.Link);

    executeGroup('', appInfo, map, vars, map.GroupList[0]);
  finally
    vars.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.executeGroup(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup);
var
  r : TFHIRStructureMapGroupRule;
begin
  log(indent+'Group : '+group.Name);
  // todo: extends
  // todo: check inputs
  for r in group.ruleList do
    executeRule(indent+'  ', appInfo, map, vars, group, r);
end;

procedure TFHIRStructureMapUtilities.executeRule(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule);
var
  srcVars, v : TVariables;
  sources : TAdvList<TVariables>;
  t : TFHIRStructureMapGroupRuleTarget;
  childrule : TFHIRStructureMapGroupRule;
  dependent : TFHIRStructureMapGroupRuleDependent;
begin
  log(indent+'rule : '+rule.Name);
  srcVars := vars.copy();
  try
    if (rule.sourceList.count <> 1) then
      raise Exception.create('not handled yet');
    sources := analyseSource(appInfo, srcVars, rule.sourceList[0]);
    try
      if (sources <> nil) then
      begin
        for v in sources do
        begin
          for t in rule.TargetList do
            processTarget(appInfo, v, map, t);
          if (rule.ruleList.Count > 0) then
          begin
            for childrule in rule.ruleList do
              executeRule(indent +'  ', appInfo, map, v, group, childrule);
          end
          else if (rule.dependentList.Count > 0) then
          begin
            for dependent in rule.dependentList do
              executeDependency(indent+'  ', appInfo, map, v, group, dependent);
          end;
        end;
      end;
    finally
      sources.Free;
    end;
  finally
    srcVars.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.executeDependency(indent : String; appInfo : TAdvObject; map : TFHIRStructureMap; vin : TVariables; group : TFHIRStructureMapGroup; dependent : TFHIRStructureMapGroupRuleDependent);
var
  targetMap, impMap : TFHIRStructureMap;
  target, grp : TFHIRStructureMapGroup;
  imp : TFHIRUri;
  v : TVariables;
  i : integer;
  input : TFHIRStructureMapGroupInput;
  vr : TFHIRString;
  mode : TVariableMode;
  vv : TFHIRBase;
begin
  targetMap := nil;
  target := nil;
  for grp in map.GroupList do
  begin
    if (grp.Name = dependent.Name) then
    begin
      if (targetMap = nil) then
      begin
        targetMap := map;
        target := grp;
      end
      else
        raise Exception.create('Multiple possible matches for rule ''+dependent.Name+''');
    end;
  end;

  for imp in map.importList do
  begin
    if not FLib.containsKey(imp.value) then
      raise Exception.create('Unable to find map '+imp.Value);
    impMap := Flib[imp.Value];
    for grp in impMap.GroupList do
    begin
      if (grp.Name = dependent.Name) then
      begin
        if (targetMap = nil) then
        begin
          targetMap := impMap;
          target := grp;
        end
        else
          raise Exception.create('Multiple possible matches for rule ''+dependent.Name+''');
      end;
    end;
  end;
  if (target = nil) then
    raise Exception.create('No matches found for rule ''+dependent.Name+''');

  if (target.InputList.count <> dependent.variableList.count) then
    raise Exception.create('Rule ''+dependent.Name+'' has '+Integer.toString(target.InputList.count)+' but the invocation has '+Integer.toString(dependent.variableList.count)+' variables');

  v := TVariables.create;
  try
    for i := 0 to target.InputList.count - 1 do
    begin
      input := target.InputList[i];
      vr := dependent.variableList[i];
      if input.mode = MapInputModeSource then
        mode := vmINPUT
      else
        mode := vmOUTPUT;
      vv := vin.get(mode, vr.Value);
      if (vv = nil) then
        raise Exception.create('Rule ''+dependent.Name+'' '+CODES_TVariableMode[mode]+' variable "'+input.Name+'" has no value');
      v.add(mode, input.Name, vv.Link);
    end;
    executeGroup(indent+'  ', appInfo, targetMap, v, target);
  finally
    v.free;
  end;
end;


procedure TFHIRStructureMapUtilities.getChildrenByName(item : TFHIRBase; name : String; result : TAdvList<TFHIRBase>);
var
  v : TFHIRObject;
  list : TFHIRObjectList;
begin
  list := TFHIRObjectList.create;
  try
    item.ListChildrenByName(name, list);
    for v in list do
			if (v <> nil) then
				result.add(v.Link as TFhirBase);
  finally
    list.Free;
  end;
end;


function TFHIRStructureMapUtilities.getGroup(map: TFHIRConceptMap; source, target: String): TFHIRConceptMapGroup;
var
  g : TFHIRConceptMapGroup;
begin
  for g in map.groupList do
    if (g.source = source) and (g.target = target) then
      exit(g);
  g := map.groupList.Append;
  g.source := source;
  g.target := target;
  exit(g);
end;

function TFHIRStructureMapUtilities.analyseSource(appInfo : TAdvObject; vars : TVariables; src : TFHIRStructureMapGroupRuleSource) : TAdvList<TVariables>;
var
  b, r : TFhirBase;
  expr : TFHIRExpressionNode;
  items : TAdvList<TFHIRBase>;
  v : TVariables;
begin
  b := vars.get(vmINPUT, src.Context);
  if (b = nil) then
    raise Exception.create('Unknown input variable '+src.Context);

  if (src.Condition <> '') then
  begin
    expr := TFHIRExpressionNode(src.conditionElement.tag);
    if (expr = nil) then
    begin
      expr := fpe.parse(src.condition);
      //        fpe.check(context.appInfo, ??, ??, expr)
      src.conditionElement.tag := expr;
    end;
    if (not fpe.evaluateToBoolean(appinfo, nil, b, expr)) then
      exit(nil);
  end;

  if (src.check <> '') then
  begin
    expr := TFHIRExpressionNode(src.checkElement.tag);
    if (expr = nil) then
    begin
      expr := fpe.parse(src.check);
      //        fpe.check(context.appInfo, ??, ??, expr)
      src.checkElement.tag := expr;
    end;
    if (not fpe.evaluateToBoolean(appinfo, nil, b, expr)) then
      raise Exception.create('Check condition failed');
  end;

  items := TAdvList<TFHIRBase>.create;
  try
    if (src.Element = '') then
      items.add(b.link)
    else
      getChildrenByName(b, src.Element, items);
    result := TAdvList<TVariables>.create;
    try
      for r in items do
      begin
        v := vars.copy();
        try
          if (src.Variable <> '') then
            v.add(vmINPUT, src.variable, r.Link);
          result.add(v.Link);
        finally
          v.Free;
        end;
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    items.Free;
  end;
end;


procedure TFHIRStructureMapUtilities.processTarget(appInfo : TAdvObject; vars : TVariables; map : TFHIRStructureMap; tgt : TFHIRStructureMapGroupRuleTarget);
var
  dest, v : TFHIRBase;
begin
  dest := vars.get(vmOUTPUT, tgt.Context);
  if (dest = nil) then
    raise Exception.create('target context not known: '+tgt.Context);
  if (tgt.Element = '') then
    raise Exception.create('Not supported yet');
  v := nil;
  try
    if (tgt.Transform <> MapTransformNull) then
    begin
      v := runTransform(appInfo, map, tgt, vars);
      if (v <> nil) then
        dest.setProperty(tgt.Element, v.Link);
    end
    else
      v := dest.makeProperty(tgt.Element).link as TFHIRBase;
    if (tgt.Variable <> '') and (v <> nil) then
      vars.add(vmOUTPUT, tgt.variable, v.Link);
  finally
    v.Free;
  end;
end;

function TFHIRStructureMapUtilities.runTransform(appInfo : TAdvObject; map : TFHIRStructureMap; tgt : TFHIRStructureMapGroupRuleTarget; vars : TVariables) : TFHIRBase;
var
  factory : TFhirResourceFactory;
  expr : TFHIRExpressionNode;
  v : TFHIRBaseList;
  src, len : String;
  l : integer;
  b : TFHIRBase;
begin
  case tgt.Transform of
    MapTransformCreate :
      begin
        factory := TFhirResourceFactory.Create;
        try
          result := factory.makeByName(getParamString(vars, tgt.ParameterList[0]));
        finally
          factory.Free;
        end;
      end;
    MapTransformCOPY :
      begin
        result := getParam(vars, tgt.ParameterList[0]).Link;
      end;
    MapTransformEVALUATE :
      begin
        expr := tgt.Tag as TFHIRExpressionNode;
        if (expr = nil) then
        begin
          expr := fpe.parse(getParamString(vars, tgt.ParameterList[1]));
          tgt.tag := expr;
        end;
        v := fpe.evaluate(nil, nil, getParam(vars, tgt.ParameterList[0]), expr);
        try
          if (v.count <> 1) then
            raise Exception.create('evaluation of '+expr.toString()+' returned '+Integer.toString(v.count)+' objects');
          result := v[0].Link;
        finally
          v.Free;
        end;
      end;
    MapTransformTRUNCATE :
      begin
        src := getParamString(vars, tgt.ParameterList[0]);
        len := getParamString(vars, tgt.ParameterList[1]);
        if (StringIsInteger32(len)) then
        begin
          l := StrToInt(len);
          if (src.length > l) then
            src := src.substring(0, l);
        end;
        result := TFHIRString(src);
      end;
    MapTransformESCAPE :
      begin
        raise Exception.create('Transform '+CODES_TFhirMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    MapTransformCAST :
      begin
        raise Exception.create('Transform '+CODES_TFhirMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    MapTransformAPPEND :
      begin
        raise Exception.create('Transform '+CODES_TFhirMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    MapTransformTRANSLATE :
      begin
        result := translate(appInfo, map, vars, tgt.ParameterList);
      end;
    MapTransformREFERENCE :
      begin
        raise Exception.create('Transform '+CODES_TFhirMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    MapTransformDATEOP :
      begin
        raise Exception.create('Transform '+CODES_TFhirMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    MapTransformUUID :
      begin
        result := TFHIRId.Create(NewGuidId);
      end;
    MapTransformPOINTER :
      begin
        b := getParam(vars, tgt.ParameterList[0]);
        if (b is TFHIRResource) then
          result := TFHIRUri.create('urn:uuid:'+TFHIRResource(b).Id)
        else
          raise Exception.create('Transform engine cannot point at an element of type '+b.fhirType());
      end
  else
    raise Exception.create('Transform Unknown');
  end;
end;


function TFHIRStructureMapUtilities.getParamString(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : String;
var
  b : TFHIRBase;
begin
  b := getParam(vars, parameter);
  if (b = nil) or not b.hasPrimitiveValue() then
    result := ''
  else
    result := b.primitiveValue();
end;

procedure TFHIRStructureMapUtilities.log(s: String);
begin
  if FServices <> nil then
    FServices.log(s);
end;

function TFHIRStructureMapUtilities.getParam(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : TFHIRBase;
var
  p : TFhirType;
begin
  p := parameter.Value;
  if not (p is TFHIRId) then
    result := p
  else
  begin
    result := vars.get(vmINPUT, TFHIRId(p).StringValue);
    if (result = nil) then
      result := vars.get(vmOUTPUT, TFHIRId(p).StringValue);
  end;
end;


function TFHIRStructureMapUtilities.translate(appInfo : TAdvObject; map : TFHIRStructureMap; vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameterList) : TFHIRBase;
var
  src : TFHIRBase;
  id, fld : String;
begin
  src := getParam(vars, parameter[0]);
  id := getParamString(vars, parameter[1]);
  fld := getParamString(vars, parameter[2]);
  result := translate(appInfo, map, src, id, fld);
end;

function TFHIRStructureMapUtilities.translate(appInfo : TAdvObject; map : TFHIRStructureMap; source : TFHIRBase; conceptMapUrl : String; fieldToReturn : String): TFHIRBase;
var
  src, outcome : TFHIRCoding;
  b : TAdvList<TFHIRBase>;
  uri, message : String;
  cmap : TFhirConceptMap;
  r : TFHIRResource;
  done : boolean;
  list : TAdvList<TFhirConceptMapGroupElement>;
  g : TFhirConceptMapGroup;
  e : TFhirConceptMapGroupElement;
  tgt : TFhirConceptMapGroupElementTarget;
begin
  b := TAdvList<TFHIRBase>.create;
  g := nil;
  src := TFHIRCoding.create;
  try
    if (source.isPrimitive()) then
      src.Code := source.primitiveValue()
    else if ('Coding' = source.fhirType()) then
    begin
      source.getProperty('system', true, b);
      if (b.count = 1) then
        src.System := b[0].primitiveValue();
      source.getProperty('code', true, b);
      if (b.count = 1) then
        src.Code := b[0].primitiveValue();
    end
    else if ('CE'.equals(source.fhirType())) then
    begin
      source.getProperty('codeSystem', true, b);
      if (b.count = 1) then
        src.System := b[0].primitiveValue();
      source.getProperty('code', true, b);
      if (b.count = 1) then
        src.Code := b[0].primitiveValue();
    end
    else
      raise Exception.create('Unable to translate source '+source.fhirType());

    if (conceptMapUrl.equals('http://hl7.org/fhir/ConceptMap/special-oid2uri')) then
    begin
      uri := FServices.oid2Uri(src.Code);
      if (uri = '') then
        uri := 'urn:oid:'+src.Code;
      if ('uri'.equals(fieldToReturn)) then
        result := TFHIRUri.create(uri)
      else
        raise Exception.create('Error in return code');
    end
    else
    begin
      cmap := nil;
      if (conceptMapUrl.startsWith('#')) then
      begin
        for r in map.ContainedList do
        begin
          if (r is TFHIRConceptMap) and (TFHIRConceptMap(r).Id = conceptMapUrl.substring(1)) then
            cmap := TFHIRConceptMap(r);
        end;
      end
      else
        cmap := TFhirConceptMap(FWorker.fetchResource(frtConceptMap, conceptMapUrl));
      outcome := nil;
      try
        done := false;
        message := '';
        if (cmap = nil) then
        begin
          if (FServices = nil) then
            message := 'No map found for '+conceptMapUrl
          else
          begin
            outcome := FServices.translate(appInfo, src, conceptMapUrl);
            done := true;
          end;
        end
        else
        begin
          list := TAdvList<TFhirConceptMapGroupElement>.create;
          try
            for g in cmap.GroupList do
              for e in g.ElementList do
              begin
                if (src.System = '') and (src.code = e.code) then
                  list.add(e.Link)
                else if (src.system <> '') and (src.System = g.source) and (src.code = e.code) then
                  list.add(e.Link);
              end;
            if (list.count = 0) then
              done := true
            else if (list[0].TargetList.count = 0) then
              message := 'Concept map '+conceptMapUrl+' found no translation for '+src.code
            else
            begin
              for tgt in list[0].TargetList do
              begin
                if (tgt.equivalence in [ConceptMapEquivalenceEQUAL, ConceptMapEquivalenceEQUIVALENT, ConceptMapEquivalenceWIDER]) then
                begin
                  if (done) then
                  begin
                    message := 'Concept map '+conceptMapUrl+' found multiple matches for '+src.code;
                    done := false;
                  end
                  else
                  begin
                    done := true;
                    outcome := TFHIRCoding.Create;
                    outcome.code := tgt.code;
                    outcome.system := g.Target;
                  end;
                end
                else if (tgt.equivalence = ConceptMapEquivalenceUNMATCHED) then
                begin
                  done := true;
                end;
              end;
              if (not done) then
                message := 'Concept map '+conceptMapUrl+' found no usable translation for '+src.code;
            end;
          finally
            list.Free;
          end;
        end;
        if (not done) then
          raise Exception.create(message);
        if (outcome = nil) then
          result := nil
        else if ('code' = fieldToReturn) then
          result := TFHIRCode.create(outcome.code)
        else
          result := outcome.Link;
      finally
        outcome.Free;
      end;
    end;
  finally
    src.Free;
    b.Free;
  end;
end;


{ TVariable }

function TVariable.copy: TVariable;
begin
  result := TVariable.Create;
  result.Fname := FName;
  result.Fmode := FMode;
  result.Fobj := FObj.Link;
end;

destructor TVariable.Destroy;
begin
  FObj.free;
  inherited;
end;

function TVariable.link: TVariable;
begin
  result := TVariable(inherited link);
end;

{ TVariables }

constructor TVariables.create;
begin
  inherited;
  list := TAdvList<TVariable>.create;
end;

destructor TVariables.destroy;
begin
  list.Free;
  inherited;
end;

procedure TVariables.add(mode: TVariableMode; name: String; obj: TFHIRBase);
var
  v, vv : TVariable;
begin
  vv := nil;
  for v in list do
    if (v.mode = mode) and (v.name = name) then
      vv := v;
  if (vv <> nil) then
    list.remove(vv);
  v := TVariable.create;
  list.add(v);
  v.Fname := name;
  v.Fmode := mode;
  v.Fobj := obj;
end;

function TVariables.copy: TVariables;
var
  v : TVariable;
begin
  result := TVariables.Create;
  try
    for v in list do
      result.list.add(v.copy());
    result.link;
  finally
    result.free;
  end;
end;

function TVariables.get(mode: TVariableMode; name: String): TFHIRBase;
var
  v : TVariable;
begin
  result := nil;
  for v in list do
    if (v.mode = mode) and (v.name = name) then
			exit(v.obj);
end;

function TVariables.link: TVariables;
begin
  result := TVariables(inherited Link);
end;


end.

