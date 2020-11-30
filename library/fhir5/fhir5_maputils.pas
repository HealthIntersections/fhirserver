unit fhir5_maputils;

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
  SysUtils, Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_xhtml,  fhir_pathengine, fhir_factory,
  fhir5_enums, fhir5_types, fhir5_resources, fhir5_context, fhir5_pathengine, fhir5_utilities, fhir5_pathnode, fhir5_factory, fhir5_elementmodel;

type
  TVariableMode = (vmINPUT, vmOUTPUT);

const
  CODES_TVariableMode : array [TVariableMode] of string = ('source', 'target');
  AUTO_VAR_NAME = 'vvv';
  RENDER_MULTIPLE_TARGETS_ONELINE = true;

type
  TVariable = class (TFslObject)
  private
    Fname: String;
    Fmode: TVariableMode;
    Fobj: TFHIRObject;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    function link : TVariable; overload;
    function copy : TVariable;
    function summary(mode : boolean = false) : string;

    property mode : TVariableMode read Fmode write Fmode;
    property name : String read Fname write Fname;
    property obj : TFHIRObject read Fobj write Fobj;
  end;

  TVariables = class (TFslObject)
  private
    FList : TFslList<TVariable>;
    function GetSummary: string;
    function GetCount: Integer;
    function GetVariable(index: integer): TVariable;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Link : TVariables; overload;
    procedure add(mode : TVariableMode; name : String; obj : TFHIRObject);
    function copy : TVariables;
    function get(mode : TVariableMode; name : String) : TFHIRObject;
    property summary : string read GetSummary;
    property Count : Integer read GetCount;
    property variable[index : integer] : TVariable read GetVariable; default;
    property list : TFslList<TVariable> read FList;
  end;

  TTransformerServices = class abstract (TFslObject)
  private
  public
    function link : TTransformerServices; overload;

    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; virtual; abstract;
    procedure log(s : String); virtual; abstract;
    function performSearch(appInfo : TFslObject; url : String) : TFslList<TFHIRObject>; virtual; abstract;
    function createType(appInfo : TFslObject; tn : String) : TFHIRObject; virtual; abstract;
    procedure createResource(appInfo : TFslObject; res : TFHIRObject; atRootofTransform : boolean); virtual; abstract; // an already created resource is provided; this is to identify/store it
  end;

  TResolvedGroup = class (TFslObject)
  private
    FMap: TFhirStructureMap;
    FGroup: TFhirStructureMapGroup;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(map : TFhirStructureMap; group : TFhirStructureMapGroup);
    destructor Destroy; override;

    property map : TFhirStructureMap read FMap;
    property group : TFhirStructureMapGroup read FGroup;
  end;

  TFHIRStructureMapDebuggingStatus = (dsRunToBreakpoint, dsStepOut, dsStepOver, dsStepIn);

  TFHIRStructureMapDebugContext = class (TFslObject)
  private
    FRule: TFhirStructureMapGroupRule;
    FMap: TFHIRStructureMap;
    FAppInfo: TFslObject;
    FTarget: TFhirStructureMapGroupRuleTarget;
    FVariables: TVariables;
    FGroup: TFhirStructureMapGroup;
    FParent: TFHIRStructureMapDebugContext;
    FStatus: TFHIRStructureMapDebuggingStatus;
    function getdescription: String;
    function GetFocus: TFHIRObject;
    function GetName: String;
    function GetLine: integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor create(parent : TFHIRStructureMapDebugContext; appInfo : TFslObject;
       map : TFHIRStructureMap; group : TFhirStructureMapGroup; rule: TFhirStructureMapGroupRule; target : TFhirStructureMapGroupRuleTarget;
       variables : TVariables);
    destructor Destroy; override;
    function link : TFHIRStructureMapDebugContext; overload;

    property parent : TFHIRStructureMapDebugContext read FParent;
    property appInfo : TFslObject read FAppInfo;
    property map : TFHIRStructureMap read FMap;
    property group : TFhirStructureMapGroup read FGroup;
    property rule: TFhirStructureMapGroupRule read FRule;
    property target : TFhirStructureMapGroupRuleTarget read FTarget;
    property variables : TVariables read FVariables;

    property name : String read GetName;
    property description : String read GetDescription;
    property focus : TFHIRObject read GetFocus;
    property line : integer read GetLine;

    property status : TFHIRStructureMapDebuggingStatus read FStatus write FStatus;
  end;

  TFHIRStructureMapDebugEvent = procedure(sender : TObject; info : TFHIRStructureMapDebugContext) of Object;

  TFHIRStructureMapUtilities = class (TFslObject)
  private
    FWorker : TFHIRWorkerContext;
    fpp : TFHIRPathParser;
    fpe : TFHIRPathEngine;
    FLib : TFslMap<TFHIRStructureMap>;
    FServices : TTransformerServices;
    FFactory : TFHIRFactoryR5;
    FOnDebug: TFHIRStructureMapDebugEvent;
    procedure renderContained(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderUses(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderImports(b : TStringBuilder; map : TFHIRStructureMap);
    procedure renderGroup(b : TStringBuilder; g : TFHIRStructureMapGroup);
    procedure renderDoco(b : TStringBuilder; doco : String);
    procedure RenderRule(b : TStringBuilder; r : TFHIRStructureMapGroupRule; indent : integer);
    procedure RenderSource(b : TStringBuilder; rs : TFHIRStructureMapGroupRuleSource; canbeAbbreviated : boolean);
    procedure renderTarget(b : TStringBuilder; rt : TFHIRStructureMapGroupRuleTarget; canbeAbbreviated : boolean);
    procedure renderTransformParam(b : TStringBuilder; rtp : TFHIRStructureMapGroupRuleTargetParameter);
    procedure renderConceptMap(b : TStringBuilder; map : TFHIRConceptMap);

    function getGroup(map : TFHIRConceptMap; source, target : String) : TFHIRConceptMapGroup;
    function fromEnum(s : String; codes : Array of String; lexer : TFHIRPathLexer) : integer;
    function readPrefix(prefixes : TFslStringDictionary; lexer : TFHIRPathLexer) : String;
    function readEquivalence(lexer : TFHIRPathLexer) : TFhirConceptMapRelationshipEnum;
    function readConstant(s : String; lexer : TFHIRPathLexer) : TFHIRDataType;
    function isSimpleSyntax(rule : TFhirStructureMapGroupRule) : boolean;
    procedure parseConceptMap(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseUses(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseImports(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseGroup(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
    procedure parseInput(group : TFHIRStructureMapGroup; lexer : TFHIRPathLexer; newFmt : boolean);
    procedure parseRule(list : TFhirStructureMapGroupRuleList; lexer : TFHIRPathLexer; newFmt : boolean);
    procedure parseSource(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseTarget(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseRuleReference(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
    procedure parseParameter(target : TFHIRStructureMapGroupRuleTarget; lexer : TFHIRPathLexer);

    function debug(dbgContext : TFHIRStructureMapDebugContext; appInfo : TFslObject; map : TFHIRStructureMap; group : TFhirStructureMapGroup; rule: TFhirStructureMapGroupRule; target : TFhirStructureMapGroupRuleTarget; variables : TVariables) : TFHIRStructureMapDebugContext;
    procedure log(s : String);
    procedure getChildrenByName(item : TFHIRObject; name : String; result : TFslList<TFHIRObject>);
    function runTransform(ruleId : String; appInfo : TFslObject; map : TFHIRStructureMap; group : TFHIRStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; vars : TVariables; dest : TFHIRObject; element, srcVar : String; root : boolean) : TFHIRObject;
    function buildCoding(uri, code : String) : TFHIRCoding;
    function findMatchingMaps(value : String) : TFslList<TFHIRStructureMap>;
    function matchesType(map : TFHIRStructureMap; actualType, statedType : String): boolean;
    function matchesByType(map : TFHIRStructureMap; ruleId : String; grp : TFHIRStructureMapGroup; tn: String) : boolean; overload;
    function matchesByType(map : TFHIRStructureMap; ruleId : String; grp : TFHIRStructureMapGroup; tnSrc, tnTgt: String) : boolean;  overload;
    function getActualType(map : TFHIRStructureMap; statedType : String) : String;
    procedure resolveGroupReference(map : TFHIRStructureMap; source : TFHIRStructureMapGroup; name : TFhirId; var tgtGroup : TFHIRStructureMapGroup; var tgtMap : TFHIRStructureMap);
    procedure resolveGroupByTypes(map : TFHIRStructureMap; ruleid : String; source : TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule; srcType, tgtType : String; var tgtGroup : TFHIRStructureMapGroup; var tgtMap : TFHIRStructureMap);
    function determineTypeFromSourceType(map : TFHIRStructureMap; ruleId : String; source : TFHIRStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; base : TFHIRObject; types : TArray<String>) : String;
    procedure executeGroup(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; atRoot : boolean; dbgContext : TFHIRStructureMapDebugContext);
    procedure executeRule(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule; atRoot : boolean; dbgContext : TFHIRStructureMapDebugContext);
    function analyseSource(groupId, ruleId : String; appInfo : TFslObject; vars : TVariables; src : TFHIRStructureMapGroupRuleSource; errorlocation, indent : String) : TFslList<TVariables>;
    procedure processTarget(ruleId : String; appInfo : TFslObject; vars : TVariables; map : TFHIRStructureMap; group : TFhirStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; srcVar : String; atRoot: boolean);
    procedure executeDependency(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vin : TVariables; group : TFHIRStructureMapGroup; dependent : TFHIRStructureMapGroupRuleDependent; dbgContext : TFHIRStructureMapDebugContext);
    function getParamStringNoNull(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter; message : String) : String;
    function getParamString(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : String;
    function getParam(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : TFHIRObject;
    function translate(appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameterList) : TFHIRObject; overload;
    function translate(appInfo : TFslObject; map : TFHIRStructureMap; source : TFHIRObject; conceptMapUrl : String; fieldToReturn : String): TFHIRObject; overload;
    function checkisSimple(rule: TFhirStructureMapGroupRule): boolean;
    procedure SetServices(const Value: TTransformerServices);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(context : TFHIRWorkerContext; lib : TFslMap<TFHIRStructureMap>; services : TTransformerServices; factory : TFHIRFactoryR5);
    destructor Destroy; override;

    property Lib : TFslMap<TFHIRStructureMap> read FLib;

    function render(map : TFHIRStructureMap) : String; overload;
    function render(map : TFHIRConceptMap) : String; overload;
    function parse(text, sourceName : String) : TFHIRStructureMap;
    procedure transform(appInfo : TFslObject; source : TFHIRObject; map : TFHIRStructureMap; target : TFHIRObject);

    property OnDebug : TFHIRStructureMapDebugEvent read FOnDebug write FOnDebug;
    property Services : TTransformerServices read FServices write SetServices;
  end;


implementation

{ TFHIRStructureMapUtilities }

constructor TFHIRStructureMapUtilities.Create(context: TFHIRWorkerContext; lib: TFslMap<TFHIRStructureMap>; services: TTransformerServices; factory : TFHIRFactoryR5);
begin
  inherited Create;
  FWorker := context;
  FLib := lib;
  FServices := services;
  fpe := TFHIRPathEngine.Create(context.link, nil);
  fpp := TFHIRPathParser.create;
  FFactory := factory;
end;

function TFHIRStructureMapUtilities.debug(dbgContext: TFHIRStructureMapDebugContext; appInfo: TFslObject; map: TFHIRStructureMap; group: TFhirStructureMapGroup; rule: TFhirStructureMapGroupRule; target: TFhirStructureMapGroupRuleTarget; variables: TVariables): TFHIRStructureMapDebugContext;
begin
  if assigned(FOnDebug) then
  begin
    result := TFHIRStructureMapDebugContext.create(dbgContext, appInfo, map, group, rule, target, variables);
    try
      FOnDebug(self, result);
      result.link;
    finally
      result.Free;
    end;
  end
  else
    result := nil;
end;

destructor TFHIRStructureMapUtilities.destroy;
begin
  FWorker.Free;
  fpe.Free;
  fpp.Free;
  FLib.Free;
  FServices.Free;
  FFactory.Free;
  inherited;
end;

function TFHIRStructureMapUtilities.determineTypeFromSourceType(map: TFHIRStructureMap; ruleId : String; source: TFHIRStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; base: TFHIRObject; types: TArray<String>): String;
var
  tn : String;
  rm : TFHIRStructureMap;
  grp, rg : TFhirStructureMapGroup;
  imp : TFhirCanonical;
  impMapList : TFslList<TFHIRStructureMap>;
  impMap : TFHIRStructureMap;
begin
  tn := base.fhirType();
  if (tgt.hasTag('type')) then
    exit(tgt.Tags['type']);

  rm := nil;
  rg := nil;
  for grp in map.groupList do
  begin
    if (matchesByType(map, ruleId, grp, tn)) then
    begin
      if (rg = nil) then
      begin
        rm := map;
        rg := grp;
      end
      else
        raise EFHIRException.create('Multiple possible matches looking for default rule for "'+tn+'"');
    end;
  end;
  if (rg <> nil) then
  begin
    tn := getActualType(rm, rg.inputList[1].type_);
    tgt.tags['type'] := tn;
    exit(tn);
  end;

  for imp in map.importList do
  begin
    impMapList := findMatchingMaps(imp.value);
    try
      if (impMapList.count = 0) then
        raise EFHIRException.create('Unable to find map(s) for '+imp.value);
      for impMap in impMapList do
      begin
        if (impMap.url <> map.url) then
        begin
          for grp in impMap.groupList do
          begin
            if (matchesByType(impMap, ruleId, grp, tn)) then
            begin
              if (rg = nil) then
              begin
                rm := impMap;
                rg := grp;
              end
              else
                raise EFHIRException('Multiple possible matches for default rule for "'+tn+'" in '+rm.url+'#'+rg.name+' and '+impMap.url+'#'+grp.name);
            end;
          end;
        end;
      end;
    finally
      impMapList.Free;
    end;
  end;
  if (rg = nil) then
    raise EFHIRException.create('No matches found for default rule for "'+tn+'" from '+map.url);
  tn := getActualType(rm, rg.inputList[1].type_);
  tgt.tags['type'] := tn;
  exit(tn);
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
    b.append('" ');
    if (s.alias <> '') then
    begin
      b.append('alias ');
      b.append(s.alias);
      b.append(' ');
    end;
    b.append('as ');
    b.append(CODES_TFhirStructureMapModelModeEnum[s.Mode]);
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
  first : boolean;
begin
  b.append('group ');
  b.append(g.Name);
  b.append('(');
  first := true;
  for gi in g.inputList do
  begin
    if (first) then
      first := false
    else
      b.append(', ');
    b.append(CODES_TFhirStructureMapInputModeEnum[gi.Mode]);
    b.append(' ');
    b.append(gi.name);
    if (gi.type_ <> '') then
    begin
      b.append(' : ');
      b.append(gi.type_);
    end;
  end;
  b.append(')');
  if (g.extends <> '') then
  begin
    b.append(' extends ');
    b.append(g.Extends);
  end;
  case g.typeMode of
    StructureMapGroupTypeModeTypes: b.append(' <<types>>');
    StructureMapGroupTypeModeTypeAndTypes: b.append(' <<type+>>');
  end;
  b.append(' {');
  if (g.Documentation <> '') then
    renderDoco(b, g.Documentation);
  b.append(#13#10);
  for r in g.ruleList do
    renderRule(b, r, 2);
  b.append('}'#13#10#13#10);
  end;

function TFHIRStructureMapUtilities.checkisSimple(rule: TFhirStructureMapGroupRule): boolean;
begin
  result := (rule.sourceList.count = 1) and (rule.sourceList[0].element <> '') and (rule.sourceList[0].variable <> '') and
        (rule.targetList.count = 1) and (rule.targetList[0].Variable <> '') and (rule.targetList[0].transform in [StructureMapTransformNull, StructureMapTransformCreate]) and (rule.targetList[0].parameterList.count = 0) and
        (rule.dependentList.count = 0) and (rule.ruleList.count = 0);
end;

function matchesname(n : string; source : TFhirStructureMapGroupRuleSourceList) : boolean;
var
  s : String;
begin
  result := false;
  if (source.count = 1) and (source[0].element <> '') then
  begin
    s := source[0].element;
    if (n = s) or (n = '"'+s+'"') then
      result := true
    else if (source[0].type_ <> '') then
    begin
      s := source[0].element+'-'+source[0].type_;
      if (n = s) or (n = '"'+s+'"') then
        result := true
    end;
  end;
end;

procedure TFHIRStructureMapUtilities.RenderRule(b : TStringBuilder; r : TFHIRStructureMapGroupRule; indent : integer);
var
  first, ifirst : boolean;
  rs : TFHIRStructureMapGroupRuleSource;
  rt : TFHIRStructureMapGroupRuleTarget;
  rd : TFHIRStructureMapGroupRuleDependent;
  ir : TFHIRStructureMapGroupRule;
  rdp : TFHIRString;
  canBeAbbreviated : boolean;
begin
  b.append(StringPadLeft('', ' ', indent));
  canBeAbbreviated := checkisSimple(r);
  first := true;
  for rs in r.sourceList do
  begin
    if (first) then
      first := false
    else
      b.append(', ');
    renderSource(b, rs, canBeAbbreviated);
  end;
  if (r.targetList.count > 1) then
  begin
    b.append(' -> ');
    first := true;
    for rt in r.targetList do
    begin
      if (first) then
        first := false
      else
      begin
        b.append(',');
        if (RENDER_MULTIPLE_TARGETS_ONELINE) then
          b.append(' ')
        else
        begin
          b.append('\r\n');
          b.append(StringPadLeft('', ' ', indent+4));
        end;
      end;
      renderTarget(b, rt, false);
    end;
  end
  else if (r.targetList.Count > 0) then
  begin
    b.append(' -> ');
    renderTarget(b, r.targetList[0], canBeAbbreviated);
  end;
  if (r.ruleList.count > 0) then
  begin
    b.append(' then {');
    b.append(#13#10);
    for ir in r.ruleList do
      renderRule(b, ir, indent+2);
    b.append(StringPadLeft('', ' ', indent));
    b.append('}');
  end
  else
  begin
    if r.dependentList.Count > 0 then
    begin
      b.append(' then ');
      first := true;
      for rd in r.dependentList do
      begin
       if (first) then
          first := false
        else
          b.append(', ');
        b.append(rd.name);
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
        b.append(')');
      end;
    end;
  end;
  if (r.name <> '') then
  begin
    if not matchesName(r.name, r.sourceList) then
    begin
      b.append(' "');
      b.append(r.name);
      b.append('"');
    end;
  end;
  b.append(';');
  renderDoco(b, r.documentation);
  b.append(#13#10);
end;

procedure TFHIRStructureMapUtilities.RenderSource(b : TStringBuilder; rs : TFHIRStructureMapGroupRuleSource; canbeAbbreviated : boolean);
begin
  b.append(rs.Context);
  if (rs.context = '@search') then
  begin
    b.append('(');
    b.append(rs.element);
    b.append(')');
  end
  else if (rs.element <> '') then
  begin
    b.append('.');
    b.append(rs.element);
  end;
  if (rs.type_ <> '') then
  begin
      b.append(' : ');
      b.append(rs.type_);
      if (rs.min <> '') then
      begin
        b.append(' ');
        b.append(rs.Min);
        b.append('..');
        b.append(rs.Max);
      end;
  end;
  if (rs.ListMode <> StructureMapSourceListModeNull) then
  begin
    b.append(' ');
    b.append(CODES_TFhirStructureMapSourceListModeEnum[rs.ListMode]);
  end;
  if (rs.defaultValue <> nil) then
  begin
    b.append(' default ');
    assert(rs.defaultValue is TFhirString);
    b.append('''+jsonEscape((rs.defaultValue as TFhirString).value, true)+''');
  end;
  if not canbeAbbreviated and (rs.Variable <> '') then
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
  if (rs.logMessage <> '') then
  begin
    b.append(' log ');
    b.append(rs.logMessage);
  end;
end;

procedure TFHIRStructureMapUtilities.renderTarget(b : TStringBuilder; rt : TFHIRStructureMapGroupRuleTarget; canbeAbbreviated : boolean);
var
  first : boolean;
  rtp : TFHIRStructureMapGroupRuleTargetParameter;
  lm : TFhirStructureMapTargetListModeEnum;
begin
  if (rt.context <> '') then
  begin
    if (rt.contextType = StructureMapContextTypeType) then
      b.append('@');
    b.append(rt.Context);
    if (rt.Element <> '')  then
    begin
      b.append('.');
      b.append(rt.Element);
    end;
  end;
  if not canbeAbbreviated and (rt.Transform <> StructureMapTransformNull) then
  begin
    if (rt.context <> '') then
      b.append(' = ');
    if (rt.Transform = StructureMapTransformCopy) and (rt.parameterList.count = 1) then
    begin
      renderTransformParam(b, rt.ParameterList[0]);
    end
    else if (rt.Transform = StructureMapTransformEVALUATE) and (rt.ParameterList.count = 2) then
    begin
      b.append(CODES_TFhirStructureMapTransformEnum[rt.Transform]);
      b.append('(');
      b.append(TFHIRId(rt.ParameterList[0].Value).StringValue);
      b.append(TFHIRString(rt.ParameterList[1].Value).StringValue);
      b.append(')');
    end
    else
    begin
      b.append(CODES_TFhirStructureMapTransformEnum[rt.Transform]);
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
  if not canbeAbbreviated and (rt.Variable <> '') then
  begin
    b.append(' as ');
    b.append(rt.Variable);
  end;
  for lm := low(TFhirStructureMapTargetListModeEnum) to high(TFhirStructureMapTargetListModeEnum) do
    if lm in rt.listMode then
    begin
      b.append(' ');
      b.append(CODES_TFhirStructureMapTargetListModeEnum[lm]);
      if (lm = StructureMapTargetListModeShare) then
      begin
        b.append(' ');
        b.append(rt.listRuleId);
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
    b.append('''');
    b.append(jsonEscape((rtp.Value as TFHIRString).StringValue, true));
    b.append('''');
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

function isToken(s : String) : Boolean;
var
  i : integer;
begin
  if (s = '') then
    result := false
  else if not CharInSet(s[1], ['A'..'Z', 'a'..'z', '_']) then
    result := false
  else
  begin
    result := true;
    for i := 2 to s.Length do
      if not CharInSet(s[i], ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
        exit(false);
  end;
end;

type
   TPrefixMode = (pmSource, pmDepends, pmTarget, pmProduct);
   TPrefixInformation = class (TFslObject)
   private
    FMode: TPrefixMode;
    FAbbrev: String;
    FUrl: String;
  protected
    function sizeInBytesV : cardinal; override;
   public
     constructor create(mode : TPrefixMode; abbrev, url : String);
     property mode : TPrefixMode read FMode write FMode;
     property abbrev : String read FAbbrev write FAbbrev;
     property url : String read FUrl write FUrl;
   end;

function TFHIRStructureMapUtilities.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FWorker.sizeInBytes);
  inc(result, fpp.sizeInBytes);
  inc(result, fpe.sizeInBytes);
  inc(result, FLib.sizeInBytes);
  inc(result, FServices.sizeInBytes);
  inc(result, FFactory.sizeInBytes);
end;

{ TPrefixInformation }

constructor TPrefixInformation.create(mode: TPrefixMode; abbrev, url: String);
begin
  inherited create;
  FMode := mode;
  FAbbrev := abbrev;
  FUrl := url;
end;


function intToStrND(i, v : integer) : String;
begin
  if i = v then
    result := ''
  else
    result := inttostr(i);
end;

function TPrefixInformation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FAbbrev.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
end;

procedure TFHIRStructureMapUtilities.renderConceptMap(b: TStringBuilder; map: TFHIRConceptMap);
const
  CHARS_RELATIONSHIP : array [TFhirConceptMapRelationshipEnum] of string = ('??', '-', '=', '>', '<', '!=');
var
  prefixes : TFslList<TPrefixInformation>;
  g : TFhirConceptMapGroup;
  e : TFhirConceptMapGroupElement;
  t : TFhirConceptMapGroupElementTarget;
  d : TFhirConceptMapGroupElementTargetDependsOn;
  pi : TPrefixInformation;
  f : boolean;
  function getUri(mode : TPrefixMode; url : String) : TPrefixInformation;
  var
    pi : TPrefixInformation;
  begin
    result := nil;
    for pi in prefixes do
      if (pi.mode = mode) and (pi.url = url) then
        exit(pi);
  end;

  function hasAbbrev(abbrev : String) : boolean;
  var
    pi : TPrefixInformation;
  begin
    result := false;
    for pi in prefixes do
      if (pi.abbrev = abbrev) then
        exit(true);
  end;

  procedure seeSystem(mode : TPrefixMode; abbrev, url : String);
  var
    i : integer;
  begin
    i := 0;
    if getUri(mode, url) <> nil then
      exit;
    while hasAbbrev(abbrev+intToStrND(i, 0)) do
      inc(i);
    prefixes.Add(TPrefixInformation.create(mode, abbrev+intToStrND(i, 0), url));
  end;

  procedure app(mode : TPrefixMode; system, code : String);
  begin
    b.append(getUri(mode, system).FAbbrev);
    b.append(':');
    if not isToken(code) then
      b.append('"'+jsonEscape(code, true)+'"')
    else
      b.append(jsonEscape(code, false));
  end;
begin
  prefixes := TFslList<TPrefixInformation>.create;
  try
    b.append('conceptmap "');
    b.append(map.id);
    b.append('" {'#13#10#13#10);

    if (map.source is TFhirUri) then
    begin
      b.Append(' source "');
      b.append(jsonEscape(TFhirUri(map.source).value, true));
      b.append('"'#13#10);
    end;
    if (map.target is TFhirUri) then
    begin
      b.Append(' target "');
      b.append(jsonEscape(TFhirUri(map.target).value, true));
      b.append('"'#13#10);
    end;

    // first pass: determine prefixes:
    for g in map.groupList do
      for e in g.elementList do
      begin
        seeSystem(pmSource, 's', g.source);
        for t in e.targetList do
        begin
          seeSystem(pmTarget, 't', g.target);
          for d in t.dependsOnList do
           seeSystem(pmDepends, 'd', d.system);
          for d in t.productList do
           seeSystem(pmProduct, 'p', d.system);
        end;
      end;
    for pi in prefixes do
    begin
      b.Append(' prefix ');
      b.append(pi.abbrev);
      b.Append(' = "');
      b.append(jsonEscape(pi.url, true));
      b.append('"'#13#10);
    end;
    b.append(#13#10);

    for g in map.groupList do
    begin
      if (g.unmapped <> nil) then
      begin
        b.append('  unmapped for ');
        b.append(getUri(pmSource, g.source).FAbbrev);
        b.append(' = ');
        b.append(CODES_TFhirConceptMapGroupUnmappedModeEnum[g.unmapped.Create.mode]);
        b.append(#13#10#13#10);
      end;
    end;


    // now render
    for g in map.groupList do
      for e in g.elementList do
      begin
        for t in e.targetList do
        begin
          b.Append(' ');
          app(pmSource, g.source, e.code);
          b.Append(' ');
          if (t.dependsOnList.Count > 0) then
          begin
            b.Append('[');
            f := true;
            for d in t.dependsOnList do
            begin
              if f then f := false else b.Append(', ');
              app(pmDepends, d.system, d.code);
            end;
            b.Append(']');
          end;
          b.Append(CHARS_RELATIONSHIP[t.relationship]);
          b.Append(' ');
          if (t.code <> '') then
          begin
            app(pmTarget, g.target, t.code);
            if (t.productList.Count > 0) then
            begin
              b.Append('[');
              f := true;
              for d in t.productList do
              begin
                if f then f := false else b.Append(', ');
                app(pmProduct, d.system, d.code);
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

function TFHIRStructureMapUtilities.parse(text, sourceName : String) : TFHIRStructureMap;
var
  lexer : TFHIRPathLexer;
begin
  lexer := TFHIRPathLexer5.Create(fpV2, text);
  try
    lexer.SourceName := sourceName;
    if (lexer.done()) then
      raise EFHIRException.create('Map Input cannot be empty');
    lexer.skipWhitespaceAndComments();
    lexer.token('map');
    result := TFHIRStructureMap.Create;
    try
      result.LocationData.ParseStart := lexer.CurrentStartLocation;
      result.Url := lexer.readConstant('url');
      result.id := result.url.Substring(result.url.LastIndexOf('/')+1);
      lexer.token('=');
      result.Name := lexer.readConstant('name');
      lexer.skipWhitespaceAndComments();
      result.status := PublicationStatusDraft;

      while (lexer.hasToken('conceptmap')) do
        parseConceptMap(result, lexer);

      while (lexer.hasToken('uses')) do
        parseUses(result, lexer);
      while (lexer.hasToken('imports')) do
        parseImports(result, lexer);

      while (not lexer.done()) do
        parseGroup(result, lexer);

      result.text := TFhirNarrative.Create;
      result.text.status := NarrativeStatusGenerated;
      result.text.div_ := TFHIRXhtmlParser.parse(FWorker.lang, xppReject, [], '<div><pre>'+FormatTextToXML(text, xmlText)+'</pre></div>');
      result.LocationData.ParseFinish := lexer.CurrentLocation;
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
  prefixes : TFslStringDictionary;
  g : TFhirConceptMapGroup;
  e : TFhirConceptMapGroupElement;
  tgt : TFhirConceptMapGroupElementTarget;
  vs, vc, vt : String;
  eq : TFhirConceptMapRelationshipEnum;
begin
  lexer.token('conceptmap');
  map := TFhirConceptMap.create;
  result.ContainedList.add(map);
  id := lexer.readConstant('map id');
  if (id.startsWith('#')) then
    raise lexer.error('Concept Map identifier ('+id+') must not start with #');
  map.Id := id;
  lexer.token('{');
  lexer.skipWhitespaceAndComments();
  //    lexer.token('source');
  //    map.Source := new UriType(lexer.readConstant('source')));
  //    lexer.token('target');
  //    map.Source := new UriType(lexer.readConstant('target')));
  prefixes := TFslStringDictionary.create;
  try
    while (lexer.hasToken('prefix')) do
    begin
      lexer.token('prefix');
      n := lexer.take();
      lexer.token('=');
      v := lexer.readConstant('prefix url');
      prefixes.add(n, v);
    end;
    while (lexer.hasToken('unmapped')) do
    begin
      lexer.token('unmapped');
      lexer.token('for');
      n := readPrefix(prefixes, lexer);
      g := getGroup(map, n, '');
      if g.unmapped = nil then
        g.unmapped := TFhirConceptMapGroupUnmapped.Create;
      lexer.token('=');
      v := lexer.take();
      if (v = 'provided') then
        g.unmapped.Mode := ConceptMapGroupUnmappedModeProvided
      else
        raise lexer.error('Only unmapped mode PROVIDED is supported at this time');
    end;
    while (not lexer.hasToken('}')) do
    begin
      vs := readPrefix(prefixes, lexer);
      lexer.token(':');
      if lexer.isStringConstant then
        vc := lexer.readConstant('token')
      else
        vc := lexer.take();
      eq := readEquivalence(lexer);
      if (eq <> ConceptMapRelationshipNotRelatedTo) then
        vt := readPrefix(prefixes, lexer)
      else
        vt := '';

      e := getGroup(map, vs, vt).elementList.Append;
      e.Code := vc;
      tgt := e.targetList.Append;
      tgt.relationship := eq;
      if (tgt.relationship <> ConceptMapRelationshipNotRelatedTo) then
      begin
        lexer.token(':');
        if lexer.isStringConstant then
          tgt.Code := lexer.readConstant('token')
        else
          tgt.Code := lexer.take();
      end;
      if (lexer.hasComment())  then
        tgt.Comment := lexer.take().substring(2).trim();
    end;
    lexer.token('}');
  finally
    prefixes.Free;
  end;
end;


function TFHIRStructureMapUtilities.readPrefix(prefixes : TFslStringDictionary; lexer : TFHIRPathLexer) : String;
var
  prefix : String;
begin
  prefix := lexer.take();
  if (not prefixes.containsKey(prefix)) then
    raise EFHIRException.create('Unknown prefix "'+prefix+'"');
  result := prefixes[prefix];
end;

function TFHIRStructureMapUtilities.readEquivalence(lexer : TFHIRPathLexer) : TFhirConceptMapRelationshipEnum;
var
  token : string;
begin
  token := lexer.take();
  if (token = '??') then
    result := ConceptMapRelationshipNull
  else if (token = '-') then
    result := ConceptMapRelationshipRelatedTo
  else if (token = '=') then
    result := ConceptMapRelationshipEquivalent
  else if (token = '!=') then
    result := ConceptMapRelationshipNotRelatedTo
  else if (token = '<') then
    result := ConceptMapRelationshipSourceIsNarrowerThanTarget
  else if (token = '>') then
    result := ConceptMapRelationshipSourceIsBroaderThanTarget
  else
    raise EFHIRException.create('Unknown equivalence token "'+token+'"');
end;

function urlMatches(mask, url : String) : boolean;
begin
  result := (url.length > mask.length) and url.startsWith(mask.substring(0, mask.indexOf('*'))) and url.endsWith(mask.substring(mask.indexOf('*')+1));
end;

function TFHIRStructureMapUtilities.findMatchingMaps(value: String): TFslList<TFHIRStructureMap>;
var
  sm : TFHIRStructureMap;
begin
  result := TFslList<TFHIRStructureMap>.create;
  try
    if (value.contains('*')) then
    begin
      for sm in FLib.Values do
        if (urlMatches(value, sm.url)) then
          result.add(sm.Link);
    end
    else
    begin
      if FLib.TryGetValue(value, sm) then
        result.add(sm.Link);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStructureMapUtilities.fromEnum(s : String; codes : Array of String; lexer : TFHIRPathLexer) : integer;
begin
  result := StringArrayIndexOfSensitive(codes, s);
  if result = -1 then
    raise lexer.error('The code "'+s+'" is not known');
end;


procedure TFHIRStructureMapUtilities.parseUses(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
var
  st : TFHIRStructureMapStructure;
begin
  lexer.token('uses');
  st := result.StructureList.Append;
  st.Url := lexer.readConstant('url');
  if lexer.hasToken('alias') then
  begin
    lexer.token('alias');
    st.alias := lexer.take;
  end;
  lexer.token('as');
  st.Mode := TFhirStructureMapModelModeEnum(fromEnum(lexer.take(), CODES_TFhirStructureMapModelModeEnum, lexer));
  lexer.skiptoken(';');
  if (lexer.hasComment()) then
    st.Documentation := lexer.take().substring(2).trim();
  lexer.skipWhitespaceAndComments();
end;

procedure TFHIRStructureMapUtilities.parseImports(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
begin
  lexer.token('imports');
  result.ImportList.add(TFHIRCanonical.Create(lexer.readConstant('url')));
  lexer.skiptoken(';');
  if (lexer.hasComment()) then
    lexer.next();
  lexer.skipWhitespaceAndComments();
end;

procedure TFHIRStructureMapUtilities.parseGroup(result : TFHIRStructureMap; lexer : TFHIRPathLexer);
var
  group : TFHIRStructureMapGroup;
  newFmt : boolean;
  loc : TSourceLocation;
begin
  loc := lexer.CurrentStartLocation;
  lexer.token('group');
  group := result.GroupList.Append;
  group.LocationData.ParseStart := loc;
  newFmt := false;
  if (lexer.hasToken('for')) then
  begin
    lexer.token('for');
    if (lexer.current = 'type') then
    begin
      lexer.token('type');
      lexer.token('+');
      lexer.token('types');
      group.TypeMode := StructureMapGroupTypeModeTYPEANDTYPES;
    end
    else
    begin
      lexer.token('types');
      group.TypeMode := StructureMapGroupTypeModeTYPES;
     end;
  end;

  group.Name := lexer.take();

  if (lexer.hasToken('(')) then
  begin
    newFmt := true;
    lexer.take();
    while (not lexer.hasToken(')')) do
    begin
      parseInput(group, lexer, true);
      if (lexer.hasToken(',')) then
        lexer.token(',');
    end;
    lexer.take();
  end;
  if (lexer.hasToken('extends')) then
  begin
    lexer.next();
    group.extends := lexer.take();
  end;
  if (newFmt) then
  begin
    if (lexer.hasToken('<')) then
    begin
      lexer.token('<');
      lexer.token('<');
      if (lexer.hasToken('types')) then
      begin
        lexer.token('types');
        group.TypeMode := StructureMapGroupTypeModeTYPES;
      end
      else
      begin
        lexer.token('type');
        lexer.token('+');
        group.TypeMode := StructureMapGroupTypeModeTYPEANDTYPES;
      end;
      lexer.token('>');
      lexer.token('>');
    end;
    lexer.token('{');
  end;
  lexer.skipWhitespaceAndComments();
  if (newFmt) then
  begin
    while (not lexer.hasToken('}')) do
    begin
      if (lexer.done()) then
        raise lexer.error('premature termination expecting "endgroup"');
      parseRule(group.ruleList, lexer, true);
    end;
  end
  else
  begin
    while (lexer.hasToken('input')) do
      parseInput(group, lexer, false);
    while (not lexer.hasToken('endgroup')) do
    begin
      if (lexer.done()) then
        raise lexer.error('premature termination expecting "endgroup"');
      parseRule(group.ruleList, lexer, false);
    end;
  end;
  lexer.next();
  if (newFmt and lexer.hasToken(';')) then
    lexer.next();
  lexer.skipWhitespaceAndComments();
  group.LocationData.ParseFinish := lexer.CurrentLocation;
end;

procedure TFHIRStructureMapUtilities.parseInput(group : TFHIRStructureMapGroup; lexer : TFHIRPathLexer; newFmt : boolean);
var
  input : TFHIRStructureMapGroupInput;
begin
  input := group.inputList.Append;
  if (newFmt) then
    input.Mode := TFhirStructureMapInputModeEnum(fromEnum(lexer.take(), CODES_TFhirStructureMapInputModeEnum, lexer))
  else
    lexer.token('input');
  input.Name := lexer.take();
  if (lexer.hasToken(':')) then
  begin
    lexer.token(':');
    input.Type_ := lexer.take();
  end;
  if (not newFmt) then
  begin
    lexer.token('as');
    input.Mode := TFhirStructureMapInputModeEnum(fromEnum(lexer.take(), CODES_TFhirStructureMapInputModeEnum, lexer));
    if (lexer.hasComment()) then
      input.Documentation := lexer.take().substring(2).trim();
    lexer.skipToken(';');
    lexer.skipWhitespaceAndComments();
  end;
end;

procedure TFHIRStructureMapUtilities.parseRule(list : TFhirStructureMapGroupRuleList; lexer : TFHIRPathLexer; newFmt : boolean);
var
  rule : TFhirStructureMapGroupRule;
  done : boolean;
begin
  rule := list.Append;
  rule.LocationData.ParseStart := lexer.CurrentStartLocation;
  if not newFMt then
  begin
    rule.Name := lexer.takeDottedToken();
    lexer.token(':');
    lexer.token('for');
  end;
  done := false;
  while (not done) do
  begin
    parseSource(rule, lexer);
    done := not lexer.hasToken(',');
    if (not done) then
      lexer.next();
  end;
  if (newFmt and lexer.hasToken('->')) or (not newFmt and lexer.hasToken('make')) then
  begin
    if newFmt then
      lexer.token('->')
    else
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
      lexer.skipWhitespaceAndComments();
      while (not lexer.hasToken('}')) do
      begin
        if (lexer.done()) then
          raise EFHIRException.create('premature termination expecting "}" in nested group');
        parseRule(rule.ruleList, lexer, newFmt);
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
  if (isSimpleSyntax(rule)) then
  begin
    rule.sourceList[0].variable := AUTO_VAR_NAME;
    rule.targetList[0].variable := AUTO_VAR_NAME;
    rule.targetList[0].transform := StructureMapTransformCreate; // with no parameter - e.g. imply what is to be created
    // no dependencies - imply what is to be done based on types
  end;
  if (newFmt) then
  begin
    if (lexer.isConstant(true)) then
    begin
      rule.Name := lexer.take();
      if rule.name.startsWith('"') and rule.name.endsWith('"') then
      begin
        rule.name := rule.name.subString(1);
        rule.name := rule.name.subString(0, rule.name.length - 1);
      end;
    end
    else
    begin
      if (rule.sourceList.count <> 1) or (rule.sourceList[0].element = '') then
        raise lexer.error('Complex rules must have an explicit name');
      if (rule.sourceList[0].type_ <> '') then
        rule.Name := rule.sourceList[0].Element+'-'+rule.sourceList[0].type_
      else
        rule.Name := rule.sourceList[0].Element;
    end;
    lexer.token(';');
  end;
  if (lexer.hasComment()) then
    rule.Documentation := lexer.take().substring(2).trim();
  lexer.skipWhitespaceAndComments();
  rule.LocationData.ParseFinish := lexer.CurrentLocation;
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
  source : TFHIRStructureMapGroupRuleSource;
  node : TFHIRPathExpressionNode;
begin
  source := rule.sourceList.Append;
  source.LocationData.ParseStart := lexer.CurrentStartLocation;
  source.Context := lexer.take();

  if (source.Context = 'search') and lexer.hasToken('(') then
  begin
    source.Context := '@search';
    lexer.take();
    node := fpp.parse(lexer);
    source.element := node.toString();
    source.elementElement.Tag := node;
    lexer.token(')');
  end
  else if (lexer.hasToken('.')) then
  begin
    lexer.token('.');
    source.element := lexer.take();
  end;
  if (lexer.hasToken(':')) then
  begin
    // type and cardinality
    lexer.token(':');
    source.type_ := lexer.takeDottedToken();
    if (not lexer.hasToken(['as', 'first', 'last', 'not_first', 'not_last', 'only_one', 'default'])) then
    begin
      source.Min := lexer.take();
      lexer.token('..');
      source.Max := lexer.take();
    end;
  end;
  if (lexer.hasToken('default')) then
  begin
    lexer.token('default');
    source.DefaultValue := TFhirString.create(lexer.readConstant('default value'));
  end;
  if (StringArrayExistsSensitive(['first', 'last', 'not_first', 'not_last', 'only_one'], lexer.current)) then
    source.ListMode := TFhirStructureMapSourceListModeEnum(fromEnum(lexer.take(), CODES_TFhirStructureMapSourceListModeEnum, lexer));
  if (lexer.hasToken('as')) then
  begin
    lexer.take();
    source.variable := lexer.take();
  end;
  if (lexer.hasToken('where')) then
  begin
    lexer.take();
    node := fpp.parse(lexer);
    source.Condition := node.toString();
    source.conditionElement.Tag := node;
  end;
  if (lexer.hasToken('check')) then
  begin
    lexer.take();
    node := fpp.parse(lexer);
    source.check := node.toString();
    source.checkElement.Tag := node;
  end;
  if (lexer.hasToken('log')) then
  begin
    lexer.take();
    node := fpp.parse(lexer);
    source.logMessage := node.toString();
    source.logMessageElement.Tag := node;
  end;
  source.LocationData.ParseFinish := lexer.CurrentLocation;
end;

procedure TFHIRStructureMapUtilities.parseTarget(rule : TFHIRStructureMapGroupRule; lexer : TFHIRPathLexer);
var
  target : TFHIRStructureMapGroupRuleTarget;
  isConstant : boolean;
  name, id, start : String;
  node : TFHIRPathExpressionNode;
  p : TFhirStructureMapGroupRuleTargetParameter;
begin
  target := rule.targetList.Append;
  target.LocationData.ParseStart := lexer.CurrentStartLocation;
  start := lexer.take();
  if (lexer.hasToken('.')) then
  begin
    target.Context := start;
    target.ContextType := StructureMapContextTypeVariable;
    start := '';
    lexer.token('.');
    target.Element := lexer.take();
  end;
  isConstant := false;
  if (lexer.hasToken('=')) then
  begin
    if (start <> '') then
       target.Context := start;
    lexer.token('=');
    isConstant := lexer.isConstant(true);
    name := lexer.take();
  end
  else
   name := start;

  if (name = '(') then
  begin
    // inline fluentpath expression
    target.transform := StructureMapTransformEVALUATE;
    p := target.parameterList.Append;
    node := fpp.parse(lexer);
    p.Tag := node;
    p.Value := TFHIRString.create(node.toString());
    lexer.token(')');
  end
  else if (lexer.hasToken('(')) then
  begin
    target.Transform := TFhirStructureMapTransformEnum(fromEnum(name, CODES_TFhirStructureMapTransformEnum, lexer));
    lexer.token('(');
    if (target.Transform = StructureMapTransformEVALUATE) then
    begin
      parseParameter(target, lexer);
      lexer.token(',');
      p := target.parameterList.Append;
      node := fpp.parse(lexer);
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
  else if (name <> '') then
  begin
    target.Transform := StructureMapTransformCopy;
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
  if (lexer.hasToken('as')) then
  begin
    lexer.take();
    target.Variable := lexer.take();
  end;
  if StringArrayExistsInsensitive(['first', 'last', 'share', 'only_one'], lexer.current) then
  begin
    if (lexer.current = 'share') then
    begin
      target.listMode := target.listMode + [StructureMapTargetListModeShare];
      lexer.next();
      target.ListRuleId := lexer.take();
    end
    else if (lexer.current = 'first') then
      target.listMode := target.listMode + [StructureMapTargetListModeFirst]
    else
      target.listMode := target.listMode; // + [MapListModeLAST];
    lexer.next();
  end;
  target.LocationData.ParseFinish := lexer.CurrentLocation;
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

function TFHIRStructureMapUtilities.readConstant(s : String; lexer : TFHIRPathLexer) : TFHIRDataType;
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


procedure TFHIRStructureMapUtilities.transform(appInfo : TFslObject; source : TFHIRObject; map : TFHIRStructureMap; target : TFHIRObject);
var
  vars : TVariables;
  dbg : TFHIRStructureMapDebugContext;
begin
  log('Start Transform '+map.Url);
  vars := TVariables.create;
  try
    vars.add(vmINPUT, 'src', source.Link);
    if (target <> nil) then
      vars.add(vmOUTPUT, 'tgt', target.Link);
    dbg := debug(nil, appInfo, map, nil, nil, nil, vars);
    try
      executeGroup('', appInfo, map, vars, map.GroupList[0], true, dbg);
    finally
      dbg.free;
    end;
//    if target is TFHIRMMElement then
//      (target as TFHIRMMElement).sort;
  finally
    vars.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.executeGroup(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; atRoot : boolean; dbgContext : TFHIRStructureMapDebugContext);
var
  r : TFHIRStructureMapGroupRule;
  rg : TFHIRStructureMapGroup;
  rm  : TFHIRStructureMap;
  dbg : TFHIRStructureMapDebugContext;
begin
  log(indent+'Group : '+group.Name+'; vars = '+vars.summary);
  dbg := debug(dbgContext, appInfo, map, group, nil, nil, vars);
  try
    // todo: check inputs
    if (group.extends <> '') then
    begin
      resolveGroupReference(map, group, group.extendsElement, rg, rm);
      executeGroup(indent+' ', appInfo, rm, vars, rg, false, dbg);
    end;

    for r in group.ruleList do
      executeRule(indent+'  ', appInfo, map, vars, group, r, atRoot, dbg);
  finally
    dbg.free;
  end;
end;

procedure TFHIRStructureMapUtilities.executeRule(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; group : TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule; atRoot : boolean; dbgContext : TFHIRStructureMapDebugContext);
var
  srcVars, v, vdef : TVariables;
  sources : TFslList<TVariables>;
  t : TFHIRStructureMapGroupRuleTarget;
  childrule : TFHIRStructureMapGroupRule;
  dependent : TFHIRStructureMapGroupRuleDependent;
  src, tgt : TFHIRObject;
  srcType, tgtType : String;
  rg : TFHIRStructureMapGroup;
  rm  : TFHIRStructureMap;
  dbg1, dbg2 : TFHIRStructureMapDebugContext;
begin
  log(indent+'rule : "'+rule.Name+'"; vars = '+vars.summary);
  srcVars := vars.copy();
  try
    if (rule.sourceList.count <> 1) then
      raise EFHIRException.create('Rule '+group.name+' not handled yet');
    dbg1 := debug(dbgContext, appInfo, map, group, rule, nil, vars);
    try
      sources := analyseSource(group.name, rule.name, appInfo, srcVars, rule.sourceList[0], map.url, indent);
      try
        if (sources <> nil) then
        begin
          for v in sources do
          begin
            for t in rule.TargetList do
            begin
              dbg2 := debug(dbg1, appInfo, map, group, rule, t, v);
              try
                if rule.sourceList.Count = 1 then
                  processTarget(rule.name, appInfo, v, map, group, t, rule.sourceList[0].variable, atRoot)
                else
                  processTarget(rule.name, appInfo, v, map, group, t, '', atRoot);
              finally
                dbg2.Free;
              end;
            end;

            if (rule.ruleList.Count > 0) then
            begin
              for childrule in rule.ruleList do
                executeRule(indent +'  ', appInfo, map, v, group, childrule, false, dbg1);
            end
            else if (rule.dependentList.Count > 0) then
            begin
              for dependent in rule.dependentList do
                executeDependency(indent+'  ', appInfo, map, v, group, dependent, dbg1);
            end
            else if (rule.sourceList.count = 1) and (rule.sourceList[0].variable <> '') and (rule.targetList.count = 1) and
               (rule.targetList[0].variable <> '') and (rule.targetList[0].transform = StructureMapTransformCreate) and (not rule.targetList[0].hasParameterList) then
            begin
              // simple inferred, map by type
              src := v.get(vmINPUT, rule.sourceList[0].variable);
              if (src = nil) then
                raise Exception.Create('No source at rule '+map.url+'#'+group.name+'.'+rule.name);
              tgt := v.get(vmOUTPUT, rule.targetList[0].variable);
              if (tgt = nil) then
                raise Exception.Create('No target at rule '+map.url+'#'+group.name+'.'+rule.name);
              srcType := src.fhirType();
              tgtType := tgt.fhirType();
              resolveGroupByTypes(map, rule.name, group, rule, srcType, tgtType, rg, rm);
              vdef := TVariables.create;
              try
                vdef.add(vmINPUT, rg.inputList[0].name, src.link);
                vdef.add(vmOUTPUT, rg.inputList[1].name, tgt.link);
                executeGroup(indent+'  ', appInfo, rm, vdef, rg, false, dbg1);
              finally
                vdef.Free;
              end;
            end;
          end;
        end;
      finally
        sources.Free;
      end;
    finally
      dbg1.Free;
    end;
  finally
    srcVars.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.resolveGroupByTypes(map: TFHIRStructureMap; ruleid: String; source: TFHIRStructureMapGroup; rule : TFHIRStructureMapGroupRule; srcType, tgtType: String; var tgtGroup: TFHIRStructureMapGroup; var tgtMap: TFHIRStructureMap);
var
  grp : TFHIRStructureMapGroup;
  imp : TFhirCanonical;
  impMapList : TFslList<TFHIRStructureMap>;
  impMap : TFHIRStructureMap;
begin
  tgtMap := nil;
  tgtGroup := nil;
  for grp in map.groupList do
  begin
    if (matchesByType(map, ruleId, grp, srcType, tgtType)) then
    begin
      if (tgtMap = nil) then
      begin
          tgtMap := map;
          tgtGroup := grp;
      end
      else
        raise EFHIRException.create('Multiple possible matches looking for rule for "'+srcType+'"/"'+tgtType+'", from rule "'+ruleid+'"');
    end;
    if (tgtMap <> nil) then
      exit;
  end;

  for imp in map.importList do
  begin
    impMapList := findMatchingMaps(imp.value);
    try
      if (impMapList.count = 0) then
        raise EFHIRException('Unable to find map(s) for '+imp.value);
      for impMap in impMapList do
      begin
        if (impMap.url <> map.url) then
        begin
          for grp in impMap.groupList do
          begin
            if (matchesByType(impMap, ruleId, grp, srcType, tgtType)) then
              if (tgtMap = nil) then
              begin
                tgtMap := impMap;
                tgtGroup := grp;
              end
              else
                raise EFHIRException.create('Multiple possible matches for rule for "'+srcType+'"/"'+tgtType+'" in '+tgtMap.url+' and '+impMap.url+', from rule "'+ruleid+'"');
          end;
        end;
      end;
    finally
      impMapList.Free;
    end;
  end;
  if (tgtMap = nil) then
      raise EFHIRException.create('No matches found for rule for "'+srcType+'" to "'+tgtType+'" from '+map.url+', from rule "'+ruleid+'"');
end;

procedure TFHIRStructureMapUtilities.resolveGroupReference(map : TFHIRStructureMap; source : TFHIRStructureMapGroup; name : TFhirId; var tgtGroup : TFHIRStructureMapGroup; var tgtMap : TFHIRStructureMap);
var
  grp : TFHIRStructureMapGroup;
  imp : TFhirCanonical;
  impMapList : TFslList<TFHIRStructureMap>;
  impMap : TFHIRStructureMap;
begin
  if name.Tag is TResolvedGroup then
  begin
    tgtGroup := (name.Tag as TResolvedGroup).FGroup;
    tgtMap := (name.Tag as TResolvedGroup).FMap;
    exit;
  end;

  tgtGroup := nil;
  tgtMap := nil;

  for grp in map.groupList do
  begin
    if (grp.name = name.value) then
    begin
      if (tgtMap = nil) then
      begin
        tgtGroup := grp;
        tgtMap := map;
      end
      else
        raise EFHIRException.Create('Multiple possible matches for rule "'+name.value+'"');
    end;
  end;
  if (tgtGroup <> nil) then
  begin
 //   name.Tag := TResolvedGroup.Create(tgtMap.Link, tgtGroup.link);
    exit;
  end;

  for imp in map.importList do
  begin
    impMapList := findMatchingMaps(imp.value);
    try
      if (impMapList.count = 0) then
        raise EFHIRException.create('Unable to find map(s) for '+imp.value);
      for impMap in impMapList do
      begin
        if (impMap.url <> map.url) then
        begin
          for grp in impMap.groupList do
          begin
            if (grp.name = name.value) then
            begin
              if (tgtMap = nil) then
              begin
                tgtMap := impMap;
                tgtGroup := grp;
              end
              else
                raise EFHIRException.create('Multiple possible matches for rule group "'+name.value+'" in '+
                 tgtMap.url+'#'+tgtGroup.Name+' and '+
                 impMap.url+'#'+grp.name);
            end;
          end;
        end;
      end;
    finally
      impMapList.free;
    end;
  end;
  if (tgtGroup = nil) then
    raise EFHIRException.create('No matches found for rule "'+name.value+'". Reference found in '+map.url);
//  name.Tag := TResolvedGroup.Create(tgtMap.Link, tgtGroup.link);
end;

procedure TFHIRStructureMapUtilities.executeDependency(indent : String; appInfo : TFslObject; map : TFHIRStructureMap; vin : TVariables; group : TFHIRStructureMapGroup; dependent : TFHIRStructureMapGroupRuleDependent; dbgContext : TFHIRStructureMapDebugContext);
var
  targetMap : TFHIRStructureMap;
  target : TFHIRStructureMapGroup;
  v : TVariables;
  i : integer;
  input : TFHIRStructureMapGroupInput;
  vr : TFHIRString;
  mode : TVariableMode;
  vv : TFHIRObject;
begin
  resolveGroupReference(map, group, dependent.nameElement, target, targetMap);

  if (target.InputList.count <> dependent.variableList.count) then
    raise EFHIRException.create('Rule ''+dependent.Name+'' has '+Integer.toString(target.InputList.count)+' but the invocation has '+Integer.toString(dependent.variableList.count)+' variables');

  v := TVariables.create;
  try
    for i := 0 to target.InputList.count - 1 do
    begin
      input := target.InputList[i];
      vr := dependent.variableList[i];
      if input.mode = StructureMapInputModeSource then
        mode := vmINPUT
      else
        mode := vmOUTPUT;
      vv := vin.get(mode, vr.Value);
      if (vv = nil) then
        raise EFHIRException.create('Rule ''+dependent.Name+'' '+CODES_TVariableMode[mode]+' variable "'+input.Name+'" has no value');
      v.add(mode, input.Name, vv.Link);
    end;
    executeGroup(indent+'  ', appInfo, targetMap, v, target, false, dbgContext);
  finally
    v.free;
  end;
end;


function TFHIRStructureMapUtilities.getActualType(map: TFHIRStructureMap; statedType: String): String;
begin
end;

procedure TFHIRStructureMapUtilities.getChildrenByName(item : TFHIRObject; name : String; result : TFslList<TFHIRObject>);
var
  v : TFHIRSelection;
  list : TFHIRSelectionList;
begin
  list := TFHIRSelectionList.create;
  try
    item.ListChildrenByName(name, list);
    for v in list do
      if (v <> nil) then
        result.add(v.value.Link);
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

function TFHIRStructureMapUtilities.analyseSource(groupId, ruleId : String; appInfo : TFslObject; vars : TVariables; src : TFHIRStructureMapGroupRuleSource; errorlocation, indent : String) : TFslList<TVariables>;
var
  b : TFHIRObject;
  expr : TFHIRPathExpressionNode;
  items, work : TFslList<TFHIRObject>;
  item : TFHIRObject;
  v : TVariables;
  search : String;
  cb : TFslStringBuilder;
begin
  items := TFslList<TFHIRObject>.create;
  try
    if (src.context = '@search') then
    begin
      expr := src.contextElement.Tag as TFHIRPathExpressionNode;
      if (expr = nil) then
      begin
        expr := fpe.parse(src.condition);
        src.contextElement.tag := expr;
      end;
      search := fpe.evaluateToString(vars, src.contextElement, expr); // src.contextElement serves as a holder of nothing to ensure that variables are processed correctly
      work := services.performSearch(appInfo, search);
      try
        items.addAll(work);
      finally
        work.free;
      end;
    end
    else
    begin
      b := vars.get(vmINPUT, src.Context);
      if (b = nil) then
        raise EFHIRException.create('Unknown input variable '+src.context+' in '+errorlocation+'#'+groupId+'.'+ruleId+' (vars = '+vars.summary+')');
      if (src.element = '') then
        items.Add(b.Link)
      else
      begin
        getChildrenByName(b, src.element, items);
        if (items.Empty and (src.defaultValue <> nil)) then
          items.Add(src.defaultValue.Link);
      end;
    end;

    if src.type_ <> '' then
    begin
      work := TFslList<TFHIRObject>.create;
      try
        for item in items do
          if (item <> nil) and not item.hasType(src.type_) then
            work.Add(item.Link);
        items.RemoveAll(work);
      finally
        work.Free;
      end;
    end;

    if (src.Condition <> '') then
    begin
      expr := TFHIRPathExpressionNode(src.conditionElement.tag);
      if (expr = nil) then
      begin
        expr := fpe.parse(src.condition);
        src.conditionElement.tag := expr;
      end;
      work := TFslList<TFHIRObject>.create;
      try
        for item in items do
        begin
          if (not fpe.evaluateToBoolean(appinfo, nil, item, expr)) then
            work.Add(item.Link);
        end;
        items.RemoveAll(work);
      finally
        work.Free;
      end;
    end;

    if (src.check <> '') then
    begin
      expr := TFHIRPathExpressionNode(src.checkElement.tag);
      if (expr = nil) then
      begin
        expr := fpe.parse(src.check);
        src.checkElement.tag := expr;
      end;
      for item in items do
        if (not fpe.evaluateToBoolean(appinfo, nil, item, expr)) then
          raise EFHIRException.create('Check condition failed');
    end;

    if (src.logMessage <> '') then
    begin
      expr := TFHIRPathExpressionNode(src.logMessageElement.tag);
      if (expr = nil) then
      begin
        expr := fpe.parse(src.logMessage);
        src.logMessageElement.tag := expr;
      end;
      cb := TFslStringBuilder.Create;
      try
        for item in items do
          cb.CommaAdd(fpe.evaluateToString(vars, item, expr));
        if (cb.Length > 0) then
          FServices.log(cb.toString());
      finally
        cb.Free;
      end;
    end;

    if (src.listMode <> StructureMapSourceListModeNull) and not items.Empty then
    begin
      case src.listMode of
        StructureMapSourceListModeFirst: if items.Count > 1 then items.DeleteRange(1, items.Count-1);
        StructureMapSourceListModeNotFirst: items.Delete(0);
        StructureMapSourceListModeLast: if items.Count > 1 then items.DeleteRange(0, items.Count-2);
        StructureMapSourceListModeNotLast:  items.Delete(items.Count - 1);
        StructureMapSourceListModeOnlyOne: if (items.count > 1) then
            raise EFHIRException.create('Rule "'+ruleId+'": Check condition failed: the collection has more than one item');
      end;
    end;
    result := TFslList<TVariables>.create;
    try
      for item in items do
      begin
        v := vars.copy();
        try
          if (src.variable <> '') then
            v.add(vmINPUT, src.variable, item.link);
          result.add(v.link);
        finally
          v.free;
        end;
      end;
      result.link;
    finally
      result.free;
    end;
  finally
    items.Free;
  end;
end;


procedure TFHIRStructureMapUtilities.processTarget(ruleId : String; appInfo : TFslObject; vars : TVariables; map : TFHIRStructureMap; group : TFhirStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; srcVar : String; atRoot: boolean);
var
  dest, v, v1 : TFHIRObject;
begin
  dest := nil;
  if tgt.context <> '' then
  begin
    dest := vars.get(vmOUTPUT, tgt.Context);
    if (dest = nil) then
        raise EFHIRException.create('Rule "'+ruleId+'": target context not known: '+tgt.context);
    if (tgt.element = '') then
        raise EFHIRException.create('Rule "'+ruleId+'": Not supported yet');
  end;

  v := nil;
  try
    if (tgt.transform <> StructureMapTransformNull) then
    begin
      v := runTransform(ruleId, appInfo, map, group, tgt, vars, dest, tgt.element, srcVar, atRoot);
      if (v <> nil) and (dest <> nil) then
      begin
        v1 := dest.setProperty(tgt.element, v.Link).link; // reset v because some implementations may have to rewrite v when setting the value
        v.Free;
        v := v1;
      end
    end
    else if (dest <> nil) then
    begin
      v := dest.createPropertyValue(tgt.element);
      dest.setProperty(tgt.element, v.link);
    end;
    if (tgt.variable <> '') and (v <> nil) then
      vars.add(vmOUTPUT, tgt.variable, v.Link);
  finally
    v.Free;
  end;
end;

function TFHIRStructureMapUtilities.runTransform(ruleId : String; appInfo : TFslObject; map : TFHIRStructureMap; group : TFHIRStructureMapGroup; tgt : TFHIRStructureMapGroupRuleTarget; vars : TVariables; dest : TFHIRObject; element, srcVar : String; root : boolean) : TFHIRObject;
var
  expr : TFHIRPathExpressionNode;
  v : TFHIRSelectionList;
  src, len, tn, id : String;
  i, l : integer;
  b : TFHIRObject;
  types : TArray<String>;
  uses_ : TFhirStructureMapStructure;
  sb : TStringBuilder;
  cc : TFhirCodeableConcept;
begin
  case tgt.Transform of
    StructureMapTransformCreate :
      begin
      if (tgt.parameterList.isEmpty) then
      begin
        // we have to work out the type. First, we see if there is a single type for the target. If there is, we use that
        types := dest.getTypesForProperty(element).Split(['|']);
        if (length(types) = 1) and not ('*' = types[0]) and not (types[0] = 'Resource') then
          tn := types[0]
        else if (srcVar <> '') then
          tn := determineTypeFromSourceType(map, ruleId, group, tgt, vars.get(vmINPUT, srcVar), types)
        else
          raise EFHIRException.create('Cannot determine type implicitly because there is no single input variable');
      end
      else
        tn := getParamStringNoNull(vars, tgt.parameterList[0], tgt.toString());
      // ok, now we resolve the type name against the import statements
      for uses_ in map.structureList do
      begin
        if (uses_.mode = StructureMapModelModeTarget) and (uses_.alias = tn) then
        begin
          tn := uses_.url;
          break;
        end;
      end;
      if FServices <> nil then
        result := FServices.createType(appInfo, tn)
      else
        result := FFactory.makeByName(tn);
      try
        if (result.isResource and (result.fhirType <> 'Parameters')) then
        begin
  //          res.setIdBase(tgt.getParameter().size() > 1 ? getParamString(vars, tgt.getParameter().get(0)) : UUID.randomUUID().toString().toLowerCase());
          if (Fservices <> nil) then
            FServices.createResource(appInfo, result, root);
          if (tgt.HasTag('profile')) then
            result.tags['profile'] := tgt.tags['profile'];
        end;
        result.Link;
      finally
        result.Free;
      end;
      end;
    StructureMapTransformCOPY :
      begin
        result := getParam(vars, tgt.ParameterList[0]).Link;
      end;
    StructureMapTransformEVALUATE :
      begin
        expr := tgt.Tag as TFHIRPathExpressionNode;
        if (expr = nil) then
        begin
          expr := fpe.parse(getParamString(vars, tgt.ParameterList[1]));
          tgt.tag := expr;
        end;
        v := fpe.evaluate(nil, nil, getParam(vars, tgt.ParameterList[0]), expr);
        try
          if (v.count <> 1) then
            raise EFHIRException.create('evaluation of '+expr.toString()+' returned '+Integer.toString(v.count)+' objects');
          result := v[0].value.Link;
        finally
          v.Free;
        end;
      end;
    StructureMapTransformTRUNCATE :
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
    StructureMapTransformESCAPE :
      begin
        raise EFHIRException.create('Transform '+CODES_TFhirStructureMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    StructureMapTransformCAST :
      begin
      src := getParamString(vars, tgt.parameterList[0]);
      if (tgt.parameterList.Count = 1) then
        raise EFHIRException.create('Implicit type parameters on cast not yet supported');
      tn := getParamString(vars, tgt.parameterList[1]);
      if (tn = 'string') then
        result := TFHIRString.create(src)
      else
        raise EFHIRException.create('cast to '+tn+' not yet supported');
      end;
    StructureMapTransformAPPEND :
      begin
        sb := TStringBuilder.create(getParamString(vars, tgt.parameterList[0]));
        try
          for i := 1 to tgt.parameterList.count - 1 do
            sb.append(getParamString(vars, tgt.parameterList[i]));
          result := TFHIRString.create(sb.toString());
        finally
          sb.free;
        end;
      end;
    StructureMapTransformTRANSLATE :
      begin
        result := translate(appInfo, map, vars, tgt.ParameterList);
      end;
    StructureMapTransformREFERENCE :
      begin
        b := getParam(vars, tgt.parameterList[0]).link;
        try
        if (b = nil) then
          raise EFHIRException.create('Rule "'+ruleId+'": Unable to find parameter '+tgt.parameterList[0].primitiveValue);
        if not b.isResource then
          raise EFHIRException.create('Rule "'+ruleId+'": Transform engine cannot point at an element of type '+b.fhirType());
        id := b.id;
          if (id = '') then
          begin
            id := NewGuidId;
            b.id := id;
          end;
          result := TFHIRReference.create(b.fhirType()+'/'+id);
        finally
          b.Free;
        end;
      end;
    StructureMapTransformDATEOP :
      begin
        raise EFHIRException.create('Transform '+CODES_TFhirStructureMapTransformEnum[tgt.Transform]+' not supported yet');
      end;
    StructureMapTransformUUID :
      begin
        result := TFHIRId.Create(NewGuidId);
      end;
    StructureMapTransformPOINTER :
      begin
        b := getParam(vars, tgt.ParameterList[0]);
        if (b is TFHIRResource) then
          result := TFHIRUri.create('urn:uuid:'+TFHIRResource(b).Id)
        else
          raise EFHIRException.create('Transform engine cannot point at an element of type '+b.fhirType());
      end;
    StructureMapTransformCC:
      begin
      cc := TFHIRCodeableConcept.Create;
      try
        cc.codingList.Add(buildCoding(getParamStringNoNull(vars, tgt.parameterList[0], tgt.toString), getParamStringNoNull(vars, tgt.parameterList[1], tgt.toString())));
        result := cc.Link;
      finally
        cc.Free;
      end;
      end;
    StructureMapTransformC:
      result := buildCoding(getParamStringNoNull(vars, tgt.parameterList[0], tgt.toString()), getParamStringNoNull(vars, tgt.parameterList[1], tgt.toString()));
  else
    raise EFHIRException.create('Transform Unknown');
  end;
end;

function TFHIRStructureMapUtilities.buildCoding(uri, code : String) : TFHIRCoding;
var
  display : String;
begin
  // if we can get this as a valueSet, we will
//
//    ValidationResult vr = worker.validateCode(system, code, null);
//    if (vr != null && vr.getDisplay() != null)
//      display = vr.getDisplay();
  result := TFhirCoding.Create(uri, code);
  try
    result.display := display;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFHIRStructureMapUtilities.SetServices(const Value: TTransformerServices);
begin
  FServices.Free;
  FServices := Value;
end;

function TFHIRStructureMapUtilities.getParamString(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : String;
var
  b : TFHIRObject;
begin
  b := getParam(vars, parameter);
  if (b = nil) or not b.hasPrimitiveValue() then
    result := ''
  else
    result := b.primitiveValue();
end;

function TFHIRStructureMapUtilities.getParamStringNoNull(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter; message : String) : String;
var
  b : TFHIRObject;
begin
  b := getParam(vars, parameter);
  if (b = nil) then
    raise EFHIRException.Create('Unable to find a value for '+parameter.toString()+'. Context: '+message);
  if not b.hasPrimitiveValue() then
    raise EFHIRException.Create('Found a value for '+parameter.toString()+', but it has a type of '+b.fhirType()+' and cannot be treated as a string. Context: '+message);
  result := b.primitiveValue();
end;

function TFHIRStructureMapUtilities.isSimpleSyntax(rule: TFhirStructureMapGroupRule): boolean;
begin
  result := (rule.sourceList.count = 1) and (rule.sourceList[0].context <> '') and (rule.sourceList[0].element <> '') and (rule.sourceList[0].variable = '') and
        (rule.targetList.count = 1) and (rule.targetList[0].Context <> '') and (rule.targetList[0].Element <> '') and (rule.targetList[0].Variable = '') and (rule.targetList[0].parameterList.count = 0)
        and (rule.dependentList.count = 0) and (rule.ruleList.count = 0);
end;

procedure TFHIRStructureMapUtilities.log(s: String);
begin
  if FServices <> nil then
    FServices.log(s);
end;

function TFHIRStructureMapUtilities.matchesByType(map: TFHIRStructureMap; ruleId : String; grp: TFHIRStructureMapGroup; tnSrc, tnTgt: String): boolean;
begin
  if (grp.typeMode = StructureMapGroupTypeModeNull) then
    exit(false);
  if (grp.inputList.Count <> 2) or (grp.inputList[0].mode <> StructureMapInputModeSource) or (grp.inputList[1].mode <> StructureMapInputModeTarget) then
    exit(false);
  if (grp.inputList[0].type_ = '') or (grp.inputList[1].type_ = '') then
    exit(false);
  result := matchesType(map, tnSrc, grp.inputList[0].type_) and matchesType(map, tnTgt, grp.inputList[1].type_);
end;

function TFHIRStructureMapUtilities.matchesByType(map: TFHIRStructureMap; ruleId: String; grp: TFHIRStructureMapGroup; tn: String): boolean;
begin
  if (grp.typeMode <> StructureMapGroupTypeModeTypeAndTypes) then
    exit(false);
  if (grp.inputList.Count <> 2) or (grp.inputList[0].mode <> StructureMapInputModeSource) or (grp.inputList[1].mode <> StructureMapInputModeTarget) then
    exit(false);
  result := matchesType(map, tn, grp.inputList[0].type_);
end;

function TFHIRStructureMapUtilities.matchesType(map : TFHIRStructureMap; actualType, statedType : String): boolean;
var
  imp : TFhirStructureMapStructure;
  sd : TFhirStructureDefinition;
begin
  // check the aliases
  for imp in map.structureList do
  begin
    if (imp.alias <> '') and (statedType = imp.alias) then
    begin
      sd := FWorker.fetchStructureDefinition(imp.url);
      try
        if (sd <> nil) then
          statedType := sd.type_;
        break;
      finally
        sd.Free;
      end;
    end;
  end;

  if (isAbsoluteUrl(actualType)) then
  begin
    sd := FWorker.fetchStructureDefinition(actualType);
    try
      if (sd <> nil) then
        actualType := sd.type_;
    finally
      sd.Free;
    end;
  end;
  if (isAbsoluteUrl(statedType)) then
  begin
    sd := FWorker.fetchStructureDefinition(statedType) ;
    try
      if (sd <> nil) then
        statedType := sd.type_;
    finally
      sd.Free;
    end;
  end;
  result := actualType = statedType;
end;


function TFHIRStructureMapUtilities.getParam(vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameter) : TFHIRObject;
var
  p : TFhirDataType;
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


function TFHIRStructureMapUtilities.translate(appInfo : TFslObject; map : TFHIRStructureMap; vars : TVariables; parameter : TFHIRStructureMapGroupRuleTargetParameterList) : TFHIRObject;
var
  src : TFHIRObject;
  id, fld : String;
begin
  src := getParam(vars, parameter[0]);
  id := getParamString(vars, parameter[1]);
  fld := getParamString(vars, parameter[2]);
  result := translate(appInfo, map, src, id, fld);
end;

function TFHIRStructureMapUtilities.translate(appInfo : TFslObject; map : TFHIRStructureMap; source : TFHIRObject; conceptMapUrl : String; fieldToReturn : String): TFHIRObject;
var
  src, outcome : TFHIRCoding;
  b : TFslList<TFHIRObject>;
  uri, message : String;
  cmap : TFhirConceptMap;
  r : TFHIRResource;
  done : boolean;
  list : TFslList<TFhirConceptMapGroupElement>;
  g : TFhirConceptMapGroup;
  e : TFhirConceptMapGroupElement;
  tgt : TFhirConceptMapGroupElementTarget;
begin
  b := TFslList<TFHIRObject>.create;
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
      raise EFHIRException.create('Unable to translate source '+source.fhirType());

    if (conceptMapUrl.equals('http://hl7.org/fhir/ConceptMap/special-oid2uri')) then
    begin
      uri := FWorker.oid2Uri(src.Code);
      if (uri = '') then
        uri := 'urn:oid:'+src.Code;
      if ('uri'.equals(fieldToReturn)) then
        result := TFHIRUri.create(uri)
      else
        raise EFHIRException.create('Error in return code');
    end
    else
    begin
      cmap := nil;
      if (conceptMapUrl.startsWith('#')) then
      begin
        for r in map.ContainedList do
        begin
          if (r is TFHIRConceptMap) and (TFHIRConceptMap(r).Id = conceptMapUrl.substring(1)) then
            cmap := TFHIRConceptMap(r).Link;
        end;
      end
      else
        cmap := FWorker.fetchConceptMap(conceptMapUrl);
      try
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
            list := TFslList<TFhirConceptMapGroupElement>.create;
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
                  if (tgt.relationship <> ConceptMapRelationshipNotRelatedTo) then
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
                  else if (tgt.relationship = ConceptMapRelationshipNotRelatedTo) then
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
            raise EFHIRException.create(message);
          if (outcome = nil) then
            result := nil
          else if ('code' = fieldToReturn) then
            result := TFHIRCode.create(outcome.code)
          else
            result := outcome.Link;
        finally
          outcome.Free;
        end;
      finally
        cmap.Free;
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

function TVariable.summary(mode : boolean = false): string;
begin
  if Fobj = nil then
    result := name+': null'
  else if Fobj is TFHIRPrimitiveType then
    result := name+': "'+(Fobj as TFHIRPrimitiveType).StringValue+'"'
  else
    result := name+': ('+FObj.fhirType()+')';
  if mode then
    result := CODES_TVariableMode[Fmode]+': ' +result;
end;

function TVariable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fname.length * sizeof(char)) + 12);
  inc(result, Fobj.sizeInBytes);
end;

{ TVariables }

constructor TVariables.create;
begin
  inherited;
  Flist := TFslList<TVariable>.create;
end;

destructor TVariables.destroy;
begin
  list.Free;
  inherited;
end;

procedure TVariables.add(mode: TVariableMode; name: String; obj: TFHIRObject);
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

function TVariables.get(mode: TVariableMode; name: String): TFHIRObject;
var
  v : TVariable;
begin
  result := nil;
  for v in list do
    if (v.mode = mode) and (v.name = name) then
      exit(v.obj);
end;

function TVariables.GetCount: Integer;
begin
  result := list.Count;
end;

function TVariables.GetSummary: string;
var
  s, t : TFslStringBuilder;
  v : TVariable;
begin
  s := TFslStringBuilder.Create;
  t := TFslStringBuilder.Create;
  try
    for v in list do
    begin
      if (v.mode = vmINPUT) then
        s.CommaAdd(v.summary())
      else
        t.CommaAdd(v.summary());
    end;
    result := 'source variables ['+s.toString()+'], target variables ['+t.toString()+']';
  finally
    s.Free;
    t.Free;
  end;
end;

function TVariables.GetVariable(index: integer): TVariable;
begin
  result := list[index];
end;

function TVariables.link: TVariables;
begin
  result := TVariables(inherited Link);
end;

function TVariables.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TTransformerServices }

function TTransformerServices.link: TTransformerServices;
begin
  result := TTransformerServices(inherited link);
end;

{ TResolvedGroup }

constructor TResolvedGroup.Create(map: TFhirStructureMap; group: TFhirStructureMapGroup);
begin
  inherited create;
  FMap := map;
  FGroup := group;
end;

destructor TResolvedGroup.Destroy;
begin
  FMap.Free;
  FGroup.Free;
  inherited;
end;

function TResolvedGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMap.sizeInBytes);
  inc(result, FGroup.sizeInBytes);
end;

{ TFHIRStructureMapDebugContext }

constructor TFHIRStructureMapDebugContext.create(parent: TFHIRStructureMapDebugContext; appInfo: TFslObject; map: TFHIRStructureMap; group: TFhirStructureMapGroup; rule: TFhirStructureMapGroupRule; target: TFhirStructureMapGroupRuleTarget; variables: TVariables);
begin
  inherited create;
  FParent := parent.link;
  FRule := rule.Link;
  FMap := map.link;
  FAppInfo := appInfo.link;
  FTarget := target.link;
  FVariables := variables.link;
  FGroup := group.link;
  if FParent = nil then
    status := dsRunToBreakpoint
  else case FParent.status of
    dsRunToBreakpoint: status := dsRunToBreakpoint;
    dsStepOut: status := dsRunToBreakpoint;
    dsStepOver: status := dsRunToBreakpoint;
    dsStepIn: status := dsStepOver;
  end;
end;

function TFHIRStructureMapDebugContext.getdescription: String;
begin
  if target <> nil then
    result := map.url+'#'+group.name+'.'+rule.name+'.[target]'
  else if rule <> nil then
    result := map.url+'#'+group.name+'.'+rule.name
  else if group <> nil then
    result := map.url+'#'+group.name
  else
    result := map.url;
end;

function TFHIRStructureMapDebugContext.GetFocus: TFHIRObject;
begin
  if target <> nil then
    result := FTarget
  else if rule <> nil then
    result := rule
  else if group <> nil then
    result := group
  else
    result := map;
end;

function TFHIRStructureMapDebugContext.GetLine: integer;
begin
  if target <> nil then
    result := FTarget.LocationData.ParseStart.line
  else if rule <> nil then
    result := rule.LocationData.ParseStart.line
  else if group <> nil then
    result := group.LocationData.ParseStart.line
  else
    result := map.LocationData.ParseStart.line;
end;

function TFHIRStructureMapDebugContext.GetName: String;
begin
  if rule <> nil then
    result := group.name +'.'+rule.name
  else if group <> nil then
    result := group.name
  else
    result := map.name;
end;

destructor TFHIRStructureMapDebugContext.Destroy;
begin
  FRule.Free;
  FMap.Free;
  FAppInfo.Free;
  FTarget.Free;
  FVariables.Free;
  FGroup.Free;
  FParent.Free;
  inherited;
end;

function TFHIRStructureMapDebugContext.link: TFHIRStructureMapDebugContext;
begin
  result := TFHIRStructureMapDebugContext(inherited link);
end;

function TFHIRStructureMapDebugContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FRule.sizeInBytes);
  inc(result, FMap.sizeInBytes);
  inc(result, FAppInfo.sizeInBytes);
  inc(result, FTarget.sizeInBytes);
  inc(result, FVariables.sizeInBytes);
  inc(result, FGroup.sizeInBytes);
  inc(result, map.sizeInBytes);
end;

end.

