unit fhir3_pathnode;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_xml, fsl_http,
  fhir_objects, fhir_factory, fhir_pathengine, fhir_parser, 
  fhir3_types, fhir3_resources, fhir3_context, fhir3_parser, fhir3_resources_base;

type
  TFHIRPathOperation = (
       popNull, popEquals, popEquivalent, popNotEquals, popNotEquivalent, popLessThan, popGreater, popLessOrEqual, popGreaterOrEqual, popIs, popAs,
       popUnion, popOr, popAnd, popXor, popImplies, popTimes, popDivideBy, popPlus, popConcatenate, popMinus, popDiv, popMod, popIn, popContains, popCustom);
  TFHIRPathOperationSet = set of TFHIRPathOperation;

  TFHIRPathFunction = (
    pfNull, pfEmpty, pfNot, pfExists, pfSubsetOf, pfSupersetOf, pfIsDistinct, pfDistinct, pfCount, pfWhere, pfSelect, pfAll,
    pfRepeat, pfAggregate, pfItem, pfAs, pfIs, pfSingle, pfFirst, pfLast, pfTail, pfSkip, pfTake, pfUnion, pfCombine, pfIntersect, pfExclude,
    pfIif, pfUpper, pfLower, pfToChars,
    pfSubstring, pfStartsWith, pfEndsWith, pfMatches, pfReplaceMatches, pfContains, pfReplace, pfLength, pfChildren, pfDescendants,
    pfMemberOf, pfTrace, pfToday, pfNow, pfResolve, pfExtension, pfHasExtension, pfAllFalse, pfAnyFalse, pfAllTrue, pfAnyTrue,
    pfElementDefinition, pfSlice, pfCheckModifiers, pfConformsTo, pfHasValue, pfHtmlChecks, pfOfType, pfType,
    pfConvertsToBoolean, pfConvertsToInteger, pfConvertsToString, pfConvertsToDecimal, pfConvertsToQuantity, pfConvertsToDateTime, pfConvertsToDate, pfConvertsToTime,
    pfToBoolean, pfToInteger, pfToString, pfToDecimal, pfToQuantity, pfToDateTime, pfToTime,
    pfAbs, pfCeiling, pfExp, pfFloor, pfLn, pfLog, pfPower, pfTruncate, pfRound, pfSqrt,
    pfForHtml, pfEncode, pfDecode, pfEscape, pfUnescape, pfTrim, pfSplit, pfJoin,
    pfCustom);

  TFHIRPathExpressionNodeKind = (enkName, enkFunction, enkConstant, enkGroup, enkStructure, enkUnary); // structure is not used in fhir4_pathengine, but is in CQL
  TFHIRCollectionStatus = (csNULL, csSINGLETON, csORDERED, csUNORDERED);

const
  CODES_TFHIRPathOperation : array [TFHIRPathOperation] of String = (
    '', '=' , '~' , '!=' , '!~' , '<' , '>' , '<=' , '>=' , 'is', 'as', '|', 'or' , 'and' , 'xor', 'implies',
     '*', '/', '+' , '&', '-', 'div', 'mod', 'in', 'contains', 'xx-custom-xx');

  CODES_TFHIRPathFunctions : array [TFHIRPathFunction] of String = (
    '', 'empty', 'not', 'exists', 'subsetOf', 'supersetOf', 'isDistinct', 'distinct', 'count', 'where', 'select', 'all',
    'repeat', 'aggregate', '[]', 'as', 'is', 'single', 'first', 'last', 'tail', 'skip', 'take', 'union', 'combine', 'intersect', 'exclude',
    'iif', 'upper', 'lower', 'toChars',
    'substring', 'startsWith', 'endsWith', 'matches', 'replaceMatches', 'contains', 'replace', 'length', 'children', 'descendants',
    'memberOf', 'trace', 'today', 'now', 'resolve', 'extension', 'hasExtension', 'allFalse', 'anyFalse', 'allTrue', 'anyTrue',
    'elementDefinition', 'slice', 'checkModifiers', 'conformsTo', 'hasValue', 'htmlchecks', 'ofType', 'type',
    'convertsToBoolean', 'convertsToInteger', 'convertsToString', 'convertsToDecimal', 'convertsToQuantity', 'convertsToDateTime', 'convertsToDate', 'convertsToTime',
    'toBoolean', 'toInteger', 'toString', 'toDecimal', 'toQuantity', 'toDateTime', 'toTime',
    'abs', 'ceiling', 'exp', 'floor', 'ln', 'log', 'power', 'truncate', 'round', 'sqrt',
    'forHtml', 'encode', 'decode', 'escape', 'unescape', 'trim', 'split', 'join',
    'xx-custom-xx');

  FHIR_SD_NS = 'http://hl7.org/fhir/StructureDefinition/';
  FP_NS = 'http://hl7.org/fhirpath/';
  FP_String = 'http://hl7.org/fhirpath/String';
  FP_Boolean = 'http://hl7.org/fhirpath/Boolean';
  FP_Integer = 'http://hl7.org/fhirpath/Integer';
  FP_Decimal = 'http://hl7.org/fhirpath/Decimal';
  FP_Quantity = 'http://hl7.org/fhirpath/Quantity';
  FP_DateTime = 'http://hl7.org/fhirpath/DateTime';
  FP_Time = 'http://hl7.org/fhirpath/Time';
  FP_SimpleTypeInfo = 'http://hl7.org/fhirpath/SimpleTypeInfo';
  FP_ClassInfo = 'http://hl7.org/fhirpath/ClassInfo';


type
  TFHIRProfiledType = class (TFslObject)
  private
    Furi : String;
    FProfiles : TStringList; // or, not and
    FBindings : TFslList<TFHIRElementDefinitionBinding>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(s : string);
    destructor Destroy; override;
    function Link : TFHIRProfiledType; overload;

    property uri : String read FUri;
    property profiles : TStringList read FProfiles;
    property bindings : TFslList<TFHIRElementDefinitionBinding> read FBindings;

    function hasProfiles : boolean;
    function hasBindings : boolean;
    function hasBinding(b : TFhirElementDefinitionBinding) : boolean;
    procedure addProfile(s : String); overload;
    procedure addProfiles(list : TFhirUriList); overload;
    procedure addBinding(b : TFHIRElementDefinitionBinding);
    function isSystemType: boolean;

    class function ns(s : String) : String;
  end;

  TFHIRTypeDetails = class (TFslObject)
  private
    id : integer;
    FTypes : TFslList<TFHIRProfiledType>;
    FCollectionStatus : TFHIRCollectionStatus;
    function typesContains(t : String) : boolean;
    function getSystemType(url : string) : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(status : TFHIRCollectionStatus; types : array of String);
    constructor CreateList(status : TFHIRCollectionStatus; types : TFslList<TFHIRProfiledType>); overload;
    constructor CreateList(status : TFHIRCollectionStatus; types : TStringList); overload;
    destructor Destroy; override;
    function Link : TFHIRTypeDetails; overload;

    procedure addType(pt : TFHIRProfiledType); overload;
    function addType(n : String) : String; overload;
    function addType(n, p : String) : String; overload;

    procedure addTypes(n : TStringList); overload;
    procedure addTypes(types : array of String); overload;
    function hasType(context : TFHIRWorkerContext; typeName : String) : boolean; overload;
    function hasType(context : TFHIRWorkerContext; types : array of String) : boolean; overload;
    function hasType(context : TFHIRWorkerContext; types : TStringList) : boolean; overload;
    procedure update(source : TFHIRTypeDetails); overload;
    procedure update(status : TFHIRCollectionStatus; types : TStringList); overload;
    function union(right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function intersect(right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function hasNoTypes() : boolean;
    function toSingleton : TFHIRTypeDetails;
    property types : TFslList<TFHIRProfiledType> read FTypes;
    property CollectionStatus : TFHIRCollectionStatus read FCollectionStatus;
    function describe : String;
    function type_ : String;
  end;

  TFHIRPathExpressionNode = class (TFHIRPathExpressionNodeV)
  private
    FName: String;
    FConstant : TFHIRObject;
    FFunctionId : TFHIRPathFunction;
    FParameters : TFslList<TFHIRPathExpressionNode>;
    FInner: TFHIRPathExpressionNode;
    FGroup: TFHIRPathExpressionNode;
    FOperation : TFHIRPathOperation;
    FProximal : boolean;
    FOpNext: TFHIRPathExpressionNode;
    FTypes : TFHIRTypeDetails;
    FOpTypes : TFHIRTypeDetails;
    FKind: TFHIRPathExpressionNodeKind;
    FSourceLocationStart : TSourceLocation;
    FSourceLocationEnd : TSourceLocation;
    FOpSourceLocationStart : TSourceLocation;
    FOpSourceLocationEnd : TSourceLocation;

    procedure SetOpNext(const Value: TFHIRPathExpressionNode);
    procedure SetInner(const Value: TFHIRPathExpressionNode);
    procedure SetGroup(const Value: TFHIRPathExpressionNode);
    procedure SetFunctionId(const Value: TFHIRPathFunction);
    procedure SetTypes(const Value: TFHIRTypeDetails);
    procedure SetOpTypes(const Value: TFHIRTypeDetails);
    procedure write(b : TStringBuilder);
    procedure SetConstant(const Value: TFHIRObject);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(uniqueId : Integer);
    destructor Destroy; override;

    function ToString : String; override;
    function Link : TFHIRPathExpressionNode; overload;
    function checkName : boolean;

    // version overrides;
    {$IFNDEF FPC}
    procedure visitAll(context : pointer; proc : TFHIRPathExpressionNodeVisitProc); override;
    {$ENDIF}
    function nodeOpName : String; override;
    function nodeName : String; override;
    function nodeChildCount : integer; override;
    function nodeOpNext : TFHIRPathExpressionNodeV; override;
    function nodeGetChild(nodeIndex : integer; var offset : integer) : TFHIRPathExpressionNodeV; override;

    property SourceLocationStart : TSourceLocation read FSourceLocationStart write FSourceLocationStart;
    property SourceLocationEnd : TSourceLocation read FSourceLocationEnd write FSourceLocationEnd;
    property OpSourceLocationStart : TSourceLocation read FOpSourceLocationStart write FOpSourceLocationStart;
    property OpSourceLocationEnd : TSourceLocation read FOPSourceLocationEnd write FOpSourceLocationEnd;

    function summary : String; override;
    function ParameterCount : integer;
    function Canonical : String;
    function check(out msg : String; refCount : integer): boolean;
    function location : String;
    function opLocation : String;
    function presentConstant: String;

    property kind : TFHIRPathExpressionNodeKind read FKind write FKind;
    property name : String read FName write FName;
    property constant : TFHIRObject read FConstant write SetConstant;
    property FunctionId : TFHIRPathFunction read FFunctionId write SetFunctionId;
    property Parameters : TFslList<TFHIRPathExpressionNode> read FParameters;
    property Inner : TFHIRPathExpressionNode read FInner write SetInner;
    property Group : TFHIRPathExpressionNode read FGroup write SetGroup;
    property Operation : TFHIRPathOperation read FOperation write FOperation;
    property Proximal : boolean read FProximal write FProximal;
    property OpNext : TFHIRPathExpressionNode read FOpNext write SetOpNext;
    property Types : TFHIRTypeDetails read FTypes write SetTypes;
    property OpTypes : TFHIRTypeDetails read FOpTypes write SetOpTypes;
  end;

  TFHIRExpressionNodeComposer = class (TFslObject)
  private
    FStyle : TFHIROutputStyle;
    FLang : THTTPLanguages;
    procedure ComposeXml(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TFslStringSet);
    procedure composeXmlExpression(xml: TXmlBuilder; expr: TFHIRPathExpressionNode);
    procedure ComposeJson(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TFslStringSet);
    procedure ComposeJsonExpression(json: TJSONWriter; expr : TFHIRPathExpressionNode); reintroduce; overload; virtual;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(style : TFHIROutputStyle; const lang : THTTPLanguages); Virtual;

    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items : TFHIRObjectList; types : TFslStringSet); Virtual;
    function Compose(expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items : TFHIRObjectList; types : TFslStringSet): String; Overload;
  end;


implementation


{ TFHIRPathExpressionNode }

function TFHIRPathExpressionNode.Canonical: String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    write(b);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPathExpressionNode.check(out msg: String; refCount : integer): boolean;
var
  n : TFHIRPathExpressionNode;
begin
  msg := '';
  if refCount <> FslObjectReferenceCount then
    msg := 'Reference Count mistmatch'
  else
    case kind of
    enkName:
      if Name = '' then
        msg := 'No Name provided @ '+location;
    enkFunction:
      begin
        if (FFunctionId = pfNull) and (FName <> 'null') then
          msg := 'No Function id provided @ '+location;
        for n in parameters do
          if not n.check(msg, 0) then
            break;
      end;
    enkConstant:
      ; // nothing
    enkUnary:
      ; // nothing
    enkGroup:
      if FGroup = nil then
        msg := 'No Group provided @ '+location
      else
        FGroup.check(msg, 0);
  end;
  if (msg = '') and (FInner <> nil) then
    FInner.check(msg, 0);
  if (msg = '') then
    begin
    if FOperation = popNull then
    begin
      if FOpNext <> nil then
        msg := 'Next provided when it shouldn''t be @ '+location
    end
    else
      if FOpNext = nil then
        msg := 'No Next provided @ '+location
      else
        FOpNext.check(msg, 0);
    end;
  result := msg = '';
end;

function TFHIRPathExpressionNode.checkName: boolean;
begin
  if (name.StartsWith('$')) then
    result := StringArrayExistsSensitive(['$this', '$resource', '$total'], name)
  else
    result := true;
end;

constructor TFHIRPathExpressionNode.Create(uniqueId : Integer);
begin
  inherited Create;
  FUniqueId := uniqueId
end;

destructor TFHIRPathExpressionNode.Destroy;
begin
  FParameters.free;
  FOpNext.Free;
  FInner.Free;
  FGroup.Free;
  FTypes.Free;
  FOpTypes.Free;
  FConstant.Free;
  inherited;
end;

function TFHIRPathExpressionNode.Link: TFHIRPathExpressionNode;
begin
  result := TFHIRPathExpressionNode(inherited Link);
end;

function TFHIRPathExpressionNode.location: String;
begin
  result := SourceLocationStart.describe;
end;

function TFHIRPathExpressionNode.nodeChildCount: integer;
begin
  result := ParameterCount;
  if (Inner <> nil) then
    inc(result);
  if (Group <> nil) then
    inc(result);
  if (Operation <> popNull) then
    inc(result, 2);
end;

function TFHIRPathExpressionNode.nodeGetChild(nodeIndex: integer; var offset: integer): TFHIRPathExpressionNodeV;
begin
  result := nil;
  offset := 0;
  case kind of
    enkName: offset := 0; // no child nodes
    enkFunction:
      begin
        offset := Parameters.Count;
        if nodeIndex < offset then
          result := Parameters[nodeIndex];
      end;
    enkConstant: offset := 0; // no children
    enkGroup:
      begin
        offset := 1;
        if nodeIndex = 0 then
          result := Group;
      end;
  end;
  if (Inner <> nil) then
  begin
    if nodeIndex = offset then
      result := Inner;
    inc(offset);
  end;
end;

function TFHIRPathExpressionNode.nodeName: String;
begin
  case kind of
    enkName : result := name;
    enkFunction : result := CODES_TFHIRPathFunctions[FunctionId]+'()';
    enkConstant : result := '"'+presentConstant+'"';
    enkGroup : result := '(Group)';
  end;
  if Types <> nil then
    result := result + ': '+Types.describe;
end;

function TFHIRPathExpressionNode.nodeOpName: String;
begin
  result := CODES_TFHIRPathOperation[Operation];
end;

function TFHIRPathExpressionNode.nodeOpNext: TFHIRPathExpressionNodeV;
begin
  result := OpNext;
end;

function TFHIRPathExpressionNode.opLocation: String;
begin
  result := OpSourceLocationStart.describe;
end;

function TFHIRPathExpressionNode.ParameterCount: integer;
begin
  if FParameters = nil then
    result := 0
  else
    result := FParameters.Count;
end;

procedure TFHIRPathExpressionNode.SetConstant(const Value: TFHIRObject);
begin
  FConstant.Free;
  FConstant := Value;
end;

procedure TFHIRPathExpressionNode.SetFunctionId(const Value: TFHIRPathFunction);
begin
  FFunctionId := Value;
  if FParameters = nil then
    FParameters := TFslList<TFHIRPathExpressionNode>.create;
end;

procedure TFHIRPathExpressionNode.SetOpNext(const Value: TFHIRPathExpressionNode);
begin
  FOpNext.Free;
  FOpNext := Value;
end;

procedure TFHIRPathExpressionNode.SetTypes(const Value: TFHIRTypeDetails);
begin
  FTypes.Free;
  FTypes := Value;
end;

function TFHIRPathExpressionNode.summary: String;
begin
  case FKind of
    enkName: result := inttostr(uniqueId)+': '+FName;
    enkFunction: result := inttostr(uniqueId)+': '+CODES_TFHIRPathFunctions[FFunctionId]+'()';
    enkConstant: result := inttostr(uniqueId)+': "'+presentConstant+'"';
    enkGroup: result := inttostr(uniqueId)+': (Group)';
  end;
end;

function TFHIRPathExpressionNode.presentConstant: String;
var
  q : TFhirQuantity;
begin
  if (FConstant is TFHIRString) then
    result := ''''+jsonEscape(FConstant.primitiveValue, true)+''''
  else if (FConstant is TFHIRQuantity) then
  begin
    q := constant as TFhirQuantity;
    result := q.value+' '''+q.unit_+'''';
  end
  else
    result := jsonEscape(Fconstant.primitiveValue, true);
end;

function isToken(s : String) : boolean;
var
  i : integer;
begin
  if s = '' then
    exit(false);
  if s = '$this' then
    exit(true);
  if not CharInSet(s[1], ['a'..'z', 'A'..'Z', '_']) then
    exit(false);
  for i := 2 to length(s) do
    if not CharInSet(s[1], ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
      exit(false);
  result := true;
end;

function TFHIRPathExpressionNode.toString: String;
var
  b : TStringBuilder;
  first : boolean;
  n : TFHIRPathExpressionNode;
begin
  b := TStringBuilder.create();
  try
    case kind of
    enkName:
      begin
        if isToken(name) then
          b.append(name)
        else
          b.append('"'+name+'"');
      end;
    enkFunction:
      begin
        if (FunctionId = pfItem) then
          b.append('[')
        else
        begin
          b.append(name);
          b.append('(');
        end;
        first := true;
        for n in parameters do
        begin
          if (first) then
            first := false
          else
            b.append(', ');
          b.append(n.toString());
        end;
        if (FunctionId = pfItem) then
          b.append(']')
        else
          b.append(')');
      end;
    enkConstant:
      b.append(presentConstant);
    enkGroup:
      begin
        if group.ToString.Trim = '- 1' then // hack special case becuse of how negation works
          b.Append('-1')
        else
        begin
        b.append('(');
        b.append(group.toString());
        b.append(')');
      end;
    end;
    end;

    if (inner <> nil) then
    begin
      b.append('.');
      b.append(inner.toString());
    end;
    if (operation <> popNull) then
    begin
      b.append(' ');
      b.append(CODES_TFHIRPathOperation[operation]);
      b.append(' ');
      b.append(opNext.toString());
    end;

    result := b.toString();
  finally
    b.free;
  end;
end;

{$IFNDEF FPC}
procedure TFHIRPathExpressionNode.visitAll(context : pointer; proc: TFHIRPathExpressionNodeVisitProc);
var
  c : TFHIRPathExpressionNode;
begin
  proc(context, self);
  if ParameterCount > 0 then
    for c in Parameters do
      c.visitAll(context, proc);
  if Inner <> nil then
    Inner.visitAll(context, proc);
  if Group <> nil then
    Group.visitAll(context, proc);
  if OpNext <> nil then
    OpNext.visitAll(context, proc);
end;
{$ENDIF}

procedure TFHIRPathExpressionNode.write(b: TStringBuilder);
var
  f : boolean;
  n : TFHIRPathExpressionNode;
begin
  case fKind of
    enkName:
      b.Append(FName);
    enkConstant:
      b.Append(FConstant);
    enkFunction:
      begin
        b.Append(CODES_TFHIRPathFunctions[FFunctionId]);
        b.Append('(');
        f := true;
        for n in Parameters do
        begin
          if f then
            f := false
          else
            b.Append(', ');
          n.write(b);
        end;
        b.Append(')');
      end;
    enkGroup:
      begin
        b.Append('(');
        FGroup.write(b);
        b.Append(')');
      end;
  end;
  if inner <> nil then
  begin
    b.Append('.');
    inner.write(b);
  end;
  if Operation <> popNull then
  begin
    b.Append(' ');
    b.Append(CODES_TFHIRPathOperation[Operation]);
    b.Append(' ');
    OpNext.write(b);
  end;
end;

procedure TFHIRPathExpressionNode.SetOpTypes(const Value: TFHIRTypeDetails);
begin
  FOpTypes.Free;
  FOpTypes := Value;
end;

procedure TFHIRPathExpressionNode.SetInner(const Value: TFHIRPathExpressionNode);
begin
  FInner.free;
  FInner := Value;
end;

procedure TFHIRPathExpressionNode.SetGroup(const Value: TFHIRPathExpressionNode);
begin
  FGroup.Free;
  FGroup := Value;
end;

function TFHIRPathExpressionNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FConstant.sizeInBytes);
  inc(result, FParameters.sizeInBytes);
  inc(result, FInner.sizeInBytes);
  inc(result, FGroup.sizeInBytes);
  inc(result, FOpNext.sizeInBytes);
  inc(result, FTypes.sizeInBytes);
  inc(result, FOpTypes.sizeInBytes);
end;

{ TFHIRProfiledType }

constructor TFHIRProfiledType.Create(s: string);
begin
  inherited Create;
  FUri := ns(s);
end;

destructor TFHIRProfiledType.Destroy;
begin
  FProfiles.Free;
  FBindings.Free;
  inherited;
end;

function TFHIRProfiledType.Link : TFHIRProfiledType;
begin
  result := TFHIRProfiledType(inherited Link);
end;

procedure TFHIRProfiledType.addBinding(b: TFHIRElementDefinitionBinding);
begin
  if (Fbindings = nil) then
    Fbindings := TFslList<TFHIRElementDefinitionBinding>.create;
  FBindings.add(b);
end;

procedure TFHIRProfiledType.addProfile(s: String);
begin
  if (FProfiles = nil) then
    Fprofiles := TStringList.Create;
  Fprofiles.add(s);
end;

procedure TFHIRProfiledType.addProfiles(list: TFhirUriList);
var
  t : TFHIRUri;
begin
  for t in list do
    addProfile(t.value);
end;

function TFHIRProfiledType.hasProfiles: boolean;
begin
  result := (FProfiles <> nil) and (FProfiles.count > 0);
end;

function TFHIRProfiledType.hasBinding(b: TFhirElementDefinitionBinding): boolean;
begin
  result := false;
end;

function TFHIRProfiledType.hasBindings: boolean;
begin
  result := (FBindings <> nil) and (FBindings.count > 0);
end;

function TFHIRProfiledType.isSystemType : boolean;
begin
  result := Furi.startsWith(FP_NS);
end;

function TFHIRProfiledType.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Furi.length * sizeof(char)) + 12);
  inc(result, FProfiles.sizeInBytes);
  inc(result, FBindings.sizeInBytes);
end;

class function TFHIRProfiledType.ns(s: String): String;
begin
  if s.StartsWith('http:') then
    result := s
  else if (s.contains('.')) then
    result := FHIR_SD_NS+s.substring(0, s.indexof('.'))+'#'+s
  else
    result := FHIR_SD_NS+s;
end;

{ TFHIRTypeDetails }

var
  gc : integer = 0;

constructor TFHIRTypeDetails.createList(status: TFHIRCollectionStatus; types: TFslList<TFHIRProfiledType>);
begin
  inherited Create;
  FTypes := TFslList<TFHIRProfiledType>.create;
  FCollectionStatus := status;
  FTypes.AddAll(types);
  inc(gc);
  id := gc;
end;

constructor TFHIRTypeDetails.create(status: TFHIRCollectionStatus; types: array of String);
begin
  inherited Create;
  FTypes := TFslList<TFHIRProfiledType>.create;
  FCollectionStatus := status;
  addTypes(types);
  inc(gc);
  id := gc;
end;

constructor TFHIRTypeDetails.createList(status: TFHIRCollectionStatus; types: TStringList);
begin
  inherited Create;
  FTypes := TFslList<TFHIRProfiledType>.create;
  FCollectionStatus := status;
  addTypes(types);
  inc(gc);
  id := gc;
end;

destructor TFHIRTypeDetails.Destroy;
begin
  FTypes.Free;
  inherited;
end;

function TFHIRTypeDetails.Link: TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails(inherited Link);
end;

procedure TFHIRTypeDetails.addType(pt : TFHIRProfiledType);
var
  et : TFHIRProfiledType;
  p : string;
  b : TFHIRElementDefinitionBinding;
begin
  for et in Ftypes do
  begin
    if (et.uri = pt.uri) then
    begin
      if (pt.profiles <> nil) then
      begin
        for p in pt.profiles do
          if (not et.hasProfiles or (et.profiles.IndexOf(p) = -1)) then
            et.addProfile(p);
      end;

      if (pt.bindings <> nil) then
      begin
        for b in pt.bindings do
          if (not et.hasBindings or not et.hasBinding(b)) then
            et.addBinding(b.link);
      end;
      exit;
    end;
  end;
  Ftypes.add(pt.Link);
end;

function TFHIRTypeDetails.addType(n: String) : String;
var
  pt : TFHIRProfiledType;
begin
  pt := TFHIRProfiledType.Create(n);
  try
    result := pt.uri;
    addType(pt);
  finally
    pt.Free;
  end;
end;

function TFHIRTypeDetails.addType(n, p: String) : String;
var
  pt : TFHIRProfiledType;
begin
  pt := TFHIRProfiledType.Create(n);
  try
    pt.addProfile(p);
    result := pt.uri;
    addType(pt);
  finally
    pt.Free;
  end;
end;

procedure TFHIRTypeDetails.addTypes(n: TStringList);
var
  t : String;
begin
  for t in n do
    addType(t);
end;

procedure TFHIRTypeDetails.addTypes(types: array of String);
var
  t : String;
begin
  for t in types do
    addType(t);
end;

function TFHIRTypeDetails.hasType(context : TFHIRWorkerContext; typeName: String): boolean;
begin
  result := hasType(context, [typename])
end;

function TFHIRTypeDetails.hasType(context : TFHIRWorkerContext; types: TStringList): boolean;
begin
  result := hasType(context, types.ToStringArray);
end;

function TFHIRTypeDetails.typesContains(t : String) : boolean;
var
  pt : TFHIRProfiledType;
begin
  for pt in FTypes do
    if (pt.uri = t) then
      exit(true);
  exit(false);
end;


function TFHIRTypeDetails.type_: String;
begin
  result := describe; // todo?
end;

function TFHIRTypeDetails.hasType(context : TFHIRWorkerContext; types: array of String): boolean;
var
  n, t, id, tail : String;
  sd, w : TFhirStructureDefinition;
begin
  for n in types do
  begin
    t := TFHIRProfiledType.ns(n);
    if (typesContains(t)) then
      exit(true);
    if (StringArrayExistsSensitive(['boolean', 'string', 'integer', 'decimal', 'Quantity', 'dateTime', 'time', 'ClassInfo', 'SimpleTypeInfo'], n)) then
    begin
      t := FP_NS+capitalise(n);
      if (typesContains(t)) then
        exit(true);
    end;
  end;

  for n in types do
  begin
    if n.contains('#') then
      id := n.substring(0, n.indexOf('#'))
    else
      id := n;
    tail := '';
    if (n.contains('#')) then
    begin
      tail := n.substring( n.indexOf('#')+1);
      tail := tail.substring(tail.indexOf('.'));
    end;
    t := TFHIRProfiledType.ns(n);
    sd := context.fetchResource(frtStructureDefinition, t) as TFhirStructureDefinition;
    try
      while (sd <> nil) do
      begin
        if (tail = '') and (typesContains(sd.Url)) then
          exit(true);
        if (tail = '') and (getSystemType(sd.url) <> '') and typesContains(getSystemType(sd.Url)) then
          exit(true);
        if (tail <> '') and typesContains(sd.Url+'#'+sd.type_+tail) then
          exit(true);
        if (sd.BaseDefinition <> '')  then
        begin
          if (sd.baseDefinition = 'http://hl7.org/fhir/StructureDefinition/Element') and
            (sd.Type_ <> 'string') and (sd.Type_ <> 'uri') then
              w := context.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/string') as TFhirStructureDefinition
            else
              w := context.fetchResource(frtStructureDefinition, sd.BaseDefinition) as TFhirStructureDefinition;
        end
        else
          w := nil;
        sd.Free;
        sd := w;
      end;
    finally
      sd.Free;
    end;
  end;
  result := false;
end;

function TFHIRTypeDetails.getSystemType(url : String) : String;
var
  code : String;
begin
  result := '';
  if (url.startsWith('http://hl7.org/fhir/StructureDefinition/')) then
  begin
    code := url.substring(40);
    if StringArrayExistsSensitive(['string',  'boolean', 'integer', 'decimal', 'dateTime', 'time', 'Quantity'], code) then
      exit(FP_NS+capitalise(code));
  end;
end;

procedure TFHIRTypeDetails.update(source: TFHIRTypeDetails);
var
  pt : TFHIRProfiledType;
begin
  for pt in source.Types do
    addType(pt);
  if (FcollectionStatus = csNULL) then
    FcollectionStatus := source.collectionStatus
  else if (source.FcollectionStatus = csUNORDERED) then
    FcollectionStatus := source.collectionStatus
  else
    FcollectionStatus := csORDERED;
end;

function TFHIRTypeDetails.union(right: TFHIRTypeDetails): TFHIRTypeDetails;
var
  pt : TFHIRProfiledType;
begin
  result := TFHIRTypeDetails.create(csNULL, []);
  try
    if (right.FcollectionStatus = csUNORDERED) or (FCollectionStatus = csUNORDERED) then
      result.FcollectionStatus := csUNORDERED
    else
      result.FcollectionStatus := csORDERED;
    for pt in types do
      result.addType(pt);
    for pt in right.types do
      result.addType(pt);
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFHIRTypeDetails.update(status: TFHIRCollectionStatus; types: TStringList);
var
  s : String;
begin
  for s in Types do
    addType(s);
  if (FcollectionStatus = csNULL) then
    FcollectionStatus := status
  else if (status = csUNORDERED) then
    FcollectionStatus := status
  else
    FcollectionStatus := csORDERED;
end;

function TFHIRTypeDetails.intersect(right: TFHIRTypeDetails): TFHIRTypeDetails;
var
  pt, r : TFHIRProfiledType;
  found : boolean;
begin
  result := TFHIRTypeDetails.create(csNULL, []);
  try
    if (right.FcollectionStatus in [csUNORDERED, csSINGLETON]) then
      result.FcollectionStatus := csUNORDERED
    else
      result.FcollectionStatus := csORDERED;
    for pt in types do
    begin
      found := false;
      for r in right.types do
        found := found or (pt.uri = r.uri);
      if (found) then
        result.addType(pt);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRTypeDetails.hasNoTypes: boolean;
begin
  result := FTypes.count = 0;
end;

function TFHIRTypeDetails.toSingleton: TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails.create(csSINGLETON, []);
  try
    result.FTypes.AddAll(FTypes);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRTypeDetails.describe: String;
var
  pt : TFHIRProfiledType;
begin
   result := '';
   for pt in Types do
     CommaAdd(result, pt.uri);
end;

function TFHIRTypeDetails.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTypes.sizeInBytes);
end;

{ TFHIRExpressionNodeComposer }

constructor TFHIRExpressionNodeComposer.Create(style: TFHIROutputStyle; const lang : THTTPLanguages);
begin
  inherited Create;
  FLang := lang;
  FStyle := Style;
end;

function TFHIRExpressionNodeComposer.Compose(expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items: TFHIRObjectList; types : TFslStringSet): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    composeExpression(stream, expr, fmt, items, types);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeExpression(stream: TStream; expr: TFHIRPathExpressionNode; fmt: TFHIRFormat; items: TFHIRObjectList; types: TFslStringSet);
begin
  case fmt of
    ffXml : ComposeXml(stream, expr, items, types);
    ffJson: ComposeJson(stream, expr, items, types);
  else
    raise EFHIRException.create('ComposeExpression is Not supported for '+CODES_TFHIRFormat[fmt]);
  end;
end;

procedure TFHIRExpressionNodeComposer.composeXml(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TFslStringSet);
var
  xml : TXmlBuilder;
  base : TFHIRObject;
  x : TFHIRXmlComposerBase;
begin
  x := TFHIRParsers3.composer(nil, ffXml, FLang, FStyle) as TFHIRXmlComposerBase;
  try
    xml := TFslXmlBuilder.Create;
    try
      xml.IsPretty := FStyle = OutputStylePretty;
      xml.NoHeader := true;
      xml.CurrentNamespaces.DefaultNS := FHIR_NS;
      xml.Start;
      xml.Open('Expression');
      if items <> nil then
        xml.addattribute('count', inttostr(items.count))
      else
        xml.addattribute('count', 'nil');
      xml.Open('outcome');
      if items <> nil then
        for base in items do
          if (base = nil) then
            xml.tag('Null')
          else
            x.ComposeBase(xml, base.FhirType, base);
      xml.Close('outcome');
      xml.TagText('canonical', expr.Canonical);
      xml.Open('tree');
      composeXmlExpression(xml, expr);
      xml.Close('tree');
      if (types <> nil) then
      begin
        xml.AddAttribute('value', types.ToString);
        xml.Tag('types');
      end;
      xml.Close('Expression');
      xml.Finish;
      xml.Build(stream);
    finally
      xml.Free;
    end;
  finally
    x.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.composeXmlExpression(xml: TXmlBuilder; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
  begin
    xml.AddAttribute('value', 'true');
    xml.Tag('proximal');
  end;

  case expr.kind of
    enkName :
      begin
        xml.AddAttribute('value', expr.name);
        xml.Tag('name');
      end;
    enkFunction :
      begin
        xml.AddAttribute('value', CODES_TFHIRPathFunctions[expr.FunctionId]);
        xml.Tag('function');
        for p in expr.Parameters do
        begin
          xml.open('parameter');
          composeXmlExpression(xml, p);
          xml.close('parameter');
        end;
      end;
    enkConstant :
      begin
        xml.AddAttribute('value', expr.presentConstant);
        xml.Tag('constant');
      end;
    enkGroup :
      begin
        xml.Open('group');
        composeXmlExpression(xml, expr.Group);
        xml.Close('group');
      end;
  end;
  if expr.Types <> nil then
  begin
    xml.AddAttribute('value', expr.types.ToString);
    xml.Tag('types');
  end;
  if expr.Inner <> nil then
  begin
    xml.open('inner');
    composeXmlExpression(xml, expr.Inner);
    xml.close('inner');
  end;
  if expr.Operation <> popNull then
  begin
    xml.AddAttribute('kind', CODES_TFHIRPathOperation[expr.Operation]);
    xml.open('operation');
    composeXmlExpression(xml, expr.OpNext);
    xml.close('operation');
  end;
  if expr.OpTypes <> nil then
  begin
    xml.AddAttribute('value', expr.optypes.ToString);
    xml.Tag('op-types');
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeJson(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TFslStringSet);
var
  oStream : TFslVCLStream;
  json : TJSONWriter;
  base : TFHIRObject;
  j : TFHIRJsonComposerBase;
begin
  j := TFHIRParsers3.composer(nil, ffJson, FLang, FStyle) as TFHIRJsonComposerBase;
  try
    json := TJsonWriterDirect.create;
    try
      oStream := TFslVCLStream.Create;
      json.Stream := oStream;
      oStream.Stream := stream;
      json.HasWhitespace := FStyle = OutputStylePretty;
      json.Start(true);
      json.ValueArray('outcome');
      for base in items do
        j.ComposeBase(json, '', base);
      json.FinishArray;
      json.Value('canonical', expr.Canonical);
      json.ValueObject('tree');
      composeJsonExpression(json, expr);
      json.FinishObject;
      if (types <> nil) then
        json.Value('types', types.ToString);
      json.Finish(true);
    finally
      json.free;
    end;
  finally
    j.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeJsonExpression(json: TJSONWriter; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
    json.value('proximal', true);

  case expr.kind of
    enkName: json.value('name', expr.name);
    enkFunction:
      begin
        json.value('function', CODES_TFHIRPathFunctions[expr.FunctionId]);
        json.ValueArray('parameters');
        for p in expr.Parameters do
        begin
          json.ValueObject('');
          ComposeJsonExpression(json, p);
          json.FinishObject;
        end;
        json.FinishArray();
      end;
    enkConstant: json.value('constant', expr.presentConstant);
    enkGroup:
      begin
      json.valueObject('group');
      ComposeJsonExpression(json, expr.Group);
      json.FinishObject;
      end;
  end;
  if expr.Types <> nil then
    json.value('types', expr.types.ToString);
  if expr.Inner <> nil then
  begin
    json.ValueObject('inner');
    ComposeJsonExpression(json, expr.Inner);
    json.FinishObject;
  end;
  if expr.Operation <> popNull then
  begin
    json.ValueObject('operation');
    json.value('kind', CODES_TFHIRPathOperation[expr.Operation]);
    ComposeJsonExpression(json, expr.OpNext);
    json.FinishObject;
  end;
  if expr.OpTypes <> nil then
    json.value('op-types', expr.optypes.ToString);
end;


function TFHIRExpressionNodeComposer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLang.sizeInBytes);
end;

end.
