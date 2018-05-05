unit FHIR.R2.PathNode;


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

interface

uses
  SysUtils, Classes,
  FHIR.Support.Strings, FHIR.Support.Text,
  FHIR.Support.Objects, FHIR.Support.Generics,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.PathEngine,
  FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Context;

type
  TFHIRPathOperation = (
       popNull, popEquals, popEquivalent, popNotEquals, popNotEquivalent, popLessThan, popGreater, popLessOrEqual, popGreaterOrEqual, popIs, popAs,
       popUnion, popOr, popAnd, popXor, popImplies, popTimes, popDivideBy, popPlus, popConcatenate, popMinus, popDiv, popMod, popIn, popContains, popCustom);
  TFHIRPathOperationSet = set of TFHIRPathOperation;

  TFHIRPathFunction = (
    pfNull, pfEmpty, pfNot, pfExists, pfSubsetOf, pfSupersetOf, pfIsDistinct, pfDistinct, pfCount, pfWhere, pfSelect, pfAll,
    pfRepeat, pfItem, pfAs, pfIs, pfSingle, pfFirst, pfLast, pfTail, pfSkip, pfTake, pfIif, pfToInteger, pfToDecimal, pfToString,
    pfSubstring, pfStartsWith, pfEndsWith, pfMatches, pfReplaceMatches, pfContains, pfReplace, pfLength, pfChildren, pfDescendants,
    pfMemberOf, pfTrace, pfToday, pfNow, pfResolve, pfExtension, pfAllFalse, pfAnyFalse, pfCombine, pfType, pfOfType,
    pfElementDefinition, pfSlice, pfCheckModifiers, pfConformsTo, pfHasValue, pfHtmlChecks, pfCustom);

  TFHIRPathExpressionNodeKind = (enkName, enkFunction, enkConstant, enkGroup, enkStructure); // structure is not used in FHIR.Tools.PathEngine, but is in CQL
  TFHIRCollectionStatus = (csNULL, csSINGLETON, csORDERED, csUNORDERED);

const
  CODES_TFHIRPathOperation : array [TFHIRPathOperation] of String = (
    '', '=' , '~' , '!=' , '!~' , '<' , '>' , '<=' , '>=' , 'is', 'as', '|', 'or' , 'and' , 'xor', 'implies',
     '*', '/', '+' , '&', '-', 'div', 'mod', 'in', 'contains', 'xx-custom-xx');

  CODES_TFHIRPathFunctions : array [TFHIRPathFunction] of String = (
    '', 'empty', 'not', 'exists', 'subsetOf', 'supersetOf', 'isDistinct', 'distinct', 'count', 'where', 'select', 'all',
    'repeat', '[]', 'as', 'is', 'single', 'first', 'last', 'tail', 'skip', 'take', 'iif', 'toInteger', 'toDecimal', 'toString',
    'substring', 'startsWith', 'endsWith', 'matches', 'replaceMatches', 'contains', 'replace', 'length', 'children', 'descendants',
    'memberOf', 'trace', 'today', 'now', 'resolve', 'extension', 'allFalse', 'anyFalse', 'combine', 'type', 'ofType',
    'elementDefinition', 'slice', 'checkModifiers', 'conformsTo', 'hasValue', 'htmlchecks', 'xx-custom-xx');



type
  TFHIRTypeDetails = class (TFHIRTypeDetailsV)
  private
    id : integer;
    FTypes : TStringList;
    FCollectionStatus : TFHIRCollectionStatus;
  public
    constructor create(status : TFHIRCollectionStatus; types : array of String);
    constructor createList(status : TFHIRCollectionStatus; types : TStringList);
    destructor Destroy; override;
    function Link : TFHIRTypeDetails; overload;
    procedure addType(n : String);
    procedure addTypes(n : TStringList); overload;
    procedure addTypes(types : array of String); overload;
    function hasType(types : array of String) : boolean; overload;
    function hasType(types : TStringList) : boolean; overload;
    function hasType(typeName : String) : boolean; overload;
    procedure update(source : TFHIRTypeDetails);
    function union(right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function hasNoTypes() : boolean;
    function toSingleton : TFHIRTypeDetails;
    property types : TStringList read FTypes;
    property CollectionStatus : TFHIRCollectionStatus read FCollectionStatus;
    function describe : String;
    function type_ : String;
  end;

  TFHIRPathExpressionNode = class (TFHIRPathExpressionNodeV)
  private
    FName: String;
    FConstant : string;
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
  public
    Constructor Create(uniqueId : Integer);
    Destructor Destroy; override;

    function ToString : String; override;
    function Link : TFHIRPathExpressionNode; overload;
    function checkName : boolean;

    // version overrides;
    procedure visitAll(proc : TFHIRPathExpressionNodeVisitProc); override;
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
    function presentConstant : String;

    property kind : TFHIRPathExpressionNodeKind read FKind write FKind;
    property name : String read FName write FName;
    property constant : String read FConstant write FConstant;
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
  if refCount <> AdvObjectReferenceCount then
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
      if FConstant = '' then
        msg := 'No Constant provided @ '+location;
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
    result := StringArrayExistsSensitive(['$this', '$resource'], name)
  else
    result := true;
end;

constructor TFHIRPathExpressionNode.Create;
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
  inherited;
end;

function TFHIRPathExpressionNode.Link: TFHIRPathExpressionNode;
begin
  result := TFHIRPathExpressionNode(inherited Link);
end;

function TFHIRPathExpressionNode.location: String;
begin
  result := inttostr(SourceLocationStart.line)+', '+inttostr(SourceLocationStart.col);
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
    enkConstant : result := '"'+constant+'"';
    enkGroup : result := '(Group)';
  end;
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
  result := inttostr(OpSourceLocationStart.line)+', '+inttostr(OpSourceLocationStart.col);
end;

function TFHIRPathExpressionNode.ParameterCount: integer;
begin
  if FParameters = nil then
    result := 0
  else
    result := FParameters.Count;
end;

function TFHIRPathExpressionNode.presentConstant: String;
begin
  result := constant;
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
    enkConstant: result := inttostr(uniqueId)+': "'+FConstant+'"';
    enkGroup: result := inttostr(uniqueId)+': (Group)';
  end;
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
        b.append(name);
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
      begin
    	  b.append(jsonEscape(constant, true));
			end;
		enkGroup:
      begin
  			b.append('(');
	  		b.append(group.toString());
		  	b.append(')');
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

procedure TFHIRPathExpressionNode.visitAll(proc: TFHIRPathExpressionNodeVisitProc);
var
  c : TFHIRPathExpressionNode;
begin
  proc(self);
  if ParameterCount > 0 then
    for c in Parameters do
      c.visitAll(proc);
  if Inner <> nil then
    Inner.visitAll(proc);
  if Group <> nil then
    Group.visitAll(proc);
  if OpNext <> nil then
    OpNext.visitAll(proc);
end;

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


{ TFHIRTypeDetails }

var
  gc : integer = 0;

constructor TFHIRTypeDetails.createList(status: TFHIRCollectionStatus; types: TStringList);
begin
  inherited Create;
  FTypes := TStringList.create;
  FTypes.Sorted := true;
  FCollectionStatus := status;
  addTypes(types);
  inc(gc);
  id := gc;
end;

constructor TFHIRTypeDetails.create(status: TFHIRCollectionStatus; types: array of String);
begin
  inherited Create;
  FTypes := TStringList.create;
  FTypes.Sorted := true;
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

procedure TFHIRTypeDetails.addType(n: String);
begin
  if (n <> '') then
    if not hasType(n) then
      FTypes.add(n);
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

function TFHIRTypeDetails.describe: String;
begin
  result := FTypes.commaText;
end;

function TFHIRTypeDetails.hasNoTypes: boolean;
begin
  result := FTypes.count = 0;
end;

function TFHIRTypeDetails.hasType(types: TStringList): boolean;
var
  t : String;
begin
  result := false;
  for t in types do
    if hasType(t) then
      exit(true);
end;

function TFHIRTypeDetails.hasType(typeName: String): boolean;
begin
  result := FTypes.indexOf(typeName) > -1;
end;

function TFHIRTypeDetails.Link: TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails(inherited Link);
end;

function TFHIRTypeDetails.hasType(types: array of String): boolean;
var
  t : String;
begin
  result := false;
  for t in types do
    if hasType(t) then
      exit(true);
end;

function TFHIRTypeDetails.toSingleton: TFHIRTypeDetails;
begin
  result := TfhirTypeDetails.createList(csSINGLETON, FTypes);
end;

function TfhirTypeDetails.type_: String;
begin

end;

function TFHIRTypeDetails.union(right: TFHIRTypeDetails): TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails.createList(csNULL, FTypes);
  if (right.FcollectionStatus in [csUNORDERED, csUNORDERED]) then
    result.FcollectionStatus := csUNORDERED
  else
    result.FcollectionStatus := csORDERED;
  result.addTypes(types);
  result.addTypes(right.types);
end;

procedure TFHIRTypeDetails.update(source: TFHIRTypeDetails);
begin
  addTypes(source.types);
  if (FcollectionStatus = csNULL) then
    FcollectionStatus := source.collectionStatus
  else if (source.FcollectionStatus = csUNORDERED) then
    FcollectionStatus := source.collectionStatus
  else
    FcollectionStatus := csORDERED;
end;



end.
