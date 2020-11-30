unit fhir2_pathengine;

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
  SysUtils, Classes, Math, {$IFDEF DELPHI} RegularExpressions, {$ENDIF} Generics.Collections, Character,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc,
  fsl_ucum,
  fhir_objects, fhir_factory, fhir_pathengine, 
  fhir2_pathnode, fhir2_types, fhir2_utilities, fhir2_context, fhir2_constants,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

const
  FHIR_TYPES_STRING : Array[0..8] of String = ('string', 'uri', 'code', 'oid', 'id', 'uuid', 'sid', 'markdown', 'base64Binary');

type
  TFHIRPathExecutionTypeContext = class (TFslObject)
  private
    FAppInfo : TFslObject;
    FResourceType : String;
    FContext : TFHIRTypeDetails;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(appInfo : TFslObject; resourceType : String; context : TFHIRTypeDetails);
    destructor Destroy; override;
    function Link : TFHIRPathExecutionTypeContext; overload;
    property appInfo : TFslObject read FappInfo;
    property resourceType : String read FResourceType;
    property context : TFHIRTypeDetails read FContext;
  end;

  TFHIRPathParser = class (TFslObject)
  private
    function parseExpression(lexer: TFHIRPathLexer; proximal : boolean): TFHIRPathExpressionNode;
    procedure organisePrecedence(lexer : TFHIRPathLexer; var node: TFHIRPathExpressionNode);
    procedure gatherPrecedence(lexer : TFHIRPathLexer; var start: TFHIRPathExpressionNode; ops: TFHIRPathOperationSet);
    function newGroup(lexer : TFHIRPathLexer; next: TFHIRPathExpressionNode): TFHIRPathExpressionNode;
  protected
    procedure checkParameters(lexer : TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode);
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; count : integer); overload;
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; countMin, countMax : integer); overload;
  public
    // Parse a path for later use using execute
    function parse(path : String) : TFHIRPathExpressionNode; overload;
    function parse(lexer : TFHIRPathLexer) : TFHIRPathExpressionNode; overload;
  end;

  TFHIRPathLexer2 = class (TFHIRPathLexer)
  protected
    function opCodes : TArray<String>; override;
  public
    function processConstant : TFHIRObject; overload; override;
  end;

  TFHIRPathEngine = class;
  TFHIRResolveReferenceEvent = function (source : TFHIRPathEngine; appInfo : TFslObject; url : String) : TFHIRObject of object;

  TFHIRPathEngine = class (TFHIRPathEngineV)
  private
    worker : TFHIRWorkerContext;
    FLog : TStringBuilder;
    primitiveTypes, allTypes : TStringList;
    Fucum : TUcumServiceInterface;

    procedure log(name, value : String);

    function execute(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode; atEntry : boolean) : TFHIRSelectionList; overload;
    function execute(context : TFHIRPathExecutionContext; item : TFHIRObject; exp : TFHIRPathExpressionNode; atEntry : boolean) : TFHIRSelectionList; overload;
    procedure debug(context : TFHIRPathExecutionContext; exp : TFHIRPathExpressionNode; op : boolean; input1, input2, outcome : TFHIRSelectionList);
    function replaceFixedConstant(context : TFHIRPathExecutionContext; const s : String) : TFHIRObject;

    function evaluateFunction(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
    function preOperate(left : TFHIRSelectionList; op : TFHIRPathOperation) : TFHIRSelectionList;
    function operate(left : TFHIRSelectionList; op : TFHIRPathOperation; right : TFHIRSelectionList) : TFHIRSelectionList;
    function readConstant(context : TFHIRPathExecutionContext; constant : String) : TFHIRObject;
    procedure ListAllChildren(item: TFHIRObject; results: TFHIRSelectionList; recurse: boolean);
    procedure ListChildrenByName(focus: TFHIRObject; name : String; results: TFHIRSelectionList);

    function childTypes(focus : TFHIRTypeDetails; mask : string) : TFHIRTypeDetails;
    procedure checkParamTypes(funcId : TFHIRPathFunction; paramTypes : TFslList<TFHIRTypeDetails>; typeSet : array of TFHIRTypeDetails);
    function executeType(ctxt: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode; atEntry : boolean) : TFHIRTypeDetails; overload;
    function executeType(focus: String; exp: TFHIRPathExpressionNode; atEntry : boolean) : TFHIRTypeDetails; overload;
    function evaluateFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode): TFHIRTypeDetails;
    function operateTypes(left : TFHIRTypeDetails; op : TFHIRPathOperation; right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function readConstantType(ctxt: TFHIRPathExecutionTypeContext; constant : String) : string;

    procedure ListChildTypesByName(item, name : string; result : TFHIRTypeDetails);

    function getElementDefinition(sd : TFHIRStructureDefinition; path : String; allowPM : boolean; var specifiedType : String) : TFHIRElementDefinition;
    function getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
    function hasDataType(ed : TFhirElementDefinition) : boolean;

    function funcEmpty(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcItem(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcWhere(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcAll(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcFirst(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcLast(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcTail(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcCount(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcLength(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcDistinct(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcNot(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcResolve(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcContains(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcMatches(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcStartsWith(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSubString(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcExtension(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcExists(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSubsetOf(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSupersetOf(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsDistinct(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSelect(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcRepeat(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcAs(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIs(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSingle(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSkip(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcTake(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIif(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToInteger(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToDecimal(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToString(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcEndsWith(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcReplaceMatches(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcReplace(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcChildren(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcDescendants(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcMemberOf(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcTrace(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToday(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcNow(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcAllFalse(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcAnyFalse(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcCombine(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcType(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcOfType(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcElementDefinition(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcSlice(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcCheckModifiers(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcConformsTo(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcHasValue(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcHtmlChecks(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;


    function equal(left, right : TFHIRObject) : boolean;  overload;
    function equivalent(left, right : TFHIRObject) : boolean;  overload;

    function opequal(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opNotequal(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opLessThan(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opGreater(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opLessOrEqual(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opGreaterOrEqual(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opIn(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opContains(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opPlus(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opConcatenate(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opMinus(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opEquivalent(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opNotEquivalent(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opUnion(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opAnd(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opOr(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opXor(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opImplies(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opTimes(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opDivideBy(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opDiv(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opMod(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opIs(left, right : TFHIRSelectionList) : TFHIRSelectionList;
    function opAs(left, right : TFHIRSelectionList) : TFHIRSelectionList;

    function contains(list: TFHIRSelectionList; item: TFHIRObject): boolean;
    function isAbstractType(list: TFHIRElementDefinitionTypeList): boolean;

  protected
    function funcCustom(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList; virtual;
    function evaluateCustomFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode): TFHIRTypeDetails; virtual;
    function executeV(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; override;
    function executeV(context : TFHIRPathExecutionContext; item : TFHIRObject; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(context : TFHIRWorkerContext; ucum : TUcumServiceInterface);
    destructor Destroy; override;

    // Parse a path for later use using execute
    function parse(path : String) : TFHIRPathExpressionNode; overload;
    function parseV(path : String) : TFHIRPathExpressionNodeV; overload; override;

    // check that paths referred to in the expression are valid
    function check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNode; xPathStartsWithValueRef : boolean) : TFHIRTypeDetails; overload;
    function check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNodeV; xPathStartsWithValueRef : boolean) : TFHIRTypeDetailsV; overload; override;

    // evaluate a path and return the matching elements
    function evaluate(appInfo : TFslObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; override;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; override;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; override;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; override;

    // evaluate a path and return true or false
    function evaluateToBoolean(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : boolean; overload; override;
    function evaluateToBoolean(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : boolean; overload; override;

    // evaluate a path and return a string describing the outcome
    function evaluateToString(appInfo : TFslObject; base : TFHIRObject; path : String) : string; overload; override;
    function evaluateToString(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : string; overload; override;

    // worker routine for converting a set of objects to a string representation
    function convertToString(items : TFHIRSelectionList) : String; overload; override;
    function convertToString(item : TFHIRObject) : String; overload;

    // worker routine for converting a set of objects to a boolean representation
    function convertToBoolean(items : TFHIRSelectionList) : boolean;

    function UseLog : String;
    property context : TFHIRWorkerContext read worker;
  end;
  TFHIRPathEngine2 = TFHIRPathEngine;

implementation

{ TFHIRPathEvaluator }

function TFHIRPathEngine.check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNode; xPathStartsWithValueRef : boolean) : TFHIRTypeDetails;
var
  types : TFHIRTypeDetails;
  ctxt : TFHIRPathExecutionTypeContext;
  sd : TFHIRStructureDefinition;
  ed : TFHIRElementDefinition;
  td : TFhirElementDefinitionType;
  t : String;
begin
  if (xPathStartsWithValueRef and context.contains('.') and path.startsWith(context.substring(context.lastIndexOf('.')+1))) then
    types := TFHIRTypeDetails.Create(csSINGLETON, [context.substring(0, context.lastIndexOf('.'))])
  else if not context.contains('.') then
    types := TFHIRTypeDetails.Create(csSINGLETON, [context])
  else
  begin
    sd := worker.getStructure('http://hl7.org/fhir/StructureDefinition/'+context.Substring(0, context.IndexOf('.')));
    try
      if (sd = nil) then
        raise EFHIRPath.Create('Unknown context '+context);
      ed := getElementDefinition(sd, context, true, t);
      if (ed = nil) then
        raise EFHIRPath.Create('Unknown context element '+context);
      if (t <> '') then
        types := TFHIRTypeDetails.Create(csSINGLETON, [t])
      else if (ed.type_List.Count = 0) or isAbstractType(ed.type_List) then
        types := TFHIRTypeDetails.Create(csSINGLETON, [context])
      else
      begin
        types := TFHIRTypeDetails.Create(csSINGLETON, []);
        for td in ed.type_List do
          types.addType(td.code);
      end;
    finally
      sd.free;
    end;
  end;

  try
    ctxt := TFHIRPathExecutionTypeContext.create(appInfo, resourceType, types.Link);
    try
      result := executeType(ctxt, types, expr, true);
    finally
      ctxt.free;
    end;
  finally
    types.Free;
  end;
end;

function TFHIRPathEngine.convertToBoolean(items: TFHIRSelectionList): boolean;
begin
  if (items = nil) then
    result := false
  else if (items.count = 1) and (items[0].value is TFHIRBoolean) then
    result := TFHIRBoolean(items[0].value).value
  else
    result := items.count > 0;
end;

function TFHIRPathEngine.convertToString(item: TFHIRObject): String;
begin
  if item = nil then
    result := ''
  else if item.isPrimitive then
    result := item.primitiveValue
  else if item is TFhirType then
    result := gen(item as TFHIRType)
  else
    result := '';
end;


constructor TFHIRPathEngine.create(context: TFHIRWorkerContext; ucum : TUcumServiceInterface);
var
  sd : TFhirStructureDefinition;
  list : TFslList<TFHIRStructureDefinition>;
begin
  inherited Create;
  worker := context;
  FUcum := ucum;
  FLog := TStringBuilder.Create;
  allTypes := TStringList.Create;
  primitiveTypes := TStringList.Create;
  if (worker <> nil) then
  begin
    list := TFslList<TFHIRStructureDefinition>.create;
    try
      worker.listStructures(list);
      for sd in list do
        if (sd.kind <> StructureDefinitionKindLogical) then
        begin
          if (sd.constrainedType = '') then
            allTypes.add(sd.id);
          if (sd.constrainedType = '') and isPrimitiveType(sd.id) then
            primitiveTypes.add(sd.id);
      end;
    finally
      list.free;
    end;
  end;
end;

procedure TFHIRPathEngine.debug(context : TFHIRPathExecutionContext; exp: TFHIRPathExpressionNode; op : boolean; input1, input2, outcome: TFHIRSelectionList);
var
  pack : TFHIRPathDebugPackage;
begin
  if assigned(Ondebug) then
  begin
    pack := TFHIRPathDebugPackage.Create;
    try
      if (input2 = nil) then
      begin
        pack.SourceStart := exp.SourceLocationStart;
        pack.SourceEnd := exp.SourceLocationEnd;
      end
      else
      begin
        pack.SourceStart := exp.OpSourceLocationStart;
        pack.SourceEnd := exp.OpSourceLocationEnd;
      end;
      pack.Expression := exp.Link;
      pack.IsOperation := op;
      pack.context := context.Link;
      pack.input1 := input1.Link;
      pack.input2 := input2.Link;
      pack.outcome := outcome.Link;
      Ondebug(self, pack);
    finally
      pack.Free;
    end;
  end
  else
  begin
//    if op then
//      if input2 <> nil then
//        writeln(input1.ToString+' '+CODES_TFHIRPathOperation[exp.Operation]+' '+input2.ToString+' = ' + outcome.ToString)
//      else
//        writeln(input1.ToString+' '+CODES_TFHIRPathOperation[exp.Operation]+' (nil) = ' + outcome.ToString)
//    else
//      writeln('@'+exp.ToString+': '+input1.ToString+' -> '+outcome.ToString);
  end;
end;

destructor TFHIRPathEngine.destroy;
begin
  FLog.Free;
  worker.Free;
  primitiveTypes.Free;
  allTypes.Free;
  FUcum.Free;

  inherited;
end;

function TFHIRPathEngine.convertToString(items: TFHIRSelectionList): String;
var
  b : TStringBuilder;
  first : boolean;
  item : TFHIRSelection;
begin
  b := TStringBuilder.Create;
  try
    first := true;
    for item in items do
    begin
      if (first) then
        first := false
      else
        b.Append(',');
      b.Append(convertToString(item.value));
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPathEngine.evaluate(appInfo : TFslObject; base: TFHIRObject; path: String): TFHIRSelectionList;
var
  exp : TFHIRPathExpressionNode;
  list : TFHIRSelectionList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  exp := parse(path);
  try
    list := TFHIRSelectionList.Create(base.Link);
    try
      ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, nil, base.Link);
      try
        result := execute(ctxt, list, exp, true);
      finally
        ctxt.free;
      end;
    finally
      list.Free;
    end;
  finally
    exp.free;
  end;
end;

function TFHIRPathEngine.evaluate(appInfo : TFslObject; base: TFHIRObject; expr : TFHIRPathExpressionNodeV): TFHIRSelectionList;
var
  list : TFHIRSelectionList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  list := TFHIRSelectionList.Create(base.Link);
  try
    ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, nil, base.Link);
    try
      result := execute(ctxt, list, expr as TFHIRPathExpressionNode, true);
    finally
      ctxt.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRPathEngine.evaluate(appInfo : TFslObject; resource : TFHIRObject; base: TFHIRObject; path: String): TFHIRSelectionList;
var
  exp : TFHIRPathExpressionNode;
  list : TFHIRSelectionList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  exp := parse(path);
  try
    list := TFHIRSelectionList.Create(base.Link);
    try
      ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, resource.Link, base.Link);
      try
        result := execute(ctxt, list, exp, true);
      finally
        ctxt.Free;
      end;
    finally
      list.Free;
    end;
  finally
    exp.free;
  end;
end;

function TFHIRPathEngine.equal(left, right: TFHIRObject): boolean;
begin
  if (left.isPrimitive and right.isPrimitive) then
    result := left.primitiveValue = right.primitiveValue
  else
    result := compareDeep(left, right, false);
end;

function equivalentNumber(l, r : String) : boolean ;
begin
  if (l = '') and (r = '') then
    result := true
  else if (l = '') or (r = '') then
    result := false
  else
  begin
    l := l.ToLower.trim;
    r := r.ToLower.trim; // not that this should make any difference
    result := l.startsWith(r) or r.startsWith(l);
  end
end;

function TFHIRPathEngine.equivalent(left, right: TFHIRObject): boolean;
begin
  if (left.hasType('integer') and right.hasType('integer')) then
    result := equal(left, right)
  else if (left.hasType('boolean') and right.hasType('boolean')) then
    result := equal(left, right)
  else if (left.hasType(['integer', 'decimal']) and right.hasType(['integer', 'decimal'])) then
    result :=  equivalentNumber(left.primitiveValue(), right.primitiveValue())
  else if (left.hasType(['date', 'dateTime', 'time', 'instant']) and right.hasType(['date', 'dateTime', 'time', 'instant'])) then
    result :=  equivalentNumber(left.primitiveValue(), right.primitiveValue())
  else if (left.hasType(['string', 'id', 'code', 'uri']) and right.hasType(['string', 'id', 'code', 'uri'])) then
    result :=  equivalentNumber(convertToString(left), convertToString(right))
  else
    raise EFHIRPath.create(StringFormat('Unable to determine equivalence between %s and %s', [left.fhirType(), right.fhirType()]));
end;

function TFHIRPathEngine.evaluate(appInfo : TFslObject; resource : TFHIRObject; base: TFHIRObject; expr : TFHIRPathExpressionNodeV): TFHIRSelectionList;
var
  list : TFHIRSelectionList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  list := TFHIRSelectionList.Create(base.Link);
  try
    ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, resource.Link, base.Link);
    try
      result := execute(ctxt, list, expr as TFHIRPathExpressionNode, true);
    finally
      ctxt.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRPathEngine.evaluateCustomFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode): TFHIRTypeDetails;
begin
  raise EFHIRPath.create('Unknown Function '+exp.name);
end;

function TFHIRPathEngine.evaluateToBoolean(appInfo : TFslObject; resource : TFHIRObject; base: TFHIRObject; path: String): boolean;
var
  res : TFHIRSelectionList;
begin
  res := evaluate(appInfo, resource, base, path);
  try
    result := convertToBoolean(res);
  finally
    res.Free;
  end;
end;

function TFHIRPathEngine.evaluateToBoolean(appInfo: TFslObject; resource, base: TFHIRObject; expr: TFHIRPathExpressionNodeV): boolean;
var
  res : TFHIRSelectionList;
begin
  res := evaluate(appInfo, resource, base, expr);
  try
    result := convertToBoolean(res);
  finally
    res.Free;
  end;
end;

function TFHIRPathEngine.evaluateToString(appInfo: TFslObject; base: TFHIRObject; expr: TFHIRPathExpressionNodeV): string;
var
  res : TFHIRSelectionList;
begin
  res := evaluate(appInfo, base, expr);
  try
    result := convertToString(res);
  finally
    res.Free;
  end;
end;

function TFHIRPathEngine.evaluateToString(appInfo : TFslObject; base: TFHIRObject; path: String): string;
var
  res : TFHIRSelectionList;
begin
  res := evaluate(appInfo, base, path);
  try
    result := convertToString(res);
  finally
    res.Free;
  end;
end;

function TFHIRPathEngine.execute(context : TFHIRPathExecutionContext; item : TFHIRObject; exp : TFHIRPathExpressionNode; atEntry : boolean): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if atEntry and (CharInSet(exp.name[1], ['A'..'Z'])) then // special case for start up
    begin
      if (item.FhirType = exp.name) or StringArrayExistsSensitive(['Resource', 'DomainResource'], exp.name) then
        result.Add(item.Link);
    end
    else
      ListChildrenByName(item, exp.name, result);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRPathEngine.ListAllChildren(item : TFHIRObject; results : TFHIRSelectionList; recurse : boolean);
var
  pi : TFHIRPropertyIterator;
  b : TFHIRObject;
  s : String;
begin
  s := item.fhirtype;
  pi := item.createIterator(true, false);
  try
    while pi.More do
    begin
      if pi.Current.hasValue then
      begin
        for b in pi.Current.Values do
        begin
          results.Add(item.Link, pi.Current.Name, b.Link as TFHIRObject);
          if (recurse) then
            ListAllChildren(b as TFHIRObject, results, true);
        end;
      end;
      pi.Next;
    end;
  finally
    pi.free;
  end;
end;

function TFHIRPathEngine.executeType(ctxt: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode; atEntry : boolean): TFHIRTypeDetails;
var
  s : String;
  work, work2 : TFHIRTypeDetails;
  next, last : TFHIRPathExpressionNode;
begin
  result := TFHIRTypeDetails.Create(csNULL, []);
  try
    case exp.kind of
      enkName:
        if (exp.Name = '$this')  then
          result.update(ctxt.context)
        else
        begin
          for s in focus.types do
          begin
            work := executeType(s, exp, atEntry);
            try
              result.update(work);
            finally
              work.Free;
            end;
          end;
          if (result.hasNoTypes) then
            raise EFHIRPath.create('The name '+exp.Name+' was not valid for any of the possible types: '+focus.describe());
        end;
      enkFunction:
        begin
          work := evaluateFunctionType(ctxt, focus, exp);
          try
            result.update(work)
          finally
            work.Free;
          end;
        end;
      enkConstant:
        result.addType(readConstantType(ctxt, exp.Constant));
      enkGroup:
        begin
          work := executeType(ctxt, focus, exp.Group, atEntry);
          try
            result.update(work)
          finally
            work.Free;
          end;
        end;
    end;

    exp.types := result.Link;

    if (exp.Inner <> nil) then
    begin
      work := executeType(ctxt, result, exp.Inner, false);
      result.Free;
      result := work;
    end;

    if (exp.proximal and (exp.Operation <> popNull)) then
    begin
      next := exp.OpNext;
      last := exp;
      while (next <> nil) do
      begin
        if (last.Operation in [popIs, popAs]) then
          work := TFHIRTypeDetails.create(csSINGLETON, next.name)
        else
          work := executeType(ctxt, focus, next, atEntry);
        try
          work2 := operateTypes(result, last.Operation, work);
          result.Free;
          result := work2;
        finally
          work.Free;
        end;
        last := next;
        next := next.OpNext;
      end;
      exp.opTypes := result.Link;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcAll(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
  pc, res : TFHIRSelectionList;
  all, v : boolean;
begin
  result := TFHIRSelectionList.Create();
  try
    if (exp.Parameters.count = 1) then
    begin
      all := true;
      pc := TFHIRSelectionList.Create;
      try
        for item in focus do
        begin
          pc.Clear;
          pc.Add(item.Link);
          res := execute(context, pc, exp.Parameters[0], false);
          try
            if not convertToBoolean(res) then
            begin
              all := false;
              break;
            end;
          finally
            res.Free;
          end;
        end;
      finally
        pc.Free;
      end;
      result.add(TFhirBoolean.Create(all));
    end
    else // (exp.getParameters().size() == 0)
    begin
      all := true;
      for item in focus do
      begin
        if (item.value is TFHIRBoolean) then
          v := TFHIRBoolean(item.value).value
        else
          v := item.value <> nil;
        if (not v) then
        begin
          all := false;
          break;
        end;
      end;
      result.add(TFhirBoolean.Create(all));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcAs(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  tn : String;
  b : TFHIRSelection;
begin
  tn := exp.Parameters[0].name;
  result := TFHIRSelectionList.Create;
  try
    for b in focus do
      if (b.value.hasType(tn)) then
        result.add(b.Link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcChildren(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
begin
  result := TFHIRSelectionList.Create;
  try
    for item in focus do
      listAllChildren(item.value, result, false);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcContains(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  res : TFHIRSelectionList;
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).contains(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcCount(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirInteger.Create(inttostr(focus.Count)));
end;

function TFHIRPathEngine.funcDescendants(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
begin
  result := TFHIRSelectionList.Create;
  try
    for item in focus do
      listAllChildren(item.value, result, true);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcDistinct(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  i, j : integer;
  found : boolean;
begin
  if (focus.count <= 1) then
    exit(focus.Link);

  result := TFHIRSelectionList.Create;
  try
    for i := 0 to focus.count - 1 do
    begin
      found := false;
      for j := i+1 to focus.count - 1 do
      begin
        if (equal(focus[j].value, focus[i].value)) then
        begin
          found := true;
          break;
        end;
      end;
      if (not found) then
        result.add(focus[i].Link);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcEmpty(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirBoolean.Create(focus.Count = 0));
end;

function TFHIRPathEngine.funcEndsWith(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  res : TFHIRSelectionList;
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).endsWith(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcFirst(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if focus.Count > 0 then
    result.Add(focus[0].Link);
end;

function TFHIRPathEngine.funcHasValue(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
      result.add(TFHIRBoolean.create(focus[0].value.hasPrimitiveValue))
    else
      result.add(TFHIRBoolean.create(false));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcHtmlChecks(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    result.add(TFHIRBoolean.create(true));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIif(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  n1 : TFHIRSelectionList;
  v : boolean;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
  try
    v := convertToBoolean(n1);

    if (v) then
      result := execute(context, focus, exp.parameters[1], true)
    else if (exp.parameters.count < 3) then
      result := TFHIRSelectionList.Create
    else
      result := execute(context, focus, exp.parameters[2], true);
  finally
    n1.free;
  end;
end;

function TFHIRPathEngine.funcIs(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  tn : string;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 0) or (focus.count > 1) then
      result.add(TFHIRBoolean.create(false))
    else
    begin
      tn := exp.Parameters[0].name;
      result.add(TFHIRBoolean.create(focus[0].value.hasType(tn)));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.funcIsDistinct( context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  distinct : boolean;
  i , j : integer;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <= 1) then
      result.add(TFHIRBoolean.create(true))
    else
    begin
      distinct := true;
      for i := 0 to focus.count - 1 do
        for j := i+1 to focus.count - 1 do
          if (equal(focus[j].value, focus[i].value)) then
          begin
            distinct := false;
            break;
          end;
      result.add(TFHIRBoolean.create(distinct));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.funcItem(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : String;
  res : TFHIRSelectionList;
begin
  res := execute(context, focus, exp.Parameters[0], false);
  try
    s := convertToString(res);
  finally
    res.Free;
  end;
  result := TFHIRSelectionList.Create;
  if StringIsInteger16(s) and (focus.Count > StrToInt(s)) then
    result.Add(focus[StrToInt(s)].Link);
end;

function TFHIRPathEngine.funcLast(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if focus.Count > 0 then
    result.Add(focus[focus.Count - 1].Link);
end;

function TFHIRPathEngine.funcLength(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : String;
begin
  result := TFHIRSelectionList.Create();
  try
    if (focus.count = 1) then
    begin
      s := convertToString(focus[0].value);
      result.add(TFHIRInteger.create(inttostr(s.length)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcTrace(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  n1 : TFHIRSelectionList;
  name : String;
begin
  n1 := execute(context, focus, exp.Parameters[0], false);
  try
    name := n1[0].value.primitiveValue;
    log(name, convertToString(focus));
    result := focus.Link;
  finally
    n1.Free;
  end;
end;

function TFHIRPathEngine.funcMatches(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
  res : TFHIRSelectionList;
  s, p : String;
  reg : TRegEx;
begin
  result := TFHIRSelectionList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      p := convertToString(res);
    finally
      res.free;
    end;
    reg := TRegEx.Create(p, [roCompiled]);
    for item in focus do
    begin
      s := convertToString(item.value);
      if (reg.isMatch(s)) then
        result.Add(item.Link);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcMemberOf(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  raise EFHIRPathTodo.create('TFHIRPathEngine.funcMemberOf');
end;

function TFHIRPathEngine.funcNot(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirBoolean.Create(not convertToBoolean(focus)));
end;

function TFHIRPathEngine.funcNow(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirDateTime.Create(TFslDateTime.makeLocal));
end;

function TFHIRPathEngine.funcRepeat(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  current, added, pc, work : TFHIRSelectionList;
  item : TFHIRSelection;
  ctxt : TFHIRPathExecutionContext;
  more : boolean;
begin
  result := TFHIRSelectionList.Create;
  current := TFHIRSelectionList.Create;
  added := TFHIRSelectionList.Create;
  try
    current.AddAll(focus);
    more := true;
    while (more) do
    begin
      added.clear;
      pc := TFHIRSelectionList.Create;
      try
        for item in current do
        begin
          pc.clear();
          pc.add(item.link);
          ctxt := TFHIRPathExecutionContext.Create(context.AppInfo.Link, context.resource.Link, item.value.Link);
          try
            work := execute(ctxt, pc, exp.parameters[0], false);
            try
              added.addAll(work);
            finally
              work.Free;
            end;
          finally
            ctxt.Free;
          end;
        end;
      finally
        pc.free;
      end;
      more := added.Count > 0;
      result.addAll(added);
      current.clear();
      current.addAll(added);
    end;
    result.Link;
  finally
    current.Free;
    added.Free;
    result.Free;
  end;
end;

function TFHIRPathEngine.funcReplace(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  raise EFHIRPathTodo.create('TFHIRPathEngine.funcReplace');
end;

function TFHIRPathEngine.funcReplaceMatches( context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  raise EFHIRPathTodo.create('TFHIRPathEngine.funcReplaceMatches(');
end;

function TFHIRPathEngine.funcResolve(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
  s, id : String;
  p : TFHIRProperty;
  res, c : TFHIRObject;
begin
  result := TFHIRSelectionList.Create();
  try
    for item in focus do
    begin
      s := convertToString(item.value);
      if (item.value.fhirType = 'Reference') then
      begin
        p := item.value.getPropertyValue('reference');
        try
          if (p.hasValue) then
            s := convertToString(p.Values[0]);
        finally
          p.free;
        end;
      end;
      res := nil;
      if (s.startsWith('#')) then
      begin
        id := s.substring(1);
        p := context.resource.getPropertyValue('contained');
        try
          for c in p.Values do
          begin
            if (id = c.getId) then
            begin
              res := c;
              break;
            end;
          end
        finally
          p.Free;
        end;
      end
      else
      begin
        if not assigned(FOnResolveReference) then
          raise EFHIRPath.create('resolve() - resolution services are '+exp.name+' not implemented yet');
        res := FOnResolveReference(self, context.appInfo, s);
      end;
      if (res <> nil) then
        result.add(res);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcSelect(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  pc, work : TFHIRSelectionList;
  item : TFHIRSelection;
  ctxt : TFHIRPathExecutionContext;
begin
  result := TFHIRSelectionList.Create;
  try
    pc := TFHIRSelectionList.Create;
    try
      for item in focus do
      begin
        pc.clear();
        pc.add(item.link);
        ctxt := TFHIRPathExecutionContext.Create(context.AppInfo.Link, context.resource.Link, item.value.Link);
        try
          work := execute(ctxt, pc, exp.parameters[0], false);
          try
            result.addAll(work);
          finally
            work.Free;
          end;
        finally
          ctxt.Free;
        end;
      end;
    finally
      pc.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcSingle(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  if (focus.count <> 1) then
    raise EFHIRPath.create(StringFormat('Single() : checking for 1 item but found %d items', [focus.count]));
  result := focus.link;
end;

function TFHIRPathEngine.funcSkip(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  bl : TFHIRSelectionList;
  i, i1 : integer;
begin
  bl := execute(context, focus, exp.Parameters[0], true);
  try
    result := TFHIRSelectionList.Create;
    i1 := StrToInt(bl[0].value.primitiveValue);
    for i := i1 to focus.count - 1 do
      result.add(focus[i].Link);
  finally
    bl.free;
  end;
end;

function TFHIRPathEngine.funcStartsWith(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  res : TFHIRSelectionList;
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).startsWith(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcSubsetOf(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  target : TFHIRSelectionList;
  valid, found : boolean;
  item, t : TFHIRSelection;
begin
  target := execute(context, focus, exp.Parameters[0], true);
  try
    valid := true;
    for item in focus do
    begin
      found := false;
      for t in target do
      begin
        if (equal(item.value, t.value)) then
        begin
          found := true;
          break;
        end;
      end;
      if (not found) then
      begin
        valid := false;
        break;
      end;
    end;
    result := TFHIRSelectionList.Create(TFHIRBoolean.create(valid));
  finally
    target.free;
  end;
end;

function TFHIRPathEngine.funcSubString(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s, sw : String;
  n1, n2 : TFHIRSelectionList;
  i1, i2 : integer;
begin
  n1 := nil;
  n2 := nil;
  i2 := 0;
  result := TFHIRSelectionList.Create;
  try
    n1 := execute(context, focus, exp.Parameters[0], false);
    i1 := StrToInt(n1[0].value.primitiveValue);
    if (exp.ParameterCount = 2) then
    begin
      n2 := execute(context, focus, exp.Parameters[1], false);
      i2 := StrToInt(n2[0].value.primitiveValue);
    end;

    if focus.count = 1 then
    begin
      sw := convertToString(focus[0].value);
      if (i1 >= 0) and (i1 < sw.length) then
      begin
        if n2 <> nil then
          s := sw.Substring(i1, i2)
        else
          s := sw.Substring(i1);
        if (s <> '') then
          result.Add(TFhirString.Create(s));
      end;
    end;

    result.Link;
  finally
    n1.free;
    n2.free;
    result.Free;
  end;
end;

function TFHIRPathEngine.funcExists(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  result.add(TFHIRBoolean.create(focus.count > 0));
end;

function TFHIRPathEngine.funcSupersetOf( context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  target : TFHIRSelectionList;
  valid, found : boolean;
  item, t : TFHIRSelection;
begin
  target := execute(context, focus, exp.Parameters[0], true);
  try
    valid := true;
    for item in target do
    begin
      found := false;
      for t in focus do
      begin
        if (equal(item.value, t.value)) then
        begin
          found := true;
          break;
        end;
      end;
      if (not found) then
      begin
        valid := false;
        break;
      end;
    end;
    result := TFHIRSelectionList.Create(TFHIRBoolean.create(valid));
  finally
    target.free;
  end;
end;

function TFHIRPathEngine.funcExtension(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item, ex : TFHIRSelection;
  vl : TFHIRSelectionList;
  url : String;
  n1, ext : TFHIRSelectionList;
begin
  n1 := nil;
  result := TFHIRSelectionList.Create;
  try
    n1 := execute(context, focus, exp.Parameters[0], false);
    url := n1[0].value.primitiveValue;

    for item in focus do
    begin
      ext := TFHIRSelectionList.Create;
      try
        ListChildrenByName(item.value, 'extension', ext);
        ListChildrenByName(item.value, 'modifierExtension', ext);
        for ex in ext do
        begin
          vl := TFHIRSelectionList.Create;
          try
            ListChildrenByName(ex.value, 'url', vl);
            if convertToString(vl) = url then
              result.Add(ex.Link);
          finally
            vl.Free;
          end;
        end;
      finally
        ext.Free;
      end;
    end;

    result.Link;
  finally
    n1.free;
    result.Free;
  end;
end;

function TFHIRPathEngine.funcTail(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
   i : integer;
begin
  result := TFHIRSelectionList.Create;
  for i := 1 to focus.Count -1 do
    result.Add(focus[i].Link);
end;

function TFHIRPathEngine.funcTake(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  n1 : TFHIRSelectionList;
  i, i1 : integer;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
  try
    i1 := strtoint(n1[0].value.primitiveValue());

    result := TFHIRSelectionList.Create;
    for i := 0 to integerMin(focus.Count, i1) -1 do
      result.Add(focus[i].Link);
  finally
    n1.free;
  end;
end;

function TFHIRPathEngine.funcToday(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirDate.Create(TFslDateTime.makeToday));
end;

function TFHIRPathEngine.funcToDecimal(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRSelectionList.Create;
  if (StringIsDecimal(s)) then
    result.add(TFHIRDecimal.Create(s));
end;

function TFHIRPathEngine.funcToInteger(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRSelectionList.Create;
  if (StringIsInteger32(s)) then
    result.add(TFHIRInteger.Create(s));
end;

function TFHIRPathEngine.funcToString(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  result.add(TFHIRString.Create(convertToString(focus)));
end;

function TFHIRPathEngine.funcWhere(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  item : TFHIRSelection;
  pc, res : TFHIRSelectionList;
  ctxt : TFHIRPathExecutionContext;
begin
  result := TFHIRSelectionList.Create;
  try
    pc := TFHIRSelectionList.Create;
    try
      for item in focus do
      begin
        pc.Clear;
        pc.Add(item.Link);
        ctxt := TFHIRPathExecutionContext.Create(context.AppInfo.Link, context.resource.Link, item.value.Link);
        try
          res := execute(ctxt, pc, exp.Parameters[0], false);
          try
            if convertToBoolean(res) then
              result.Add(item.Link);
          finally
            res.Free;
          end;
        finally
          ctxt.Free;
        end;
      end;
    finally
      pc.Free;
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcAllFalse(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  pc, res : TFHIRSelectionList;
  all, v : boolean;
begin
  result := TFHIRSelectionList.Create();
  try
    if (exp.Parameters.count = 1) then
    begin
      all := true;
      pc := TFHIRSelectionList.Create;
      try
        for item in focus do
        begin
          pc.Clear;
          pc.Add(item.Link);
          res := execute(context, pc, exp.Parameters[0], false);
          try
            if convertToBoolean(res) then
            begin
              all := false;
              break;
            end;
          finally
            res.Free;
          end;
        end;
      finally
        pc.Free;
      end;
      result.add(TFhirBoolean.Create(all));
    end
    else // (exp.getParameters().size() == 0)
    begin
      all := true;
      for item in focus do
      begin
        if (item.value is TFHIRBoolean) then
          v := TFHIRBoolean(item.value).value
        else
          v := item.value <> nil;
        if (v) then
        begin
          all := false;
          break;
        end;
      end;
      result.add(TFhirBoolean.Create(all));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcAnyFalse(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  pc, res : TFHIRSelectionList;
  any, v : boolean;
begin
  result := TFHIRSelectionList.Create();
  try
    if (exp.Parameters.count = 1) then
    begin
      any := false;
      pc := TFHIRSelectionList.Create;
      try
        for item in focus do
        begin
          pc.Clear;
          pc.Add(item.Link);
          res := execute(context, pc, exp.Parameters[0], false);
          try
            if not convertToBoolean(res) then
            begin
              any := true;
              break;
            end;
          finally
            res.Free;
          end;
        end;
      finally
        pc.Free;
      end;
      result.add(TFhirBoolean.Create(any));
    end
    else // (exp.getParameters().size() == 0)
    begin
      any := false;
      for item in focus do
      begin
        if (item.value is TFHIRBoolean) then
          v := TFHIRBoolean(item.value).value
        else
          v := item.value <> nil;
        if (not v) then
        begin
          any := true;
          break;
        end;
      end;
      result.add(TFhirBoolean.Create(any));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcCombine(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  res : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    for item in focus do
      result.add(item.link);
    res := execute(context, focus, exp.Parameters[0], false);
    try
      for item in res do
        result.add(item.link);
    finally
      res.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcType(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcType');
end;

function TFHIRPathEngine.funcOfType(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  n1 : TFHIRSelectionList;
  tn : String;
  b : TFHIRSelection;
begin
  n1 := execute(context, focus, exp.Parameters[0], false);
  try
    tn := convertToString(n1);
    result := TFHIRSelectionList.Create;
    try
      for b in focus do
        if (b.value.hasType(tn)) then
          result.add(b.Link);
      result.Link;
    finally
      result.Free;
    end;
  finally
    n1.Free;
  end;
end;

function TFHIRPathEngine.funcElementDefinition(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcElementDefinition');
end;

function TFHIRPathEngine.funcSlice(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcSlice');
end;

function TFHIRPathEngine.funcCheckModifiers(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  n1, me, u : TFHIRSelectionList;
  item, e, b : TFHIRSelection;
  urlm, urlt : String;
  found, ok : boolean;
begin
  result := nil;
  n1 := execute(context, focus, exp.Parameters[0], false);
  try
    for item in focus do
    begin
      me := TFHIRSelectionList.Create;
      try
        ListChildrenByName(item.value, 'modifierExtension', me);
        ok := true;
        for e in me do
        begin
          u := TFHIRSelectionList.Create;
          try
            ListChildrenByName(e.value, 'url', u);
            urlm := convertToString(u);
            found := false;
            for b in n1 do
            begin
              urlt := b.toString;
              found := urlm = urlt;
            end;
            if not found then
              ok := false;
          finally
            u.Free;
          end;
        end;
        result := TFHIRSelectionList.Create();
        result.add(TFhirBoolean.Create(ok));
      finally
        me.Free;
      end;
    end;
  finally
    n1.Free;
  end;
end;

function TFHIRPathEngine.funcConformsTo(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcConformsTo');
end;


function isBoolean(list : TFHIRSelectionList; b : boolean) : boolean;
begin
  result := (list.count = 1) and (list[0].value is TFHIRBoolean) and (TFHIRBoolean(list[0].value).value = b);
end;

function TFHIRPathEngine.preOperate(left: TFHIRSelectionList; op: TFHIRPathOperation): TFHIRSelectionList;
begin
  result := nil;
  case op of
    popAnd: if isBoolean(left, false) then
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(false));
    popOr: if isBoolean(left, true) then
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(true));
    popImplies: if (not convertToBoolean(left)) then
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(true));
  end;
end;

function TFHIRPathEngine.operate(left: TFHIRSelectionList; op: TFHIRPathOperation; right: TFHIRSelectionList): TFHIRSelectionList;
begin
  case op of
    popNull: raise EFHIRPath.create('An internal error has occurred');
    popEquals: result := opequal(left, right);
    popEquivalent: result := opEquivalent(left, right);
    popNotEquals: result := opNotequal(left, right);
    popNotEquivalent: result := opNotEquivalent(left, right);
    popLessThan: result := opLessThan(left, right);
    popGreater: result := opGreater(left, right);
    popLessOrEqual: result := opLessOrEqual(left, right);
    popGreaterOrEqual: result := opGreaterOrEqual(left, right);
    popUnion: result := opUnion(left, right);
    popIn: result := opIn(left, right);
    popContains: result := opContains(left, right);
    popOr:  result := opOr(left, right);
    popAnd:  result := opAnd(left, right);
    popXor: result := opXor(left, right);
    popImplies: result := opImplies(left, right);
    popPlus: result := opPlus(left, right);
    popConcatenate: result := opConcatenate(left, right);
    popTimes: result := opTimes(left, right);
    popMinus: result := opMinus(left, right);
    popDivideBy: result := opDivideBy(left, right);
    popDiv: result := opDiv(left, right);
    popMod: result := opMod(left, right);
    popIs: result := opIs(left, right);
    popAs: result := opAs(left, right);
    popCustom : raise EFHIRPath.create('An internal error has occurred (custom operation not implemented)');
  else
    raise EFHIRPath.create('An internal error has occurred (operation not implemented)');
  end;
end;


function TFHIRPathEngine.operateTypes(left: TFHIRTypeDetails; op: TFHIRPathOperation; right: TFHIRTypeDetails): TFHIRTypeDetails;
begin
  case op of
    popEquals: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popEquivalent: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popNotEquals: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popNotEquivalent: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popLessThan: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popGreater: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popLessOrEqual: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popGreaterOrEqual: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popIs: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popAs: result := TFHIRTypeDetails.createList(csSINGLETON, right.Types);
    popUnion: result := left.union(right);
    popOr: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popAnd: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popXor: result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popImplies : result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popTimes: begin
      result := TFHIRTypeDetails.create(csSINGLETON, ['']);
      if (left.hasType('integer')) and (right.hasType('integer')) then
        result.addType('integer')
      else if (left.hasType(['integer', 'decimal'])) and (right.hasType(['integer', 'decimal'])) then
        result.addType('decimal');
    end;
    popDivideBy: begin
      result := TFHIRTypeDetails.create(csSINGLETON, ['']);
      if (left.hasType('integer')) and (right.hasType('integer')) then
        result.addType('decimal')
      else if (left.hasType(['integer', 'decimal'])) and (right.hasType(['integer', 'decimal'])) then
        result.addType('decimal');
    end;
    popPlus:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, ['']);
      if (left.hasType('integer')) and (right.hasType('integer')) then
        result.addType('integer')
      else if (left.hasType(['integer', 'decimal'])) and (right.hasType(['integer', 'decimal'])) then
        result.addType('decimal')
      else if (left.hasType(['string', 'id', 'code', 'uri'])) and (right.hasType(['string', 'id', 'code', 'uri'])) then
        result.addType('string');
    end;
    popConcatenate : result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
    popMinus:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, ['']);
      if (left.hasType('integer')) and (right.hasType('integer')) then
        result.addType('integer')
      else if (left.hasType(['integer', 'decimal'])) and (right.hasType(['integer', 'decimal'])) then
        result.addType('decimal');
    end;
    popDiv, popMod:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, ['']);
      if (left.hasType('integer')) and (right.hasType('integer')) then
        result.addType('integer')
      else if (left.hasType(['integer', 'decimal'])) and (right.hasType(['integer', 'decimal'])) then
        result.addType('decimal');
    end;
    popIn:  result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popContains:  result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
    popCustom : raise EFHIRPath.create('An internal error has occurred (operation not implemented)');
  else
    raise EFHIRPathTodo.create('TFHIRPathEngine.operateTypes');
  end;
end;

function TFHIRPathEngine.opAnd(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
  if (left.Empty) and (right.empty) then
    // nothing
  else if (isBoolean(left, false)) or (isBoolean(right, false)) then
    result.Add(TFhirBoolean.Create(false))
  else if (left.Empty) or (right.Empty) then
    // noothing
  else if (convertToBoolean(left)) and (convertToBoolean(right)) then
    result.Add(TFhirBoolean.Create(true))
  else
    result.Add(TFhirBoolean.Create(false));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.opAs(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  tn : String;
  b : TFHIRSelection;
begin
  tn := convertToString(right);
  result := TFHIRSelectionList.Create;
  try
    for b in left do
      if (b.value.hasType(tn)) then
        result.add(b.Link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opContains(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  ans, f : boolean;
  l, r : TFHIRSelection;
begin
  ans := true;
  for r in right do
  begin
    f := false;
    for l in left do
      if equal(l.value, r.value) then
      begin
        f := true;
        break;
      end;
    if not f then
    begin
      ans := false;
      break;
    end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(ans));
end;

function TFHIRPathEngine.opDiv(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1, d2, d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing div: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing div: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing div: left operand has the wrong type (%s)', [left[0].value.fhirType]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing div: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing div: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing div: right operand has the wrong type (%s)', [right[0].value.fhirType]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strtoInt(l.primitiveValue()) div strtoInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.divInt(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing div: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opDivideBy(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1, d2, d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing /: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing /: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing /: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing /: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing /: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.divide(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing /: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));
    result.link;
  finally
    result.free;
  end;

end;

function TFHIRPathEngine.opequal(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  res : boolean;
  i : integer;
begin
  if left.count <> right.count then
    res := false
  else
  begin
    res := true;
    for i := 0 to left.count - 1 do
      if not equal(left[i].value, right[i].value) then
      begin
        res := false;
        break;
      end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRPathEngine.opEquivalent(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  res, found : boolean;
  i, j : integer;
begin
  if left.count <> right.count then
    res := false
  else
  begin
    res := true;
    for i := 0 to left.count - 1 do
    begin
      found := false;
      for j := 0 to right.count - 1 do
      begin
        if equivalent(left[i].value, right[j].value) then
        begin
          found := true;
          break;
        end;
      end;
      if (not found) then
      begin
        res := false;
        break;
      end;
    end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRPathEngine.opGreater(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  lUnit, rUnit : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) > StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
        ListChildrenByName(left[0].value, 'unit', lUnit);
        ListChildrenByName(right[0].value, 'unit', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opGreater(lUnit, rUnit);
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn''t done yet');
      finally
        lUnit.Free;
        rUnit.Free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opGreaterOrEqual(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  lUnit, rUnit : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) >= StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue));
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
        ListChildrenByName(left[0].value, 'unit', lUnit);
        ListChildrenByName(right[0].value, 'unit', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opGreaterOrEqual(lUnit, rUnit);
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn"t done yet');
      finally
        lUnit.Free;
        rUnit.Free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opIn(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  ans, f : boolean;
  l, r : TFHIRSelection;
begin
  ans := true;
  for l in left do
  begin
    f := false;
    for r in right do
      if equal(l.value, r.value) then
      begin
        f := true;
        break;
      end;
    if not f then
    begin
      ans := false;
      break;
    end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(ans));
end;

function TFHIRPathEngine.opIs(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  tn : string;
begin
  result := TFHIRSelectionList.Create;
  try
    if (left.count = 0) or (left.count > 1) then
      result.add(TFHIRBoolean.create(false))
    else
    begin
      tn := convertToString(right);
      result.add(TFHIRBoolean.create(left[0].value.hasType(tn)));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.opLessOrEqual(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  lUnit, rUnit : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value;
      r := right[0].value;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) <= StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
        ListChildrenByName(left[0].value, 'unit', lUnit);
        ListChildrenByName(right[0].value, 'unit', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opLessOrEqual(lUnit, rUnit);
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn"t done yet (units =  '+lUnit[0].value.primitiveValue+'/'+rUnit[0].value.primitiveValue+')');
      finally
        lUnit.Free;
        rUnit.Free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opLessThan(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  lUnit, rUnit : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) < StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
        ListChildrenByName(left[0].value, 'unit', lUnit);
        ListChildrenByName(right[0].value, 'unit', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opLessThan(lUnit, rUnit);
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn"t done yet');
      finally
        lUnit.Free;
        rUnit.Free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opMinus(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1,d2,d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing -: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing -: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing -: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing -: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;


    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) - strToInt(r.primitiveValue()))))
    else if (l.hasType('decimal')) and (r.hasType('decimal')) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Subtract(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing -: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opMod(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1, d2, d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing mod: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing mod: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing mod: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing mod: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing mod: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing mod: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) mod strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Modulo(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing mod: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opNotequal(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  res : boolean;
  i : integer;
begin
  if left.count <> right.count then
    res := true
  else
  begin
    res := false;
    for i := 0 to left.count - 1 do
      if not equal(left[i].value, right[i].value) then
      begin
        res := true;
        break;
      end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRPathEngine.opNotEquivalent(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  res, found : boolean;
  i, j : integer;
begin
  if left.count <> right.count then
    res := false
  else
  begin
    res := true;
    for i := 0 to left.count - 1 do
    begin
      found := false;
      for j := 0 to right.count - 1 do
      begin
        if equivalent(left[i].value, right[j].value) then
        begin
          found := true;
          break;
        end;
      end;
      if (not found) then
      begin
        res := false;
        break;
      end;
    end;
  end;
  result := TFHIRSelectionList.Create;
  result.Add(TFhirBoolean.Create(not res));
end;

function TFHIRPathEngine.opOr(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if (left.Empty) and (right.Empty) then
    // nothing
  else if (convertToBoolean(left)) or (convertToBoolean(right)) then
    result.Add(TFhirBoolean.Create(true))
  else if (left.Empty) or (right.Empty) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(false));
end;

function TFHIRPathEngine.opConcatenate(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFHIRString.create(convertToString(left) + convertToString(right)));
end;

function TFHIRPathEngine.opPlus(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1,d2,d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing +: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing +: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing +: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing +: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType(['string', 'id', 'code', 'uri'])) and (r.hasType(['string', 'id', 'code', 'uri'])) then
      result.add(TFHIRString.create(l.primitiveValue() + r.primitiveValue()))
    else if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) + strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Add(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing +: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opTimes(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1, d2, d3 : TFslDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing *: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing *: left operand has more than one value');
  if (not left[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing *: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing *: right operand has more than one value');
  if (not right[0].value.isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing *: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) * strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Multiply(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing /: left and right operand have incompatible or illegal types (%s, %s)', [left[0].value.fhirType(), right[0].value.fhirType()]));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.contains(list : TFHIRSelectionList; item : TFHIRObject) : boolean;
var
  test : TFHIRSelection;
begin
  result := false;
  for test in list do
    if equal(test.value, item) then
        exit(true);
end;

function TFHIRPathEngine.opUnion(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  item : TFHIRSelection;
begin
  result := TFHIRSelectionList.create;
  try
    for item in left do
      if not contains(result, item.value) then
        result.add(item.link);
    for item in right do
      if not contains(result, item.value) then
        result.add(item.link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.opXor(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if (left.Empty) or (right.Empty) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(convertToBoolean(left) xor convertToBoolean(right)));
end;

function TFHIRPathEngine.opImplies(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if (not convertToBoolean(left)) then
    result.Add(TFhirBoolean.Create(true))
  else if (right.count = 0) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(convertToBoolean(right)));
end;

function TFHIRPathEngine.execute(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode; atEntry : boolean): TFHIRSelectionList;
var
  work, work2 : TFHIRSelectionList;
  item, base : TFHIRSelection;
  outcome : TFHIRSelectionList;
  next, last : TFHIRPathExpressionNode;
begin
  work := TFHIRSelectionList.Create;
  try
    case exp.kind of
      enkName:
        if (exp.name = '$this') then
          work.add(context.context.Link)
        else
          for item in focus do
          begin
            outcome := execute(context, item.value, exp, atEntry);
            try
              for base in outcome do
                if (base.value <> nil) then
                  work.Add(base.Link);
            finally
              outcome.Free;
            end;
          end;
      enkFunction:
        begin
        work2 := evaluateFunction(context, focus, exp);
        try
          work.addAll(work2);
        finally
          work2.Free;
        end;
        end;
      enkConstant:
        begin
        item := TFHIRSelection.Create(readConstant(context, exp.constant));
        if (item.value <> nil) then
          work.Add(item)
        else
          item.free;
        end;
      enkGroup:
        begin
        work2 := execute(context, focus, exp.group, atEntry);
        try
          work.addAll(work2);
        finally
          work2.Free;
        end;
        end;
    end;

    Debug(context, exp, false, focus, nil, work);

    if (exp.Inner <> nil) then
    begin
      result := execute(context, work, exp.Inner, false);
      work.Free;
      work := result;
    end;

    if (exp.proximal and (exp.Operation <> popNull)) then
    begin
      next := exp.OpNext;
      last := exp;
      while (next <> nil) do
      begin
        // and and or - may be able to avoid executing the right side
        work2 := preOperate(work, last.Operation);
        if work2 <> nil then
        begin
          Debug(context, exp, true, work, nil, work2);
          work.Free;
          work := work2;
        end
        else
        begin
          if (last.Operation in [popIs, popAs]) then
            work2 := TFHIRSelectionList.Create(TFHIRString.Create(next.name))
          else
            work2 := execute(context, focus, next, true);
          try
            result := operate(work, last.Operation, work2);
            try
              Debug(context, exp, true, work, work2, result);
            finally
              work.Free;
              work := result;
            end;
          finally
            work2.Free;
          end;
        end;
        last := next;
        next := next.OpNext;
      end;
    end;
    result := work.Link;
  finally
    work.Free;
  end;
end;

function TFHIRPathEngine.executeType(focus: String; exp: TFHIRPathExpressionNode; atEntry : boolean): TFHIRTypeDetails;
begin
  if (atEntry and exp.Name[1].IsUpper) and (focus = exp.Name) then
    result := TFHIRTypeDetails.create(csSINGLETON, [focus])
  else
  begin
    result := TFHIRTypeDetails.create(csNULL, []);
    try
      ListChildTypesByName(focus, exp.name, result);
      result.Link;
    finally
      result.Free;
    end;
  end;
end;

function TFHIRPathEngine.executeV(context: TFHIRPathExecutionContext; item: TFHIRObject; exp: TFHIRPathExpressionNodeV; atEntry: boolean): TFHIRSelectionList;
begin
  result := execute(context, item, exp as TFHIRPathExpressionNode, atEntry);
end;

function TFHIRPathEngine.executeV(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNodeV; atEntry: boolean): TFHIRSelectionList;
begin
  result := execute(context, focus, exp as TFHIRPathExpressionNode, atEntry);
end;

function TFHIRPathEngine.evaluateFunction(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  case exp.FunctionId of
    pfEmpty : result := funcEmpty(context, focus, exp);
    pfNot : result := funcNot(context, focus, exp);
    pfExists : result := funcExists(context, focus, exp);
    pfSubsetOf : result := funcSubsetOf(context, focus, exp);
    pfSupersetOf : result := funcSupersetOf(context, focus, exp);
    pfIsDistinct : result := funcIsDistinct(context, focus, exp);
    pfDistinct : result := funcDistinct(context, focus, exp);
    pfCount : result := funcCount(context, focus, exp);
    pfWhere : result := funcWhere(context, focus, exp);
    pfSelect : result := funcSelect(context, focus, exp);
    pfAll : result := funcAll(context, focus, exp);
    pfRepeat : result := funcRepeat(context, focus, exp);
    pfItem : result := funcItem(context, focus, exp);
    pfAs : result := funcAs(context, focus, exp);
    pfIs : result := funcIs(context, focus, exp);
    pfSingle : result := funcSingle(context, focus, exp);
    pfFirst : result := funcFirst(context, focus, exp);
    pfLast : result := funcLast(context, focus, exp);
    pfTail : result := funcTail(context, focus, exp);
    pfSkip : result := funcSkip(context, focus, exp);
    pfTake : result := funcTake(context, focus, exp);
    pfIif : result := funcIif(context, focus, exp);
    pfToInteger : result := funcToInteger(context, focus, exp);
    pfToDecimal : result := funcToDecimal(context, focus, exp);
    pfToString : result := funcToString(context, focus, exp);
    pfSubstring : result := funcSubstring(context, focus, exp);
    pfStartsWith : result := funcStartsWith(context, focus, exp);
    pfEndsWith : result := funcEndsWith(context, focus, exp);
    pfMatches : result := funcMatches(context, focus, exp);
    pfReplaceMatches : result := funcReplaceMatches(context, focus, exp);
    pfContains : result := funcContains(context, focus, exp);
    pfReplace : result := funcReplace(context, focus, exp);
    pfLength : result := funcLength(context, focus, exp);
    pfChildren : result := funcChildren(context, focus, exp);
    pfDescendants : result := funcDescendants(context, focus, exp);
    pfMemberOf : result := funcMemberOf(context, focus, exp);
    pfTrace : result := funcTrace(context, focus, exp);
    pfToday : result := funcToday(context, focus, exp);
    pfNow : result := funcNow(context, focus, exp);
    pfResolve: result := funcResolve(context, focus, exp);
    pfExtension: result := funcExtension(context, focus, exp);
    pfAllFalse: result := funcAllFalse(context, focus, exp);
    pfAnyFalse: result := funcAnyFalse(context, focus, exp);
    pfCombine: result := funcCombine(context, focus, exp);
    pfType: result := funcType(context, focus, exp);
    pfOfType: result := funcOfType(context, focus, exp);
    pfElementDefinition: result := funcElementDefinition(context, focus, exp);
    pfSlice: result := funcSlice(context, focus, exp);
    pfCheckModifiers: result := funcCheckModifiers(context, focus, exp);
    pfConformsTo: result := funcConformsTo(context, focus, exp);
    pfHasValue : result := funcHasValue(context, focus, exp);
    pfHtmlChecks : result := funcHtmlChecks(context, focus, exp);
    pfCustom : result := funcCustom(context, focus, exp);
  else
    raise EFHIRPath.create('Unknown Function '+exp.name);
  end;
end;

function TFHIRPathEngine.funcCustom(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  raise EFHIRPath.create('Unknown Function '+exp.name);
end;

function TFHIRPathEngine.check(appInfo: TFslObject; resourceType, context, path: String; expr: TFHIRPathExpressionNodeV; xPathStartsWithValueRef: boolean): TFHIRTypeDetailsV;
begin
  result := check(appInfo, resourceType, Context, path, expr as TFHIRPathExpressionNode, xPathStartsWithValueRef);
end;

procedure TFHIRPathEngine.checkParamTypes(funcId : TFHIRPathFunction; paramTypes : TFslList<TFHIRTypeDetails>; typeSet : array of TFHIRTypeDetails);
var
  i : integer;
  pt, actual : TFHIRTypeDetails;
  a : String;
begin
  try
    i := 0;
    for pt in typeSet do
    begin
      if (i = paramTypes.count) then
        exit;
      actual := paramTypes[i];
      inc(i);
      for a in actual.types do
        if (not pt.hasType(a)) then
          raise EFHIRPath.create('The parameter type "'+a+'" is not legal for '+CODES_TFHIRPathFunctions[funcId]+' parameter '+Integer.toString(i)+', expecting '+pt.describe());
    end;
  finally
    for pt in typeSet do
      pt.Free;
  end;
end;

function TFHIRPathEngine.childTypes(focus : TFHIRTypeDetails; mask : string) : TFHIRTypeDetails;
var
  f : String;
begin
  result := TFHIRTypeDetails.create(csUNORDERED, []);
  try
    for f in focus.types do
      ListChildTypesByName(f, mask, result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.evaluateFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode): TFHIRTypeDetails;
var
  expr : TFHIRPathExpressionNode;
  paramTypes : TFslList<TFHIRTypeDetails>;
  nc : TFHIRPathExecutionTypeContext;
begin
  paramTypes := TFslList<TFHIRTypeDetails>.create;
  try
    if (exp.FunctionId in [pfIs, pfAs]) then
      paramTypes.add(TFHIRTypeDetails.create(csSINGLETON, ['string']))
    else
    begin
      if (exp.FunctionId in [pfWhere, pfSelect, pfRepeat]) then
        nc := TFHIRPathExecutionTypeContext.Create(context.appInfo.Link, context.FResourceType, focus.Link)
      else
        nc := context.Link;
      try
        for expr in exp.Parameters do
          paramTypes.add(executeType(nc, focus, expr, true));
      finally
        nc.Free;
      end;
    end;

    case exp.FunctionId of
      pfEmpty :
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfNot :
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfExists :
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfSubsetOf :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [focus.link]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfSupersetOf :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [focus.link]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfIsDistinct :
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfDistinct :
        result := focus.Link;
      pfCount :
        result := TFHIRTypeDetails.create(csSINGLETON, ['integer']);
      pfWhere :
        result := focus.Link;
      pfSelect :
        result := TFHIRTypeDetails.createList(focus.CollectionStatus, allTypes);
      pfAll :
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfRepeat :
        result := TFHIRTypeDetails.createList(focus.CollectionStatus, allTypes);
      pfItem :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
         checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['integer'])]);
        result := focus.Link;
        end;
      pfAs :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, exp.Parameters[0].Name);
        end;
      pfIs :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfSingle :
        result := focus.toSingleton();
      pfFirst :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        result := focus.toSingleton();
        end;
      pfLast :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        result := focus.toSingleton();
        end;
      pfTail :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        result := focus.Link;
        end;
      pfSkip :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['integer'])]);
        result := focus.Link;
        end;
      pfTake :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['integer'])]);
        result := focus.Link;
        end;
      pfIif :
        begin
        result := TFHIRTypeDetails.create(csNull, []);
        result.update(paramTypes[0]);
        if (paramTypes.count > 1) then
          result.update(paramTypes[1]);
        end;
      pfToInteger :
        begin
        if (not focus.hasType(primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['integer']);
        end;
      pfToDecimal :
        begin
        if (not focus.hasType(primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['decimal']);
        end;
      pfToString :
        begin
        if (not focus.hasType(primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
        end;
      pfSubstring :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['integer']), TFHIRTypeDetails.create(csSINGLETON, ['integer'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
        end;
      pfStartsWith :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfEndsWith :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfMatches :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfReplaceMatches :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string']), TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
        end;
      pfContains :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfReplace :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string']), TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
        end;
      pfLength :
        begin
        if (not focus.hasType(primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['integer']);
        end;
      pfChildren :
        result := childTypes(focus, '*');
      pfDescendants :
        result := childTypes(focus, '**');
      pfMemberOf :
        begin
        if (not focus.hasType(['string', 'code', 'uri', 'Coding', 'CodeableConcept'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, Coding, CodeableConcept not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfTrace :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := focus.Link;
        end;
      pfToday :
        result := TFHIRTypeDetails.create(csSINGLETON, ['date']);
      pfNow :
        result := TFHIRTypeDetails.create(csSINGLETON, ['dateTime']);
      pfResolve :
        begin
        if (not focus.hasType(['uri', 'Reference'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on uri, Reference not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['DomainResource']);
        end;
      pfExtension :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['Extension']);
        end;
      pfAllFalse:
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfAnyFalse:
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfCombine:
        begin
        result := TFHIRTypeDetails.create(csUNORDERED, []);
        result.update(focus);
        result.update(paramTypes[0]);
        end;
      pfType:
        result := TFHIRTypeDetails.create(csSINGLETON, ['SimpleTypeInfo']);
      pfOfType:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(focus.CollectionStatus, ['*']);
        end;
      pfElementDefinition:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['ElementDefinition']);
        end;
      pfSlice:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string', 'string'])]);
        result := focus.Link;
        end;
      pfCheckModifiers:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csUNORDERED, ['string'])]);
        result := focus.Link;
        end;
      pfConformsTo:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
        end;
      pfHasValue:
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfHtmlChecks:
        result := TFHIRTypeDetails.create(csSINGLETON, ['boolean']);
      pfCustom :
        result := evaluateCustomFunctionType(context, focus, exp);
    else
      raise EFHIRPath.create('not Implemented yet?');
    end;
  finally
    paramTypes.Free;
  end;
end;


function processDateConstant(s : String) : TFHIRType;
var
  v : String;
  i : integer;
begin
  if (s.startsWith('T')) then
    exit(TFHIRTime.create(s.substring(1)));
  v := s;
  if (v.length > 10) then
  begin
    i := v.substring(10).indexOf('-');
    if (i = -1) then
      i := v.substring(10).indexOf('+');
    if (i = -1) then
      i := v.substring(10).indexOf('Z');
    if i = -1 then
      v := s
    else
      v := v.substring(0,  10+i);
  end;
  if (v.length > 10) then
    result := TFHIRDateTime.create(TFslDateTime.fromXML(s))
  else
    result := TFHIRDate.create(TFslDateTime.fromXML(s));
end;

function TFHIRPathEngine.readConstant(context : TFHIRPathExecutionContext; constant: String): TFHIRObject;
begin
  if (constant = 'true') then
    result := TFhirBoolean.Create(true)
  else if (constant = 'false') then
    result := TFhirBoolean.Create(false)
  else if (constant = '{}') then
    result := nil
  else if StringIsInteger32(constant) then
    result := TFhirInteger.Create(constant)
  else if StringIsDecimal(constant) then
    result := TFhirDecimal.Create(constant)
  else if constant.StartsWith('''') then
    result := TFhirString.Create(TFHIRPathLexer.processConstant(constant))
  else if constant.StartsWith('%') then
    result := replaceFixedConstant(context, constant)
  else if constant.StartsWith('@') then
    result := processDateConstant(constant.Substring(1))
  else
    result := TFhirString.Create(constant);
end;

function TFHIRPathEngine.replaceFixedConstant(context : TFHIRPathExecutionContext; const s: String): TFHIRObject;
begin
  if s = '%sct' then
    result := TFhirString.Create('http://snomed.info/sct')
  else if s = '%loinc' then
    result := TFhirString.Create('http://loinc.org')
  else if s = '%ucum' then
    result := TFhirString.Create('http://unitsofmeasure.org')
  else if s = '%resource' then
  begin
    if (context.resource = nil) then
      raise EFHIRPath.create('%resource cannot be used in this context');
    result := context.resource.link;
  end
  else if s = '%us-zip' then
    result := TFhirString.Create('[0-9]{5}(-[0-9]{4}){0,1}"')
  else if s.StartsWith('%"vs-') then
    result := TFhirString.Create('http://hl7.org/fhir/ValueSet/'+s.Substring(5, s.length-6))
  else if s.StartsWith('%"cs-') then
    result := TFhirString.Create('http://hl7.org/fhir/'+s.Substring(5, s.length-6))
  else if s.StartsWith('%"ext-') then
    result := TFhirString.Create('http://hl7.org/fhir/StructureDefinition/'+s.Substring(6, s.length-7))
  else
    raise EFHIRPath.create('Unknown fixed constant '+s);
end;


function TFHIRPathEngine.UseLog: String;
begin
  if (FLog <> nil) and (FLog.Length > 0) then
  begin
    result := ' ('+FLog.ToString+')';
    FLog.Clear;
  end
  else
    result := '';
end;

function TFHIRPathEngine.readConstantType(ctxt: TFHIRPathExecutionTypeContext; constant: String): string;
begin
  if (constant = 'true') then
    result := 'boolean'
  else if (constant = 'false') then
    result := 'boolean'
  else if StringIsInteger32(constant) then
    result := 'integer'
  else if IsNumericString(constant) then
    result := 'decimal'
  else if constant = '%resource' then
  begin
    if (ctxt.resourceType = '') then
      raise EFHIRPath.create('%resource cannot be used in this context');
    result := ctxt.resourceType;
  end
  else
    result := 'string';
end;

function TFHIRPathEngine.parse(path: String): TFHIRPathExpressionNode;
var
  parser : TFHIRPathParser;
begin
  parser := TFHIRPathParser.Create;
  try
    result := parser.parse(path);
    finally
    parser.Free;
  end;
end;

function TFHIRPathEngine.parseV(path: String): TFHIRPathExpressionNodeV;
begin
  result := parse(path);
end;

procedure TFHIRPathEngine.ListChildrenByName(focus: TFHIRObject; name: String; results: TFHIRSelectionList);
begin
  focus.ListChildrenByName(name, results);
end;

procedure TFHIRPathEngine.ListChildTypesByName(item, name : String; result : TFHIRTypeDetails);
var
  url, tail, specifiedType, path, tn, r, rn : String;
  sd, dt, sdi : TFhirStructureDefinition;
  sdl : TFslList<TFhirStructureDefinition>;
  ed : TFhirElementDefinition;
  t : TFhirElementDefinitionType;
  rt : TFslStringSet;
begin
  if (item = '') then
    raise EFHIRPath.create('No type provided in BuildToolPathEvaluator.ListChildTypesByName');
  if (item.equals('xhtml')) then
    exit;
  if (item.equals('Custom')) then
    exit;
  if (item.contains('.')) then
    url := 'http://hl7.org/fhir/StructureDefinition/'+item.substring(0, item.indexOf('.'))
  else
    url := 'http://hl7.org/fhir/StructureDefinition/'+item;
  sd := worker.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
  if (sd = nil) then
    raise EFHIRPath.create('Unknown item '+item); // this really is an error, because we can only get to here if the internal infrastrucgture is wrong
  ed := nil;
  sdl := TFslList<TFhirStructureDefinition>.create;
  try
    if (item.contains('.')) then
      ed := getElementDefinition(sd, item, true, specifiedType);
    if ((ed <> nil) and hasDataType(ed)) then
    begin
      if specifiedType <> '' then
      begin
        dt := worker.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+specifiedType) as TFhirStructureDefinition;
        if (dt = nil) then
          raise EFHIRPath.create('unknown data type '+specifiedType);
        sdl.add(dt);
      end
      else
        for t in ed.type_List do
        begin
          dt := worker.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+t.Code) as TFhirStructureDefinition;
          if (dt = nil) then
            raise EFHIRPath.create('unknown data type '+t.code);
          sdl.add(dt);
        end;
    end
    else
    begin
      sdl.add(sd.Link);
      if (item.contains('.')) then
        tail := item.substring(item.indexOf('.'));
    end;

    for sdi in sdl do
    begin
      path := sdi.snapshot.elementList[0].path+tail+'.';
      if (name = '**') then
      begin
        assert(result.CollectionStatus = csUNORDERED);
        for ed in sdi.snapshot.elementList do
        begin
          if (ed.path.startsWith(path)) then
            for t in ed.type_List do
            begin
              if (t.code = 'Element') or (t.code = 'BackboneElement') then
                tn := ed.path
              else
                tn := t.code;
              if (tn = 'Resource') then
              begin
                rt := worker.getResourceNames();
                try
                  for rn in rt do
                  begin
                    if (not result.hasType(rn)) then
                    begin
                      result.addType(rn);
                      listChildTypesByName(rn, '**', result);
                    end;
                  end;
                finally
                  rt.free;
                end;
              end
              else if (not result.hasType(tn)) and (tn <> '') then
              begin
                result.addType(tn);
                ListChildTypesByName(tn, '**', result);
              end;
            end;
        end;
      end
      else if (name.equals('*')) then
      begin
        for ed in sdi.snapshot.elementList do
        begin
          if (ed.path.startsWith(path) and not ed.path.substring(path.length).contains('.')) then
            for t in ed.type_List do
              if (t.code = 'Element') or (t.code = 'BackboneElement') then
                result.addType(ed.path)
              else if (t.code = 'Resource') then
              begin
                rt := worker.getResourceNames;
                try
                  for r in rt do
                    result.addType(r)
                finally
                  rt.Free;
                end;
              end
              else
                result.addType(t.code);
        end;
      end
      else
      begin
        path := sdi.snapshot.elementList[0].path+tail+'.'+name;

        ed := getElementDefinition(sdi, path, false, specifiedType);
        if (ed <> nil) then
        begin
          if (specifiedType <> '') then
            result.addType(specifiedType)
          else
          begin
            for t in ed.type_list do
            begin
              if (t.code = '') then
                break; // raise EFHIRPath.create('Illegal reference to primitive value attribute @ '+path);

              if (t.code = 'Element') or (t.code = 'BackboneElement') then
                result.addType(path)
              else if (t.code = 'Resource') then
              begin
                rt := worker.getResourceNames;
                try
                  for r in rt do
                    result.addType(r)
                finally
                  rt.Free;
                end;
              end
              else
                result.addType(t.code);
            end;
          end;
        end;
      end;
    end;
  finally
    sdl.Free;
    sd.Free;
  end;
end;

procedure TFHIRPathEngine.log(name, value: String);
begin
  if (Flog.length > 0) then
    Flog.append('; ');
  Flog.append(name);
  Flog.append(': ');
  Flog.append(value);
end;

function hasType(ed : TFhirElementDefinition; s : String) : boolean;
var
  t : TFhirElementDefinitionType;
begin
  result := false;
  for t in ed.type_List do
    if (s.equals(t.code)) then
      exit(true);
end;

function TFHIRPathEngine.isAbstractType(list : TFHIRElementDefinitionTypeList) : boolean;
var
  s : string;
begin
  if list.count <> 1 then
    exit(false);
  s := list[0].code;
  result := (s = 'Element') or (s = 'BackboneElement') or (s = 'Resource') or (s = 'DomainResource');
end;

function TFHIRPathEngine.getElementDefinition(sd : TFHIRStructureDefinition; path : String; allowPM : boolean; var specifiedType : String) : TFHIRElementDefinition;
var
  ed, m : TFhirElementDefinition;
begin
  specifiedType := '';
  result := nil;
  for ed in sd.snapshot.elementList do
  begin
    if (ed.path.equals(path)) then
    begin
      if (ed.ContentReference <> '') then
        exit(getElementDefinitionByName(sd, ed.ContentReference))
      else
        exit(ed);
    end;

    if (ed.path.endsWith('[x]')) and (path.startsWith(ed.path.substring(0, ed.path.length-3)) and (path.length = ed.path.Length - 3)) then
    begin
      specifiedType := path.Substring(ed.path.length-3);
      exit(ed);
    end;
    if (allowPM and ed.path.endsWith('[x]')) and (path.startsWith(ed.path.substring(0, ed.path.length-3)) and (path.length >= ed.path.Length + 2)) then
    begin
      specifiedType := path.Substring(ed.path.length-3);
      if isPrimitiveType(specifiedType.ToLower) then
        specifiedType := specifiedType.ToLower;
      exit(ed);
    end;

    if (ed.path.contains('.') and path.startsWith(ed.path+'.') and (ed.type_list.count > 0) and not isAbstractType(ed.type_list)) then
    begin
      // now we walk into the type.

      if (ed.type_list.count > 1) then // if there's more than one type, the test above would fail this
        raise EDefinitionException.create('Internal typing issue....');
      sd := worker.getStructure('http://hl7.org/fhir/StructureDefinition/'+ed.type_List[0].code);
      try
        if (sd = nil) then
          raise EDefinitionException.create('Unknown type '+ed.type_List[0].code);
        result := getElementDefinition(sd, sd.id+path.Substring(ed.path.Length), true, specifiedType);
      finally
        sd.Free;
      end;
    end;
    if ((ed.ContentReference <> '') and path.startsWith(ed.path+'.')) then
    begin
      m := getElementDefinitionByName(sd, ed.ContentReference);
      exit(getElementDefinition(sd, m.path+path.substring(ed.path.length), true, specifiedType));
    end;
  end;
end;

function TFHIRPathEngine.hasDataType(ed : TFhirElementDefinition) : boolean;
begin
  result := (ed.type_List.Count > 0) and not ((ed.type_list[0].code = 'Element') or (ed.type_list[0].code = 'BackboneElement'));
end;

function TFHIRPathEngine.getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
var
  ed : TFhirElementDefinition;
begin
  for ed in sd.snapshot.elementList do
    if (name.equals(ed.Name)) then
      exit(ed);
  result := nil;
end;

function TFHIRPathEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, worker.sizeInBytes);
  inc(result, primitiveTypes.sizeInBytes);
  inc(result,  allTypes.sizeInBytes);
  inc(result, Fucum.sizeInBytes);
end;

{ TFHIRPathExecutionTypeContext }

constructor TFHIRPathExecutionTypeContext.Create(appInfo: TFslObject; resourceType : String; context : TFHIRTypeDetails);
begin
  inherited Create;
  FAppInfo := appInfo;
  FResourceType := resourceType;
  FContext := context;
end;

destructor TFHIRPathExecutionTypeContext.Destroy;
begin
  FAppInfo.Free;
  FContext.Free;
  inherited;
end;

function TFHIRPathExecutionTypeContext.Link: TFHIRPathExecutionTypeContext;
begin
  result := TFHIRPathExecutionTypeContext(inherited link);
end;


function TFHIRPathExecutionTypeContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAppInfo.sizeInBytes);
  inc(result, (FResourceType.length * sizeof(char)) + 12);
  inc(result, FContext.sizeInBytes);
end;

{ TFHIRPathParser }

function TFHIRPathParser.parse(lexer: TFHIRPathLexer): TFHIRPathExpressionNode;
var
  msg : String;
begin
  if lexer.done then
    raise lexer.error('Path cannot be empty');
  result := parseExpression(lexer, true);
  try
    if not result.check(msg, 0) then
      raise EFHIRPath.create('Error "'+msg+'" parsing "'+lexer.Path);
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRPathParser.parse(path: String): TFHIRPathExpressionNode;
var
  lexer : TFHIRPathLexer;
  msg : String;
begin
  lexer := TFHIRPathLexer2.Create(fpV1, path);
  try
    if lexer.done then
      raise lexer.error('Path cannot be empty');
    result := parseExpression(lexer, true);
    try
      if not lexer.done then
        raise lexer.error('Premature expression termination at unexpected token "'+lexer.current+'"');
      if not result.check(msg, 0) then
        raise EFHIRPath.create('Error parsing "'+path+'": '+msg);

      result.Link;
    finally
      result.free;
    end;
  finally
    lexer.Free;
  end;
end;

procedure TFHIRPathParser.checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; count : integer);
begin
  if (exp.Parameters.count <> count) then
    raise lexer.error('The function "'+exp.name+'" requires '+inttostr(count)+' parameters', location);
end;

procedure TFHIRPathParser.checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; countMin, countMax : integer);
begin
    if (exp.Parameters.count < countMin) or (exp.Parameters.count > countMax) then
      lexer.error('The function "'+exp.name+'" requires between '+inttostr(countMin)+' and '+inttostr(countMax)+' parameters', location);
end;

procedure TFHIRPathParser.checkParameters(lexer: TFHIRPathLexer; location : TSourceLocation; exp: TFHIRPathExpressionNode);
begin
  case exp.FunctionId of
    pfEmpty: checkParamCount(lexer, location, exp, 0);
    pfNot: checkParamCount(lexer, location, exp, 0);
    pfExists: checkParamCount(lexer, location, exp, 0, 1); // 1 is allowed in cql, and should be allowed in fhir2_pathengine as well
    pfSubsetOf: checkParamCount(lexer, location, exp, 1);
    pfSupersetOf: checkParamCount(lexer, location, exp, 1);
    pfIsDistinct: checkParamCount(lexer, location, exp, 0);
    pfDistinct: checkParamCount(lexer, location, exp, 0);
    pfCount: checkParamCount(lexer, location, exp, 0);
    pfWhere: checkParamCount(lexer, location, exp, 1);
    pfSelect: checkParamCount(lexer, location, exp, 1);
    pfAll: checkParamCount(lexer, location, exp, 0, 1);
    pfRepeat: checkParamCount(lexer, location, exp, 1);
    pfItem: checkParamCount(lexer, location, exp, 1);
    pfAs: checkParamCount(lexer, location, exp, 1);
    pfIs: checkParamCount(lexer, location, exp, 1);
    pfSingle: checkParamCount(lexer, location, exp, 0);
    pfFirst: checkParamCount(lexer, location, exp, 0);
    pfLast: checkParamCount(lexer, location, exp, 0);
    pfTail: checkParamCount(lexer, location, exp, 0);
    pfSkip: checkParamCount(lexer, location, exp, 1);
    pfTake: checkParamCount(lexer, location, exp, 1);
    pfIif: checkParamCount(lexer, location, exp, 2,3);
    pfToInteger: checkParamCount(lexer, location, exp, 0);
    pfToDecimal: checkParamCount(lexer, location, exp, 0);
    pfToString: checkParamCount(lexer, location, exp, 0);
    pfSubstring: checkParamCount(lexer, location, exp, 1, 2);
    pfStartsWith: checkParamCount(lexer, location, exp, 1);
    pfEndsWith: checkParamCount(lexer, location, exp, 1);
    pfMatches: checkParamCount(lexer, location, exp, 1);
    pfReplaceMatches: checkParamCount(lexer, location, exp, 2);
    pfContains: checkParamCount(lexer, location, exp, 1);
    pfReplace: checkParamCount(lexer, location, exp, 2);
    pfLength: checkParamCount(lexer, location, exp, 0);
    pfChildren: checkParamCount(lexer, location, exp, 0);
    pfDescendants: checkParamCount(lexer, location, exp, 0);
    pfMemberOf: checkParamCount(lexer, location, exp, 1);
    pfTrace: checkParamCount(lexer, location, exp, 1);
    pfToday: checkParamCount(lexer, location, exp, 0);
    pfNow: checkParamCount(lexer, location, exp, 0);
    pfResolve: checkParamCount(lexer, location, exp, 0);
    pfExtension: checkParamCount(lexer, location, exp, 1);
    pfAllFalse: checkParamCount(lexer, location, exp, 0);
    pfAnyFalse: checkParamCount(lexer, location, exp, 0);
    pfCombine: checkParamCount(lexer, location, exp, 1);
    pfType: checkParamCount(lexer, location, exp, 0);
    pfOfType: checkParamCount(lexer, location, exp, 1);
    pfElementDefinition: checkParamCount(lexer, location, exp, 0);
    pfSlice: checkParamCount(lexer, location, exp, 2);
    pfCheckModifiers: checkParamCount(lexer, location, exp, 1);
    pfConformsTo: checkParamCount(lexer, location, exp, 1);
    pfHasValue: checkParamCount(lexer, location, exp, 0);
    pfCustom: ; // nothing
  end;
end;


function TFHIRPathParser.parseExpression(lexer : TFHIRPathLexer; proximal : boolean): TFHIRPathExpressionNode;
var
  focus, item : TFHIRPathExpressionNode;
begin
  result := TFHIRPathExpressionNode.Create(lexer.nextId);
  try
    result.SourceLocationStart := lexer.CurrentStartLocation;
    lexer.checkArithmeticPrefixes;
    // special:
    if (lexer.Current = '-') then
    begin
      lexer.take;
      lexer.Current := '-' + lexer.Current;
    end;

    if (lexer.Current = '+') then
    begin
      lexer.take;
      lexer.Current := '+' + lexer.Current;
    end;

    if lexer.isConstant then
    begin
      if lexer.current.startsWith('''') then
        lexer.processConstant(lexer.current);
      result.Constant := lexer.take;
      result.kind := enkConstant;
      result.SourceLocationEnd := lexer.CurrentLocation;
    end
    else if lexer.current = '(' then
    begin
      lexer.next;
      result.kind := enkGroup;
      result.group := parseExpression(lexer, true);
      if lexer.current <> ')' then
        raise lexer.error('Found '+lexer.current+' expecting a ")"');
      result.SourceLocationEnd := lexer.CurrentLocation;
      lexer.next;
    end
    else
    begin
      if not lexer.isToken and not lexer.current.startsWith('"') then
        raise lexer.error('Found '+lexer.current+' expecting a token name');

      result.Name := lexer.readIdentifier('Path Name');
      result.SourceLocationEnd := lexer.CurrentLocation;
      if not result.checkName then
        raise lexer.error('Found '+lexer.current+' expecting a valid token name');
      if (lexer.current = '(') then
      begin
        if StringArrayExistsSensitive(CODES_TFHIRPathFunctions, result.Name) then
          result.FunctionId := TFHIRPathFunction(StringArrayIndexOfSensitive(CODES_TFHIRPathFunctions, result.Name))
        else if result.Name = 'descendents' then
          result.FunctionId := pfDescendants
        else
          raise lexer.error('The name '+result.Name+' is not a valid function name');
        result.kind := enkFunction;
        lexer.next;
        while lexer.current <> ')' do
        begin
          result.Parameters.add(parseExpression(lexer, true));
          if lexer.current = ',' then
            lexer.next
          else if lexer.current <> ')' then
            raise lexer.error('The token '+lexer.current+' is not expected here - either a "," or a ")" expected');
        end;
        result.SourceLocationEnd := lexer.CurrentLocation;
        lexer.next;
        checkParameters(lexer, result.SourceLocationStart, result);
      end;
    end;
    focus := result;
    while (lexer.current = '[') do
    begin
      lexer.next();
      item := TFHIRPathExpressionNode.Create(lexer.nextId);
      item.Kind := enkFunction;
      item.Functionid := pfItem;
      item.Parameters.add(parseExpression(lexer, true));
      if (lexer.current <> ']') then
        raise lexer.error('The token '+lexer.Current+' is not expected here - a "]" expected');
      lexer.next;
      focus.inner := item;
      focus := item;
    end;
    if lexer.current = '.' then
    begin
      lexer.next;
      focus.Inner := parseExpression(lexer, false);
    end;
    result.Proximal := proximal;
    if (proximal) then
    begin
      while lexer.isOp() do
      begin
        focus.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current));
        focus.OpSourceLocationStart := lexer.CurrentStartLocation;
        focus.OpSourceLocationEnd := lexer.CurrentLocation;
        lexer.next;
        focus.opNext := parseExpression(lexer, false);
        focus := focus.OpNext;
      end;
      organisePrecedence(lexer, result);
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRPathParser.newGroup(lexer : TFHIRPathLexer; next : TFHIRPathExpressionNode) : TFHIRPathExpressionNode;
begin
  result := TFHIRPathExpressionNode.Create(lexer.nextId);
  try
    result.kind := enkGroup;
    result.Group := next.Link;
    result.Group.Proximal := true;
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRPathParser.gatherPrecedence(lexer : TFHIRPathLexer; var start : TFHIRPathExpressionNode; ops : TFHIRPathOperationSet);
var
  work : boolean;
  focus, node, group : TFHIRPathExpressionNode;
begin
  assert(start.Proximal);

  // is there anything to do?
  work := false;
  focus := start.OpNext;
  if start.Operation in ops then
    while (focus <> nil) and (focus.operation <> popNull) do
    begin
      work := work or not (focus.Operation in Ops);
      focus := focus.OpNext;
    end
  else
    while (focus <> nil) and (focus.operation <> popNull) do
    begin
      work := work or (focus.Operation in Ops);
      focus := focus.OpNext;
    end;
  if not work then
    exit;

  // entry point: tricky
  if start.Operation in ops then
  begin
    group := newGroup(lexer, start);
    group.proximal := true;
    focus := start;
    start.Free;
    start := group;
  end
  else
  begin
    node := start;
    focus := node.OpNext;
    while not (focus.Operation in Ops) do
    begin
      node := focus;
      focus := focus.OpNext;
    end;
    group := newGroup(lexer, focus);
    node.OpNext := group;
  end;

  // now, at this point:
  //   group is the group we are adding to, it already has a .group property filled out.
  //   focus points at the group.group
  repeat
    // run until we find the end of the sequence
    while (focus.Operation in ops) do
      focus := focus.OpNext;
    if (focus.Operation <> popNull) then
    begin
      group.Operation := focus.Operation;
      group.OpNext := focus.OpNext.Link;
      focus.Operation := popNull;
      focus.OpNext := nil;
      // now look for another sequence, and start it
      node := group;
      focus := group.OpNext;
      if (focus <> nil) then
      begin
        while (focus <> nil) and not (focus.Operation in Ops) do
        begin
          node := focus;
          focus := focus.OpNext;
        end;
        if (focus <> nil) { and (focus.Operation in Ops) - must be true } then
        begin
          group := newGroup(lexer, focus);
          node.OpNext := group;
        end;
      end;
    end;
  until (focus = nil) or (focus.Operation = popNull);
end;

procedure TFHIRPathParser.organisePrecedence(lexer : TFHIRPathLexer; var node : TFHIRPathExpressionNode);
begin
  gatherPrecedence(lexer, node, [popTimes, popDivideBy, popDiv, popMod]);
  gatherPrecedence(lexer, node, [popPlus, popMinus, popConcatenate]);
  gatherPrecedence(lexer, node, [popUnion]);
  gatherPrecedence(lexer, node, [popLessThan, popGreater, popLessOrEqual, popGreaterOrEqual]);
  gatherPrecedence(lexer, node, [popIs]);
  gatherPrecedence(lexer, node, [popEquals, popEquivalent, popNotEquals, popNotEquivalent]);
  gatherPrecedence(lexer, node, [popAnd]);
  gatherPrecedence(lexer, node, [popXor, popOr]);
  // last: implies
end;

{ TFHIRPathLexer2 }

function TFHIRPathLexer2.opCodes: TArray<String>;
var
  i : integer;
begin
  setLength(result, length(CODES_TFHIRPathOperation));
  for i := 0 to length(CODES_TFHIRPathOperation) -1 do
    result[i] := CODES_TFHIRPathOperation[TFHIRPathOperation(i)];
end;

function TFHIRPathLexer2.processConstant: TFHIRObject;
begin
  result := nil;
end;

end.

