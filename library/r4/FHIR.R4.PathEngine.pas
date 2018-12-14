unit FHIR.R4.PathEngine;

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
  SysUtils, Classes, Math, RegularExpressions, Generics.Collections, Character,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.PathEngine, FHIR.Base.Lang,
  FHIR.R4.PathNode, FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Utilities, FHIR.R4.Context, FHIR.R4.Constants;

const
  FHIR_TYPES_STRING : Array[0..8] of String = ('string', 'uri', 'code', 'oid', 'id', 'uuid', 'sid', 'markdown', 'base64Binary');

type
  TFHIRConstant = class (TFHIRObject)
  private
    FValue : String;
  public
    constructor Create(value : String);
    function fhirType : string; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
  end;

  TFHIRClassTypeInfo = class (TFHIRObject)
  private
    FInstance : TFHIRObject;
  protected
    procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
  public
    constructor Create(instance : TFHIRObject);
    destructor Destroy; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : string; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function getName : String;
    function getNamespace : String;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
  end;

  TFHIRPathExecutionTypeContext = class (TFslObject)
  private
    FAppInfo : TFslObject;
    FResourceType : String;
    FContext : TFHIRTypeDetails;
  public
    constructor Create(appInfo : TFslObject; resourceType : String; context : TFHIRTypeDetails);
    destructor Destroy; override;
    function Link : TFHIRPathExecutionTypeContext; overload;
    property appInfo : TFslObject read FappInfo;
    property resourceType : String read FResourceType;
    property context : TFHIRTypeDetails read FContext;
  end;

  TFHIRPathEngine = class;

  TFHIRPathParser = class (TFslObject)
  private
    FExtensions : TFslList<TFHIRPathEngineExtension>;
    function isKnownFunction(name : String) : boolean;
    function parseExpression(lexer: TFHIRPathLexer; proximal : boolean): TFHIRPathExpressionNode;
    procedure organisePrecedence(lexer : TFHIRPathLexer; var node: TFHIRPathExpressionNode);
    procedure gatherPrecedence(lexer : TFHIRPathLexer; var start: TFHIRPathExpressionNode; ops: TFHIRPathOperationSet);
    function newGroup(lexer : TFHIRPathLexer; next: TFHIRPathExpressionNode): TFHIRPathExpressionNode;
  protected
    procedure checkParameters(lexer : TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode);
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; count : integer); overload;
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRPathExpressionNode; countMin, countMax : integer); overload;
  public
    destructor Destroy; override;
    // Parse a path for later use using execute
    function parse(path : String) : TFHIRPathExpressionNode; overload;
    function parse(path : String; var i : integer) : TFHIRPathExpressionNode; overload;
    function parse(lexer : TFHIRPathLexer) : TFHIRPathExpressionNode; overload;
  end;

  TFHIRPathLexer4 = class (TFHIRPathLexer)
  protected
    function opCodes : TArray<String>; override;
  public
    function processConstant : TFHIRObject; overload; override;
  end;

  TFHIRResolveReferenceEvent = function (source : TFHIRPathEngine; appInfo : TFslObject; url : String) : TFHIRObject of object;
  TFHIRResolveConstantEvent = function (source : TFHIRPathEngine; appInfo : TFslObject; name : String; beforeContext : boolean) : TFHIRObject of object;

  TFHIRPathEngine = class (TFHIRPathEngineV)
  private
    worker : TFHIRWorkerContext;
    FLog : TStringBuilder;
    primitiveTypes, allTypes : TStringList;
    FOnResolveReference: TFHIRResolveReferenceEvent;
    FOnResolveConstant: TFHIRResolveConstantEvent;
    FUcum : TUcumServiceInterface;

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
    function resolveConstantType(ctxt: TFHIRPathExecutionTypeContext; constant : TFHIRObject) : TFHIRTypeDetails; overload;
    function resolveConstantType(ctxt: TFHIRPathExecutionTypeContext; s : String) : TFHIRTypeDetails; overload;

    procedure ListChildTypesByName(type_, name : string; result : TFHIRTypeDetails);

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

    function funcAggregate(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcUnion(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIntersect(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcExclude(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcLower(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcUpper(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToChars(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToBoolean(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToQuantity(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToDateTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcToTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsInteger(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsDecimal(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsString(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsBoolean(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsQuantity(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsDateTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
    function funcIsTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;

    function qtyToCanonical(q : TFHIRQuantity) : TFhirDecimal;
    function pairToQty(p: TUcumPair): TFHIRQuantity;
    function qtyToPair(q: TFHIRQuantity): TUcumPair;
    function qtyEqual(left : TFHIRQuantity; right : TFHIRQuantity) : boolean;
    function qtyEquivalent(left, right: TFHIRQuantity): boolean;
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

    function resolveConstant(context : TFHIRPathExecutionContext; constant : TFHIRObject) : TFHIRObject; overload;
    function resolveConstant(context : TFHIRPathExecutionContext; s : String) : TFHIRObject; overload;
    function processDateConstant(appinfo : TFslObject; value : String) : TFHIRObject; overload;
    procedure getClassInfoChildTypesByName(name: String; result: TFHIRTypeDetails);
    procedure getSimpleTypeChildTypesByName(name: String; result: TFHIRTypeDetails);
  protected
    function funcCustom(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList; virtual;
    function evaluateCustomFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode): TFHIRTypeDetails; virtual;
  public
    constructor Create(context : TFHIRWorkerContext; ucum : TUcumServiceInterface);
    destructor Destroy; override;

    property OnResolveReference : TFHIRResolveReferenceEvent read FOnResolveReference write FOnResolveReference;
    property OnResolveConstant : TFHIRResolveConstantEvent read FOnResolveConstant write FOnResolveConstant;

    // Parse a path for later use using execute
    function parse(path : String) : TFHIRPathExpressionNode; overload;
    function parse(path : String; var i : integer) : TFHIRPathExpressionNode; overload;
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
    function parseQuantityString(s: String): TFHIRQuantity;
  end;


implementation

{ TFHIRConstant }

constructor TFHIRConstant.create(value: String);
begin
  inherited create;
  FValue := value;
end;

function TFHIRConstant.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRConstant.createPropertyValue');
end;

function TFHIRConstant.fhirType: string;
begin
  result := '%constant';
end;

function TFHIRConstant.getId: String;
begin
  raise EFHIRTodo.create('TFHIRConstant.getId:');
end;

function TFHIRConstant.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRConstant.makeIntValue(v: String): TFHIRObject;
begin
  result := TFhirInteger.Create(v);
end;

function TFHIRConstant.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

procedure TFHIRConstant.setIdValue(id: String);
begin
  raise EFHIRTodo.create('TFHIRConstant.setIdValue');
end;

procedure TFHIRConstant.setProperty(propName: string; propValue: TFHIRObject);
begin
  raise EFHIRTodo.create('TFHIRConstant.setProperty');
end;

{ TFHIRClassTypeInfo }

constructor TFHIRClassTypeInfo.create(instance: TFHIRObject);
begin
  inherited create;
  FInstance := instance;
end;

function TFHIRClassTypeInfo.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRClassTypeInfo.createPropertyValue');
end;

destructor TFHIRClassTypeInfo.destroy;
begin
  FInstance.Free;
  inherited;
end;

function TFHIRClassTypeInfo.fhirType: string;
begin
  result := 'ClassInfo';
end;

procedure TFHIRClassTypeInfo.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  if (name = 'name') then
    list.add(TFHIRString.create(getName).noExtensions)
  else if (name = 'namespace') then
    list.add(TFHIRString.create(getNamespace).noExtensions)
  else
    inherited GetChildrenByName(name, list);
end;

function TFHIRClassTypeInfo.getNamespace: String;
begin
  if (FInstance is TFHIRResource) then
    result := 'FHIR'
  else if not (FInstance is TFHIRElement) or (FInstance as TFHIRElement).DisallowExtensions then
    result := 'System'
  else
    result := 'FHIR';
end;

function TFHIRClassTypeInfo.getId: String;
begin
  raise EFHIRTodo.create('TFHIRClassTypeInfo.getId:');
end;

function TFHIRClassTypeInfo.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRClassTypeInfo.makeIntValue(v: String): TFHIRObject;
begin
  result := TFhirInteger.Create(v);
end;

function TFHIRClassTypeInfo.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

procedure TFHIRClassTypeInfo.setIdValue(id: String);
begin
  raise EFHIRTodo.create('TFHIRClassTypeInfo.setIdValue');
end;

procedure TFHIRClassTypeInfo.setProperty(propName: string; propValue: TFHIRObject);
begin
  raise EFHIRTodo.create('TFHIRClassTypeInfo.setProperty');
end;

function TFHIRClassTypeInfo.getName: String;
begin
  if (FInstance is TFHIRResource) then
    result := FInstance.fhirType
  else if not (FInstance is TFHIRElement) or (FInstance as TFHIRElement).DisallowExtensions then
    result := capitalise(FInstance.fhirType)
  else
    result := FInstance.fhirType;
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
  lexer := TFHIRPathLexer4.Create(path);
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
    pfExists: checkParamCount(lexer, location, exp, 0, 1); // 1 is allowed in cql, and should be allowed in FHIR.R4.PathEngine as well
    pfSubsetOf: checkParamCount(lexer, location, exp, 1);
    pfSupersetOf: checkParamCount(lexer, location, exp, 1);
    pfIsDistinct: checkParamCount(lexer, location, exp, 0);
    pfDistinct: checkParamCount(lexer, location, exp, 0);
    pfCount: checkParamCount(lexer, location, exp, 0);
    pfWhere: checkParamCount(lexer, location, exp, 1);
    pfSelect: checkParamCount(lexer, location, exp, 1);
    pfAll: checkParamCount(lexer, location, exp, 0, 1);
    pfRepeat: checkParamCount(lexer, location, exp, 1);
    pfAggregate: checkParamCount(lexer, location, exp, 1, 2);
    pfItem: checkParamCount(lexer, location, exp, 1);
    pfAs: checkParamCount(lexer, location, exp, 1);
    pfIs: checkParamCount(lexer, location, exp, 1);
    pfSingle: checkParamCount(lexer, location, exp, 0);
    pfFirst: checkParamCount(lexer, location, exp, 0);
    pfLast: checkParamCount(lexer, location, exp, 0);
    pfTail: checkParamCount(lexer, location, exp, 0);
    pfSkip: checkParamCount(lexer, location, exp, 1);
    pfTake: checkParamCount(lexer, location, exp, 1);
    pfUnion: checkParamCount(lexer, location, exp, 1);
    pfCombine: checkParamCount(lexer, location, exp, 1);
    pfIntersect: checkParamCount(lexer, location, exp, 1);
    pfExclude: checkParamCount(lexer, location, exp, 1);
    pfIif: checkParamCount(lexer, location, exp, 2,3);
    pfLower: checkParamCount(lexer, location, exp, 0);
    pfUpper: checkParamCount(lexer, location, exp, 0);
    pfToChars: checkParamCount(lexer, location, exp, 0);
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
    pfTrace: checkParamCount(lexer, location, exp, 1, 2);
    pfToday: checkParamCount(lexer, location, exp, 0);
    pfNow: checkParamCount(lexer, location, exp, 0);
    pfResolve: checkParamCount(lexer, location, exp, 0);
    pfExtension: checkParamCount(lexer, location, exp, 1);
    pfAllFalse: checkParamCount(lexer, location, exp, 0);
    pfAnyFalse: checkParamCount(lexer, location, exp, 0);
    pfType: checkParamCount(lexer, location, exp, 0);
    pfOfType: checkParamCount(lexer, location, exp, 1);
    pfElementDefinition: checkParamCount(lexer, location, exp, 0);
    pfSlice: checkParamCount(lexer, location, exp, 2);
    pfCheckModifiers: checkParamCount(lexer, location, exp, 1);
    pfConformsTo: checkParamCount(lexer, location, exp, 1);
    pfHasValue: checkParamCount(lexer, location, exp, 0);
    pfHtmlChecks: checkParamCount(lexer, location, exp, 0);
    pfToInteger: checkParamCount(lexer, location, exp, 0);
    pfToDecimal: checkParamCount(lexer, location, exp, 0);
    pfToString: checkParamCount(lexer, location, exp, 0);
    pfToQuantity: checkParamCount(lexer, location, exp, 0);
    pfToBoolean: checkParamCount(lexer, location, exp, 0);
    pfToDateTime: checkParamCount(lexer, location, exp, 0);
    pfToTime: checkParamCount(lexer, location, exp, 0);
    pfConvertsToInteger: checkParamCount(lexer, location, exp, 0);
    pfConvertsToDecimal: checkParamCount(lexer, location, exp, 0);
    pfConvertsToString: checkParamCount(lexer, location, exp, 0);
    pfConvertsToQuantity: checkParamCount(lexer, location, exp, 0);
    pfConvertsToBoolean: checkParamCount(lexer, location, exp, 0);
    pfConvertsToDateTime: checkParamCount(lexer, location, exp, 0);
    pfConvertsToTime: checkParamCount(lexer, location, exp, 0);
    pfCustom: ; // nothing
  end;
end;


destructor TFHIRPathParser.Destroy;
begin
  FExtensions.Free;
  inherited;
end;

function TFHIRPathParser.parse(path: String; var i: integer): TFHIRPathExpressionNode;
var
  lexer : TFHIRPathLexer;
  msg : String;
begin
  lexer := TFHIRPathLexer4.Create(path, i);
  try
    if lexer.done then
      raise lexer.error('Path cannot be empty');
    result := parseExpression(lexer, true);
    try
      if not result.check(msg, 0) then
        raise EFHIRPath.create('Error parsing "'+path+'": '+msg);
      result.Link;
    finally
      result.free;
    end;
    i := lexer.CurrentStart;
  finally
    lexer.Free;
  end;
end;

function TFHIRPathParser.parseExpression(lexer : TFHIRPathLexer; proximal : boolean): TFHIRPathExpressionNode;
var
  c : Integer;
  focus, item : TFHIRPathExpressionNode;
  isString : boolean;
  ucum, s : String;
  q :  TFHIRQuantity;
begin
  result := TFHIRPathExpressionNode.Create(lexer.nextId);
  try
    result.SourceLocationStart := lexer.CurrentStartLocation;
    c := lexer.CurrentStart;
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
      isString := lexer.isStringConstant;
      result.Constant := lexer.processConstant;
      result.kind := enkConstant;
      if (not isString and not lexer.done() and (result.constant <> nil)
        and result.constant.hasType(['integer', 'decimal']) and
        (lexer.isStringConstant() or lexer.hasToken(['year', 'years', 'month', 'months', 'week', 'weeks', 'day', 'days', 'hour', 'hours', 'minute', 'minutes', 'second', 'seconds', 'millisecond', 'milliseconds']))) then
      begin
        // it's a quantity
        ucum := '';
        if (lexer.hasToken(['year', 'years', 'month', 'months', 'week', 'weeks', 'day', 'days', 'hour', 'hours', 'minute', 'minutes', 'second', 'seconds', 'millisecond', 'milliseconds'])) then
        begin
          s := lexer.take();
          if (s = 'year') or (s = 'years') then
            ucum := 'a'
          else if (s = 'month') or (s = 'month') then
            ucum := 'mo'
          else if (s = 'week') or (s = 'weeks') then
            ucum := 'wk'
          else if (s = 'day') or (s = 'days') then
            ucum := 'd'
          else if (s = 'hour') or (s = 'hours') then
            ucum := 'h'
          else if (s = 'minute') or (s = 'minutes') then
            ucum := 'min'
          else if (s = 'second') or (s = 'seconds') then
            ucum := 's'
          else // (s = 'millisecond") || s = 'milliseconds"))
            ucum := 'ms';
        end
        else
          ucum := lexer.readConstant('units');
        q := TFHIRQuantity.create;
        try
          q.value := result.constant.primitiveValue;
          q.system := 'http://unitsofmeasure.org';
          q.code := ucum;
          result.constant := q.Link;
        finally
          q.Free;
        end;
      end;
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
        raise lexer.error('Found "'+lexer.current+'" expecting a token name');

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
        else if isKnownFunction(result.name) then
          result.FunctionId := pfCustom
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

function TFHIRPathParser.isKnownFunction(name: String): boolean;
var
  ext : TFHIRPathEngineExtension;
begin
   result := false;
   for ext in FExtensions do
     if ext.isValidFunction(name) then
       exit(true);
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


{ TFHIRPathEngine }

function TFHIRPathEngine.check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNode; xPathStartsWithValueRef : boolean) : TFHIRTypeDetails;
var
  types : TFHIRTypeDetails;
  ctxt : TFHIRPathExecutionTypeContext;
  sd : TFHIRStructureDefinition;
  ed : TFHIRElementDefinition;
  td : TFhirElementDefinitionType;
  t : String;
begin
  types := nil;
  if (xPathStartsWithValueRef and context.contains('.') and path.startsWith(context.substring(context.lastIndexOf('.')+1))) then
    types := TFHIRTypeDetails.Create(csSINGLETON, [context.substring(0, context.lastIndexOf('.'))])
  else if not context.contains('.') then
    types := TFHIRTypeDetails.Create(csSINGLETON, [context])
  else
  begin
    sd := worker.getStructure('http://hl7.org/fhir/StructureDefinition/'+context.Substring(0, context.IndexOf('.')));
    try
      if (sd = nil) then
        raise EFHIRPathDefinitionCheck.Create('Unknown context '+context);
      ed := getElementDefinition(sd, context, true, t);
      if (ed = nil) then
        raise EFHIRPathDefinitionCheck.Create('Unknown context element '+context);
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
var
  q : TFHIRQuantity;
  u : String;
  plural : boolean;
begin
  if item = nil then
    result := ''
  else if item.isPrimitive then
    result := item.primitiveValue
  else if (item is TFHIRQuantity) then
  begin
    q := item as TFHIRQuantity;
    if (q.system = 'http://unitsofmeasure.org') then
    begin
      u := ''''+q.code+'''';
      plural := q.value <> '1';
      if ('a' = q.code) then
        if plural then u := 'years' else u := 'year'
      else if ('mo' = q.code) then
        if plural then u := 'months' else u := 'month'
      else if ('wk' = q.code) then
        if plural then u := 'weeks' else u := 'week'
      else if ('d' = q.code) then
        if plural then u := 'days' else u := 'day'
      else if ('h' = q.code) then
        if plural then u := 'hours' else u := 'hour'
      else if ('min' = q.code) then
        if plural then u := 'minutes' else u := 'minute'
      else if ('s' = q.code) then
        if plural then u := 'seconds' else u := 'seconds'
      else if ('ms' = q.code) then
        if plural then u := 'milliseconds' else u := 'milliseconds';
      result := q.value+' '+u;
    end;
  end
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
  self.FUcum := ucum;
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
          {$IFNDEF FHIR2}
          if (sd.derivation = TypeDerivationRuleSPECIALIZATION) then
            allTypes.add(sd.id);
          if (sd.derivation = TypeDerivationRuleSPECIALIZATION) and (sd.kind = StructureDefinitionKindPrimitiveType) then
            primitiveTypes.add(sd.id);
          {$ELSE}
          raise EFHIRException.create('Debug this');
          if (sd.constrainedType = DefinedTypesNull) then
            allTypes.add(sd.id);
          if (sd.constrainedType = DefinedTypesNull) and isPrimitive(sd) then
            primitiveTypes.add(sd.id);
          {$ENDIF}
      end;
    finally
      list.Free;
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
  FUcum.Free;
  worker.Free;
  primitiveTypes.Free;
  allTypes.Free;
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
  if (left is TFHIRQuantity) and (right is TFHIRQuantity) then
    result := qtyEqual(left as TFHIRQuantity, right as TFHIRQuantity)
  else if (left.isPrimitive and right.isPrimitive) then
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
  else if (left.hasType('Quantity') and right.hasType('Quantity')) then
    result := qtyEquivalent(left as TFHIRQuantity, right as TFHIRQuantity)
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
      ctxt.This := base;
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
var
  temp : TFHIRObject;
  ok : boolean;
begin
  result := TFHIRSelectionList.Create;
  try
    ok := false;
    if (atEntry and (context.appInfo <> nil) and assigned(FOnResolveConstant)) then
    begin
      // we'll see if the name matches a constant known by the context.
      temp := FOnResolveConstant(self, context.appInfo, exp.name, true);
      if (temp <> nil) then
      begin
        result.add(temp);
        ok := true;
      end;
    end;
    if not ok then
    begin
      if atEntry and (CharInSet(exp.name[1], ['A'..'Z'])) then // special case for start up
      begin
        if (item.FhirType = exp.name) or StringArrayExistsSensitive(['Resource', 'DomainResource'], exp.name) then
          result.Add(item.Link);
      end
      else
        ListChildrenByName(item, exp.name, result);
      if (atEntry and (context.appInfo <> nil) and assigned(FOnResolveConstant) and result.Empty) then
      begin
        // well, we didn't get a match on the name - we'll see if the name matches a constant known by the context.
        // (if the name does match, and the user wants to get the constant value, they'll have to try harder...
        temp := FOnResolveConstant(self, context.appInfo, exp.name, false);
        if (temp <> nil) then
          result.add(temp);
      end;
    end;
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

function TFHIRPathEngine.resolveConstantType(ctxt: TFHIRPathExecutionTypeContext; constant : TFHIRObject) : TFHIRTypeDetails;
begin
  if (constant is TFHIRBoolean) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_Boolean])
  else if (constant is TFHIRInteger) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_Integer])
  else if (constant is TFHIRDecimal) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_Decimal])
  else if (constant is TFHIRQuantity) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_Quantity])
  else if (constant is TFHIRConstant) then
    result := resolveConstantType(ctxt, (constant as TFHIRConstant).FValue)
  else
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String]);
end;

function TFHIRPathEngine.resolveConstantType(ctxt: TFHIRPathExecutionTypeContext; s : String) : TFHIRTypeDetails;
begin
  if (s.startsWith('@')) then
    if (s.startsWith('@T')) then
      result := TFHIRTypeDetails.create(csSINGLETON, [FP_Time])
    else
      result := TFHIRTypeDetails.create(csSINGLETON, [FP_DateTime])
  else if (s.equals('%sct')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.equals('%loinc')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.equals('%ucum')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.equals('%resource')) then
  begin
    if (ctxt.resourceType = '') then
      raise EFHIRPath.create('%resource cannot be used in this context');
    result := TFHIRTypeDetails.create(csSINGLETON, [ctxt.resourceType]);
  end else if (s.equals('%context')) then
    result := ctxt.context.link
  else if (s.equals('%map-codes')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.equals('%us-zip')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.startsWith('%"vs-')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.startsWith('%"cs-')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else if (s.startsWith('%"ext-')) then
    result := TFHIRTypeDetails.create(csSINGLETON, [FP_String])
  else
    raise EFHIRPath.create('Unknown fixed constant type for "'+s+'"');
end;

function TFHIRPathEngine.executeType(ctxt: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRPathExpressionNode; atEntry : boolean): TFHIRTypeDetails;
var
  s : TFHIRProfiledType;
  work, work2 : TFHIRTypeDetails;
  next, last : TFHIRPathExpressionNode;
begin
  result := TFHIRTypeDetails.Create(csNULL, []);
  try
    case exp.kind of
      enkName:
        if atEntry and (exp.Name = '$this') then
          result.update(ctxt.context)
        else if atEntry and (exp.Name = '$total') then
          result.update(csUNORDERED, allTypes)
        else
        begin
          for s in focus.types do
          begin
            work := executeType(s.uri, exp, atEntry);
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
        begin
          work := resolveConstantType(ctxt, exp.Constant);
          try
            result.update(work)
          finally
            work.Free;
          end;
        end;
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
          res := execute(context, pc, exp.Parameters[0], true);
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
      result.add(TFhirBoolean.Create(all).noExtensions);
    end
    else // (exp.getParameters().size() == 0)
    begin
      all := true;
      for item in focus do
      begin
        v := false;
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
      result.add(TFhirBoolean.Create(all).noExtensions);
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
    res := execute(context, focus, exp.Parameters[0], true);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).contains(sw)).noExtensions)
    else
      result.add(TFHIRBoolean.create(false).noExtensions);

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcCount(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirInteger.Create(inttostr(focus.Count)).noExtensions);
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
    result := focus.Link;

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
  result := TFHIRSelectionList.Create(TFhirBoolean.Create(focus.Count = 0).noExtensions);
end;

function TFHIRPathEngine.funcEndsWith(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  res : TFHIRSelectionList;
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], true);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).endsWith(sw)).noExtensions)
    else
      result.add(TFHIRBoolean.create(false).noExtensions);

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
      result.add(TFHIRBoolean.create(focus[0].value.hasPrimitiveValue).noExtensions)
    else
      result.add(TFHIRBoolean.create(false).noExtensions);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcHtmlChecks(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    result.add(TFHIRBoolean.create(true).noExtensions);
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
  ns, n : string;
  texp : TFHIRPathExpressionNode;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 0) or (focus.count > 1) then
      result.add(TFHIRBoolean.create(false).noExtensions)
    else
    begin
      ns := '';
      n := '';
      texp := exp.Parameters[0];
      if (texp.Kind <> enkName) then
        raise EFHIRPath.create('Unsupported Expression type for Parameter on Is');
      if (texp.inner <> nil) then
      begin
        if (texp.Kind <> enkName) then
          raise EFHIRPath.create('Unsupported Expression type for Parameter on Is');
        ns := texp.Name;
        n := texp.inner.Name;
      end
      else if (StringArrayExistsSensitive(['Boolean', 'Integer', 'Decimal', 'String', 'DateTime', 'Time', 'SimpleTypeInfo', 'ClassInfo'], texp.name)) then
      begin
        ns := 'System';
        n := texp.name;
      end
      else
      begin
        ns := 'FHIR';
        n := texp.name;
      end;
      if (ns = 'System') then
      begin
        if (not (focus[0].value is TFHIRElement) or (focus[0].value as TFHIRElement).DisallowExtensions) then
          result := TFHIRSelectionList.create(TFHIRBoolean.create(n = capitalise(focus[0].value.fhirType)).noExtensions)
        else
          result := TFHIRSelectionList.create(TFHIRBoolean.create(false).noExtensions);
      end
      else if (ns = 'FHIR') then
        result := TFHIRSelectionList.create(TFHIRBoolean.create(n = focus[0].value.fhirType).noExtensions)
      else
        result := TFHIRSelectionList.create(TFHIRBoolean.create(false).noExtensions);
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
      result.add(TFHIRBoolean.create(true).noExtensions)
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
      result.add(TFHIRBoolean.create(distinct).noExtensions);
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
  i : integer;
begin
  res := execute(context, focus, exp.Parameters[0], true);
  try
		s := convertToString(res);
  finally
    res.Free;
  end;
  result := TFHIRSelectionList.Create;
  if StringIsInteger16(s) then
  begin
    i := StrToInt(s);
    if focus.oneBased then
      dec(i);
    if (focus.Count > i) then
      result.Add(focus[i].Link);
  end;
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
      result.add(TFHIRInteger.create(inttostr(s.length)).noExtensions);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcTrace(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  n1, n2 : TFHIRSelectionList;
  name : String;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
  try
    name := n1[0].value.primitiveValue;
    if exp.Parameters.Count = 2 then
    begin
      n2 := execute(context, focus, exp.Parameters[1], true);
      try
        log(name, convertToString(n2));
      finally
        n2.Free;
      end;
    end
    else
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
    res := execute(context, focus, exp.Parameters[0], true);
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
  result := TFHIRSelectionList.Create(TFhirBoolean.Create(not convertToBoolean(focus)).noExtensions);
end;

function TFHIRPathEngine.funcNow(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFhirDateTime.Create(TDateTimeEx.makeLocal).noExtensions);
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
          ctxt := context.changeThis(item.value);
          try
            work := execute(ctxt, pc, exp.parameters[0], true);
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
      s := '';
      if (item.value.fhirType = 'Reference') then
      begin
        p := item.value.getPropertyValue('reference');
        try
          if (p.hasValue) then
            s := convertToString(p.Values[0]);
        finally
          p.free;
        end;
      end
      else
        s := convertToString(item.value);
      res := nil;
      if (s <> '') then
      begin
        if (s.startsWith('#') and (context.resource <> nil)) then
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
            raise EFHIRPath.create('resolve() - resolution services for '+exp.name+' not implemented yet');
          res := FOnResolveReference(self, context.appInfo, s);
        end;
        if (res <> nil) then
          result.add(res);
      end;
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
        ctxt := context.changeThis(item.value);
        try
          work := execute(ctxt, pc, exp.parameters[0], true);
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
    res := execute(context, focus, exp.Parameters[0], true);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0].value).startsWith(sw)).noExtensions)
    else
      result.add(TFHIRBoolean.create(false).noExtensions);

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
    result := TFHIRSelectionList.Create(TFHIRBoolean.create(valid).noExtensions);
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
    n1 := execute(context, focus, exp.Parameters[0], true);
    i1 := StrToInt(n1[0].value.primitiveValue);
    if (exp.ParameterCount = 2) then
    begin
      n2 := execute(context, focus, exp.Parameters[1], true);
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
          result.Add(TFhirString.Create(s).noExtensions);
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
  result.add(TFHIRBoolean.create(focus.count > 0).noExtensions);
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
    result := TFHIRSelectionList.Create(TFHIRBoolean.create(valid).noExtensions);
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
    n1 := execute(context, focus, exp.Parameters[0], true);
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
  result := TFHIRSelectionList.Create(TFhirDate.Create(TDateTimeEx.makeToday).noExtensions);
end;

function TFHIRPathEngine.funcToDecimal(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRSelectionList.Create;
  if (StringIsDecimal(s)) then
    result.add(TFHIRDecimal.Create(s).noExtensions)
  else if ('true' = s) then
    result.add(TFHIRDecimal.Create('1').noExtensions())
  else if ('false' = s) then
    result.add(TFHIRDecimal.Create('0').noExtensions());
 end;

function TFHIRPathEngine.funcToInteger(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRSelectionList.Create;
  if (StringIsInteger32(s)) then
    result.add(TFHIRInteger.Create(s).noExtensions)
  else if ('true' = s) then
    result.add(TFHIRInteger.Create('1').noExtensions())
  else if ('false' = s) then
    result.add(TFHIRInteger.Create('0').noExtensions());
end;

function TFHIRPathEngine.funcToString(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  result.add(TFHIRString.Create(convertToString(focus)).noExtensions);
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
        ctxt := context.changeThis(item.value);
        try
          res := execute(ctxt, pc, exp.Parameters[0], true);
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
          res := execute(context, pc, exp.Parameters[0], true);
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
      result.add(TFhirBoolean.Create(all).noExtensions);
    end
    else // (exp.getParameters().size() == 0)
    begin
      all := true;
      for item in focus do
      begin
        v := false;
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
      result.add(TFhirBoolean.Create(all).noExtensions);
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
          res := execute(context, pc, exp.Parameters[0], true);
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
      result.add(TFhirBoolean.Create(any).noExtensions);
    end
    else // (exp.getParameters().size() == 0)
    begin
      any := false;
      for item in focus do
      begin
        v := false;
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
      result.add(TFhirBoolean.Create(any).noExtensions);
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
    res := execute(context, focus, exp.Parameters[0], true);
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
var
  item : TFHIRSelection;
begin
  result := TFHIRSelectionList.create;
  for item in focus do
    result.add(TFHIRClassTypeInfo.create(item.value.Link));
end;

function TFHIRPathEngine.funcOfType(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  n1 : TFHIRSelectionList;
  tn : String;
  b : TFHIRSelection;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
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
  n1 := execute(context, focus, exp.Parameters[0], true);
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
        result.add(TFhirBoolean.Create(ok).noExtensions);
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

function TFHIRPathEngine.funcAggregate(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  total, work, pc : TFHIRSelectionList;
  item : TFHIRSelection;
  c : TFHIRPathExecutionContext;
begin
  if (exp.ParameterCount > 1) then
    total := execute(context, focus, exp.Parameters[1], true)
  else
    total := TFHIRSelectionList.Create;
  try
    pc := TFHIRSelectionList.Create;
    try
      for item in focus do
      begin
        c := context.changeThis(item.value);
        try
          c.total := total.Link;
          work := execute(c, pc, exp.Parameters[0], true);
          try
            total.Free;
            total := work.Link;
          finally
            work.Free;
          end;
        finally
          c.Free;
        end;
      end;
    finally
      pc.Free;
    end;
    result := total.Link;
  finally
    total.Free;
  end;
end;

function TFHIRPathEngine.funcUnion(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  res : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    for item in focus do
      if not contains(result, item.value) then
        result.add(item.link);
    res := execute(context, focus, exp.Parameters[0], true);
    try
      for item in res do
        if not contains(result, item.value) then
          result.add(item.link);
    finally
      res.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIntersect(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  res : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    res := execute(context, focus, exp.Parameters[0], true);
    try
      for item in focus do
        if not contains(result, item.value) and contains(res, item.value) then
          result.add(item.link);
    finally
      res.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcExclude(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  item : TFHIRSelection;
  res : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    res := execute(context, focus, exp.Parameters[0], true);
    try
      for item in focus do
        if not contains(result, item.value) and not contains(res, item.value) then
          result.add(item.link);
    finally
      res.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcLower(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
    begin
      sw := convertToString(focus[0].value);
      if sw <> '' then
        result.add(TFHIRString.create(sw.ToLower).noExtensions);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcUpper(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
    begin
      sw := convertToString(focus[0].value);
      if sw <> '' then
        result.add(TFHIRString.create(sw.ToUpper).noExtensions);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcToChars(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  c : char;
  sw : String;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
    begin
      sw := convertToString(focus[0].value);
      for c in sw do
        result.add(TFHIRString.create(c).noExtensions);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcToBoolean(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
    begin
      if (focus[0].value is TFHIRBoolean) then
        result.add(focus[0].value)
      else if (focus[0].value is TFHIRInteger) then
        result.add(TFHIRBoolean.create(focus[0].value.primitiveValue() <> '0').noExtensions())
      else if (focus[0].value is TFHIRString) then
        if ('true' = focus[0].value.primitiveValue) then
          result.add(TFHIRBoolean.create(true).noExtensions())
        else if ('false' = focus[0].value.primitiveValue) then
          result.add(TFHIRBoolean.create(false).noExtensions());
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcToQuantity(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  q : TFHIRQuantity;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count = 1) then
    begin
      if (focus[0].value is TFHIRQuantity) then
        result.add(focus[0].link)
      else if (focus[0].value is TFHIRString) then
      begin
        q := parseQuantityString(focus[0].value.primitiveValue);
        if (q <> nil) then
          result.add(q.noExtensions());
      end
      else if (focus[0].value is TFHIRInteger) then
        result.add(TFHIRQuantity.fromUcum(focus[0].value.primitiveValue, '1').noExtensions())
      else if (focus[0].value is TFHIRDecimal) then
        result.add(TFHIRQuantity.fromUcum(focus[0].value.primitiveValue, '1').noExtensions());
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcToDateTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcToDateTime');
end;

function TFHIRPathEngine.funcToTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  raise EFHIRTodo.create('TFHIRPathEngine.funcToTime');
end;

function TFHIRPathEngine.funcIsInteger(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRInteger) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRBoolean) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
      result.add(TFHIRBoolean.create(StringIsInteger32(convertToString(focus[0].value))).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIsDecimal(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRInteger) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRBoolean) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRDecimal) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
      result.add(TFHIRBoolean.create(StringIsDecimal(convertToString(focus[0].value))).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIsString(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if not (focus[0].value is TFHIRDateTime) and not (focus[0].value is TFHIRTime) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIsBoolean(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRInteger) and (StrToIntDef((focus[0].value as TFHIRInteger).value, -1) >= 0) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRBoolean) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
      result.add(TFHIRBoolean.create(StringArrayExistsSensitive(['true', 'false'], convertToString(focus[0].value))).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.parse(path: String; var i: integer): TFHIRPathExpressionNode;
var
  parser : TFHIRPathParser;
begin
  parser := TFHIRPathParser.Create;
  try
    parser.FExtensions := FExtensions.Link;
    result := parser.parse(path, i);
  finally
    parser.Free;
  end;
end;

function TFHIRPathEngine.parseQuantityString(s : String) : TFHIRQuantity;
var
  v : String;
begin
  result := nil;
  if (s <> '') then
  begin
    s := s.trim();
    if (s.contains(' ')) then
    begin
      v := s.substring(0, s.indexOf(' ')).trim();
      s := s.substring(s.indexOf(' ')).trim();
      if (StringIsDecimal(v)) then
      begin
        if (s.startsWith('''') and s.endsWith('''')) then
          result := TFHIRQuantity.fromUcum(v, s.substring(1, s.length-2))
        else if (s = 'year') or (s = 'years') then
          result := TFHIRQuantity.fromUcum(v, 'a')
        else if (s = 'month') or (s = 'month') then
          result := TFHIRQuantity.fromUcum(v, 'mo')
        else if (s = 'week') or (s = 'weeks') then
          result := TFHIRQuantity.fromUcum(v, 'wk')
        else if (s = 'day') or (s = 'days') then
          result := TFHIRQuantity.fromUcum(v, 'd')
        else if (s = 'hour') or (s = 'hours') then
          result := TFHIRQuantity.fromUcum(v, 'h')
        else if (s = 'minute') or (s = 'minutes') then
          result := TFHIRQuantity.fromUcum(v, 'min')
        else if (s = 'second') or (s = 'seconds') then
          result := TFHIRQuantity.fromUcum(v, 's')
        else if (s = 'millisecond') or (s = 'milliseconds') then
          result := TFHIRQuantity.fromUcum(v, 'ms')
      end;
    end
    else if (StringIsDecimal(s)) then
    begin
      result := TFHIRQuantity.fromUcum(s, '1');
    end;
  end;
end;

function TFHIRPathEngine.parseV(path: String): TFHIRPathExpressionNodeV;
begin
  result := parse(path);
end;

function TFHIRPathEngine.funcIsQuantity(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
var
  q : TFhirQuantity;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRInteger) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRDecimal) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRQuantity) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
    begin
      q := parseQuantityString(focus[0].value.primitiveValue());
      result.add(TFHIRBoolean.create(q <> nil).noExtensions());
    end
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;


function TFHIRPathEngine.funcIsDateTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRDateTime) or (focus[0].value is TFHIRDate) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
      result.add(TFHIRBoolean.create(TRegEx.Match(convertToString(focus[0].value),
          '([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?'
       ).Success).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.funcIsTime(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNode) : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus.count <> 1) then
      result.add(TFHIRBoolean.create(false).noExtensions())
    else if (focus[0].value is TFHIRTime) then
      result.add(TFHIRBoolean.create(true).noExtensions())
    else if (focus[0].value is TFHIRString) then
      result.add(TFHIRBoolean.create(TRegEx.Match(convertToString(focus[0].value),
          'T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?').Success).noExtensions())
    else
      result.add(TFHIRBoolean.create(false).noExtensions());
    result.Link;
  finally
    result.Free;
  end;
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
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(false).noExtensions);
    popOr: if isBoolean(left, true) then
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(true).noExtensions);
    popImplies: if (not convertToBoolean(left)) then
        result := TFHIRSelectionList.Create(TFHIRBoolean.Create(true).noExtensions);
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
    popEquals: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popEquivalent: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popNotEquals: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popNotEquivalent: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popLessThan: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popGreater: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popLessOrEqual: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popGreaterOrEqual: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popIs: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popAs: result := TFHIRTypeDetails.createList(csSINGLETON, right.Types);
    popUnion: result := left.union(right);
    popOr: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popAnd: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popXor: result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popImplies : result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popTimes: begin
      result := TFHIRTypeDetails.create(csSINGLETON, []);
      if (left.hasType(context, 'integer')) and (right.hasType(context, 'integer')) then
        result.addType(FP_integer)
      else if (left.hasType(context, ['integer', 'decimal'])) and (right.hasType(context, ['integer', 'decimal'])) then
        result.addType(FP_decimal);
    end;
    popDivideBy: begin
      result := TFHIRTypeDetails.create(csSINGLETON, []);
      if (left.hasType(context, 'integer')) and (right.hasType(context, 'integer')) then
        result.addType(FP_decimal)
      else if (left.hasType(context, ['integer', 'decimal'])) and (right.hasType(context, ['integer', 'decimal'])) then
        result.addType(FP_decimal);
    end;
    popPlus:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, []);
      if (left.hasType(context, 'integer')) and (right.hasType(context, 'integer')) then
        result.addType(FP_integer)
      else if (left.hasType(context, ['integer', 'decimal'])) and (right.hasType(context, ['integer', 'decimal'])) then
        result.addType(FP_decimal)
      else if (left.hasType(context, ['string', 'id', 'code', 'uri'])) and (right.hasType(context, ['string', 'id', 'code', 'uri'])) then
        result.addType(FP_string);
    end;
    popConcatenate : result := TFHIRTypeDetails.create(csSINGLETON, ['string']);
    popMinus:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, []);
      if (left.hasType(context, 'integer')) and (right.hasType(context, 'integer')) then
        result.addType(FP_integer)
      else if (left.hasType(context, ['integer', 'decimal'])) and (right.hasType(context, ['integer', 'decimal'])) then
        result.addType(FP_decimal);
    end;
    popDiv, popMod:  begin
      result := TFHIRTypeDetails.create(csSINGLETON, []);
      if (left.hasType(context, 'integer')) and (right.hasType(context, 'integer')) then
        result.addType(FP_integer)
      else if (left.hasType(context, ['integer', 'decimal'])) and (right.hasType(context, ['integer', 'decimal'])) then
        result.addType(FP_Decimal);
    end;
    popIn:  result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    popContains:  result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
    // todo: add memberOf
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
      result.Add(TFhirBoolean.Create(false).noExtensions)
    else if (left.Empty) or (right.Empty) then
      // noothing
    else if (convertToBoolean(left)) and (convertToBoolean(right)) then
      result.Add(TFhirBoolean.Create(true).noExtensions)
    else
      result.Add(TFhirBoolean.Create(false).noExtensions);
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
  result.Add(TFhirBoolean.Create(ans).noExtensions);
end;

function TFHIRPathEngine.opDiv(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  d1, d2, d3 : TFslDecimal;
  pl, pr : TUcumPair;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing div: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing div: left operand has more than one value');
  if (not left[0].value.isPrimitive()) and not left[0].value.hasType('Quantity') then
    raise EFHIRPath.create(StringFormat('Error performing div: left operand has the wrong type (%s)', [left[0].value.fhirType]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing div: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing div: right operand has more than one value');
  if (not right[0].value.isPrimitive()) and not right[0].value.hasType('Quantity') then
    raise EFHIRPath.create(StringFormat('Error performing div: right operand has the wrong type (%s)', [right[0].value.fhirType]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strtoInt(l.primitiveValue()) div strtoInt(r.primitiveValue()))).noExtensions)
    else if (l.hasType(['quantity'])) and (r.hasType(['quantity'])) and (FUcum <> nil) then
    begin
      pl := qtyToPair(l as TFHIRQuantity);
      try
        pr := qtyToPair(r as TFHIRQuantity);
        try
          try
// todo
//            p := FUcum.divideBy(pl, pr);
//            try
//              result.add(pairToQty(p));
//            finally
//              p.Free;
//            end;
          except
          end;
        finally
          pr.Free;
        end;
      finally
        pl.Free;
      end;
    end
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.divInt(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
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
  pl, pr : TUcumPair;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing /: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing /: left operand has more than one value');
  if (not left[0].value.isPrimitive()) and not left[0].value.hasType('Quantity') then
    raise EFHIRPath.create(StringFormat('Error performing -: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing /: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing /: right operand has more than one value');
  if (not right[0].value.isPrimitive()) and not right[0].value.hasType('Quantity') then
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
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
    end
    else if (l.hasType(['quantity'])) and (r.hasType(['quantity'])) and (FUcum <> nil) then
    begin
      pl := qtyToPair(l as TFHIRQuantity);
      try
        pr := qtyToPair(r as TFHIRQuantity);
        try
          try
// todo
//            p := FUcum.divideBy(pl, pr);
//            try
//              result.add(pairToQty(p));
//            finally
//              p.Free;
//            end;
          except
          end;
        finally
          pr.Free;
        end;
      finally
        pl.Free;
      end;
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
  result.Add(TFhirBoolean.Create(res).noExtensions);
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
  result.Add(TFhirBoolean.Create(res).noExtensions);
end;

function TFHIRPathEngine.opGreater(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  l, r : TFHIRObject;
  lUnit, rUnit : TFHIRSelectionList;
  dl, dr : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue).noExtensions)
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) > StrToFloat(r.primitiveValue)).noExtensions)
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue).noExtensions)
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue).noExtensions)
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
			  ListChildrenByName(left[0].value, 'code', lUnit);
			  ListChildrenByName(right[0].value, 'code', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opGreater(lUnit, rUnit);
        end
        else if FUcum <> Nil then
        begin
          dl := TFHIRSelectionList.Create;
          dr := TFHIRSelectionList.Create;
          try
            dl.add(qtyToCanonical(left[0].value as TFhirQuantity));
            dr.add(qtyToCanonical(right[0].value as TFhirQuantity));
            result := opGreater(dl, dr);
          finally
            dl.Free;
            dr.Free;
          end;
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn''t available');
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
  dl, dr : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue).noExtensions)
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) >= StrToFloat(r.primitiveValue)).noExtensions)
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue).noExtensions)
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue).noExtensions);
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
        else if FUcum <> Nil then
        begin
          dl := TFHIRSelectionList.Create;
          dr := TFHIRSelectionList.Create;
          try
            dl.add(qtyToCanonical(left[0].value as TFhirQuantity));
            dr.add(qtyToCanonical(right[0].value as TFhirQuantity));
            result := opGreaterOrEqual(dl, dr);
          finally
            dl.Free;
            dr.Free;
          end;
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn''t available');
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
  result.Add(TFhirBoolean.Create(ans).noExtensions);
end;

function TFHIRPathEngine.opIs(left, right: TFHIRSelectionList): TFHIRSelectionList;
var
  tn : string;
begin
  result := TFHIRSelectionList.Create;
  try
    if (left.count = 0) or (left.count > 1) then
      result.add(TFHIRBoolean.create(false).noExtensions)
    else
    begin
      tn := convertToString(right);
      if not (left[0].value is TFHIRElement) or (left[0].value as TFHIRElement).DisallowExtensions then
        result.add(TFHIRBoolean.create(capitalise(left[0].value.fhirType) = tn).noExtensions)
      else
        result.add(TFHIRBoolean.create(left[0].value.hasType(tn)).noExtensions);
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
  dl, dr : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value;
      r := right[0].value;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue).noExtensions)
      else if l.hasType(['decimal', 'integer', 'unsignedInt', 'positiveInt']) and r.hasType(['decimal', 'integer', 'unsignedInt', 'positiveInt']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) <= StrToFloat(r.primitiveValue)).noExtensions)
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue).noExtensions)
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue).noExtensions)
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
        else if FUcum <> Nil then
        begin
          dl := TFHIRSelectionList.Create;
          dr := TFHIRSelectionList.Create;
          try
            dl.add(qtyToCanonical(left[0].value as TFhirQuantity));
            dr.add(qtyToCanonical(right[0].value as TFhirQuantity));
            result := opLessOrEqual(dl, dr);
          finally
            dl.Free;
            dr.Free;
          end;
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn''t available');
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
  dl, dr : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].value.isPrimitive) and (right[0].value.isPrimitive) then
    begin
      l := left[0].value as TFHIRObject;
      r := right[0].value as TFHIRObject;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue).noExtensions)
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) < StrToFloat(r.primitiveValue)).noExtensions)
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue).noExtensions)
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue).noExtensions)
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].value.hasType('Quantity') and right[0].value.hasType('Quantity') then
    begin
      lUnit := TFHIRSelectionList.create;
      rUnit := TFHIRSelectionList.create;
      try
			  ListChildrenByName(left[0].value, 'code', lUnit);
			  ListChildrenByName(right[0].value, 'code', rUnit);
        if (TFHIRSelectionList.compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0].value, 'value', lUnit);
          ListChildrenByName(right[0].value, 'value', rUnit);
          result := opLessThan(lUnit, rUnit);
        end
        else if FUcum <> Nil then
        begin
          dl := TFHIRSelectionList.Create;
          dr := TFHIRSelectionList.Create;
          try
            dl.add(qtyToCanonical(left[0].value as TFhirQuantity));
            dr.add(qtyToCanonical(right[0].value as TFhirQuantity));
            result := opLessThan(dl, dr);
          finally
            dl.Free;
            dr.Free;
          end;
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn''t available');
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
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) - strToInt(r.primitiveValue()))).noExtensions)
    else if (l.hasType('decimal')) and (r.hasType('decimal')) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Subtract(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
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
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) mod strToInt(r.primitiveValue()))).noExtensions)
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Modulo(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
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
  result.Add(TFhirBoolean.Create(res).noExtensions);
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
  result.Add(TFhirBoolean.Create(not res).noExtensions);
end;

function TFHIRPathEngine.opOr(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if (left.Empty) and (right.Empty) then
    // nothing
  else if (convertToBoolean(left)) or (convertToBoolean(right)) then
    result.Add(TFhirBoolean.Create(true).noExtensions)
  else if (left.Empty) or (right.Empty) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(false).noExtensions);
end;

function TFHIRPathEngine.opConcatenate(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create(TFHIRString.create(convertToString(left) + convertToString(right)).noExtensions);
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
      result.add(TFHIRString.create(l.primitiveValue() + r.primitiveValue()).noExtensions)
    else if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) + strToInt(r.primitiveValue()))).noExtensions)
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Add(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
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
  p, pl, pr : TUcumPair;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing *: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing *: left operand has more than one value');
  if (not left[0].value.isPrimitive()) and not left[0].value.hasType('Quantity') then
    raise EFHIRPath.create(StringFormat('Error performing +: left operand has the wrong type (%s)', [left[0].value.fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing *: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing *: right operand has more than one value');
  if (not right[0].value.isPrimitive()) and not right[0].value.hasType('Quantity') then
    raise EFHIRPath.create(StringFormat('Error performing *: right operand has the wrong type (%s)', [right[0].value.fhirType()]));

  result := TFHIRSelectionList.Create();
  try
    l := left[0].value;
    r := right[0].value;

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) * strToInt(r.primitiveValue()))).noExtensions)
    else if (l.hasType(['Quantity'])) and (r.hasType(['Quantity'])) and (FUcum <> nil) then
    begin
      pl := qtyToPair(l as TFHIRQuantity);
      try
        pr := qtyToPair(r as TFHIRQuantity);
        try
          try
            p := FUcum.multiply(pl, pr);
            try
              result.add(pairToQty(p));
            finally
              p.Free;
            end;
          except
          end;
        finally
          pr.Free;
        end;
      finally
        pl.Free;
      end;
    end
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TFslDecimal.valueOf(l.primitiveValue());
      d2 := TFslDecimal.valueOf(r.primitiveValue());
      d3 := d1.Multiply(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal).noExtensions);
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
    result.Add(TFhirBoolean.Create(convertToBoolean(left) xor convertToBoolean(right)).noExtensions);
end;

function TFHIRPathEngine.opImplies(left, right: TFHIRSelectionList): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  if (not convertToBoolean(left)) then
    result.Add(TFhirBoolean.Create(true).noExtensions)
  else if (right.count = 0) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(convertToBoolean(right)).noExtensions);
end;

function TFHIRPathEngine.resolveConstant(context : TFHIRPathExecutionContext; constant : TFHIRObject) : TFHIRObject;
var
  c : TFHIRConstant;
begin
  if (not (constant is TFHIRConstant)) then
    exit(constant.link);

  c := constant as TFHIRConstant;
  if (c.FValue.startsWith('%')) then
    result := resolveConstant(context, c.FValue)
  else if (c.FValue.startsWith('@')) then
    result := processDateConstant(context.appInfo, c.FValue.substring(1))
  else
    raise EFHIRPath.create('Invaild FHIR Constant '+c.FValue);
end;

function TFHIRPathEngine.processDateConstant(appinfo : TFslObject; value : String) : TFHIRObject;
var
  i : integer;
  v : string;
begin
  if (value.startsWith('T')) then
    exit(TFHIRTime.create(value.substring(1)).noExtensions());

  v := value;
  if (v.length > 10) then
  begin
    i := v.substring(10).indexOf('-');
    if (i = -1) then
      i := v.substring(10).indexOf('+');
    if (i = -1) then
      i := v.substring(10).indexOf('Z');
    if (i > -1) then
      v := v.substring(0, 10+i);
  end;
  if (v.length > 10) then
    result := TFHIRDateTime.create(TDateTimeEx.fromXML(value)).noExtensions()
  else
    result := TFHIRDate.create(TDateTimeEx.fromXML(value)).noExtensions();
end;

function TFHIRPathEngine.qtyEqual(left, right: TFHIRQuantity): boolean;
var
  dl, dr : TFhirDecimal;
begin
  if (FUcum <> nil) then
  begin
    dl := qtyToCanonical(left);
    try
      dr := qtyToCanonical(right);
      try
        if (dl <> nil) and (dr <> nil) then
          exit(equal(dl,  dr));
      finally
        dr.free;
      end;
    finally
      dl.free;
    end;
  end;
  result := compareDeep(left, right, false);
end;

function TFHIRPathEngine.qtyToCanonical(q: TFHIRQuantity): TFhirDecimal;
var
  p, c : TUcumPair;
begin
  if ('http://unitsofmeasure.org' <> q.system) then
    exit(nil);
  try
    p := TUcumPair.Create(TFslDecimal.ValueOf(q.value), q.code);
    try
      c := FUcum.getCanonicalForm(p);
      try
        result := TFhirDecimal.Create(c.Value.AsDecimal);
        result.noExtensions;
      finally
        c.Free;
      end;
    finally
      p.free;
    end;
  except
    result := nil;
  end;
end;

function TFHIRPathEngine.pairToQty(p : TUcumPair) : TFHIRQuantity;
begin
  result := TFHIRQuantity.Create;
  try
    result.value := p.value.AsString;
    result.system :='http://unitsofmeasure.org';
    result.code := p.UnitCode;
    result.noExtensions;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.qtyToPair(q : TFHIRQuantity) : TUcumPair;
begin
  if ('http://unitsofmeasure.org' <> q.system) then
    exit(nil);
  result := TUcumPair.Create;
  try
    result.value := TFslDecimal.ValueOf(q.value);
    result.Unitcode := q.code;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPathEngine.qtyEquivalent(left, right: TFHIRQuantity): boolean;
var
  dl, dr : TFhirDecimal;
begin
  if (FUcum <> nil) then
  begin
    dl := qtyToCanonical(left);
    try
      dr := qtyToCanonical(right);
      try
        if (dl <> nil) and (dr <> nil) then
          exit(equivalent(dl,  dr));
      finally
        dr.free;
      end;
    finally
      dl.free;
    end;
  end;
  result := false;
  if (left.system = right.system) and (left.code = right.code) then
  begin
    dl := TFhirDecimal.Create(left.value);
    try
      dr := TFhirDecimal.Create(left.value);
      try
        result := equivalent(dl, dr);
      finally
        dr.Free;
      end;
    finally
      dl.Free;
    end;
  end;
end;


function TFHIRPathEngine.resolveConstant(context : TFHIRPathExecutionContext; s : String) : TFHIRObject;
begin
  if (s = '%sct') then
    result := TFHIRString.create('http://snomed.info/sct').noExtensions()
  else if (s = '%loinc') then
    result := TFHIRString.create('http://loinc.org').noExtensions()
  else if (s = '%ucum') then
    result := TFHIRString.create('http://unitsofmeasure.org').noExtensions()
  else if (s = '%resource') then
  begin
    if (context.resource = nil) then
      raise EFHIRPath.create('Cannot use %resource in this context');
    result := context.resource.Link;
  end
  else if (s = '%context') then
    result := context.context.link
  else if (s = '%us-zip') then
    result := TFHIRString.create('[0-9]{5}(-[0-9]{4}){0,1}').noExtensions()
  else if (s.startsWith('%"vs-')) then
    result := TFHIRString.create('http://hl7.org/fhir/ValueSet/'+s.substring(5, s.length-6)).noExtensions()
  else if (s.startsWith('%"cs-')) then
    result := TFHIRString.create('http://hl7.org/fhir/'+s.substring(5, s.length-1)).noExtensions()
  else if (s.startsWith('%"ext-')) then
    result := TFHIRString.create('http://hl7.org/fhir/StructureDefinition/'+s.substring(6, s.length-7)).noExtensions()
  else
    raise EFHIRPath.create('Unknown fixed constant "'+s+'"')
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
        if atEntry and (exp.name = '$this') then
          work.add(context.this.Link)
        else if atEntry and (exp.name = '$total') then
          work.addAll(context.total)
        else
          for item in focus do
          begin
            outcome := execute(context, item.value, exp, atEntry);
            try
              if work.oneBased <> outcome.OneBased then
                if work.count = 0 then
                  work.oneBased := outcome.OneBased
                else
                  raise EFHIRPath.Create('Cannot mix 0-bazed and 1-based selections');

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
        item := TFHIRSelection.Create(resolveConstant(context, exp.constant));
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
            work2 := TFHIRSelectionList.Create(TFHIRString.Create(next.name).noExtensions)
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
  if (atEntry and exp.Name[1].IsUpper) and (focus = TFHIRProfiledType.ns(exp.Name)) then
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
    pfAggregate : result := funcAggregate(context, focus, exp);
    pfItem : result := funcItem(context, focus, exp);
    pfAs : result := funcAs(context, focus, exp);
    pfOfType : result := funcOfType(context, focus, exp);
    pfType : result := funcType(context, focus, exp);
    pfIs : result := funcIs(context, focus, exp);
    pfSingle : result := funcSingle(context, focus, exp);
    pfFirst : result := funcFirst(context, focus, exp);
    pfLast : result := funcLast(context, focus, exp);
    pfTail : result := funcTail(context, focus, exp);
    pfSkip : result := funcSkip(context, focus, exp);
    pfTake : result := funcTake(context, focus, exp);
    pfUnion : result := funcUnion(context, focus, exp);
    pfCombine : result := funcCombine(context, focus, exp);
    pfIntersect : result := funcIntersect(context, focus, exp);
    pfExclude : result := funcExclude(context, focus, exp);
    pfIif : result := funcIif(context, focus, exp);
    pfLower : result := funcLower(context, focus, exp);
    pfUpper : result := funcUpper(context, focus, exp);
    pfToChars : result := funcToChars(context, focus, exp);
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
    pfElementDefinition: result := funcElementDefinition(context, focus, exp);
    pfSlice: result := funcSlice(context, focus, exp);
    pfCheckModifiers: result := funcCheckModifiers(context, focus, exp);
    pfConformsTo: result := funcConformsTo(context, focus, exp);
    pfHasValue : result := funcHasValue(context, focus, exp);
    pfHtmlChecks : result := funcHtmlChecks(context, focus, exp);
    pfToInteger : result := funcToInteger(context, focus, exp);
    pfToDecimal : result := funcToDecimal(context, focus, exp);
    pfToString : result := funcToString(context, focus, exp);
    pfToBoolean : result := funcToBoolean(context, focus, exp);
    pfToQuantity : result := funcToQuantity(context, focus, exp);
    pfToDateTime : result := funcToDateTime(context, focus, exp);
    pfToTime : result := funcToTime(context, focus, exp);
    pfConvertsToInteger : result := funcIsInteger(context, focus, exp);
    pfConvertsToDecimal : result := funcIsDecimal(context, focus, exp);
    pfConvertsToString : result := funcIsString(context, focus, exp);
    pfConvertsToBoolean : result := funcIsBoolean(context, focus, exp);
    pfConvertsToQuantity : result := funcIsQuantity(context, focus, exp);
    pfConvertsToDateTime : result := funcIsDateTime(context, focus, exp);
    pfConvertsToTime : result := funcIsTime(context, focus, exp);
    pfCustom : result := funcCustom(context, focus, exp);
  else
    raise EFHIRPath.create('Unknown Function '+exp.name);
  end;
end;

function TFHIRPathEngine.funcCustom(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNode): TFHIRSelectionList;
var
  ext : TFHIRPathEngineExtension;
  item : TFHIRSelection;
  res, work : TFHIRSelectionList;
  params : TFslList<TFHIRObject>;
  i : integer;
  couldHaveBeen, done : boolean;
begin
  result := TFHIRSelectionList.Create;
  try
    couldHaveBeen := false;
    done := false;
    for ext in FExtensions do
    begin
      couldHaveBeen := couldHaveBeen or ext.isValidFunction(exp.name);
      if ext.functionApplies(context, focus, exp.name) then
      begin
        done := true;
        for item in focus do
        begin
          work := ext.execute(context, item.value, exp.name, TFslList<TFHIRPathExpressionNodeV>(exp.Parameters), self);
          try
            result.addAll(work);
          finally
            work.Free;
          end;
        end;
        break;
      end;
    end;
    if not done and (not couldHaveBeen or (focus.Count > 0)) then
      raise EFHIRPath.create('Unknown Function '+exp.name);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRPathEngine.check(appInfo: TFslObject; resourceType, context, path: String; expr: TFHIRPathExpressionNodeV; xPathStartsWithValueRef: boolean): TFHIRTypeDetailsV;
begin
  result := nil;
end;

procedure TFHIRPathEngine.checkParamTypes(funcId : TFHIRPathFunction; paramTypes : TFslList<TFHIRTypeDetails>; typeSet : array of TFHIRTypeDetails);
var
  i : integer;
  pt, actual : TFHIRTypeDetails;
  a : TFHIRProfiledType;
  sd : TFhirStructureDefinition;
  ok : boolean;
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
      begin
        if (not pt.hasType(context, a.uri)) then
        begin
          ok := false;
          sd := context.fetchResource(frtStructureDefinition, a.uri) as TFhirStructureDefinition;
          while not ok and (sd <> nil) do
          begin
            ok := pt.hasType(context, sd.type_);
            sd := context.fetchResource(frtStructureDefinition, sd.baseDefinition) as TFhirStructureDefinition;
          end;
          if (not ok) then
          raise EFHIRPath.create('The parameter type "'+a.uri+'" is not legal for '+CODES_TFHIRPathFunctions[funcId]+' parameter '+Integer.toString(i)+', expecting '+pt.describe());
    end;
      end;
    end;
  finally
    for pt in typeSet do
      pt.Free;
  end;
end;

function TFHIRPathEngine.childTypes(focus : TFHIRTypeDetails; mask : string) : TFHIRTypeDetails;
var
  f : TFHIRProfiledType;
begin
  result := TFHIRTypeDetails.create(csUNORDERED, []);
  try
    for f in focus.types do
      ListChildTypesByName(f.uri, mask, result);
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
  s, c : boolean;
  pt : TFHIRProfiledType;
begin
  paramTypes := TFslList<TFHIRTypeDetails>.create;
  try
    if (exp.FunctionId in [pfIs, pfAs]) then
      paramTypes.add(TFHIRTypeDetails.create(csSINGLETON, [FP_string]))
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
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfNot :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfExists :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfSubsetOf :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [focus.link]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfSupersetOf :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [focus.link]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfIsDistinct :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfDistinct :
        result := focus.Link;
      pfCount :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_integer]);
      pfWhere :
        result := focus.Link;
      pfSelect :
        result := TFHIRTypeDetails.createList(focus.CollectionStatus, allTypes);
      pfAll :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfRepeat :
        result := TFHIRTypeDetails.createList(focus.CollectionStatus, allTypes);
      pfAggregate :
        result := TFHIRTypeDetails.createList(focus.CollectionStatus, allTypes);
      pfItem :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
         checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_integer])]);
        result := focus.Link;
        end;
      pfOfType :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, TFHIRTypeDetails.create(csSINGLETON, [FP_String]));
        result := TFHIRTypeDetails.create(csSINGLETON, [exp.Parameters[0].name]);
        end;
      pfType :
        begin
        s := false;
        c := false;
        for pt in focus.types do
        begin
          s := s or pt.isSystemType();
          c := c or not pt.isSystemType();
        end;
        if (s and c) then
          result := TFHIRTypeDetails.create(csSINGLETON, [FP_SimpleTypeInfo, FP_ClassInfo])
        else if (s) then
          result := TFHIRTypeDetails.create(csSINGLETON, [FP_SimpleTypeInfo])
        else
          result := TFHIRTypeDetails.create(csSINGLETON, [FP_ClassInfo]);
        end;
      pfAs :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, exp.Parameters[0].Name);
        end;
      pfIs :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
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
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_integer])]);
        result := focus.Link;
        end;
      pfTake :
        begin
        if (focus.CollectionStatus = csUNORDERED) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on ordered collections');
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_integer])]);
        result := focus.Link;
        end;
      pfUnion :
        result := focus.union(paramTypes[0]);
      pfCombine :
        result := focus.union(paramTypes[0]);
      pfIntersect :
        result := focus.intersect(paramTypes[0]);
      pfExclude :
        result := focus.link;
      pfIif :
        begin
        result := TFHIRTypeDetails.create(csNull, []);
        result.update(paramTypes[0]);
        if (paramTypes.count > 1) then
          result.update(paramTypes[1]);
        end;
      pfLower :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfUpper :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfToChars :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfSubstring :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_Integer]), TFHIRTypeDetails.create(csSINGLETON, [FP_integer])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfStartsWith :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfEndsWith :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfMatches :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfReplaceMatches :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string]), TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfContains :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfReplace :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'id'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, id'+' not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string]), TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfLength :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_integer]);
        end;
      pfChildren :
        result := childTypes(focus, '*');
      pfDescendants :
        result := childTypes(focus, '**');
      pfMemberOf :
        begin
        if (not focus.hasType(self.context, ['string', 'code', 'uri', 'Coding', 'CodeableConcept'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on string, code, uri, Coding, CodeableConcept not '+focus.describe);
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfTrace :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := focus.Link;
        end;
      pfToday :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_DateTime]);
      pfNow :
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_dateTime]);
      pfResolve :
        begin
        if (not focus.hasType(self.context, ['uri', 'Reference'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on uri, Reference not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['DomainResource']);
        end;
      pfExtension :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['Extension']);
        end;
      pfAllFalse:
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfAnyFalse:
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfElementDefinition:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['ElementDefinition']);
        end;
      pfSlice:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string, FP_string])]);
        result := focus.Link;
        end;
      pfCheckModifiers:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csUNORDERED, [FP_string])]);
        result := focus.Link;
        end;
      pfConformsTo:
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, [FP_string])]);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
        end;
      pfHasValue:
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfHtmlChecks:
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_boolean]);
      pfToInteger :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_integer]);
        end;
      pfToDecimal :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_decimal]);
        end;
      pfToString :
        begin
        if (not focus.hasType(self.context, primitiveTypes) and not focus.hasType(self.context, 'Quantity')) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_string]);
        end;
      pfToQuantity :
        begin
        if (not focus.hasType(self.context, primitiveTypes) and not focus.hasType(self.context, 'Quantity')) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_Quantity]);
        end;
      pfToBoolean :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_Boolean]);
        end;
      pfToDateTime :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_DateTime]);
        end;
      pfToTime :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_Time]);
        end;
      pfConvertsToString, pfConvertsToQuantity :
        begin
        if (not focus.hasType(self.context, primitiveTypes) and not focus.hasType(self.context, 'Quantity')) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_Boolean]);
        end;
      pfConvertsToInteger, pfConvertsToDecimal, pfConvertsToDateTime, pfConvertsToTime, pfConvertsToBoolean :
        begin
        if (not focus.hasType(self.context, primitiveTypes)) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on '+primitiveTypes.CommaText+' not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, [FP_Boolean]);
        end;
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
    exit(TFHIRTime.create(s.substring(1)).noExtensions as TFhirType);
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
    result := TFHIRDateTime.create(TDateTimeEx.fromXML(s)).noExtensions as TFhirType
  else
    result := TFHIRDate.create(TDateTimeEx.fromXML(s)).noExtensions as TFhirType;
end;

function TFHIRPathEngine.readConstant(context : TFHIRPathExecutionContext; constant: String): TFHIRObject;
begin
  if (constant = 'true') then
    result := TFhirBoolean.Create(true).noExtensions
  else if (constant = 'false') then
    result := TFhirBoolean.Create(false).noExtensions
  else if (constant = '{}') then
    result := nil
  else if StringIsInteger32(constant) then
    result := TFhirInteger.Create(constant).noExtensions
  else if StringIsDecimal(constant) then
    result := TFhirDecimal.Create(constant).noExtensions
  else if constant.StartsWith('''') then
    result := TFhirString.Create(TFHIRPathLexer.processConstant(constant)).noExtensions
  else if constant.StartsWith('%') then
    result := replaceFixedConstant(context, constant)
  else if constant.StartsWith('@') then
    result := processDateConstant(nil, constant.Substring(1))
  else
    result := TFhirString.Create(constant).noExtensions;
end;

function TFHIRPathEngine.replaceFixedConstant(context : TFHIRPathExecutionContext; const s: String): TFHIRObject;
begin
  if s = '%sct' then
    result := TFhirString.Create('http://snomed.info/sct').noExtensions
  else if s = '%loinc' then
    result := TFhirString.Create('http://loinc.org').noExtensions
  else if s = '%ucum' then
    result := TFhirString.Create('http://unitsofmeasure.org').noExtensions
  else if s = '%resource' then
  begin
    if (context.resource = nil) then
      raise EFHIRPath.create('%resource cannot be used in this context');
    result := context.resource.link;
  end
  else if s = '%us-zip' then
    result := TFhirString.Create('[0-9]{5}(-[0-9]{4}){0,1}"').noExtensions
  else if s.StartsWith('%"vs-') then
    result := TFhirString.Create('http://hl7.org/fhir/ValueSet/'+s.Substring(5, s.length-6)).noExtensions
  else if s.StartsWith('%"cs-') then
    result := TFhirString.Create('http://hl7.org/fhir/'+s.Substring(5, s.length-6)).noExtensions
  else if s.StartsWith('%"ext-') then
    result := TFhirString.Create('http://hl7.org/fhir/StructureDefinition/'+s.Substring(6, s.length-7)).noExtensions
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

function TFHIRPathEngine.parse(path: String): TFHIRPathExpressionNode;
var
  parser : TFHIRPathParser;
begin
  parser := TFHIRPathParser.Create;
  try
    parser.FExtensions := FExtensions.Link;
    result := parser.parse(path);
  finally
    parser.Free;
  end;
end;

procedure TFHIRPathEngine.ListChildrenByName(focus: TFHIRObject; name: String; results: TFHIRSelectionList);
begin
  focus.ListChildrenByName(name, results);
end;


procedure TFHIRPathEngine.getClassInfoChildTypesByName(name: String; result : TFHIRTypeDetails);
begin
  if (name = 'namespace') then
    result.addType(FP_String);
  if (name = 'name') then
    result.addType(FP_String);
end;

procedure TFHIRPathEngine.getSimpleTypeChildTypesByName(name: String; result : TFHIRTypeDetails);
begin
  if (name = 'namespace') then
    result.addType(FP_String);
  if (name = 'name') then
    result.addType(FP_String);
end;

procedure TFHIRPathEngine.ListChildTypesByName(type_, name : String; result : TFHIRTypeDetails);
var
  url, tail, specifiedType, path, tn, r, rn : String;
  sd, dt, sdi : TFhirStructureDefinition;
  sdl : TFslList<TFhirStructureDefinition>;
  m, ed : TFhirElementDefinition;
  t : TFhirElementDefinitionType;
  rt : TFslStringSet;
begin
  if (type_ = '') then
    raise EFHIRPath.create('No type provided in BuildToolPathEvaluator.ListChildTypesByName');
  if (type_ = 'http://hl7.org/fhir/StructureDefinition/xhtml') then
    exit;
  if (type_ = 'Custom') or (type_ = 'http://hl7.org/fhir/StructureDefinition/Custom') then
    exit;
  if (type_ = FP_SimpleTypeInfo) then
    getSimpleTypeChildTypesByName(name, result)
  else if (type_ = FP_ClassInfo) then
    getClassInfoChildTypesByName(name, result)
  else
  begin
    if (type_.startsWith('http:')) then
    begin
      if (type_.contains('#')) then
        url := type_.substring(0, type_.indexOf('#'))
      else
        url := type_;
    end
    else
    begin
      if (type_.contains('.')) then
      begin
        url := TFHIRProfiledType.ns(type_.substring(0, type_.indexOf('.')));
        type_ := url+'#'+type_;
      end
      else
        url := TFHIRProfiledType.ns(type_);
    end;

    sd := worker.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
    if (sd = nil) then
      raise EFHIRPath.create('Unknown type '+type_); // this really is an error, because we can only get to here if the internal infrastrucgture is wrong
    m := nil;
    sdl := TFslList<TFhirStructureDefinition>.create;
    try
      if (type_.contains('#')) then
        m := getElementDefinition(sd, type_.substring(type_.indexOf('#')+1), true, specifiedType);
      if ((m <> nil) and hasDataType(m)) then
      begin
        if specifiedType <> '' then
        begin
          dt := worker.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+specifiedType) as TFhirStructureDefinition;
          if (dt = nil) then
            raise EFHIRPath.create('unknown data type '+specifiedType);
          sdl.add(dt);
        end
        else
          for t in m.type_List do
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
        if (type_.contains('#')) then
        begin
          tail := type_.substring(type_.indexOf('#')+1);
          tail := tail.substring(tail.indexOf('.'));
        end;
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
                      if (not result.hasType(context, rn)) then
                      begin
                        result.addType(rn);
                        listChildTypesByName(rn, '**', result);
                      end;
                    end;
                  finally
                    rt.free;
                  end;
                end
                else if (not result.hasType(context, tn)) and (tn <> '') then
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
        raise EFHIRException.create('Internal typing issue....');
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
    {$IFNDEF FHIR2}
    if (name.equals('#'+ed.id)) then
    {$ELSE}
    if (name.equal(ed.Name)) then
    {$ENDIF}
      exit(ed);
  result := nil;
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

{ TFHIRPathLexer4 }


function TFHIRPathLexer4.opCodes: TArray<String>;
var
  i : integer;
  s : string;
begin
  setLength(result, length(CODES_TFHIRPathOperation));
  for i := 0 to length(CODES_TFHIRPathOperation) - 1 do
  begin
    s := CODES_TFHIRPathOperation[TFHIRPathOperation(i)];
    result[i] := s;
    s := '';
  end;
end;

function TFHIRPathLexer4.processConstant : TFHIRObject;
begin
  if (isStringConstant()) then
    result := TFHIRString.create(TFHIRPathLexer.processConstant(take())).noExtensions()
   else if (StringIsInteger32(current)) then
    result := TFHIRInteger.create(take).noExtensions()
   else if (StringIsDecimal(current)) then
     result := TFHIRDecimal.create(take).noExtensions()
   else if (StringArrayExistsSensitive(['true', 'false'], current)) then
     result := TFHIRBoolean.create(take = 'true').noExtensions()
   else if (current = '{}') then
   begin
      take;
      result := nil;
   end
   else if (current.startsWith('%') or current.startsWith('@')) then
      result := TFHIRConstant.create(take)
   else
      raise error('Invalid Constant '+current);
end;

end.

