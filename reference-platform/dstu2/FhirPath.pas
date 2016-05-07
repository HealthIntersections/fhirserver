unit FHIRPath;

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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


interface

uses
  SysUtils, Classes, Math, RegExpr, Generics.Collections, Character,
  StringSupport, TextUtilities, SystemSupport, MathSupport,
  AdvObjects, AdvGenerics, DecimalSupport, DateAndTime,
  XmlBuilder,

  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRContext, FHIRConstants,
  FHIRParser;

const
  FHIR_TYPES_STRING : Array of String = ['string', 'uri', 'code', 'oid', 'id', 'uuid', 'sid', 'markdown', 'base64Binary'];

type
  EFHIRPath = class (Exception)
  public
     constructor create(problem : String); overload;
     constructor create(path : String; offset : integer; problem : String); overload;
  end;

  TFHIRPathExecutionContext = class (TAdvObject)
  private
    FAppInfo : TAdvObject;
    FResource : TFHIRBase;
    FContext : TFHIRBase;
  public
    Constructor Create(appInfo : TAdvObject; resource : TFHIRBase; context : TFHIRBase);
    destructor Destroy; override;
    function Link : TFHIRPathExecutionContext; overload;
    property appInfo : TAdvObject read FappInfo;
    property resource : TFHIRBase read FResource;
    property context : TFHIRBase read Fcontext;
  end;

  TFHIRPathExecutionTypeContext = class (TAdvObject)
  private
    FAppInfo : TAdvObject;
    FResourceType : String;
    FContext : TFHIRTypeDetails;
  public
    Constructor Create(appInfo : TAdvObject; resourceType : String; context : TFHIRTypeDetails);
    destructor Destroy; override;
    function Link : TFHIRPathExecutionTypeContext; overload;
    property appInfo : TAdvObject read FappInfo;
    property resourceType : String read FResourceType;
    property context : TFHIRTypeDetails read FContext;
  end;


  TFHIRPathLexer = class (TAdvObject)
  private
    FPath : String;
    FCursor : integer;
    FCurrentLocation : TSourceLocation;
    FCurrent : String;
    FCurrentStart : integer;
    FCurrentStartLocation : TSourceLocation;
    FId : integer;
  public
    constructor Create(path : String); overload;
    destructor Destroy; override;
    procedure next;
    property current : String read FCurrent;
    property CurrentStart : integer read FCurrentStart;
    function done : boolean;
    function take : String;

    function hasComment: boolean;
    procedure skipComments;

    function nextId : integer;
    function error(msg : String) : Exception; overload;
    function error(msg : String; offset : integer) : Exception; overload;
    function error(msg : String; location : TSourceLocation) : Exception; overload;

    function isConstant(incDoubleQuotes : boolean = false) : boolean;
    function isStringConstant: boolean;
    function readConstant(desc: String): String;
    class function processConstant(s: String): String;

    function isOp : boolean;
    function isToken : boolean; overload;
    function hasToken(kw : String) : boolean; overload;
    procedure token(kw : String); overload;
    procedure skiptoken(kw : String); overload;
    function takeDottedToken: String;
  end;

  TFHIRPathDebugPackage = class (TAdvObject)
  private
    FSourceEnd: TSourceLocation;
    Fcontext: TFHIRPathExecutionContext;
    Finput2: TFHIRBaseList;
    Finput1: TFHIRBaseList;
    FExpression: TFHIRExpressionNode;
    FSourceStart: TSourceLocation;
    Foutcome: TFHIRBaseList;
    FIsOperation: boolean;
    procedure Setcontext(const Value: TFHIRPathExecutionContext);
    procedure SetExpression(const Value: TFHIRExpressionNode);
    procedure Setinput1(const Value: TFHIRBaseList);
    procedure Setinput2(const Value: TFHIRBaseList);
    procedure Setoutcome(const Value: TFHIRBaseList);
  public
    destructor Destroy; override;
    function Link : TFHIRPathDebugPackage; overload;
    property SourceStart : TSourceLocation read FSourceStart write FSourceStart;
    property SourceEnd : TSourceLocation read FSourceEnd write FSourceEnd;
    property Expression : TFHIRExpressionNode read FExpression write SetExpression;
    property IsOperation : boolean read FIsOperation write FIsOperation;
    property context : TFHIRPathExecutionContext read Fcontext write Setcontext;
    property input1 : TFHIRBaseList read Finput1 write Setinput1;
    property input2 : TFHIRBaseList read Finput2 write Setinput2;
    property outcome : TFHIRBaseList read Foutcome write Setoutcome;
  end;

  TFHIRExpressionEngine = class;

  TFHIRPathDebugEvent = procedure (source : TFHIRExpressionEngine; package : TFHIRPathDebugPackage) of object;

  TFHIRExpressionEngine = class (TAdvObject)
  private
    worker : TWorkerContext;
    FOndebug : TFHIRPathDebugEvent;
    FLog : TStringBuilder;
    primitiveTypes, allTypes : TStringList;

    procedure log(name, value : String);
    function parseExpression(lexer: TFHIRPathLexer; proximal : boolean): TFHIRExpressionNode;
    procedure organisePrecedence(lexer : TFHIRPathLexer; var node: TFHIRExpressionNode);
    procedure gatherPrecedence(lexer : TFHIRPathLexer; var start: TFHIRExpressionNode; ops: TFHIRPathOperationSet);
    function newGroup(lexer : TFHIRPathLexer; next: TFHIRExpressionNode): TFHIRExpressionNode;
    procedure checkParameters(lexer : TFHIRPathLexer; location : TSourceLocation; offset : Integer; exp : TFHIRExpressionNode);
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRExpressionNode; count : integer); overload;
    procedure checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRExpressionNode; countMin, countMax : integer); overload;

    function execute(context : TFHIRPathExecutionContext; focus : TFHIRBaseList; exp : TFHIRExpressionNode; atEntry : boolean) : TFHIRBaseList; overload;
    function execute(context : TFHIRPathExecutionContext; item : TFHIRBase; exp : TFHIRExpressionNode; atEntry : boolean) : TFHIRBaseList; overload;
    procedure debug(context : TFHIRPathExecutionContext; exp : TFHIRExpressionNode; op : boolean; input1, input2, outcome : TFHIRBaseList);
    function replaceFixedConstant(context : TFHIRPathExecutionContext; const s : String) : TFHIRBase;

    function evaluateFunction(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
    function preOperate(left : TFHIRBaseList; op : TFHIRPathOperation) : TFHIRBaseList;
    function operate(left : TFHIRBaseList; op : TFHIRPathOperation; right : TFHIRBaseList) : TFHIRBaseList;
    function readConstant(context : TFHIRPathExecutionContext; constant : String) : TFHIRBase;
    procedure ListAllChildren(item: TFHIRBase; results: TFHIRBaseList; recurse: boolean);
    procedure ListChildrenByName(focus: TFHIRBase; name : String; results: TFHIRBaseList);

    function childTypes(focus : TFHIRTypeDetails; mask : string) : TFHIRTypeDetails;
    procedure checkParamTypes(funcId : TFHIRPathFunction; paramTypes : TAdvList<TFHIRTypeDetails>; typeSet : array of TFHIRTypeDetails);
    function executeType(ctxt: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRExpressionNode; atEntry : boolean) : TFHIRTypeDetails; overload;
    function executeType(focus: String; exp: TFHIRExpressionNode; atEntry : boolean) : TFHIRTypeDetails; overload;
    function evaluateFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRExpressionNode): TFHIRTypeDetails;
    function operateTypes(left : TFHIRTypeDetails; op : TFHIRPathOperation; right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function readConstantType(ctxt: TFHIRPathExecutionTypeContext; constant : String) : string;

    procedure ListChildTypesByName(item, name : string; result : TFHIRTypeDetails);

    function getElementDefinition(sd : TFHIRStructureDefinition; path : String; allowPM : boolean; var specifiedType : String) : TFHIRElementDefinition;
    function getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
    function hasDataType(ed : TFhirElementDefinition) : boolean;

    function funcEmpty(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcItem(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcWhere(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcAll(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcFirst(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcLast(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcTail(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcCount(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcLength(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcDistinct(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcNot(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcResolve(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcContains(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcMatches(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcStartsWith(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSubString(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcExtension(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcExists(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSubsetOf(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSupersetOf(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcIsDistinct(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSelect(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcRepeat(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcAs(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcIs(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSingle(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcSkip(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcTake(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcIif(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcToInteger(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcToDecimal(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcToString(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcEndsWith(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcReplaceMatches(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcReplace(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcChildren(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcDescendents(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcMemberOf(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcTrace(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcToday(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;
    function funcNow(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp : TFHIRExpressionNode) : TFHIRBaseList;


    function equals(left, right : TFHIRBase) : boolean;  overload;
    function equivalent(left, right : TFHIRBase) : boolean;  overload;

    function opEquals(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opNotEquals(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opLessThan(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opGreater(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opLessOrEqual(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opGreaterOrEqual(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opIn(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opContains(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opPlus(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opConcatenate(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opMinus(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opEquivalent(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opNotEquivalent(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opUnion(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opAnd(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opOr(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opXor(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opImplies(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opTimes(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opDivideBy(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opDiv(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opMod(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opIs(left, right : TFHIRBaseList) : TFHIRBaseList;
    function opAs(left, right : TFHIRBaseList) : TFHIRBaseList;

    function areDistinct(a1, a2: array of TFHIRBaseList): boolean;
    function contains(list: TFHIRBaseList; item: TFHIRBase): boolean;
    function isAbstractType(list: TFHIRElementDefinitionTypeList): boolean;

  public
    constructor Create(context : TWorkerContext);
    destructor Destroy; override;
    property Ondebug : TFHIRPathDebugEvent read FOndebug write FOndebug;

    // Parse a path for later use using execute
    function parse(path : String) : TFHIRExpressionNode; overload;
    function parse(lexer : TFHIRPathLexer) : TFHIRExpressionNode; overload;

    // check that paths referred to in the expression are valid
    function check(appInfo : TAdvObject; resourceType, context, path : String; expr : TFHIRExpressionNode; xPathStartsWithValueRef : boolean) : TFHIRTypeDetails;

    // evaluate a path and return the matching elements
    function evaluate(appInfo : TAdvObject; base : TFHIRBase; path : String) : TFHIRBaseList; overload;
    function evaluate(appInfo : TAdvObject; base : TFHIRBase; expr : TFHIRExpressionNode) : TFHIRBaseList; overload;
    function evaluate(appInfo : TAdvObject; resource : TFHIRBase; base : TFHIRBase; path : String) : TFHIRBaseList; overload;
    function evaluate(appInfo : TAdvObject; resource : TFHIRBase; base : TFHIRBase; expr : TFHIRExpressionNode) : TFHIRBaseList; overload;

    // evaluate a path and return true or false
    function evaluateToBoolean(appInfo : TAdvObject; resource : TFHIRBase; base : TFHIRBase; path : String) : boolean; overload;
    function evaluateToBoolean(appInfo : TAdvObject; resource : TFHIRBase; base : TFHIRBase; expr : TFHIRExpressionNode) : boolean; overload;

    // evaluate a path and return a string describing the outcome
    function evaluateToString(appInfo : TAdvObject; base : TFHIRBase; path : String) : string;

    // worker routine for converting a set of objects to a string representation
    function convertToString(items : TFHIRBaseList) : String; overload;
    function convertToString(item : TFHIRBase) : String; overload;

    // worker routine for converting a set of objects to a boolean representation
    function convertToBoolean(items : TFHIRBaseList) : boolean;

    function UseLog : String;
  end;

implementation

{ TFHIRPathEvaluator }

function TFHIRExpressionEngine.check(appInfo : TAdvObject; resourceType, context, path : String; expr : TFHIRExpressionNode; xPathStartsWithValueRef : boolean) : TFHIRTypeDetails;
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
      raise Exception.Create('Unknown context '+context);
    ed := getElementDefinition(sd, context, true, t);
    if (ed = nil) then
      raise Exception.Create('Unknown context element '+context);
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

procedure TFHIRExpressionEngine.checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRExpressionNode; count : integer);
  begin
  if (exp.Parameters.count <> count) then
    lexer.error('The function "'+exp.name+'" requires '+inttostr(count)+' parameters', location);
  end;

procedure TFHIRExpressionEngine.checkParamCount(lexer: TFHIRPathLexer; location : TSourceLocation; exp : TFHIRExpressionNode; countMin, countMax : integer);
  begin
    if (exp.Parameters.count < countMin) or (exp.Parameters.count > countMax) then
      lexer.error('The function "'+exp.name+'" requires between '+inttostr(countMin)+' and '+inttostr(countMax)+' parameters', location);
  end;

procedure TFHIRExpressionEngine.checkParameters(lexer: TFHIRPathLexer; location : TSourceLocation; offset: Integer; exp: TFHIRExpressionNode);
begin
  case exp.FunctionId of
    pfEmpty: checkParamCount(lexer, location, exp, 0);
    pfNot: checkParamCount(lexer, location, exp, 0);
    pfExists: checkParamCount(lexer, location, exp, 0);
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
    pfDescendents: checkParamCount(lexer, location, exp, 0);
    pfMemberOf: checkParamCount(lexer, location, exp, 1);
    pfTrace: checkParamCount(lexer, location, exp, 1);
    pfToday: checkParamCount(lexer, location, exp, 0);
    pfNow: checkParamCount(lexer, location, exp, 0);
    pfResolve: checkParamCount(lexer, location, exp, 0);
    pfExtension: checkParamCount(lexer, location, exp, 1);
  end;
end;

function TFHIRExpressionEngine.convertToBoolean(items: TFHIRBaseList): boolean;
begin
  if (items = nil) then
    result := false
  else if (items.count = 1) and (items[0] is TFHIRBoolean) then
    result := TFHIRBoolean(items[0]).value
  else
    result := items.count > 0;
end;

function TFHIRExpressionEngine.convertToString(item: TFHIRBase): String;
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

function isPrimitive(sd : TFhirStructureDefinition) : boolean;
var
  ed : TFHIRElementDefinition;
begin
  result := false;
  for ed in sd.snapshot.elementList do
    if (ed.path = sd.name+'.value') and (PropertyRepresentationXmlAttr in ed.representation) then
      exit(true);
end;

constructor TFHIRExpressionEngine.create(context: TWorkerContext);
var
  sd : TFhirStructureDefinition;
begin
  inherited Create;
  worker := context;
  FLog := TStringBuilder.Create;
  allTypes := TStringList.Create;
  primitiveTypes := TStringList.Create;
  for sd in worker.allStructures do
    if (sd.kind <> StructureDefinitionKindLogical) then
  begin
    {$IFDEF FHIR3}
    if (sd.derivation = TypeDerivationRuleSPECIALIZATION) then
      allTypes.add(sd.id);
    if (sd.derivation = TypeDerivationRuleSPECIALIZATION) and isPrimitive(sd) then
      primitiveTypes.add(sd.id);
    {$ELSE}
    raise Exception.Create('Debug this');
    if (sd.constrainedType = '') then
      allTypes.add(sd.id);
    if (sd.constrainedType = '') and isPrimitive(sd) then
      primitiveTypes.add(sd.id);
    {$ENDIF}
  end;
end;

procedure TFHIRExpressionEngine.debug(context : TFHIRPathExecutionContext; exp: TFHIRExpressionNode; op : boolean; input1, input2, outcome: TFHIRBaseList);
var
  pack : TFHIRPathDebugPackage;
begin
  if assigned(FOndebug) then
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
      FOndebug(self, pack);
    finally
      pack.Free;
    end;
  end;
end;

destructor TFHIRExpressionEngine.destroy;
begin
  FLog.Free;
  worker.Free;
  primitiveTypes.Free;
  allTypes.Free;

  inherited;
end;

function TFHIRExpressionEngine.convertToString(items: TFHIRBaseList): String;
var
  b : TStringBuilder;
  first : boolean;
  item : TFHIRBase;
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
      b.Append(convertToString(item));
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRExpressionEngine.evaluate(appInfo : TAdvObject; base: TFHIRBase; path: String): TFHIRBaseList;
var
  exp : TFHIRExpressionNode;
  list : TFHIRBaseList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  exp := parse(path);
  try
    list := TFHIRBaseList.Create(base.Link);
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

function TFHIRExpressionEngine.evaluate(appInfo : TAdvObject; base: TFHIRBase; expr : TFHIRExpressionNode): TFHIRBaseList;
var
  list : TFHIRBaseList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  list := TFHIRBaseList.Create(base.Link);
  try
    ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, nil, base.Link);
    try
      result := execute(ctxt, list, expr, true);
    finally
      ctxt.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRExpressionEngine.evaluate(appInfo : TAdvObject; resource : TFHIRBase; base: TFHIRBase; path: String): TFHIRBaseList;
var
  exp : TFHIRExpressionNode;
  list : TFHIRBaseList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  exp := parse(path);
  try
    list := TFHIRBaseList.Create(base.Link);
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

function TFHIRExpressionEngine.equals(left, right: TFHIRBase): boolean;
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

function TFHIRExpressionEngine.equivalent(left, right: TFHIRBase): boolean;
var
  ok : boolean;
begin
  if (left.hasType('integer') and right.hasType('integer')) then
    result := equals(left, right)
  else if (left.hasType('boolean') and right.hasType('boolean')) then
    result := equals(left, right)
  else if (left.hasType(['integer', 'decimal']) and right.hasType(['integer', 'decimal'])) then
    result :=  equivalentNumber(left.primitiveValue(), right.primitiveValue())
  else if (left.hasType(['date', 'dateTime', 'time', 'instant']) and right.hasType(['date', 'dateTime', 'time', 'instant'])) then
    result :=  equivalentNumber(left.primitiveValue(), right.primitiveValue())
  else if (left.hasType(['string', 'id', 'code', 'uri']) and right.hasType(['string', 'id', 'code', 'uri'])) then
    result :=  equivalentNumber(convertToString(left), convertToString(right))
  else
    raise EFHIRPath.create(StringFormat('Unable to determine equivalence between %s and %s', [left.fhirType(), right.fhirType()]));
end;

function TFHIRExpressionEngine.evaluate(appInfo : TAdvObject; resource : TFHIRBase; base: TFHIRBase; expr : TFHIRExpressionNode): TFHIRBaseList;
var
  list : TFHIRBaseList;
  ctxt : TFHIRPathExecutionContext;
begin
  FLog.clear;
  list := TFHIRBaseList.Create(base.Link);
  try
    ctxt := TFHIRPathExecutionContext.Create(appInfo.Link, resource.Link, base.Link);
    try
      result := execute(ctxt, list, expr, true);
    finally
      ctxt.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRExpressionEngine.evaluateToBoolean(appInfo : TAdvObject; resource : TFHIRBase; base: TFHIRBase; path: String): boolean;
var
  res : TFHIRBaseList;
begin
  res := evaluate(appInfo, resource, base, path);
  try
    result := convertToBoolean(res);
  finally
    res.Free;
  end;
end;

function TFHIRExpressionEngine.evaluateToBoolean(appInfo: TAdvObject; resource, base: TFHIRBase; expr: TFHIRExpressionNode): boolean;
var
  res : TFHIRBaseList;
begin
  res := evaluate(appInfo, resource, base, expr);
  try
    result := convertToBoolean(res);
  finally
    res.Free;
  end;
end;

function TFHIRExpressionEngine.evaluateToString(appInfo : TAdvObject; base: TFHIRBase; path: String): string;
var
  res : TFHIRBaseList;
begin
  res := evaluate(appInfo, base, path);
  try
    result := convertToString(res);
  finally
    res.Free;
  end;
end;

function TFHIRExpressionEngine.execute(context : TFHIRPathExecutionContext; item : TFHIRBase; exp : TFHIRExpressionNode; atEntry : boolean): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
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

procedure TFHIRExpressionEngine.ListAllChildren(item : TFHIRBase; results : TFHIRBaseList; recurse : boolean);
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
          results.Add(b.Link as TFhirBase);
          if (recurse) then
            ListAllChildren(b as TFHIRBase, results, true);
        end;
      end;
      pi.Next;
    end;
  finally
    pi.free;
  end;
end;

function TFHIRExpressionEngine.executeType(ctxt: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRExpressionNode; atEntry : boolean): TFHIRTypeDetails;
var
  s : String;
  work, work2 : TFHIRTypeDetails;
  next, last : TFHIRExpressionNode;
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

function TFHIRExpressionEngine.funcAll(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  all, v : boolean;
begin
  result := TFHIRBaseList.Create();
  try
    if (exp.Parameters.count = 1) then
begin
  all := true;
  pc := TFHIRBaseList.Create;
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
        v := false;
        if (item is TFHIRBoolean) then
          v := TFHIRBoolean(item).value
        else
          v := item <> nil;
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

function TFHIRExpressionEngine.funcAs(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  tn : String;
  b : TFHIRBase;
begin
  tn := exp.Parameters[0].name;
  result := TFHIRBaseList.Create;
  try
    for b in focus do
      if (b.hasType(tn)) then
        result.add(b.Link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcChildren(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
begin
  result := TFHIRBaseList.Create;
  try
    for item in focus do
      listAllChildren(item, result, false);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcContains(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  res : TFHIRBaseList;
  sw : String;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0]).contains(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcCount(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFhirInteger.Create(inttostr(focus.Count)));
end;

function TFHIRExpressionEngine.areDistinct(a1, a2 : array of TFHIRBaseList) : boolean;
var
  i : integer;
  res : TFHIRBaseList;
begin
  result := false;
  for i := 0 to length(a1) - 1 do
  begin
    res := opEquals(a1[i], a2[i]);
    try
      if not convertToBoolean(res) then
      begin
        result := true;
        exit;
      end;
    finally
      res.Free;
    end;
  end;
end;

function TFHIRExpressionEngine.funcDescendents(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
begin
  result := TFHIRBaseList.Create;
  try
    for item in focus do
      listAllChildren(item, result, true);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcDistinct(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  i, j : integer;
  found : boolean;
begin
  if (focus.count <= 1) then
    result := focus.Link;

  result := TFHIRBaseList.Create;
  try
    for i := 0 to focus.count - 1 do
    begin
      found := false;
      for j := i+1 to focus.count - 1 do
        begin
        if (equals(focus[j], focus[i])) then
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

function TFHIRExpressionEngine.funcEmpty(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFhirBoolean.Create(focus.Count = 0));
end;

function TFHIRExpressionEngine.funcEndsWith(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  res : TFHIRBaseList;
  sw : String;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0]).endsWith(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcFirst(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if focus.Count > 0 then
    result.Add(focus[0].Link);
end;

function TFHIRExpressionEngine.funcIif(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  n1 : TFHIRBaseList;
  v : boolean;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
  try
    v := convertToBoolean(n1);

    if (v) then
      result := execute(context, focus, exp.parameters[1], true)
    else if (exp.parameters.count < 3) then
      result := TFHIRBaseList.Create
    else
      result := execute(context, focus, exp.parameters[2], true);
  finally
    n1.free;
  end;
end;

function TFHIRExpressionEngine.funcIs(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  tn : string;
begin
  result := TFHIRBaseList.Create;
  try
    if (focus.count = 0) or (focus.count > 1) then
      result.add(TFHIRBoolean.create(false))
    else
    begin
      tn := exp.Parameters[0].name;
      result.add(TFHIRBoolean.create(focus[0].hasType(tn)));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpressionEngine.funcIsDistinct( context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  distinct : boolean;
  i , j : integer;
begin
  result := TFHIRBaseList.Create;
  try
    if (focus.count <= 1) then
      result.add(TFHIRBoolean.create(true))
    else
    begin
      distinct := true;
      for i := 0 to focus.count - 1 do
        for j := i+1 to focus.count - 1 do
          if (equals(focus[j], focus[i])) then
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

function TFHIRExpressionEngine.funcItem(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  s : String;
  res : TFHIRBaseList;
begin
  res := execute(context, focus, exp.Parameters[0], false);
  try
		s := convertToString(res);
  finally
    res.Free;
  end;
  result := TFHIRBaseList.Create;
  if StringIsInteger16(s) and (focus.Count > StrToInt(s)) then
    result.Add(focus[StrToInt(s)].Link);
end;

function TFHIRExpressionEngine.funcLast(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if focus.Count > 0 then
    result.Add(focus[focus.Count - 1].Link);
end;

function TFHIRExpressionEngine.funcLength(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  s : String;
begin
  result := TFHIRBaseList.Create();
  try
    if (focus.count = 1) then
  begin
      s := convertToString(focus[0]);
      result.add(TFHIRInteger.create(inttostr(s.length)));
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcTrace(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  n1 : TFHIRBaseList;
  name : String;
begin
  n1 := execute(context, focus, exp.Parameters[0], false);
  try
    name := n1[0].primitiveValue;
    log(name, convertToString(focus));
    result := focus.Link;
  finally
    n1.Free;
  end;
end;

function TFHIRExpressionEngine.funcMatches(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  res : TFHIRBaseList;
  s, p : String;
  reg : TRegExpr;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      p := convertToString(res);
    finally
      res.free;
    end;
    reg := TRegExpr.Create;
    try
      reg.Expression := p;
      for item in focus do
      begin
        s := convertToString(item);
        if (reg.Exec(s)) then
          result.Add(item.Link);
      end;
    finally
      reg.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcMemberOf(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  raise EFHIRPath.create('Not Done Yet');
end;

function TFHIRExpressionEngine.funcNot(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFhirBoolean.Create(not convertToBoolean(focus)));
end;

function TFHIRExpressionEngine.funcNow(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFhirDateTime.Create(NowLocal));
end;

function TFHIRExpressionEngine.funcRepeat(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  current, added, pc, work : TFHIRBaseList;
  item : TFHIRBase;
  ctxt : TFHIRPathExecutionContext;
  more : boolean;
begin
  result := TFHIRBaseList.Create;
  current := TFHIRBaseList.Create;
  added := TFHIRBaseList.Create;
  try
    current.AddAll(focus);
    more := true;
    while (more) do
    begin
      added.clear;
      pc := TFHIRBaseList.Create;
      try
        for item in current do
        begin
          pc.clear();
          pc.add(item.link);
          ctxt := TFHIRPathExecutionContext.Create(context.FAppInfo.Link, context.resource.Link, item.Link);
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

function TFHIRExpressionEngine.funcReplace(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
    begin
  raise EFHIRPath.create('Not Done Yet');
    end;

function TFHIRExpressionEngine.funcReplaceMatches( context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
    begin
  raise EFHIRPath.create('Not Done Yet');
    end;

function TFHIRExpressionEngine.funcResolve(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  raise EFHIRPath.create('The function '+exp.name+' is not done yet');
end;

function TFHIRExpressionEngine.funcSelect(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  pc, work : TFHIRBaseList;
  item : TFHIRBase;
  ctxt : TFHIRPathExecutionContext;
begin
  result := TFHIRBaseList.Create;
  try
    pc := TFHIRBaseList.Create;
    try
      for item in focus do
    begin
        pc.clear();
        pc.add(item.link);
        ctxt := TFHIRPathExecutionContext.Create(context.FAppInfo.Link, context.resource.Link, item.Link);
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

function TFHIRExpressionEngine.funcSingle(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  if (focus.count <> 1) then
    raise EFHIRPath.create(StringFormat('Single() : checking for 1 item but found %d items', [focus.count]));
  result := focus.link;
end;

function TFHIRExpressionEngine.funcSkip(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  bl : TFHIRBaseList;
  i, i1 : integer;
begin
  bl := execute(context, focus, exp.Parameters[0], true);
  try
    result := TFHIRBaseList.Create;
    i1 := StrToInt(bl[0].primitiveValue);
    for i := i1 to focus.count - 1 do
      result.add(focus[i].Link);
  finally
    bl.free;
  end;
end;

function TFHIRExpressionEngine.funcStartsWith(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  res : TFHIRBaseList;
  sw : String;
begin
  result := TFHIRBaseList.Create;
  try
    res := execute(context, focus, exp.Parameters[0], false);
    try
      sw := convertToString(res);
    finally
      res.free;
    end;

    if (focus.count = 1) and (sw <> '') then
      result.add(TFHIRBoolean.create(convertToString(focus[0]).startsWith(sw)))
    else
      result.add(TFHIRBoolean.create(false));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.funcSubsetOf(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  target : TFHIRBaseList;
  valid, found : boolean;
  item, t : TFHIRBase;
begin
  target := execute(context, focus, exp.Parameters[0], true);
  try
    valid := true;
    for item in focus do
    begin
      found := false;
      for t in target do
      begin
        if (equals(item, t)) then
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
    result := TFHIRBaseList.create(TFHIRBoolean.create(valid));
  finally
    target.free;
  end;
end;

function TFHIRExpressionEngine.funcSubString(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  s, sw : String;
  n1, n2 : TFhirBaseList;
  i1, i2 : integer;
begin
  n1 := nil;
  n2 := nil;
  result := TFHIRBaseList.Create;
  try
    n1 := execute(context, focus, exp.Parameters[0], false);
    i1 := StrToInt(n1[0].primitiveValue);
    if (exp.ParameterCount = 2) then
    begin
      n2 := execute(context, focus, exp.Parameters[1], false);
      i2 := StrToInt(n2[0].primitiveValue);
    end;

    if focus.count = 1 then
    begin
      sw := convertToString(focus[0]);
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

function TFHIRExpressionEngine.funcExists(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.add(TFHIRBoolean.create(focus.count > 0));
end;

function TFHIRExpressionEngine.funcSupersetOf( context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  target : TFHIRBaseList;
  valid, found : boolean;
  item, t : TFHIRBase;
begin
  target := execute(context, focus, exp.Parameters[0], true);
  try
    valid := true;
    for item in target do
    begin
      found := false;
      for t in focus do
      begin
        if (equals(item, t)) then
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
    result := TFHIRBaseList.create(TFHIRBoolean.create(valid));
  finally
    target.free;
  end;
end;

function TFHIRExpressionEngine.funcExtension(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item, ex : TFHIRBase;
  vl : TFHIRBaseList;
  url : String;
  n1, ext : TFhirBaseList;
begin
  n1 := nil;
  result := TFHIRBaseList.Create;
  try
    n1 := execute(context, focus, exp.Parameters[0], false);
    url := n1[0].primitiveValue;

    for item in focus do
    begin
      ext := TFHIRBaseList.Create;
      try
        ListChildrenByName(item, 'extension', ext);
        ListChildrenByName(item, 'modifierExtension', ext);
        for ex in ext do
        begin
          vl := TFHIRBaseList.Create;
          try
            ListChildrenByName(ex, 'url', vl);
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

function TFHIRExpressionEngine.funcTail(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
   i : integer;
begin
  result := TFHIRBaseList.Create;
  for i := 1 to focus.Count -1 do
    result.Add(focus[i].Link);
end;

function TFHIRExpressionEngine.funcTake(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  n1 : TFHIRBaseList;
  i, i1 : integer;
begin
  n1 := execute(context, focus, exp.Parameters[0], true);
  try
    i1 := strtoint(n1[0].primitiveValue());

    result := TFHIRBaseList.Create;
    for i := 0 to integerMin(focus.Count, i1) -1 do
      result.Add(focus[i].Link);
  finally
    n1.free;
  end;
end;

function TFHIRExpressionEngine.funcToday(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFhirDate.Create(Today));
end;

function StringIsDecimal(s : String) : boolean;
var
  ch : char;
begin
  result := true;
  for ch in s do
    if not CharInSet(ch, ['0'..'9', '.']) then
      exit(false);
  result := (StringCount(s, '.') <= 1) and not s.StartsWith('.') and not s.EndsWith('.');
end;


function TFHIRExpressionEngine.funcToDecimal(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRBaseList.Create;
  if (StringIsDecimal(s)) then
    result.add(TFHIRDecimal.Create(s));
end;

function TFHIRExpressionEngine.funcToInteger(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  s : string;
begin
  s := convertToString(focus);
  result := TFHIRBaseList.Create;
  if (StringIsInteger32(s)) then
    result.add(TFHIRInteger.Create(s));
end;

function TFHIRExpressionEngine.funcToString(context: TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  result.add(TFHIRString.Create(convertToString(focus)));
end;

function TFHIRExpressionEngine.funcWhere(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
var
  item : TFHIRBase;
  pc, res : TFHIRBaseList;
  ctxt : TFHIRPathExecutionContext;
begin
  result := TFHIRBaseList.Create;
  try
    pc := TFHIRBaseList.Create;
    try
      for item in focus do
      begin
        pc.Clear;
        pc.Add(item.Link);
        ctxt := TFHIRPathExecutionContext.Create(context.FAppInfo.Link, context.resource.Link, item.Link);
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

function isBoolean(list : TFHIRBaselist; b : boolean) : boolean;
begin
  result := (list.count = 1) and (list[0] is TFHIRBoolean) and (TFHIRBoolean(list[0]).value = b);
end;

function TFHIRExpressionEngine.preOperate(left: TFHIRBaseList; op: TFHIRPathOperation): TFHIRBaseList;
begin
  result := nil;
  case op of
    popAnd: if isBoolean(left, false) then
        result := TFHIRBaseList.Create(TFHIRBoolean.Create(false));
    popOr: if isBoolean(left, true) then
        result := TFHIRBaseList.Create(TFHIRBoolean.Create(true));
    popImplies: if (not convertToBoolean(left)) then
        result := TFHIRBaseList.Create(TFHIRBoolean.Create(true));
  end;
end;

function TFHIRExpressionEngine.operate(left: TFHIRBaseList; op: TFHIRPathOperation; right: TFHIRBaseList): TFHIRBaseList;
begin
  case op of
    popNull: raise EFHIRPath.create('An internal error has occurred');
    popEquals: result := opEquals(left, right);
    popEquivalent: result := opEquivalent(left, right);
    popNotEquals: result := opNotEquals(left, right);
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
  else
    raise EFHIRPath.create('An internal error has occurred (operation not implemented)');
  end;
end;


function TFHIRExpressionEngine.operateTypes(left: TFHIRTypeDetails; op: TFHIRPathOperation; right: TFHIRTypeDetails): TFHIRTypeDetails;
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
  else
    raise EFHIRPath.create('not done yet');
  end;
end;

function TFHIRExpressionEngine.opAnd(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if (left.isEmpty()) and (right.isEmpty()) then
    // nothing
  else if (isBoolean(left, false)) or (isBoolean(right, false)) then
    result.Add(TFhirBoolean.Create(false))
  else if (left.isEmpty()) or (right.isEmpty()) then
    // noothing
  else if (convertToBoolean(left)) and (convertToBoolean(right)) then
    result.Add(TFhirBoolean.Create(true))
  else
    result.Add(TFhirBoolean.Create(false));
end;

function TFHIRExpressionEngine.opAs(left, right: TFHIRBaseList): TFHIRBaseList;
var
  tn : String;
  b : TFHIRBase;
begin
  tn := convertToString(right);
  result := TFHIRBaseList.Create;
  try
    for b in left do
      if (b.hasType(tn)) then
        result.add(b.Link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opContains(left, right: TFHIRBaseList): TFHIRBaseList;
var
  ans, f : boolean;
  l, r : TFHIRBase;
    begin
  ans := true;
  for r in right do
  begin
    f := false;
    for l in left do
      if equals(l, r) then
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
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(ans));
    end;

function TFHIRExpressionEngine.opDiv(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1, d2, d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing div: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing div: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing div: left operand has the wrong type (%s)', [left[0].fhirType]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing div: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing div: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing div: right operand has the wrong type (%s)', [right[0].fhirType]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strtoInt(l.primitiveValue()) div strtoInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.divInt(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing div: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opDivideBy(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1, d2, d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing /: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing /: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: left operand has the wrong type (%s)', [left[0].fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing /: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing /: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing /: right operand has the wrong type (%s)', [right[0].fhirType()]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];

    if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.divide(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing /: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));
    result.link;
  finally
    result.free;
  end;

end;

function TFHIRExpressionEngine.opEquals(left, right: TFHIRBaseList): TFHIRBaseList;
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
      if not equals(left[i], right[i]) then
      begin
        res := false;
        break;
      end;
  end;
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRExpressionEngine.opEquivalent(left, right: TFHIRBaseList): TFHIRBaseList;
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
        if equivalent(left[i], right[j]) then
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
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRExpressionEngine.opGreater(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  sl, sr, tl, tr : String;
  lUnit, rUnit : TFHIRBaseList;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].isPrimitive) and (right[0].isPrimitive) then
    begin
      l := left[0] as TFHIRBase;
      r := right[0] as TFHIRBase;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) > StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue > r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].hasType('Quantity') and right[0].hasType('Quantity') then
    begin
      lUnit := TFHIRBaseList.create;
      rUnit := TFHIRBaseList.create;
      try
			  ListChildrenByName(left[0], 'unit', lUnit);
			  ListChildrenByName(right[0], 'unit', rUnit);
        if (compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0], 'value', lUnit);
          ListChildrenByName(right[0], 'value', rUnit);
          result := opGreater(lUnit, rUnit);
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

function TFHIRExpressionEngine.opGreaterOrEqual(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  lUnit, rUnit : TFHIRBaseList;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].isPrimitive) and (right[0].isPrimitive) then
    begin
      l := left[0] as TFHIRBase;
      r := right[0] as TFHIRBase;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) >= StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue >= r.primitiveValue));
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].hasType('Quantity') and right[0].hasType('Quantity') then
    begin
      lUnit := TFHIRBaseList.create;
      rUnit := TFHIRBaseList.create;
      try
			  ListChildrenByName(left[0], 'unit', lUnit);
			  ListChildrenByName(right[0], 'unit', rUnit);
        if (compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0], 'value', lUnit);
          ListChildrenByName(right[0], 'value', rUnit);
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

function TFHIRExpressionEngine.opIn(left, right: TFHIRBaseList): TFHIRBaseList;
var
  ans, f : boolean;
  l, r : TFHIRBase;
begin
  ans := true;
  for l in left do
  begin
    f := false;
    for r in right do
      if equals(l, r) then
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
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(ans));
end;

function TFHIRExpressionEngine.opIs(left, right: TFHIRBaseList): TFHIRBaseList;
var
  tn : string;
begin
  result := TFHIRBaseList.Create;
  try
    if (left.count = 0) or (left.count > 1) then
      result.add(TFHIRBoolean.create(false))
    else
    begin
      tn := convertToString(right);
      result.add(TFHIRBoolean.create(left[0].hasType(tn)));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpressionEngine.opLessOrEqual(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  lUnit, rUnit : TFHIRBaseList;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].isPrimitive) and (right[0].isPrimitive) then
    begin
      l := left[0];
      r := right[0];
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) <= StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue <= r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].hasType('Quantity') and right[0].hasType('Quantity') then
    begin
      lUnit := TFHIRBaseList.create;
      rUnit := TFHIRBaseList.create;
      try
			  ListChildrenByName(left[0], 'unit', lUnit);
			  ListChildrenByName(right[0], 'unit', rUnit);
        if (compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0], 'value', lUnit);
          ListChildrenByName(right[0], 'value', rUnit);
          result := opLessOrEqual(lUnit, rUnit);
        end
        else
          raise EFHIRPath.create('Canonical Comparison isn"t done yet (units =  '+lUnit[0].primitiveValue+'/'+rUnit[0].primitiveValue+')');
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

function TFHIRExpressionEngine.opLessThan(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  lUnit, rUnit : TFHIRBaseList;
begin
  result := TFHIRBaseList.create;
  try
    if (Left.Count = 1) and (right.count = 1) and (left[0].isPrimitive) and (right[0].isPrimitive) then
    begin
      l := left[0] as TFHIRBase;
      r := right[0] as TFHIRBase;
      if (l.hasType(FHIR_TYPES_STRING) and r.hasType(FHIR_TYPES_STRING)) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
      else if l.hasType(['decimal', 'integer']) and r.hasType(['decimal', 'integer']) then
        result.Add(TFhirBoolean.Create(StrToFloat(l.primitiveValue) < StrToFloat(r.primitiveValue)))
      else if l.hasType(['date', 'dateTime', 'instant']) and r.hasType(['date', 'dateTime', 'instant']) then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
      else if l.hasType('time') and r.hasType('time') then
        result.Add(TFhirBoolean.Create(l.primitiveValue < r.primitiveValue))
    end
    else if (Left.Count = 1) and (right.count = 1) and left[0].hasType('Quantity') and right[0].hasType('Quantity') then
    begin
      lUnit := TFHIRBaseList.create;
      rUnit := TFHIRBaseList.create;
      try
			  ListChildrenByName(left[0], 'unit', lUnit);
			  ListChildrenByName(right[0], 'unit', rUnit);
        if (compareDeep(lUnit, rUnit, true)) then
        begin
          lUnit.Clear;
          rUnit.Clear;
          ListChildrenByName(left[0], 'value', lUnit);
          ListChildrenByName(right[0], 'value', rUnit);
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

function TFHIRExpressionEngine.opMinus(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1,d2,d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing -: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing -: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: left operand has the wrong type (%s)', [left[0].fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing -: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing -: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing -: right operand has the wrong type (%s)', [right[0].fhirType()]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];


    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) - strToInt(r.primitiveValue()))))
    else if (l.hasType('decimal')) and (r.hasType('decimal')) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.Subtract(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing -: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opMod(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1, d2, d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing mod: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing mod: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing mod: left operand has the wrong type (%s)', [left[0].fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing mod: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing mod: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing mod: right operand has the wrong type (%s)', [right[0].fhirType()]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) mod strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.Modulo(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing mod: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));

    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opNotEquals(left, right: TFHIRBaseList): TFHIRBaseList;
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
      if not equals(left[i], right[i]) then
      begin
        res := true;
        break;
      end;
  end;
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(res));
end;

function TFHIRExpressionEngine.opNotEquivalent(left, right: TFHIRBaseList): TFHIRBaseList;
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
        if equivalent(left[i], right[j]) then
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
  result := TFHIRBaseList.Create;
  result.Add(TFhirBoolean.Create(not res));
end;

function TFHIRExpressionEngine.opOr(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if (left.isEmpty()) and (right.isEmpty()) then
    // nothing
  else if (convertToBoolean(left)) or (convertToBoolean(right)) then
    result.Add(TFhirBoolean.Create(true))
  else if (left.isEmpty()) or (right.isEmpty()) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(false));
end;

function TFHIRExpressionEngine.opConcatenate(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create(TFHIRString.create(convertToString(left) + convertToString(right)));
end;

function TFHIRExpressionEngine.opPlus(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1,d2,d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing +: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing +: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: left operand has the wrong type (%s)', [left[0].fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing +: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing +: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: right operand has the wrong type (%s)', [right[0].fhirType()]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];

    if (l.hasType(['string', 'id', 'code', 'uri'])) and (r.hasType(['string', 'id', 'code', 'uri'])) then
      result.add(TFHIRString.create(l.primitiveValue() + r.primitiveValue()))
    else if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) + strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.Add(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
        else
      raise EFHIRPath.create(StringFormat('Error performing +: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opTimes(left, right: TFHIRBaseList): TFHIRBaseList;
var
  l, r : TFHIRBase;
  d1, d2, d3 : TSmartDecimal;
begin
  if (left.count = 0) then
    raise EFHIRPath.create('Error performing *: left operand has no value');
  if (left.count > 1) then
    raise EFHIRPath.create('Error performing *: left operand has more than one value');
  if (not left[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing +: left operand has the wrong type (%s)', [left[0].fhirType()]));
  if (right.count = 0) then
    raise EFHIRPath.create('Error performing *: right operand has no value');
  if (right.count > 1) then
    raise EFHIRPath.create('Error performing *: right operand has more than one value');
  if (not right[0].isPrimitive()) then
    raise EFHIRPath.create(StringFormat('Error performing *: right operand has the wrong type (%s)', [right[0].fhirType()]));

  result := TFHIRBaseList.Create();
  try
    l := left[0];
    r := right[0];

    if (l.hasType('integer')) and (r.hasType('integer')) then
      result.add(TFHIRInteger.create(inttostr(strToInt(l.primitiveValue()) * strToInt(r.primitiveValue()))))
    else if (l.hasType(['integer', 'decimal'])) and (r.hasType(['integer', 'decimal'])) then
    begin
      d1 := TSmartDecimal.valueOf(l.primitiveValue());
      d2 := TSmartDecimal.valueOf(r.primitiveValue());
      d3 := d1.Multiply(d2);
      result.add(TFHIRDecimal.create(d3.asDecimal));
    end
    else
      raise EFHIRPath.create(StringFormat('Error performing /: left and right operand have incompatible or illegal types (%s, %s)', [left[0].fhirType(), right[0].fhirType()]));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpressionEngine.contains(list : TFHIRBaseList; item : TFHIRBase) : boolean;
var
  test : TFHIRBase;
begin
  result := false;
  for test in list do
    if equals(test, item) then
        exit(true);
end;

function TFHIRExpressionEngine.opUnion(left, right: TFHIRBaseList): TFHIRBaseList;
var
  item : TFHIRBase;
begin
  result := TFHIRBaseList.create;
  try
    for item in left do
      if not contains(result, item) then
        result.add(item.link);
    for item in right do
      if not contains(result, item) then
        result.add(item.link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRExpressionEngine.opXor(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if (left.isEmpty()) or (right.isEmpty()) then
    // nothing
  else
  result.Add(TFhirBoolean.Create(convertToBoolean(left) xor convertToBoolean(right)));
end;

function TFHIRExpressionEngine.opImplies(left, right: TFHIRBaseList): TFHIRBaseList;
begin
  result := TFHIRBaseList.Create;
  if (not convertToBoolean(left)) then
    result.Add(TFhirBoolean.Create(true))
  else if (right.count = 0) then
    // nothing
  else
    result.Add(TFhirBoolean.Create(convertToBoolean(right)));
end;

function TFHIRExpressionEngine.execute(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode; atEntry : boolean): TFHIRBaseList;
var
  work, work2 : TFHIRBaseList;
  item, base : TFHIRBase;
  outcome : TFHIRBaseList;
  next, last : TFHIRExpressionNode;
begin
  work := TFHIRBaseList.Create;
  try
    case exp.kind of
      enkName:
        if (exp.name = '$this') then
          work.add(context.context.Link)
        else
          for item in focus do
          begin
            outcome := execute(context, item, exp, atEntry);
            try
              for base in outcome do
                if (base <> nil) then
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
        item := readConstant(context, exp.constant);
        if (item <> nil) then
          work.Add(item);
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
            work2 := TFHIRBaseList.Create(TFHIRString.Create(next.name))
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

function TFHIRExpressionEngine.executeType(focus: String; exp: TFHIRExpressionNode; atEntry : boolean): TFHIRTypeDetails;
    begin
  if (atEntry and isUpper(exp.Name[1])) and (focus = exp.Name) then
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

function TFHIRExpressionEngine.evaluateFunction(context : TFHIRPathExecutionContext; focus: TFHIRBaseList; exp: TFHIRExpressionNode): TFHIRBaseList;
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
    pfDescendents : result := funcDescendents(context, focus, exp);
    pfMemberOf : result := funcMemberOf(context, focus, exp);
    pfTrace : result := funcTrace(context, focus, exp);
    pfToday : result := funcToday(context, focus, exp);
    pfNow : result := funcNow(context, focus, exp);
    pfResolve: result := funcResolve(context, focus, exp);
    pfExtension: result := funcExtension(context, focus, exp);
  else
    raise EFHIRPath.create('Unknown Function '+exp.name);
  end;
end;

procedure TFHIRExpressionEngine.checkParamTypes(funcId : TFHIRPathFunction; paramTypes : TAdvList<TFHIRTypeDetails>; typeSet : array of TFHIRTypeDetails);
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

function TFHIRExpressionEngine.childTypes(focus : TFHIRTypeDetails; mask : string) : TFHIRTypeDetails;
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

function TFHIRExpressionEngine.evaluateFunctionType(context: TFHIRPathExecutionTypeContext; focus: TFHIRTypeDetails; exp: TFHIRExpressionNode): TFHIRTypeDetails;
var
  expr : TFHIRExpressionNode;
  paramTypes : TAdvList<TFHIRTypeDetails>;
  nc : TFHIRPathExecutionTypeContext;
begin
  paramTypes := TAdvList<TFHIRTypeDetails>.create;
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
      pfDescendents :
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
        if (not focus.hasType(['uri, Reference'])) then
          raise EFHIRPath.create('The function "'+CODES_TFHIRPathFunctions[exp.FunctionId]+'()" can only be used on uri, Reference not '+focus.describe);
        result := TFHIRTypeDetails.create(csSINGLETON, ['DomainResource']);
        end;
      pfExtension :
        begin
        checkParamTypes(exp.FunctionId, paramTypes, [TFHIRTypeDetails.create(csSINGLETON, ['string'])]);
        result := TFHIRTypeDetails.create(csSINGLETON, ['Extension']);
        end;
    else
      raise EFHIRPath.create('not Implemented yet?');
    end;
  finally
    paramTypes.Free;
  end;
end;

class function TFHIRPathLexer.processConstant(s: String) : String;
var
  b : TStringBuilder;
  i : integer;
  ch : char;
  u : String;
begin
  b := TStringBuilder.Create;
  try
    i := 2;
    while i < length(s) do
    begin
      ch := s[i];
      if ch = '\' then
      begin
        inc(i);
        ch := s[i];
        case ch of
          't': b.Append(#9);
          'r': b.Append(#13);
          'n': b.Append(#10);
          '\': b.Append('\');
          '''': b.Append('''');
          '"': b.Append('"');
          'u':
            begin
            if i < length(s) - 5 then
            begin
              u := s.Substring(i, 4);
              b.Append(char(StrToInt('$'+u)));
              inc(i,4);
            end
        else
              raise Exception.create('Improper unicode escape in '+s);
            end
        else
          raise Exception.create('Unknown character escape \'+ch);
        end;
        inc(i);
      end
      else
      begin
        b.Append(ch);
        inc(i);
      end;
    end;
    result := b.toString;
  finally
    b.Free;
  end;
end;


function TFHIRExpressionEngine.parse(lexer: TFHIRPathLexer): TFHIRExpressionNode;
var
  msg : String;
begin
  if lexer.done then
    raise lexer.error('Path cannot be empty');
  result := parseExpression(lexer, true);
  try
    if not result.check(msg, 0) then
      raise EFHIRPath.create('Error parsing "'+lexer.FPath+'": '+msg);
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRExpressionEngine.parseExpression(lexer : TFHIRPathLexer; proximal : boolean): TFHIRExpressionNode;
var
  c : Integer;
  focus, item : TFHIRExpressionNode;
begin
  result := TFHIRExpressionNode.Create(lexer.nextId);
  try
    result.SourceLocationStart := lexer.FCurrentStartLocation;
    c := lexer.CurrentStart;
    // special:
    if (lexer.Current = '-') then
    begin
      lexer.take;
      lexer.Fcurrent := '-' + lexer.Fcurrent;
    end;

    if (lexer.Current = '+') then
    begin
      lexer.take;
      lexer.Fcurrent := '+' + lexer.Fcurrent;
    end;

    if lexer.isConstant then
    begin
      if lexer.current.startsWith('''') then
        lexer.processConstant(lexer.current);
      result.Constant := lexer.take;
      result.kind := enkConstant;
      result.SourceLocationEnd := lexer.FCurrentLocation;
    end
    else if lexer.current = '(' then
    begin
      lexer.next;
      result.kind := enkGroup;
      result.group := parseExpression(lexer, true);
      if lexer.current <> ')' then
        raise lexer.error('Found '+lexer.current+' expecting a ")"');
      result.SourceLocationEnd := lexer.FCurrentLocation;
      lexer.next;
    end
    else
    begin
      if not lexer.isToken and not lexer.current.startsWith('"') then
        raise lexer.error('Found '+lexer.current+' expecting a token name');
      if (lexer.current.startsWith('"')) then
        result.Name := lexer.readConstant('Path Name')
      else
      result.Name := lexer.take;
      result.SourceLocationEnd := lexer.FCurrentLocation;
      if not result.checkName then
        raise lexer.error('Found '+lexer.current+' expecting a valid token name');
      if (lexer.current = '(') then
      begin
        if not StringArrayExistsSensitive(CODES_TFHIRPathFunctions, result.Name) then
          raise lexer.error('The name '+result.Name+' is not a valid function name');
        result.kind := enkFunction;
        result.FunctionId := TFHIRPathFunction(StringArrayIndexOfSensitive(CODES_TFHIRPathFunctions, result.Name));
        lexer.next;
        while lexer.current <> ')' do
        begin
          result.Parameters.add(parseExpression(lexer, true));
          if lexer.current = ',' then
            lexer.next
          else if lexer.current <> ')' then
            raise lexer.error('The token '+lexer.current+' is not expected here - either a "," or a ")" expected');
        end;
        result.SourceLocationEnd := lexer.FCurrentLocation;
        lexer.next;
        checkParameters(lexer, result.SourceLocationStart, c, result);
      end;
      end;
    focus := result;
    if (lexer.current = '[') then
    begin
      lexer.next();
      item := TFHIRExpressionNode.Create(lexer.nextId);
      item.Kind := enkFunction;
      item.Functionid := pfItem;
      item.Parameters.add(parseExpression(lexer, true));
      if (lexer.current <> ']') then
        raise lexer.error('The token '+lexer.Current+' is not expected here - a "]" expected');
      lexer.next;
      result.inner := item;
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
      while lexer.isOp do
      begin
        focus.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current));
        focus.OpSourceLocationStart := lexer.FCurrentStartLocation;
        focus.OpSourceLocationEnd := lexer.FCurrentLocation;
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

function TFHIRExpressionEngine.newGroup(lexer : TFHIRPathLexer; next : TFHIRExpressionNode) : TFHIRExpressionNode;
begin
  result := TFHIRExpressionNode.Create(lexer.nextId);
  try
    result.kind := enkGroup;
    result.Group := next.Link;
    result.Group.Proximal := true;
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRExpressionEngine.gatherPrecedence(lexer : TFHIRPathLexer; var start : TFHIRExpressionNode; ops : TFHIRPathOperationSet);
var
  work : boolean;
  focus, node, group : TFHIRExpressionNode;
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

procedure TFHIRExpressionEngine.organisePrecedence(lexer : TFHIRPathLexer; var node : TFHIRExpressionNode);
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
    result := TFHIRDateTime.create(TDateAndTime.CreateXML(s))
  else
    result := TFHIRDate.create(TDateAndTime.CreateXML(s));

end;
function TFHIRExpressionEngine.readConstant(context : TFHIRPathExecutionContext; constant: String): TFHIRBase;
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

function TFHIRExpressionEngine.replaceFixedConstant(context : TFHIRPathExecutionContext; const s: String): TFHIRBase;
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


function TFHIRExpressionEngine.UseLog: String;
begin
  if (FLog <> nil) and (FLog.Length > 0) then
  begin
    result := ' ('+FLog.ToString+')';
    FLog.Clear;
  end
  else
    result := '';
end;

function TFHIRExpressionEngine.readConstantType(ctxt: TFHIRPathExecutionTypeContext; constant: String): string;
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

function TFHIRExpressionEngine.parse(path: String): TFHIRExpressionNode;
var
  lexer : TFHIRPathLexer;
  msg : String;
begin
  lexer := TFHIRPathLexer.Create(path);
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

procedure TFHIRExpressionEngine.ListChildrenByName(focus: TFHIRBase; name: String; results: TFHIRBaseList);
var
  list : TFHIRBaseList;
  item : TFHIRBase;
begin
  list := TFHIRBaseList.Create();
  try
    focus.ListChildrenByName(name, list);
    for item in list do
      if (item <> nil) then
        results.Add(item.Link);
  finally
    list.Free;
  end;
end;

procedure TFHIRExpressionEngine.ListChildTypesByName(item, name : String; result : TFHIRTypeDetails);
var
  url, tail, specifiedType, path, tn, r, rn : String;
  sd, dt, sdi : TFhirStructureDefinition;
  sdl : TAdvList<TFhirStructureDefinition>;
  ed : TFhirElementDefinition;
  t : TFhirElementDefinitionType;
  rt : TAdvStringSet;
begin
  if (item = '') then
    raise EFHIRPath.create('No type provided in BuildToolPathEvaluator.ListChildTypesByName');
  if (item.equals('xhtml')) then
    exit;
  if (item.contains('.')) then
    url := 'http://hl7.org/fhir/StructureDefinition/'+item.substring(0, item.indexOf('.'))
  else
    url := 'http://hl7.org/fhir/StructureDefinition/'+item;
  sd := worker.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
  if (sd = nil) then
    raise EFHIRPath.create('Unknown item '+item); // this really is an error, because we can only get to here if the internal infrastrucgture is wrong
  ed := nil;
  sdl := TAdvList<TFhirStructureDefinition>.create;
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

procedure TFHIRExpressionEngine.log(name, value: String);
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

function TFHIRExpressionEngine.isAbstractType(list : TFHIRElementDefinitionTypeList) : boolean;
var
  s : string;
begin
  if list.count <> 1 then
    exit(false);
  s := list[0].code;
  result := (s = 'Element') or (s = 'BackboneElement') or (s = 'Resource') or (s = 'DomainResource');
end;

function TFHIRExpressionEngine.getElementDefinition(sd : TFHIRStructureDefinition; path : String; allowPM : boolean; var specifiedType : String) : TFHIRElementDefinition;
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
        raise Exception.Create('Internal typing issue....');
      sd := worker.getStructure('http://hl7.org/fhir/StructureDefinition/'+ed.type_List[0].code);
      try
      if (sd = nil) then
          raise Exception.Create('Unknown type '+ed.type_List[0].code);
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

function TFHIRExpressionEngine.hasDataType(ed : TFhirElementDefinition) : boolean;
begin
  result := (ed.type_List.Count > 0) and not ((ed.type_list[0].code = 'Element') or (ed.type_list[0].code = 'BackboneElement'));
end;

function TFHIRExpressionEngine.getElementDefinitionByName(sd : TFHIRStructureDefinition; name : String) : TFHIRElementDefinition;
var
  ed : TFhirElementDefinition;
begin
  for ed in sd.snapshot.elementList do
    {$IFDEF FHIR3}
    if (name.equals('#'+ed.id)) then
    {$ELSE}
    if (name.equals(ed.Name)) then
    {$ENDIF}
      exit(ed);
  result := nil;
end;

{ EFHIRPath }

constructor EFHIRPath.create(path: String; offset: integer; problem: String);
begin
  inherited create('FHIRPath error in "'+path+'" at position '+inttostr(offset)+': '+problem);
end;

constructor EFHIRPath.create(problem: String);
begin
  inherited create(problem);
end;

{ TFHIRPathLexer }

constructor TFHIRPathLexer.Create(path: String);
begin
  inherited Create;
  FPath := path;
  FCursor := 1;
  FCurrentLocation.line := 1;
  FCurrentLocation.col := 1;
  next;
end;

destructor TFHIRPathLexer.Destroy;
begin

  inherited;
end;

function isWhitespace(ch : char) : Boolean;
begin
  result := CharInSet(ch, [#9, #10, #13, ' ']);
end;

function isDateChar(ch : char) : Boolean;
begin
 result := CharInSet(ch, ['-', ':', 'T', '+', 'Z', '0'..'9']);
end;

procedure TFHIRPathLexer.next;
  procedure Grab(length : Integer);
  begin
    FCurrent := copy(FPath, FCurrentStart, length);
    inc(FCursor, length);
  end;
var
  ch : char;
  escape, dotted : boolean;
  flast13 : boolean;
begin
  FCurrent := '';
  flast13 := false;
  while (FCursor <= FPath.Length) and isWhitespace(FPath[FCursor]) do
  begin
    if FPath[FCursor] = #13 then
    begin
      inc(FCurrentLocation.line);
      FCurrentLocation.col := 1;
      flast13 := true;
    end
    else if not flast13 and (FPath[FCursor] = #10) then
    begin
      inc(FCurrentLocation.line);
      FCurrentLocation.col := 1;
      flast13 := false;
    end
    else
    begin
      flast13 := false;
      inc(FCurrentLocation.col);
    end;
    inc(FCursor);
  end;
  FCurrentStart := FCursor;
  FCurrentStartLocation := FCurrentLocation;
  if (FCursor <= FPath.Length) then
  begin
    ch := FPath[FCursor];
    if charInSet(ch, ['!', '>', '<', ':', '=', '-']) then
    begin
      if (FCursor < FPath.Length) and charInSet(FPath[FCursor+1], ['=', '~', '-']) then
        Grab(2)
      else
        Grab(1);
    end
    else if CharInSet(ch, ['0'..'9']) then
    begin
      inc(FCursor);
      dotted := false;
      while (FCursor <= FPath.Length) and (CharInSet(FPath[FCursor], ['0'..'9']) or (not dotted and (FPath[FCursor] = '.'))) do
      begin
        if (FPath[FCursor] = '.') then
          dotted := true;
        inc(FCursor);
      end;
      if (FPath[FCursor-1] = '.') then
        dec(Fcursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if CharInSet(ch, ['A'..'Z', 'a'..'z']) then
    begin
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '%') then
    begin
      inc(FCursor);
      if FPath[FCursor] = '"' then
      begin
        inc(FCursor);
        while (FCursor <= FPath.Length) and (FPath[FCursor] <> '"') do
          inc(FCursor);
        inc(FCursor);
      end
      else
        while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', ':', '-']) do
          inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '/') then
    begin
      inc(FCursor);
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['/']) do
      begin
        inc(FCursor);
        while (FCursor <= FPath.Length) and not CharInSet(FPath[FCursor], [#13, #10]) do
          inc(FCursor);
      end;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '$') then
    begin
      inc(FCursor);
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['a'..'z']) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '{') then
    begin
      inc(FCursor);
      if (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['}']) then
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '"') then
    begin
      inc(FCursor);
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> '"')) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        if CharInSet(FPath[FCursor], [#13, #10, #9]) then
          raise EFHIRPath.create('illegal character in string');
        inc(FCursor);
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      inc(FCursor);
      FCurrent := '"'+copy(FPath, FCurrentStart+1, FCursor-FCurrentStart-2)+'"';
    end
    else if (ch = '''') then
    begin
      inc(FCursor);
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> ch)) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        if CharInSet(FPath[FCursor], [#13, #10, #9]) then
          raise EFHIRPath.create('illegal character in string');
        inc(FCursor);
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
      FCurrent := ''''+copy(FCurrent, 2, FCurrent.Length - 2)+'''';
    end
    else if (ch = '@') then
    begin
      inc(FCursor);
      while (FCursor <= FPath.length) and isDateChar(FPath[FCursor]) do
        inc(FCursor);
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else // if CharInSet(ch, ['.', ',', '(', ')', '=']) then
      Grab(1);
  end;
  inc(FCurrentLocation.col, FCursor - FCurrentStart);
end;


function TFHIRPathLexer.nextId: integer;
begin
  inc(FId);
  result := FId;
end;

function TFHIRPathLexer.hasComment : boolean;
begin
  result := not done() and FCurrent.startsWith('//');
end;

procedure TFHIRPathLexer.skipComments;
begin
  while (not done and hasComment()) do
    next();
end;

procedure TFHIRPathLexer.skiptoken(kw: String);
begin
  if (kw = current) then
    next();
end;

function TFHIRPathLexer.done: boolean;
begin
  result := FCurrentStart > FPath.Length;
end;

function TFHIRPathLexer.error(msg: String; location: TSourceLocation): Exception;
begin
  result := Exception.Create('Error in '+FPath+' at line '+inttostr(location.line)+' col '+inttostr(location.col)+': '+msg);
end;

function TFHIRPathLexer.error(msg: String; offset: integer): Exception;
begin
  result := Exception.Create('Error in '+FPath+' at '+inttostr(offset)+': '+msg);
end;

function TFHIRPathLexer.error(msg: String): Exception;
begin
  result := error(msg, FCurrentStart);
end;

function TFHIRPathLexer.isConstant(incDoubleQuotes : boolean): boolean;
begin
  result := (FCurrent <> '') and (CharInSet(FCurrent[1], ['''', '0'..'9', '@', '%', '-', '+']) or
    (incDoubleQuotes and (FCurrent[1] = '"')) or (FCurrent = 'true') or (FCurrent = 'false') or (FCurrent = '{}'));
end;

function TFHIRPathLexer.isOp: boolean;
begin
  result := (current <> '') and StringArrayExistsSensitive(CODES_TFHIRPathOperation, current);
end;

function TFHIRPathLexer.hasToken(kw: String): boolean;
begin
  result := not done() and (kw = current);
end;

function TFHIRPathLexer.isToken: boolean;
var
  i : integer;
begin
  if current = '' then
    result := false
  else if current.StartsWith('$') then
    result := true
  else if StringArrayExistsSensitive(['*', '**'], current) then
    result := true
  else if CharInSet(current[1], ['A'..'Z', 'a'..'z']) then
  begin
    result := true;
    for i := 1 to length(current) do
      result := result and (CharInSet(current[i], ['A'..'Z', 'a'..'z', '0'..'9', '[', ']']) or ((i = current.Length) and (current[i] = '*')));
  end
  else
    result := false;
end;

function TFHIRPathLexer.take: String;
begin
  result := current;
  next;
end;

procedure TFHIRPathLexer.token(kw: String);
begin
  if (kw <> current) then
    raise error('Found "'+current+'" expecting "'+kw+'"');
  next();
end;

function TFHIRPathLexer.readConstant(desc : String): String;
begin
  if (not isStringConstant()) then
    raise error('Found '+current+' expecting "['+desc+']"');

  result := processConstant(take);
end;

function TFHIRPathLexer.isStringConstant : boolean;
begin
  result := (current[1] = '''') or (current[1] = '"');
end;


function TFHIRPathLexer.takeDottedToken() : String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.create;
  try
    b.append(take());
    while not done() and (FCurrent = '.') do
    begin
      b.append(take());
      b.append(take());
    end;
    result := b.toString();
  finally
    b.free;
  end;
end;



{ TFHIRPathExecutionTypeContext }

constructor TFHIRPathExecutionTypeContext.Create(appInfo: TAdvObject; resourceType : String; context : TFHIRTypeDetails);
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

{ TFHIRPathExecutionContext }

constructor TFHIRPathExecutionContext.Create(appInfo: TAdvObject; resource: TFHIRBase; context: TFHIRBase);
begin
  inherited Create;
  FAppInfo := appInfo;
  FResource := resource;
  FContext := context;
end;

destructor TFHIRPathExecutionContext.Destroy;
begin
  FAppInfo.Free;
  FResource.Free;
  FContext.Free;
  inherited;
end;

function TFHIRPathExecutionContext.Link: TFHIRPathExecutionContext;
begin
  result := TFHIRPathExecutionContext(inherited Link);
end;

{ TFHIRPathDebugPackage }

destructor TFHIRPathDebugPackage.destroy;
begin
  Fcontext.Free;
  Finput2.Free;
  Finput1.Free;
  FExpression.Free;
  Foutcome.Free;
  inherited;
end;

function TFHIRPathDebugPackage.Link: TFHIRPathDebugPackage;
begin
  result := TFHIRPathDebugPackage(inherited Link);
end;

procedure TFHIRPathDebugPackage.Setcontext(const Value: TFHIRPathExecutionContext);
begin
  Fcontext.Free;
  Fcontext := Value;
end;

procedure TFHIRPathDebugPackage.SetExpression(const Value: TFHIRExpressionNode);
begin
  FExpression.Free;
  FExpression := Value;
end;

procedure TFHIRPathDebugPackage.Setinput1(const Value: TFHIRBaseList);
begin
  Finput1.Free;
  Finput1 := Value;
end;

procedure TFHIRPathDebugPackage.Setinput2(const Value: TFHIRBaseList);
begin
  Finput2.Free;
  Finput2 := Value;
end;

procedure TFHIRPathDebugPackage.Setoutcome(const Value: TFHIRBaseList);
begin
  Foutcome.Free;
  Foutcome := Value;
end;

end.

