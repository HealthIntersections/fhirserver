unit fsl_xml;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Uses
  SysUtils, Classes, Generics.Collections, Character, {$IFDEF DELPHI}RegularExpressions, {$ENDIF}
  MarkdownHTMLEntities,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_fpc;

const
  DEF_BUF_SIZE = 128;

type
  TMXPathExpressionOperation = (xeoNull, xeoEquals, xeoPlus, xeoAnd, xeoOr, xeoGreaterThan, xeoGreaterEquals, xeoNotEquals, xeoUnion, xeoLessThan, xeoLessEquals, xeoSequence, xeoMinus);
  TMXPathExpressionOperationSet = set of TMXPathExpressionOperation;
  TMXPathExpressionNodeType = (xentName, xentFunction, xentConstant, xentGroup, xentRoot, xentIterator, xentVariable);
  TMXPathExpressionAxis = (axisSelf, axisChild, axisDescendants, axisDescendantsAndSelf, axisAttribute, axisNamespace, axisParent, axisAncestor, axisAncestorOrSelf, axisFollowing, axisFollowingSibling, axisPreceding, axisPrecedingSibling);

const
  AXIS_NAMES : array[TMXPathExpressionAxis] of String = ('self', 'child', 'descendant', 'descendant-or-self',
    'attribute', 'namespace', 'parent', 'ancestor', 'ancestor-or-self', 'following', 'following-sibling', 'preceding',
    'preceding-sibling');
  Operation_CODES : array [TMXPathExpressionOperation] of String = ('', '=', '+', 'and', 'or', '>', '>=', '!=', '|', '<', '<=', ',', '-');

var
  NO_SELECT : boolean = false; // use this to turn actual selection on and off (mainly for debugging memory leaks, since the tests require selection in order to execute)

Type
  TMXmlElement = class;

  TMXmlNode = class (TFslObject)
  private
    FParent : TMXmlElement; // no own
  public
    function Link : TMXmlNode; overload;
    property Parent : TMXmlElement read FParent write FParent;
    function equal(other : TMXmlNode) : boolean; virtual;
  end;

  TMXmlPrimitive = class (TMXmlNode)
  public
    function equal(other : TMXmlNode) : boolean; override;
  end;

  TMXmlBoolean = class (TMXmlPrimitive)
  private
    FValue: boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(value : boolean);
    property value : boolean read FValue write FValue;
    function ToString : String; override;
  end;

  TMXmlNumber = class (TMXmlPrimitive)
  private
    FValue: integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(value : integer);
    property value : integer read FValue write FValue;
    function ToString : String; override;
  end;

  TMXmlString = class (TMXmlPrimitive)
  private
    FValue: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(value : String);
    property value : String read FValue write FValue;
    function ToString : String; override;
  end;

  TMXmlElementType = (ntElement, ntText, ntComment, ntDocument, ntAttribute, ntProcessingInstruction, ntDocumentDeclaration, ntCData);

  { TMXmlNamedNode }

  TMXmlNamedNode = class (TMXmlNode)
  private
    FNodeType : TMXmlElementType;
    FName : String;
    FNamespaceURI : String;
    FLocalName : String;
    FStart : TSourceLocation;
    FStop : TSourceLocation;
    function containsLocation(loc : TSourceLocation) : boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(nodeType : TMXmlElementType); overload;
    Property Name : String read FName write FName;
    property NodeType : TMXmlElementType read FNodeType write FNodeType;
    property NamespaceURI : String read FNamespaceURI write FNamespaceURI;
    property LocalName : String read FLocalName write FLocalName;
    property Start : TSourceLocation read FStart write FStart;
    property Stop : TSourceLocation read FStop write FStop;
    function equal(other : TMXmlNode) : boolean; override;
  end;

  TMXmlAttribute = class (TMXmlNamedNode)
  private
    FValue : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(); override;
    constructor Create(name, value : String); overload;

    Function Link : TMXmlAttribute; overload;
    property Value : String read FValue write FValue;
    function equal(other : TMXmlNode) : boolean; override;
    function ToString : String; override;
  end;


  TMXmlElement = class (TMXmlNamedNode)
  private
    FAttributes : TFslList<TMXmlAttribute>;
    FChildren : TFslList<TMXmlElement>;
    FText : string;
    FNext : TMXmlElement;
    function GetAttributes: TFslList<TMXmlAttribute>;
    function GetChildren: TFslList<TMXmlElement>;
    function GetHasAttributes: boolean;
    function GetHasChildren: boolean;
    function GetHasText: boolean;
    procedure SetNext(const Value: TMXmlElement);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetAttribute(name: String): String;
    procedure SetAttribute(name: String; const Value: String);
    function getAbbreviation(ns : String) : String;
    function getAbbreviationPriv(ns : String; var abbrev : String): boolean;
    procedure writeToXml(b : TStringBuilder; pretty : boolean; indent : integer);
    function GetAttributeNS(ns, name: String): String;
    procedure SetAttributeNS(ns, name: String; const Value: String);
    function GetAllText: String;
    procedure fixChildren;
    function GetHasAttribute(name: String): boolean;
    function GetAllChildrenAreText: boolean;
    function findLocation(loc : TSourceLocation; path : TFslList<TMXmlNamedNode>) : boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(nodeType : TMXmlElementType; name : String); overload; virtual;
    constructor CreateNS(nodeType : TMXmlElementType; ns, local : String); overload; virtual;
    constructor CreateNSN(nodeType : TMXmlElementType; name, ns, local : String); overload;
    destructor Destroy; override;
    Function Link : TMXmlElement; overload;

    property Attributes : TFslList<TMXmlAttribute> read GetAttributes;
    property HasAttributes  : boolean read GetHasAttributes;
    property HasAttribute[name : String]  : boolean read GetHasAttribute;
    property Children : TFslList<TMXmlElement> read GetChildren;
    property HasChildren : boolean read GetHasChildren;
    property AllChildrenAreText : boolean read GetAllChildrenAreText;
    property Text : string read GetText write SetText;
    property HasText : boolean read GetHasText;
    property Next : TMXmlElement read FNext write SetNext;

    class Function createText(text : String) : TMXmlElement;
    class Function createComment(text : String) : TMXmlElement;
    class function createProcessingInstruction(encoding : String) : TMXmlElement;

    function first : TMXmlElement;
    function firstElement : TMXmlElement;
    function nextElement : TMXmlElement;
    function previous : TMXmlElement;
    function last : TMXmlElement;
    property document : TMXmlElement read firstElement;
    property allText : String read GetAllText;
    property attribute[name : String] : String read GetAttribute write SetAttribute;
    property attributeNS[ns, name : String] : String read GetAttributeNS write SetAttributeNS;
    function getAttrByName(name: String; var attr : TMXmlAttribute): boolean;
    function RemoveAttribute(name : String) : boolean;
    function element(name : String) : TMXmlElement;
    function elementNS(ns, name : String) : TMXmlElement;
    procedure listElements(name : String; list : TFslList<TMXmlElement>);
    procedure addChild(node : TMXmlElement; fix : boolean);
    function addElement(name : String) : TMXmlElement;
    function addElementNS(ns, name : String) : TMXmlElement;
    function addText(content : String) : TMXmlElement;
    function ToXml(pretty : boolean = false) : String; overload;
    function equal(other : TMXmlNode) : boolean; override;
    function ToString : String; override;
  end;

  TMXPathExpressionNode = class (TFslObject)
  private
    FNodeType : TMXPathExpressionNodeType;
    FAxis : TMXPathExpressionAxis;
    FValue : String;
    FFilters: TFslList<TMXPathExpressionNode>;
    FNext: TMXPathExpressionNode;
    FOp : TMXPathExpressionOperation;
    FNextOp : TMXPathExpressionNode;
    FGroup : TMXPathExpressionNode;
    FParams : TFslList<TMXPathExpressionNode>;
    procedure SetNext(const Value: TMXPathExpressionNode);
    procedure SetNextOp(const Value: TMXPathExpressionNode);
    procedure SetGroup(const Value: TMXPathExpressionNode);
    function GetParams: TFslList<TMXPathExpressionNode>;
    function GetFilters: TFslList<TMXPathExpressionNode>;
    function buildConstant : TMXmlNode;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    Function Link : TMXPathExpressionNode; overload;
    function addParam : TMXPathExpressionNode;

    property NodeType : TMXPathExpressionNodeType read FNodeType write FNodeType;
    property axis : TMXPathExpressionAxis read FAxis write FAxis;
    property filters : TFslList<TMXPathExpressionNode> read GetFilters;
    function hasFilters : Boolean;
    property next : TMXPathExpressionNode read FNext write SetNext;
    property op : TMXPathExpressionOperation read FOp write FOp;
    property NextOp : TMXPathExpressionNode read FNextOp write SetNextOp;
    property Group : TMXPathExpressionNode read FGroup write SetGroup;
    property Params : TFslList<TMXPathExpressionNode> read GetParams;
    property value : String read FValue write FValue;

    Function ToString : String; override;
  end;

  TXPathVariables = class (TFslObject)
  private
    FMap : TFslMap<TMXmlNode>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(name : String; value : TMXmlNode); overload;
    destructor Destroy; override;
    function link: TXPathVariables;
    function add(name : String; value : TMXmlNode) : TXPathVariables;
    function get(name : string) : TMXmlNode;
  end;

  {private} TMXMLIterationEvent = function (Context : TObject; node : TMXmlNamedNode) : boolean of Object;

  TMXmlDocument = class(TMXmlElement)
  private
    FNamespaceAbbreviations : TFslStringDictionary;
    function list(value : boolean) : TFslList<TMXmlNode>;
    function contains(list : TFslList<TMXmlNode>; item : TMXmlNode) : boolean;

    function funcExists(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcCount(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcNot(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcContains(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcTranslate(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcTextTest(Context : TObject; node : TMXmlNamedNode) : boolean;
    procedure funcText(expr: TMXPathExpressionNode; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcComment(focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcDistinctValues(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcPosition(position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcLocalName(focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcName(focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcStartsWith(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcMatches(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    procedure funcSubStringAfter(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
    function funcNormalizeSpace(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode) : TMXmlNode;
    function funcConcat(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus : TMXmlNode) : TMXmlNode;
    function funcString(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus : TMXmlNode) : TMXmlNode;
    function funcNumber(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus : TMXmlNode) : TMXmlNode;

    function opEqual(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opNotEqual(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opAnd(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opPlus(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opMinus(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opOr(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opUnion(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opLessThan(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opLessThanEqual(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opGreaterThan(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    function opGreaterThanEqual(left, right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;

    function passesFilter(index : integer; atEntry : boolean; variables : TXPathVariables; position : integer; item : TMXmlNode; filter : TMXPathExpressionNode) : boolean;
    function evaluateNameForNode(Context : TObject; node : TMXmlNamedNode) : boolean;
    procedure evaluateName(expr: TMXPathExpressionNode; atEntry : boolean; item: TMXmlNode; focus: TFslList<TMXmlNode>);
    procedure iterate(node: TMXMLNode; axis: TMXPathExpressionAxis; list : TFslList<TMXmlNode>; context: TObject; event: TMXMLIterationEvent);
    procedure evaluateFunction(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; item : TMXmlNode; work : TFslList<TMXmlNode>); overload;
    function preOperate(left : TFslList<TMXmlNode>; op : TMXPathExpressionOperation) : TFslList<TMXmlNode>;
    function operate(left : TFslList<TMXmlNode>; op : TMXPathExpressionOperation; right : TFslList<TMXmlNode>) : TFslList<TMXmlNode>;
    procedure evaluateIterator(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus, work : TFslList<TMXmlNode>);
    function evaluate(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus : TFslList<TMXmlNode>) : TFslList<TMXmlNode>; overload;
    function evaluate(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; item : TMXmlNode) : TFslList<TMXmlNode>; overload;
    function evaluateString(nodes : TFslList<TMXmlNode>): String;
    function GetDocElement: TMXmlElement;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    constructor Create(nodeType : TMXmlElementType; name : String); overload; override;
    constructor CreateNS(nodeType : TMXmlElementType; ns, local : String); overload; override;
    destructor Destroy; override;

    property docElement : TMXmlElement read GetDocElement;
    function ToXml(pretty : boolean = false; xmlHeader : boolean = false) : String; overload;
    procedure ToXml(stream : TStream; pretty : boolean = false; xmlHeader : boolean = false); overload;
    procedure ToXml(stream : TFslStream; pretty : boolean = false; xmlHeader : boolean = false); overload;
    function select(xpath : String; focus : TMXmlElement) : TFslList<TMXmlNode>; overload;
    function selectElements(xpath : String; focus : TMXmlElement) : TFslList<TMXmlElement>; overload;
    function select(xpath : TMXPathExpressionNode; focus : TMXmlElement) : TFslList<TMXmlNode>; overload;
    function selectElements(xpath : TMXPathExpressionNode; focus : TMXmlElement) : TFslList<TMXmlElement>; overload;
    function evaluateBoolean(nodes : TFslList<TMXmlNode>): boolean;
    property NamespaceAbbreviations : TFslStringDictionary read FNamespaceAbbreviations;

    function findLocation(loc : TSourceLocation) : TFslList<TMXmlNamedNode>;
    function describePath(path : TFslList<TMXmlNamedNode>) : string;
  end;

  TMXmlParserOption = (xpResolveNamespaces, xpDropWhitespace, xpDropComments, xpHTMLEntities);
  TMXmlParserOptions = set of TMXmlParserOption;

  TMXmlParser = class (TFslObject)
  private
    reader : TFslTextReader;
    options : TMXmlParserOptions;
    b : TStringBuilder;
    FHtmlEntities : TDictionary<String, String>;

    FLocation, FStartLocation : TSourceLocation;

    procedure rule(test : boolean; message : String);

    Function xmlToText(s: String): String;
    function peek : char;
    function read : char;
    Function ReadToken(skipWhitespace : Boolean; allowEmpty : boolean = false): String;
    Function ReadXPathToken(skipWhitespace : Boolean; allowEmpty : boolean = false): String;
    Function ReadToNextChar(ch : Char): String;
    function ReadAttribute(name : String) : TMXmlAttribute;
    procedure ReadElement(parent : TMXmlElement);
    procedure ReadText(parent : TMXmlElement; text : String);
    procedure ReadComment(parent : TMXmlElement);
    procedure ReadInstruction(parent : TMXmlElement);
    function parse : TMXmlDocument; overload;
    function resolveNamespace(element : TMXmlElement; abbrev : String) : String;
    procedure resolveNamespaces(element : TMXmlElement; defNs : String);
    procedure moveOperationsToProximal(node : TMXPathExpressionNode);
    procedure organisePrecedence(var node: TMXPathExpressionNode; operations : boolean);
    procedure gatherPrecedence(var start: TMXPathExpressionNode; ops: TMXPathExpressionOperationSet);
    function readXpathExpression(node : TMXPathExpressionNode; endTokens : Array of String; alreadyRead : String = '') : string;
    function parseXPath : TMXPathExpressionNode; overload;
    function newGroup(next: TMXPathExpressionNode): TMXPathExpressionNode;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function parse(content : String; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parse(content : TStream; options : TMXmlParserOptions) :  TMXmlDocument; overload;
    class function parse(content : TFslStream; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parse(content : TBytes; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parseFile(name : String; options : TMXmlParserOptions) : TMXmlDocument; overload;

    class function isXmlNameChar(const ch: Char): Boolean;
    class function isXmlWhiteSpace(const ch: Char): Boolean;
    class function isXmlName(name : String) : boolean;
    class function isXPathName(name : String) : boolean;

    class function parseXPath(content : String) : TMXPathExpressionNode; overload;
  end;


const
  CODES_TMXmlElementType : Array [TMXmlElementType] of String = ('Element', 'Text', 'Comment', 'Document', 'Attribute', 'ProcessingInstruction', 'DocumentDeclaration', 'CData');

type
  TXmlCanonicalisationMethod = (xcmCanonicalise, xcmComments, xcmTrimWhitespace, {xcmPrefixRewrite, } xcmQNameAware);
  TXmlCanonicalisationMethodSet = set of TXmlCanonicalisationMethod;

  TXmlBuilderNamespaceList = class (TFslStringMatch)
  private
    FDefaultNS : String;
    FNew: TStringList;
    FDefaultSet: boolean;
    procedure SetDefaultNS(const Value: String);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TXmlBuilderNamespaceList; overload;
    function clone : TXmlBuilderNamespaceList; overload;

    Procedure Assign(oObject : TFslObject); Override;

    Property DefaultNS : String read FDefaultNS write SetDefaultNS;
    Property DefaultSet : boolean read FDefaultSet write FDefaultSet;
    Property new : TStringList read FNew;
  end;

  TXmlBuilder = class abstract (TFslObject)
  private
    function GetCurrentNamespaces: TXmlBuilderNamespaceList;
    function getNSAbbrev(iElement: TMXMLElement): String;
  Protected
    FNoHeader: Boolean;
    FCanonicalise: TXmlCanonicalisationMethodSet;
    FIsPretty: Boolean;
    FCharEncoding: String;
    FNamespaces : TFslObjectList;
  Public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start; overload; virtual; abstract;
    procedure StartFragment; overload; virtual; abstract;
    procedure Finish; overload; virtual; abstract;
    procedure Build(oStream: TStream);   overload; virtual; abstract;
    procedure Build(oStream: TFslStream);   overload; virtual; abstract;
    function Build : String;   overload; virtual; abstract;

    function SourceLocation : TSourceLocation;  overload; virtual; abstract;
    procedure Comment(Const sContent : String); overload; virtual; abstract;
    procedure AddAttribute(Const sName, sValue : String); overload; virtual; abstract;
    procedure AddAttributeNS(Const sNamespace, sName, sValue : String); overload; virtual; abstract;
    function Tag(Const sName : String) : TSourceLocation; overload; virtual; abstract;
    function Open(Const sName : String) : TSourceLocation; overload; virtual; abstract;
    procedure Close(Const sName : String); overload; virtual; abstract;
    function Text(Const sValue : String) : TSourceLocation; overload; virtual; abstract;
    function Entity(Const sValue : String) : TSourceLocation; overload; virtual; abstract;
    function TagText(Const sName, sValue : String) : TSourceLocation; overload; virtual; abstract;
    procedure ProcessingInstruction(sName, sText : String); overload; virtual; abstract;
    procedure DocType(sText : String); overload; virtual; abstract;

    Procedure WriteXml(iElement : TMXMLElement);

    procedure inject(Const aBytes : TBytes); Overload; virtual; abstract; // not supported on all implementations

    property IsPretty : boolean read FIsPretty Write FIsPretty;
    property CharEncoding : String read FCharEncoding write FCharEncoding;
    property CurrentNamespaces : TXmlBuilderNamespaceList Read GetCurrentNamespaces;
    property NoHeader : Boolean read FNoHeader write FNoHeader;
    property Canonicalise : TXmlCanonicalisationMethodSet read FCanonicalise write FCanonicalise;


    // consumer has to call this manually if it wants to change namespaes, before it starts playing with attributes or namesapces.
    // it must call pop if it calls push
    procedure NSPush;
    Procedure NSPop;
  End;

Type
  EFslXMLObject = Class(EFslException);

  TFslXMLAttribute = Class(TFslObject)
    Private
      FNamespace : String;
      FName : String;
      FValue : String;
      FSortKey : String;

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslXMLAttribute;
      Function Clone : TFslXMLAttribute;

      Procedure Assign(oObject : TFslObject); Override;

      Property Namespace : String Read FNamespace Write FNamespace;
      Property Name : String Read FName Write FName;
      Property Value : String Read FValue Write FValue;
      Property SortKey : String read FSortKey write FSortKey;
  End;

  TFslXMLAttributeList = Class(TFslObjectList)
    Private
      Function GetElementByIndex(Const iIndex : Integer) : TFslXMLAttribute;
      Procedure SetElementByIndex(Const iIndex : Integer; Const oValue : TFslXMLAttribute);
    function GetPropName(name: String): String;
    procedure SetPropName(name: String; const Value: String);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByNamespacedName(pA, pB : Pointer) : Integer;
      Function CompareByNamespace(pA, pB : Pointer) : Integer;
      Function CompareByName(pA, pB : Pointer) : Integer;
      Function CompareByValue(pA, pB : Pointer) : Integer;
      Function CompareBySortKey(pA, pB : Pointer) : Integer;

      Function Get(Const aValue : Integer) : TFslXMLAttribute; Reintroduce;

    Public
      Function Link : TFslXMLAttributeList;
      Function Clone : TFslXMLAttributeList;

      Function New : TFslXMLAttribute; Reintroduce;

      Function IndexByNamespacedName(Const aNamespace, aName : String) : Integer;
      Function IndexByNamespace(Const aValue : String) : Integer;
      Function IndexByName(Const aValue : String) : Integer;
      Function IndexByValue(Const aValue : String) : Integer;

      Function GetByNamespacedName(Const aNamespace, aName : String) : TFslXMLAttribute;
      Function GetByNamespace(Const aValue : String) : TFslXMLAttribute;
      Function GetByName(Const aValue : String) : TFslXMLAttribute;
      Function GetByValue(Const aValue : String) : TFslXMLAttribute;

      Function ExistsByNamespacedName(Const aNamespace, aName : String) : Boolean;
      Function ExistsByNamespace(Const aValue : String) : Boolean;
      Function ExistsByName(Const aValue : String) : Boolean;
      Function ExistsByValue(Const aValue : String) : Boolean;

      Procedure SortedByNamespacedName;
      Procedure SortedByNamespace;
      Procedure SortedByName;
      Procedure SortedByValue;
      Procedure SortedBySortKey;

      Function IsSortedByNamespacedName : Boolean;
      Function IsSortedByNamespace : Boolean;
      Function IsSortedByName : Boolean;
      Function IsSortedByValue : Boolean;
      Function IsSortedBySortKey : Boolean;

      procedure add(name, value : String); overload;

      Property ElementByIndex[Const iIndex : Integer] : TFslXMLAttribute Read GetElementByIndex Write SetElementByIndex; Default;
      Property Match[name : String] : String read GetPropName write SetPropName;
  End;

  TFslXMLAttributeMatch = Class(TFslStringMatch)
    Private
      Function GetAttribute(Const sKey: String): String;
      Procedure SetAttribute(Const sKey, sValue: String);

    Public
      constructor Create; Override;

      Property Attribute[Const sKey : String] : String Read GetAttribute Write SetAttribute; Default;
  End;

  TFslXMLNamespaceEntry = Class(TFslObject)
    Private
      FKey : String;
      FValues : TFslStringList;

      Function GetValue: String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Push(Const Value: String);
      Procedure Pop;

      Function HasValue : Boolean;

      Property Key : String Read FKey Write FKey;
      Property Value : String Read GetValue;
  End;

  TFslXMLNamespaceEntryList = Class(TFslObjectList)
    Private
      Function GetEntryByIndex(Const iIndex: Integer): TFslXMLNamespaceEntry;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByKey(pA, pB : Pointer) : Integer;

    Public
      Function IndexByKey(Const sKey : String) : Integer;

      Procedure SortedByKey;

      Property EntryByIndex[Const iIndex : Integer] : TFslXMLNamespaceEntry Read GetEntryByIndex; Default;
  End;

  TFslXMLNamespaceLevel = Class(TFslObject)
    Private
      FEntryList : TFslXMLNamespaceEntryList;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property EntryList : TFslXMLNamespaceEntryList Read FEntryList;
  End;

  TFslXMLNamespaceLevelList = Class(TFslObjectList)
    Private
      Function GetLevelByIndex(Const iIndex: Integer): TFslXMLNamespaceLevel;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property LevelByIndex[Const iIndex : Integer] : TFslXMLNamespaceLevel Read GetLevelByIndex; Default;
  End;

  TFslXMLNamespaceManager = Class(TFslObject)
    Private
      FEntryList : TFslXMLNamespaceEntryList;
      FLevelList : TFslXMLNamespaceLevelList;

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Push(Const oAttributes : TFslStringMatch);
      Procedure Pop;

      Function DefaultNamespace : String;
      Function NamespaceOfPrefix(Const sPrefix: String): String;
      Function LocalNameOf(Const sElementName: String) : String;
      Function PrefixOf(Const sElementName: String) : String;
      Function NamespaceOf(Const sElementName: String) : String;

      Procedure ListPrefixes(Const oPrefixNamespaces: TFslStringMatch);
  End;

  { TFslXMLFormatter }

  TFslXMLFormatter = Class(TFslTextFormatter)
    Private
      FAttributes : TFslXMLAttributeList;
      FBuilder : TFslStringBuilder;
      FLocation : TSourceLocation;
      FLastText : boolean;
      FPending : string;
      FNoDense : Boolean;
      FCanonicalEntities: boolean;

      function updateForText(curr : TSourceLocation; s : String) : TSourceLocation;
      procedure commitPending;
    Protected
      Function UseAttributes : String;

      Procedure ProducePretty(sValue : String);
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslXMLFormatter;
      Function Clone : TFslXMLFormatter;

      Procedure ProduceHeader;

      Procedure ProduceOpen(Const sName : String);
      Procedure ProduceClose(Const sName : String);
      Procedure ProduceTag(Const sName : String);
      Procedure ProducePI(Const sName, sText : String);
      Procedure ProduceText(Const sName, sValue : String); Overload;
      Procedure ProduceTextNoEscapeEoln(Const sName, sValue: String);
      Procedure ProduceText(Const sValue : String; processing : TEolnOption = eolnEscape); Overload;
      Procedure ProduceComment(Const sComment : String);
      Procedure ProduceCData(Const sText : String);
      Procedure ProduceBytes(Const aBytes : TBytes); Override;

      Procedure AddAttribute(Const sName, sValue : String; sNs : String = ''); // ns is only used for sorting in canonical mode
      Procedure AddNamespace(Const sAbbreviation, sURI : String);

      Property Attributes : TFslXMLAttributeList Read FAttributes;
      property Location : TSourceLocation read FLocation;
      function sourceLocationForPending : TSourceLocation;
      property NoDense : Boolean read FNoDense write FNoDense;
      property CanonicalEntities : boolean read FCanonicalEntities write FCanonicalEntities;
  End;

  TFslXmlBuilder = class (TXmlBuilder)
  private
    mem : TFslMemoryStream;
    buf : TFslBuffer;
    xml : TFslXMLFormatter;

    depth : integer;
    started : boolean;
    FCanonicalEntities: boolean;
    function getNSRep(uri, name : String):String;
    procedure SetCanonicalEntities(const Value: boolean);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; override;

    procedure defineNS(abbrev, uri : String);

    Procedure Start(); overload; override;
    Procedure StartFragment; override;
    Procedure Finish; override;
    Procedure Build(oStream: TStream);  Overload; override;
    Procedure Build(oStream: TFslStream);  Overload; override;
    Function Build : String;  Overload; override;

    Function SourceLocation : TSourceLocation; override;
    Procedure Comment(Const sContent : String); override;
    Procedure AddAttribute(Const sName, sValue : String); override;
    Procedure AddAttributeNS(Const sNamespace, sName, sValue : String); override;
    function Tag(Const sName : String) : TSourceLocation; override;
    function Open(Const sName : String) : TSourceLocation; override;
    Procedure Close(Const sName : String); override;
    function Text(Const sValue : String) : TSourceLocation; override;
    function Entity(Const sValue : String) : TSourceLocation; override;
    function TagText(Const sName, sValue : String) : TSourceLocation; override;
    procedure ProcessingInstruction(sName, sText : String); override;
    procedure DocType(sText : String); override;
    procedure CData(text : String);


    procedure inject(const bytes : TBytes); override;
    property CanonicalEntities : boolean read FCanonicalEntities write SetCanonicalEntities;
  End;

  TXmlPatchEngine = class (TFslObject)
  private
    class procedure remove(doc : TMXmlDocument; sel : String; target : TMXmlElement);
    class procedure add(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure addChildNodes(doc : TMXmlDocument; source, target : TMXmlElement; pos : String);
  public
    class procedure execute(doc : TMXmlDocument; target : TMXmlElement; patch : TMXmlElement);
  end;

  EFslXMLNamespaceManager = Class(EFslException)
  End;

  TMXmlBuilder = class (TXmlBuilder)
  private
    FExternal : Boolean;
    FStack : TFslList<TMXmlElement>;
    FDoc : TMXmlDocument;
    FAttributes : TFslList<TMXmlAttribute>;
    FSourceLocation : TSourceLocation;
    Function Pad(offset : integer = 0) : String;
    function ReadTextLength(s : string):String;
    function ReadTextLengthWithEscapes(pfx, s, sfx : string):String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; override;
    Procedure Start(oNode : TMXmlElement); overload;
    Procedure Start(); overload; override;
    Procedure StartFragment; override;
    Procedure Finish; override;
    Procedure Build(oStream: TStream);  Overload; override;
    Procedure Build(oStream: TFslStream);  Overload; override;
    Function Build : String;  Overload; override;

    Function SourceLocation : TSourceLocation; override;
    Procedure Comment(Const sContent : String); override;
    Procedure AddAttribute(Const sName, sValue : String); override;
    Procedure AddAttributeNS(Const sNamespace, sName, sValue : String); override;
    function Tag(Const sName : String) : TSourceLocation; override;
    function Open(Const sName : String) : TSourceLocation; override;
    Procedure Close(Const sName : String); override;
    function Text(Const sValue : String) : TSourceLocation; override;
    function Entity(Const sValue : String) : TSourceLocation; override;
    function TagText(Const sName, sValue : String) : TSourceLocation; override;
    procedure ProcessingInstruction(sName, sText : String); override;
    procedure DocType(sText : String); override;

    procedure inject(const bytes : TBytes); override;
  End;

Type
  TFslXMLKnownType = (TFslXMLKnownHeaderType, TFslXMLKnownCommentType, TFslXMLKnownElementType, TFslXMLKnownTextType);
  TFslXMLKnownTypes = Set Of TFslXMLKnownType;


Const
  ALL_XML_KNOWN_TYPE = [TFslXMLKnownHeaderType..TFslXMLKnownTextType];

Type
  TFslXMLExtractor = Class(TFslTextExtractor)
    Private
      FElement : String;
      FAttributes : TFslXMLAttributeMatch;
      FNamespaceManager : TFslXMLNamespaceManager;
      FNodeName : String;

    Protected
      Procedure Expected(Const sMethod, sExpected : String);

      Function SameLocalAndNamespace(Const sTag, sNamespace, sLocal: String): Boolean;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslXMLExtractor;

      Procedure ConsumeAttributes(Const setTerminal : TCharSet);

      Function ConsumeIdentifier(Const sValue : String; Const setTerminal : TCharSet) : String; Overload;
      Function ConsumeIdentifier(Const setTerminal : TCharSet) : String; Overload;

      Function ConsumeWhitespace : String;

      Function ConsumeComment : String; Overload;
      Procedure ConsumeComment(Const sComment : String); Overload;

      Function ConsumeHeader : String; Overload;
      Procedure ConsumeHeader(Const sHeader : String); Overload;

      Procedure ConsumeDocumentType;

      Procedure ConsumeOpen(Const sTag : String); Overload;
      Procedure ConsumeOpen(Const sTag, sNamespace : String); Overload;
      Function ConsumeOpen : String; Overload;

      Function ConsumeBody : String;
      Function ConsumeTextBody : String; Overload;

      Procedure ConsumeClose(Const sTag : String); Overload;
      Procedure ConsumeClose(Const sTag, sNamespace : String); Overload;
      Function ConsumeClose : String; Overload;

      Function ConsumeText(Const sTag : String) : String; Overload;
      Function ConsumeText(Const sTag, sNamespace : String) : String; Overload;

      Function ConsumeElement : String;

      Function PeekString : String;
      Function PeekXml : String;
      Function PeekIsOpenTag(Const sElement : String) : Boolean;
      Function PeekIsOpen : Boolean;
      Function PeekIsClose : Boolean;
      Function PeekIsHeader : Boolean;
      Function PeekIsComment : Boolean;
      Function PeekIsText: Boolean;
      Function PeekIsEmptyNode: Boolean;

      Procedure SkipNext; Overload;
      Procedure Skip(oSkipTypes: TFslXMLKnownTypes); Overload;

      Property Attributes : TFslXMLAttributeMatch Read FAttributes;

      // Namespaces
      Function NodeLocalName: String;
      Function NodeNamespace: String;
      Function IsNode(Const sNamespace, sLocalName: String): Boolean;

      Function GetAttribute(Const sNamespace, sLocalName: String): String; Overload;
      Function GetAttribute(Const sNamespace, sLocalName, sDefault: String): String; Overload;

      Function DefaultNamespace: String;
      Function PrefixOf(Const sNodeName: String): String;
      Function NamespaceOf(Const sNodeName: String): String; Overload;

      Procedure ListPrefixes(Const oPrefixNamespaces: TFslStringMatch);
  End;

implementation


{ TMXmlAttribute }

constructor TMXmlAttribute.Create(name, value: String);
begin
  Create;
  FName := name;
  FValue := value;
end;

function TMXmlAttribute.equal(other: TMXmlNode): boolean;
begin
  if (other is TMXmlAttribute) then
    result := inherited equal(other) and (TMXmlAttribute(other).FValue = FValue)
  else if (other is TMXmlPrimitive) then
    result := other.ToString = ToString
  else
    result := false;
end;

constructor TMXmlAttribute.Create;
begin
  inherited Create(ntAttribute);
end;

function TMXmlAttribute.Link: TMXmlAttribute;
begin
  result := TMXmlAttribute(inherited Link);
end;

function TMXmlAttribute.ToString: String;
begin
  result := FValue;
end;

function TMXmlAttribute.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TMXmlNamedNode }

function TMXmlNamedNode.containsLocation(loc : TSourceLocation): boolean;
begin
  result := loc.inSpan(FStart, FStop);
end;

constructor TMXmlNamedNode.Create(nodeType: TMXmlElementType);
begin
  inherited Create;
  FNodeType := nodeType;
end;

function TMXmlNamedNode.equal(other: TMXmlNode): boolean;
begin
  result := ((other is TMXmlNamedNode) and (FNamespaceURI = TMXmlNamedNode(other).FNamespaceURI) and (FLocalName = TMXmlNamedNode(other).FLocalName));
end;

function TMXmlNamedNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FNamespaceURI.length * sizeof(char)) + 12);
  inc(result, (FLocalName.length * sizeof(char)) + 12);
end;

{ TMXmlElement }

constructor TMXmlElement.Create(nodeType: TMXmlElementType; name: String);
begin
  Create(nodeType);
  FName := name;
end;

constructor TMXmlElement.CreateNS(nodeType: TMXmlElementType; ns, local: String);
begin
  Create(nodeType);
  FNamespaceURI := ns;
  FLocalName := local;
end;

function TMXmlElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAttributes.sizeInBytes);
  inc(result, FChildren.sizeInBytes);
  inc(result, (FText.length * sizeof(char)) + 12);
  inc(result, FNext.sizeInBytes);
end;

class function TMXmlElement.createComment(text: String): TMXmlElement;
begin
  result := TMXmlElement.Create(ntComment);
  result.Text := text;
end;

class function TMXmlElement.createProcessingInstruction(encoding: String): TMXmlElement;
begin
  result := TMXmlElement.Create(ntProcessingInstruction);
  result.Text := encoding;
end;

class function TMXmlElement.createText(text: String): TMXmlElement;
begin
  result := TMXmlElement.Create(ntText);
  result.Text := text;
end;

destructor TMXmlElement.Destroy;
begin
  FNext.Free;
  FAttributes.Free;
  FChildren.Free;
  inherited;
end;


function TMXmlElement.element(name: String): TMXmlElement;
var
  t : TMXmlElement;
begin
  result := nil;
  for t in Children do
    if t.Name = name then
      if result = nil then
        result := t
      else
        raise EXmlException.create('Multiple matches found for '+name+' at '+FStart.describe);
end;

function TMXmlElement.elementNS(ns, name: String): TMXmlElement;
var
  t : TMXmlElement;
begin
  result := nil;
  for t in Children do
    if (t.Name = name) and (t.NamespaceURI = ns) then
      if result = nil then
        result := t
      else
        raise EXmlException.create('Multiple matches found for '+ns+'::'+name+' at '+FStart.describe);
end;

function TMXmlElement.equal(other: TMXmlNode): boolean;
begin
  result := other = self;
end;

function TMXmlElement.first: TMXmlElement;
begin
  if HasChildren then
    result := FChildren[0]
  else
    result := nil;
end;

function TMXmlElement.firstElement: TMXmlElement;
begin
  result := first;
  while (result <> nil) and (result.NodeType <> ntElement) do
    result := result.Next;
end;

procedure TMXmlElement.fixChildren;
var
  c : TMXmlElement;
  i : integer;
  a : TMXmlAttribute;
begin
  if hasChildren then
  begin
    for c in Children do
      c.Parent := self;
    for i := 0 to FChildren.count - 2 do
      FChildren[i].Next := FChildren[i+1].link;
    FChildren[FChildren.count - 1].Next := nil;
  end;
  if HasAttributes then
    for a in Attributes do
      a.Parent := self;
end;

function TMXmlElement.getAbbreviationPriv(ns: String; var abbrev : String) : boolean;
var
  a : TMXmlAttribute;
begin
  if HasAttributes then
  begin
    for a in FAttributes do
    begin
      if (a.Name = 'xmlns') and (a.Value = ns) then
      begin
        abbrev := '';
        exit(true);
      end;
      if (a.Name.StartsWith('xmlns:')) and (a.Value = ns) then
      begin
        abbrev := a.name.Substring(6);
        exit(true);
      end;
    end;
  end;
  if (Parent <> nil) then
    result := parent.getAbbreviationPriv(ns, abbrev)
  else
    result := false;
end;

function TMXmlElement.getAbbreviation(ns: String): String;
var
  i : integer;
begin
  if not getAbbreviationPriv(ns, result) then
  begin
    if not hasAttribute['xmlns'] then
      attribute['xmlns'] := ns
    else
    begin
      i := 0;
      while hasAttribute['xmlns:n'+inttostr(i)] do
        inc(i);
      result := 'n'+inttostr(i);
      attribute['xmlns:'+result] := ns;
    end;
  end;
end;

function TMXmlElement.GetAllChildrenAreText: boolean;
var
  child : TMXmlElement;
begin
  result := true;
  for child in Children do
    if (child.NodeType <> ntText) then
      exit(false);
end;

function TMXmlElement.findLocation(loc : TSourceLocation; path : TFslList<TMXmlNamedNode>) : boolean;
var
  child : TMXmlElement;
  attr : TMXmlAttribute;
begin
  result := containsLocation(loc);
  if result then
  begin
    path.add(self.link);
    if FAttributes <> nil then
    begin
      for attr in FAttributes do
        if (attr.containsLocation(loc)) then
        begin
          path.add(attr.link);
          exit;
        end;
    end;
    if (FChildren <> nil) then
    begin
      for child in FChildren do
        if child.findLocation(loc, path) then
          exit;
    end;
  end;
end;

function TMXmlElement.GetAllText: String;
var
  b : TStringBuilder;
  c : TMXmlElement;
begin
  if not HasChildren then
    result := ''
  else if (FChildren.count = 1) and (FChildren[0].NodeType = ntText) then
    result := FChildren[0].Text
  else
  begin
    b := TStringBuilder.Create;
    try
      for c in FChildren do
        if c.NodeType <> ntComment then

        b.Append(c.FText);
      result := b.ToString;
    finally
      b.Free;
    end;
  end;
end;

function TMXmlElement.getAttrByName(name: String; var attr: TMXmlAttribute): boolean;
var
  a : TMXmlAttribute;
begin
  if FAttributes = nil then
    exit(false);
  result := false;
  for a in FAttributes do
    if a.Name = name then
    begin
      attr := a;
      exit(true);
    end;
end;

function TMXmlElement.GetAttribute(name: String): String;
var
  attr : TMXmlAttribute;
begin
  result := '';
  if getAttrByName(name, attr) then
    result := attr.Value;
end;

function TMXmlElement.GetAttributeNS(ns, name: String): String;
var
  attr : TMXmlAttribute;
begin
  result := '';
  if HasAttributes then
    for attr in FAttributes do
      if (attr.NamespaceURI = ns) and (attr.LocalName = name) then
        exit(attr.Value);
end;

function TMXmlElement.GetAttributes: TFslList<TMXmlAttribute>;
begin
  if (FAttributes = nil) then
    FAttributes := TFslList<TMXmlAttribute>.create;
  result := FAttributes;
end;

function TMXmlElement.GetChildren: TFslList<TMXmlElement>;
begin
  if (FChildren = nil) then
    FChildren := TFslList<TMXmlElement>.create;
  result := FChildren;
end;

function TMXmlElement.GetHasAttribute(name: String): boolean;
var
  attr : TMXmlAttribute;
begin
  if FAttributes = nil then
    exit(false);
  result := false;
  for attr in FAttributes do
    if attr.Name = name then
      exit(true);
end;

function TMXmlElement.GetHasAttributes: boolean;
begin
 result := (FAttributes <> nil) and (FAttributes.Count > 0);
end;

function TMXmlElement.GetHasChildren: boolean;
begin
 result := (FChildren <> nil) and (FChildren.Count > 0);
end;

function TMXmlElement.GetHasText: boolean;
begin
  result := FText <> '';
end;

function TMXmlElement.GetText: string;
begin
  if NodeType in [ntText, ntComment] then
    result := FText
  else if Children.Count = 0 then
    result := ''
  else if Children.Count = 1 then
    result := Children[0].Text
  else
    result := allText;
end;

function TMXmlElement.last: TMXmlElement;
begin
  if HasChildren then
    result := FChildren.Last
  else
    result := nil;
end;

function TMXmlElement.Link: TMXmlElement;
begin
  result := TMXmlElement(inherited Link);
end;

procedure TMXmlElement.listElements(name: String; list: TFslList<TMXmlElement>);
var
  t : TMXmlElement;
  n : string;
begin
  for t in children do
  begin
    n := t.FName;
    if (name = n) or (name.endsWith('[x]') and n.startsWith(name.substring(0, name.length-3))) then
      list.add(t.Link);
  end;
end;

function TMXmlElement.nextElement: TMXmlElement;
begin
  result := Next;
  while (result <> nil) and (result.NodeType <> ntElement) do
    result := result.Next;
end;

function TMXmlElement.previous: TMXmlElement;
var
  i : integer;
begin
  if parent = nil then
    exit(nil);
  i := parent.Children.IndexOf(self);
  if i = 0 then
    result := nil
  else
    result := parent.Children[i-1];
end;

function TMXmlElement.RemoveAttribute(name: String): boolean;
var
  attr : TMXmlAttribute;
begin
  result := getAttrByName(name, attr);
  if result then
    FAttributes.Remove(attr)
end;

procedure TMXmlElement.SetAttribute(name: String; const Value: String);
var
  attr : TMXmlAttribute;
begin
  if not getAttrByName(name, attr) then
  begin
    attr := TMXmlAttribute.Create(name, Value);
    Attributes.Add(attr);
  end
  else
    attr.Value := Value;
end;

procedure TMXmlElement.SetAttributeNS(ns, name: String; const Value: String);
var
  attr : TMXmlAttribute;
  done : boolean;
  s : String;
begin
  done := false;
  if HasAttributes then
    for attr in FAttributes do
      if (attr.NamespaceURI = ns) and (attr.LocalName = name) then
      begin
        attr.Value := Value;
        done := true;
      end;
  if not done then
  begin
    s := getAbbreviation(ns);
    attr := TMXmlAttribute.Create;
    try
      attr.NamespaceURI := ns;
      attr.LocalName := name;
      attr.Value := Value;
      if s = '' then
      begin
        attr.Name := name;
        Attributes.Add(attr.Link)
      end
      else
      begin
        attr.Name := s+':'+name;
        Attributes.Add(attr.Link);
      end;
    finally
      attr.Free;
    end;
  end;
end;

procedure TMXmlElement.SetNext(const Value: TMXmlElement);
begin
  FNext.Free;
  FNext := Value;
end;

procedure TMXmlElement.SetText(const Value: string);
begin
  if NodeType in [ntText, ntComment, ntProcessingInstruction] then
    FText := Value
  else
    raise EXmlException.create('Unable to set text at '+FStart.describe);
end;

function TMXmlElement.ToString: String;
begin
  case NodeType of
    ntElement: result := FName;
    ntText: result := FText;
    ntComment: result := FText;
    ntDocument: result := '[document]';
  else
    result := 'other';
  end;
end;

function TMXmlElement.ToXml(pretty : boolean = false): String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.create;
  try
    writeToXml(b, pretty, 0);
    result := b.toString();
  finally
    b.Free;
  end;
end;

procedure TMXmlElement.writeToXml(b: TStringBuilder; pretty : boolean; indent : integer);
var
  a : TMXmlAttribute;
  c : TMXmlElement;
  s : String;
begin
  if pretty then
    b.Append(StringPadLeft('', ' ', indent*2));
  case FNodeType of
    ntDocument :
      for c in Children do
        c.writeToXml(b, pretty, indent);
    ntElement:
      begin
      if (Name = '') then
      begin
        if (LocalName <> '') and (NamespaceURI <> '') then
        begin
          s := getAbbreviation(NamespaceURI);
          if s = '' then
            Name := LocalName
          else
            Name := s+':'+LocalName
        end
        else
          raise EXmlException.create('no Name or QName provided');
      end;
      b.Append('<');
      b.Append(Name);
      if HasAttributes then
      begin
        for a in attributes do
        begin
          b.Append(' ');
          b.Append(a.Name);
          b.Append('="');
          b.Append(FormatTextToXML(a.Value, xmlAttribute));
          b.Append('"');
        end;
      end;
      if HasChildren then
      begin
        b.append('>');
        if pretty then
          b.Append(#13#10);
        for c in Children do
          c.writeToXml(b, pretty, indent + 1);
        if pretty then
        begin
          b.Append(StringPadLeft('', ' ', indent*2));
        end;
        b.Append('</');
        b.Append(Name);
        b.append('>');
        if pretty then
          b.Append(#13#10);
      end
      else
      begin
        b.append('/>');
        if pretty then
          b.Append(#13#10);
      end;
      end;
    ntText:
      if pretty then
        b.Append(FormaTXmlForTextArea(Text))
      else
      begin
        b.Append(FormaTXmlForTextArea(Text.trim));
      end;
    ntComment:
      begin
      b.Append('<!-- ');
      b.Append(Text);
      b.Append(' -->');
      end;
  end;
end;

procedure TMXmlElement.addChild(node: TMXmlElement; fix : boolean);
begin
  Children.Add(node);
  node.Parent := self;
  if fix then
    fixChildren;
end;

function TMXmlElement.addElement(name: String) : TMXmlElement;
begin
  result := TMXmlElement.Create(ntElement);
  result.Name := name;
  addChild(result, false);
end;

function TMXmlElement.addElementNS(ns, name: String) : TMXmlElement;
begin
  result := TMXmlElement.Create(ntElement);
  result.LocalName := name;
  result.NamespaceURI := ns;
  addChild(result, false);
end;

function TMXmlElement.addText(content: String): TMXmlElement;
begin
  result := TMXmlElement.Create(ntText);
  result.Text := content;
  addChild(result, false);
end;

constructor TMXmlElement.CreateNSN(nodeType: TMXmlElementType; name, ns, local: String);
begin
  Create(nodeType);
  FName := name;
  FNamespaceURI := ns;
  FLocalName := local;
end;

{ TMXmlParser }

function TMXmlParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, reader.sizeInBytes);
end;

class function TMXmlParser.parse(content: TStream; options : TMXmlParserOptions): TMXmlDocument;
var
  s : TFslVCLStream;
begin
  s := TFslVCLStream.create;
  try
    s.Stream := content;
    result := parse(s, options);
  finally
    s.Free;
  end;
end;

class function TMXmlParser.parse(content: String; options : TMXmlParserOptions): TMXmlDocument;
var
  this : TMXmlParser;
begin
  this := TMXmlParser.create;
  try
    this.options := options;
    this.reader := TFslStringReader.create(content);
    try
      result := this.parse();
    finally
      this.reader.Free;
    end;
  finally
    this.free;
  end;
end;

class function TMXmlParser.parse(content: TFslStream; options : TMXmlParserOptions): TMXmlDocument;
var
  this : TMXmlParser;
begin
  this := TMXmlParser.create;
  try
    this.options := options;
    this.reader := TFslStreamReader.create(content.Link, TEncoding.UTF8, true);
    try
      result := this.parse;
    finally
      this.reader.Free;
    end;
  finally
    this.Free;
  end;
end;

class function TMXmlParser.parseFile(name: String; options : TMXmlParserOptions): TMXmlDocument;
var
  s : TFslFile;
begin
  s := TFslFile.create(name, fmOpenRead + fmShareDenyWrite);
  try
    result := parse(s, options);
  finally
    s.Free;
  end;
end;

function TMXmlParser.parseXPath: TMXPathExpressionNode;
begin
  FLocation := TSourceLocation.Create;
  FStartLocation := FLocation;

  b := TStringBuilder.Create;
  try
    result := TMXPathExpressionNode.Create;
    try
      readXpathExpression(result, []);
      result.Link;
    finally
      result.Free;
    end;
  finally
    b.Free;
  end;

end;

class function TMXmlParser.parseXPath(content: String): TMXPathExpressionNode;
var
  this : TMXmlParser;
begin
  this := TMXmlParser.Create;
  try
    this.reader := TFslStringReader.Create(content);
    try
      result := this.parseXPath;
      this.moveOperationsToProximal(result);
      this.organisePrecedence(result, true);
    finally
      this.reader.Free;
    end;
  finally
    this.Free;
  end;
end;

function TMXmlParser.peek: char;
begin
  result := char(reader.Peek);
end;

function TMXmlParser.read: char;
begin
  result := char(reader.Read);
  if not CharInSet(result, [#13, #10]) then
    FLocation.incCol
  else if (result = #10) or (peek <> #10) then
    FLocation.incLine
end;

class Function TMXmlParser.IsXmlNameChar(Const ch : Char): Boolean;
begin
  Result := CharInSet(ch, ['_', ':', '-', '.', 'A'..'Z', 'a'..'z', '0'..'9']);
end;

class Function TMXmlParser.IsXmlWhiteSpace(Const ch : Char): Boolean;
begin
  Result := CharInSet(ch, [#9,#10,#13,' ']);
end;

class function TMXmlParser.isXPathName(name: String): boolean;
var
  ch : char;
begin
  result := name.Length > 0;
  for ch in name do
    if not isXmlNameChar(ch) and (ch <> '*') then
      exit(false);
end;

procedure TMXmlParser.moveOperationsToProximal(node: TMXPathExpressionNode);
var
  n : TMXPathExpressionNode;
begin
  n := node;
  while n.next <> nil do
    n := n.next;
  if (n <> node) and (n.NextOp <> nil) then
  begin
    node.op := n.op;
    node.NextOp := n.NextOp.Link;
    n.op := xeoNull;
    n.NextOp := nil;
  end;
  if node.Group <> nil then
    moveOperationsToProximal(node.Group);
  for n in node.filters do
    moveOperationsToProximal(n);
  for n in node.Params do
    moveOperationsToProximal(n);
  if node.NextOp <> nil then
    moveOperationsToProximal(node.NextOp);
end;

function TMXmlParser.newGroup(next : TMXPathExpressionNode) : TMXPathExpressionNode;
begin
  result := TMXPathExpressionNode.Create;
  try
    result.NodeType := xentGroup;
    result.Group := next.Link;
    result.link;
  finally
    result.free;
  end;
end;

constructor TMXmlParser.Create;
begin
  inherited;
  FHtmlEntities := TDictionary<String, String>.create;
  registerEntities(FHtmlEntities);
end;

destructor TMXmlParser.Destroy;
begin
  FHtmlEntities.Free;
  inherited;
end;

procedure TMXmlParser.gatherPrecedence(var start: TMXPathExpressionNode; ops: TMXPathExpressionOperationSet);
var
  work : boolean;
  focus, node, group : TMXPathExpressionNode;
begin
  // is there anything to do?
  work := false;
  focus := start.NextOp;
  if start.op in ops then
    while (focus <> nil) and (focus.Op <> xeoNull) do
    begin
      work := work or not (focus.Op in Ops);
      focus := focus.NextOp;
    end
  else
    while (focus <> nil) and (focus.Op <> xeoNull) do
    begin
      work := work or (focus.Op in Ops);
      focus := focus.NextOp;
    end;
  if not work then
    exit;

  // entry point: tricky
  if start.Op in ops then
  begin
    group := newGroup(start);
    focus := start;
    start := group;
  end
  else
  begin
    node := start;
    focus := node.NextOp;
    while not (focus.Op in Ops) do
    begin
      node := focus;
      focus := focus.NextOp;
    end;
    group := newGroup(focus);
    node.NextOp := group;
  end;

  // now, at this point:
  //   group is the group we are adding to, it already has a .group property filled out.
  //   focus points at the group.group
  repeat
    // run until we find the end of the sequence
    while (focus.Op in ops) do
      focus := focus.NextOp;
    if (focus.Op <> xeoNull) then
    begin
      group.Op := focus.Op;
      group.NextOp := focus.NextOp.Link;
      focus.Op := xeoNull;
      focus.NextOp := nil;
      // now look for another sequence, and start it
      node := group;
      focus := group.NextOp;
      if (focus <> nil) then
      begin
        while (focus <> nil) and not (focus.Op in Ops) do
        begin
          node := focus;
          focus := focus.NextOp;
        end;
        if (focus <> nil) { and (focus.Op in Ops) - must be true } then
        begin
          group := newGroup(focus);
          node.NextOp := group;
        end;
      end;
    end;
  until (focus = nil) or (focus.Op = xeoNull);
end;

procedure TMXmlParser.organisePrecedence(var node: TMXPathExpressionNode; operations : boolean);
var
  i : integer;
  n, f, fn : TMXPathExpressionNode;
begin
  if operations then
  begin
  //  gatherPrecedence(node, [popTimes, popDivideBy, popDiv, popMod]);
    gatherPrecedence(node, [xeoPlus, xeoMinus]);
    gatherPrecedence(node, [xeoUnion, xeoSequence]);
    gatherPrecedence(node, [xeoLessThan, xeoGreaterThan, xeoLessEquals, xeoGreaterEquals]);
  //  gatherPrecedence(node, [popIs]);
    gatherPrecedence(node, [xeoEquals, xeoNotEquals]);
    gatherPrecedence(node, [xeoAnd]);
  // last:  gatherPrecedence(node, [xeoOr]);
    // ex-last: implies
  end;

  if node.FGroup <> nil then
    organisePrecedence(node.FGroup, true);
  for i := 0 to node.filters.Count - 1 do
  begin
    f := node.filters[i].Link;
    try
      fn := f;
      organisePrecedence(fn, true);
      if fn <> f then
        node.filters[i] := fn;
    finally
      f.free;
    end;
  end;
  for i := 0 to node.Params.Count - 1 do
  begin
    f := node.Params[i].Link;
    try
      fn := f;
      organisePrecedence(fn, true);
      if fn <> f then
        node.Params[i] := fn;
    finally
      f.free;
    end;
  end;

  if (operations) then
  begin
    n := node.NextOp;
    while n <> nil do
    begin
      organisePrecedence(n, false);
      n := n.NextOp;
    end;
  end;
  n := node.FNext;
  while n <> nil do
  begin
    organisePrecedence(n, true);
    n := n.Next;
  end;
end;

class function TMXmlParser.isXmlName(name : String) : boolean;
var
  ch : char;
begin
  result := name.Length > 0;
  for ch in name do
    if not isXmlNameChar(ch) then
      exit(false);
end;

Function TMXmlParser.xmlToText(s: String): String;
Var
  i, j : integer;
  v : String;
  w : WideChar;
begin
  b.Clear;
  i := 1;
  while i <= s.Length Do
  begin
    If s[i] = '&' Then
    begin
      Inc(i);
      j := i;
      Repeat
        Inc(i);
        rule(i <= s.Length, 'Unterminated Entity in "'+s+'"');
      Until s[i] = ';';
      v := Copy(s, j, i-j);
      If v[1] = '#' Then
      begin
        w := wideChar(StrToInt(v.Substring(1)));
        b.Append(UnicodeString(w));
      end
      else If v = 'quot' Then
        b.Append('"')
      else If v = 'amp' Then
        b.Append('&')
      else If v = 'lt' Then
        b.Append('<')
      else If v = 'gt' Then
        b.Append('>')
      else If v = 'apos' Then
        b.Append('''')
      else If (xpHTMLEntities in options) and FHtmlEntities.ContainsKey('&'+v+';') Then //
        b.Append(FHtmlEntities['&'+v+';'])
      else
        rule(false, 'Illegal Entity "'+v+'" in "'+s+'"');
    end
    else
    begin
      If (s[i] = #13) Then
      begin
        b.Append(#10);
        If (i < s.Length) And (s[i+1] = #10) Then
          inc(i);
      end
      else
        b.Append(s[i]);
    end;
    inc(i);
  end;
  result := b.ToString;
end;

function TMXmlParser.parse : TMXmlDocument;
var
  s : String;
begin
  FLocation := TSourceLocation.Create;
  FStartLocation := FLocation;

  b := TStringBuilder.Create;
  try
    s := ReadToken(true);
    rule(s[1] = '<', 'Unable to read Document - starts with "'+s+'"');
    If s = '<?' Then
    begin
      while (s <> '>') and (s <> '?>') do
        s := readToken(true);
      s := ReadToken(true);
    end;
    if (xpHTMLEntities in options) then
    begin
      If s = '<!' Then
      begin
        while (s <> '>') do
          s := readToken(true);
        s := ReadToken(true);
      end;
    end;

    rule(s.StartsWith('<'), 'Unable to read XML  - starts with "'+s+'"');
    result := TMXmlDocument.Create;
    try
      while s = '<!' do
      begin
        ReadComment(result);
        s := ReadToken(true);
      end;
      while (s = '<?') do
      begin
        ReadInstruction(result);
        s := ReadToken(true);
      end;
      ReadElement(result);
      result.fixChildren;
      result.Link;
    finally
      result.Free;
    end;
    if xpResolveNamespaces in options then
      resolveNamespaces(result, '');
  finally
    b.Free;
  end;
end;

procedure TMXmlParser.ReadElement(parent: TMXmlElement);
Var
  s : String;
  e : TMXmlElement;
begin
  e := TMXmlElement.Create(ntElement);
  try
    e.Start := FStartLocation;
    e.Name := ReadToken(xpDropWhitespace in options);
    s := ReadToken(true);
    while (s <> '/>') And (s <> '>') Do
      begin
      rule(isXmlName(s), 'The attribute name '+s+' is illegal');
      e.Attributes.Add(ReadAttribute(s));
      s := ReadToken(true);
      end;
    If s = '>' Then
    begin
      // there's actual content to read
      while s <> '</' Do
      begin
        s := ReadToNextChar('<');
        if (s <> '') and (not (xpDropWhitespace in options) or not StringIsWhitespace(s)) then
          readText(e, s);
        s := ReadToken(False);
        If s = '<' Then
          readElement(e)
        else if s = '<!' then
          readComment(e)
        else if s = '<?' then
          readInstruction(e);
      end;
      s := ReadToken(false);
      rule(s = e.Name, 'Element name mismatch (start: "'+e.Name+'"/ end: "'+s+'")');
      s := Readtoken(true);
      rule(s = '>', 'Element "'+e.Name+'" not terminated properly');
    end;
    e.Stop := FLocation;
    parent.addChild(e.Link, false);
    e.fixChildren;
  finally
    e.Free;
  end;
end;


procedure TMXmlParser.ReadInstruction(parent: TMXmlElement);
Var
  s : String;
  done : boolean;
  c : TStringBuilder;
  res : TMXmlElement;
begin
  res := TMXmlElement.Create(ntProcessingInstruction);
  try
    res.Start := FStartLocation;
    c := TStringBuilder.create;
    try
      done := false;
      repeat
        s := ReadToNextChar('?');
        c.Append(s);
        s := ReadToken(false);
        if (peek = '>') then
        begin
          done := true;
          read;
        end;
      until done;
      res.Stop := FLocation;
      res.Text := c.ToString.Trim;
    finally
      c.free;
    end;
    if not (xpDropComments in options) then
      parent.Children.add(res.Link);
  finally
    res.Free;
  end;
end;

function TMXmlParser.ReadAttribute;
Var
  s : String;
begin
  result := TMXmlAttribute.Create;
  try
    result.Name := name;
    result.Start := FStartLocation;
    s := ReadToken(True);
    rule(s = '=', 'Found "'+s+'" looking for "=" in attribute '+name);
    s := ReadToken(xpDropWhitespace in options);
    rule((s = '"') Or (s = ''''), 'Found "'+s+'" looking for " or '' at start of attribute '+name);
    result.Value := XmlToText(ReadToNextChar(s[1]));
    s := ReadToken(False);
    rule((s = '"') Or (s = ''''), 'Found "'+s+'" looking for " or '' at end of attribute '+name);
    result.Stop := FLocation;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TMXmlParser.ReadComment(parent: TMXmlElement);
Var
  s : String;
  done : boolean;
  c : TStringBuilder;
  res : TMXmlElement;
begin
  res := TMXmlElement.Create(ntComment);
  try
    res.Start := FStartLocation;
    c := TStringBuilder.create;
    try
      rule(read = '-', 'Syntax Error reading comment');
      rule(read = '-', 'Syntax Error reading comment');
      done := false;
      repeat
        s := ReadToNextChar('-');
        c.Append(s);
        s := ReadToken(false);
        if (s <> '--') or (peek <> '>') then
          c.Append(s)
        else
        begin
          done := true;
          read;
        end;
      until done;
      res.Stop := FLocation;
      res.Text := c.ToString.Trim;
    finally
      c.free;
    end;
    if not (xpDropComments in options) then
      parent.Children.add(res.Link);
  finally
    res.Free;
  end;
end;

procedure TMXmlParser.ReadText(parent : TMXmlElement; text: String);
var
  e : TMXmlElement;
begin
  e := TMXmlElement.Create(ntText);
  try
    e.Start := FStartLocation;
    e.Text := xmlToText(text);
    e.Stop := FLocation;
    parent.Children.add(e.Link);
  finally
    e.Free;
  end;
end;

function TMXmlParser.ReadToken(skipWhitespace: Boolean; allowEmpty : boolean = false): String;
Var
  ch : Char;
begin
  If skipWhitespace Then
    while IsXmlWhiteSpace(peek) Do
      Read;
  FStartLocation := FLocation;
  if (reader.Peek = -1) then
    if allowEmpty then
      exit('')
    else
      rule(false, 'Read off end of stream');
  ch := read;
  b.Clear;
  b.Append(ch);
  If isXmlNameChar(ch) and (ch <> '-') Then
  begin
    while isXmlNameChar(peek) Do
      b.Append(read);
  end
  else
  begin
    Case ch Of
      '<': if CharInSet(peek, ['?', '/', '!']) then
             b.Append(read);
      '/': if CharInSet(peek, ['>']) then
             b.Append(read);
      '-': if CharInSet(peek, ['-']) then
             b.Append(read);
      '?': if CharInSet(peek, ['/']) then
             b.Append(read);
    else
      // don't care
    end;
  end;
  result := b.ToString;
end;

function TMXmlParser.ReadToNextChar(ch: char): String;
begin
  b.Clear;
  while (reader.Peek <> -1) And (peek <> ch) Do
    b.Append(read);
  rule(reader.Peek <> -1, 'Read off end of stream');
  result := b.ToString;
end;

procedure TMXmlParser.rule(test: boolean; message: String);
begin
  if not test then
    raise FLocation.exception(message);
end;

function describeTokens(tokens : Array of String) : String;
var
  s : String;
begin
  if length(tokens) = 0 then
    result := ''''''
  else if length(tokens) = 1 then
    result := ''''+tokens[0]+''''
  else
  begin
    result := '[';
    for s in tokens do
      result := result + ''''+tokens[0]+'''';
    result := result + ']';
  end;
end;

type
  TExpressionSyntaxMode = (esmNormal, esmForLoop, esmIfStmt);

function TMXmlParser.readXpathExpression(node: TMXPathExpressionNode; endTokens : Array of String; alreadyRead : String = '') : String;
var
  f : TMXPathExpressionNode;
  done, readNext : boolean;
  s : String;
  mode : TExpressionSyntaxMode;
begin
  mode := esmNormal;
  readNext := true;
  if alreadyRead = '' then
    s := readXPathToken(true)
  else
    s := alreadyRead;
  node.NodeType := xentName;
  if s.Contains('::') then
  begin
    if StringArrayExistsSensitive(AXIS_NAMES, s.Substring(0, s.IndexOf('::'))) then
      node.axis := TMXPathExpressionAxis(StringArrayIndexOfSensitive(AXIS_NAMES, s.Substring(0, s.IndexOf('::'))))
    else
      raise FLocation.exception('Unknown XPath axis '+s.Substring(0, s.IndexOf('::')));
    s := s.Substring(s.IndexOf('::')+2);
  end
  else
    node.axis := axisChild;

  if isXPathName(s) or (s = '*') then
  begin
    node.Value := s;
    if StringIsInteger32(s) then
      node.NodeType := xentConstant
  end
  else if (s = '@') then
  begin
    node.Value := readXPathToken(false);
    node.axis := axisAttribute
  end
  else if (s = '$') then
  begin
    node.NodeType := xentVariable;
    node.Value := readXPathToken(false)
  end
  else if (s = '''') then
  begin
    node.NodeType := xentConstant;
    node.value := ReadToNextChar('''');
    s := readXPathToken(false);
  end
  else if (s = '(') then
  begin
    node.nodeType := xentGroup;
    s := readXPathToken(true, true);
    if s <> ')' then
    begin
      node.group := TMXPathExpressionNode.Create;
      s := readXpathExpression(node.Group, [')'], s);
    end;
    rule(s = ')', 'Expected ''('' at this point but found '+s);
  end
  else if (s = '/') then
  begin
    // starting at the root...
    if not readNext then
      raise FLocation.exception('Syntax error..');
    readNext := false;
    node.NodeType := xentRoot; // no value in this case
  end
  else
    rule(false, 'Unknown XPath name '+s);

  if readNext then
  begin
    if isXmlWhiteSpace(peek) then
      if (node.value = 'if') then
        mode := esmIfStmt
      else if (node.value = 'for') then
        mode := esmForLoop;
    s := readXPathToken(true, true);
  end;

  if mode = esmIfStmt then
  begin
    node.nodeType := xentFunction;
    readXpathExpression(node.addParam, ['then'], s);
    readXpathExpression(node.addParam, ['else']);
    s := readXpathExpression(node.addParam, endTokens);
  end
  else if mode = esmForLoop then
  begin
    rule(s = '$', 'expected a variablt ($xxx) parsing a for loop');
    node.nodeType := xentIterator;
    node.value := readXPathToken(false);
    s := readXPathToken(true);
    rule(s = 'in', 'Unexpected token '''+s+''' reading or iterator');
    s := readXpathExpression(node.addParam, ['return']);
    s := readXpathExpression(node.addParam, endTokens);
  end
  else // mode = esmNormal
  begin
    if (s = '(') then
    begin
      s := readXPathToken(true);
      done := s = ')';
      while not done do
      begin
        s := readXpathExpression(node.addParam, [',', ')'], s);
        if (s = ',') then
          s := readXPathToken(true)
        else if (s = ')') then
          done := true
        else
          rule(false, 'expected '','' at this point but found '+s);
      end;
      node.NodeType := xentFunction;
      s := readXPathToken(true, true);
    end;
    while (s = '[') do
    begin
      f := TMXPathExpressionNode.Create;
      try
        readXpathExpression(f, [']']);
        s := readXPathToken(true, true);
        node.Filters.Add(f.Link);
      finally
        f.Free;
      end;
    end;
    if (s = '/') then
    begin
      node.Next := TMXPathExpressionNode.Create;
      s := readXpathExpression(node.next, endTokens);
    end;
    if StringArrayExistsSensitive(['=', '+', 'and', 'or', '>', '>=', '!=', '|', '<', '<=', '-'], s) or ((s = ',') and not StringArrayExistsSensitive(endTokens, ',')) then
    begin
      node.Op := TMXPathExpressionOperation(StringArrayIndexOfSensitive(['', '=', '+', 'and', 'or', '>', '>=', '!=', '|', '<', '<=', ',', '-'], s));
      node.NextOp := TMXPathExpressionNode.Create;
      s := readXpathExpression(node.NextOp, endTokens);
    end;
    if s = '' then
      rule(length(endTokens) = 0, 'Unexpected end of expression expecting '+describeTokens(endTokens))
    else
      rule(StringArrayExistsSensitive(endTokens, s), 'Found '+s+' expecting '+describeTokens(endTokens));
  end;
  result := s;
end;

function TMXmlParser.ReadXPathToken(skipWhitespace, allowEmpty: boolean): String;
Var
  ch : Char;
begin
  If skipWhitespace Then
    while IsXmlWhiteSpace(peek) Do
      Read;
  FStartLocation := FLocation;
  if (reader.Peek = -1) then
    if allowEmpty then
      exit('')
    else
      rule(false, 'Read off end of stream');
  ch := read;
  b.Clear;
  b.Append(ch);
  If (ch <> '-') and (isXmlNameChar(ch) or (ch = '*')) Then
  begin
    while isXmlNameChar(peek) or (peek = '*') Do
      b.Append(read);
  end
  else
  begin
    Case ch Of
      '<': if CharInSet(peek, ['=']) then
             b.Append(read);
      '>': if CharInSet(peek, ['=']) then
             b.Append(read);
      '/': if CharInSet(peek, ['>']) then
             b.Append(read);
      '!': if CharInSet(peek, ['=']) then
             b.Append(read);
      '-': if CharInSet(peek, ['-']) then
             b.Append(read)
           else if CharInSet(peek, ['0'..'9']) then
           begin
            while CharInSet(peek, ['0'..'9']) or (peek = '.') Do
              b.Append(read);
           end;
      '?': if CharInSet(peek, ['/']) then
             b.Append(read);
    else
      // don't care
    end;
  end;
  result := b.ToString;
end;

function TMXmlParser.resolveNamespace(element: TMXmlElement; abbrev: String): String;
var
  a : TMXmlAttribute;
begin
  if abbrev = 'xml' then
    exit('http://www.w3.org/XML/1998/namespace');

  if element.hasAttributes then
    for a in element.Attributes do
      if (a.Name = 'xmlns:'+abbrev) then
        exit(a.Value);
  if element.Parent <> nil then
    result := resolveNamespace(element.Parent, abbrev)
  else
    raise EXmlException.create('Unable to resolve namespace abbreviation "'+abbrev+'"');
end;

procedure TMXmlParser.resolveNamespaces(element: TMXmlElement; defNs : String);
var
  ns : String;
  p : TArray<String>;
  c : TMXmlElement;
  a : TMXmlAttribute;
begin
  if element.hasAttributes then
  begin
    for a in element.Attributes do
      if (a.Name = 'xmlns') then
        defNs := a.Value;
    for a in element.Attributes do
    begin
      if (a.name.contains(':')) then
      begin
        ns := a.name.Substring(0, a.name.IndexOf(':'));
        if ns <> 'xmlns' then
        begin
          a.NamespaceURI := resolveNamespace(element, ns);
          a.LocalName := a.name.Substring(a.name.IndexOf(':')+1);
        end;
      end;
    end;
  end;
  p := element.Name.Split([':']);
  if length(p) = 1 then
  Begin
    element.NamespaceURI := defNs;
    element.localName := element.Name;
  end
  else if length(p) = 2 then
  begin
    element.localName := p[1];
    element.NamespaceURI := resolveNamespace(element, p[0]);
  end;
  if element.hasChildren then
    for c in element.Children do
      resolveNamespaces(c, defNs);
end;

class function TMXmlParser.parse(content: TBytes; options: TMXmlParserOptions): TMXmlDocument;
var
  s : TBytesStream;
begin
  s := TBytesStream.Create(content);
  try
    result := parse(s, options);
  finally
    s.Free;
  end;
end;

{ TMXPathExpressionNode }

function TMXPathExpressionNode.addParam: TMXPathExpressionNode;
begin
  result := TMXPathExpressionNode.Create;
  Params.Add(result);
end;

function TMXPathExpressionNode.buildConstant: TMXmlNode;
begin
  if (value = 'true') or (value = 'false') then
    result := TMXmlBoolean.Create(value = 'true')
  else if StringIsInteger32(value) then
    result := TMXmlNumber.Create(StrToInt(value))
  else
    result := TMXmlString.Create(value);
end;

destructor TMXPathExpressionNode.Destroy;
begin
  FFilters.Free;
  FNext.Free;
  FNextOp.Free;
  FGroup.Free;
  FParams.Free;
  inherited;
end;

function TMXPathExpressionNode.GetFilters: TFslList<TMXPathExpressionNode>;
begin
  if FFilters = nil then
    FFilters := TFslList<TMXPathExpressionNode>.create;
  result := FFilters;
end;

function TMXPathExpressionNode.GetParams: TFslList<TMXPathExpressionNode>;
begin
  if FParams = nil then
    FParams := TFslList<TMXPathExpressionNode>.create;
  result := FParams;
end;

function TMXPathExpressionNode.hasFilters: Boolean;
begin
  result := (FFilters <> nil) and (FFilters.Count > 0);
end;

function TMXPathExpressionNode.Link: TMXPathExpressionNode;
begin
  result := TMXPathExpressionNode(inherited Link);
end;

procedure TMXPathExpressionNode.SetGroup(const Value: TMXPathExpressionNode);
begin
  FGroup.Free;
  FGroup := Value;
end;

procedure TMXPathExpressionNode.SetNext(const Value: TMXPathExpressionNode);
begin
  FNext.Free;
  FNext := Value;
end;

procedure TMXPathExpressionNode.SetNextOp(const Value: TMXPathExpressionNode);
begin
  FNextOp.Free;
  FNextOp := Value;
end;

function TMXPathExpressionNode.ToString: String;
var
  b : TStringBuilder;
  first : boolean;
  p : TMXPathExpressionNode;
begin
  b := TStringBuilder.create;
  try
    if (FAxis <> axisChild) and (FNodeType <> xentGroup) then
    begin
      b.Append(AXIS_NAMES[FAxis]);
      b.Append('::');
    end;
    case FNodeType of
      xentName: b.Append(value);
      xentFunction:
        begin
        b.Append(value);
        b.Append('(');
        first := true;
        for p in Params do
        begin
          if first then first := false else b.Append(', ');
          b.Append(p.ToString);
        end;
        b.Append(')');
        end;
      xentConstant:
        if (value = 'true') then
          b.Append('true()')
        else if (value = 'false') then
          b.Append('false()')
        else if StringIsInteger32(value) then
          b.Append(value)
        else
        begin
          b.Append('''');
          b.Append(value);
          b.Append('''');
        end;
      xentGroup:
        begin
        b.Append('(');
        b.Append(Group.ToString);
        b.Append(')');
        end;
      xentRoot: b.Append('/');
      xentIterator: b.Append('..todo..');
      xentVariable:
        begin
        b.Append('$');
        b.Append(value);
        end;
    end;
    for p in filters do
    begin
      b.Append('[');
      b.Append(p.ToString);
      b.Append(']');
    end;
    if next <> nil then
      b.Append('/'+next.ToString);
    if NextOp <> nil then
      b.Append(' '+Operation_CODES[FOp]+' '+ NextOp.ToString);
    result := b.toString;
  finally
    b.Free;
  end;
end;

function TMXPathExpressionNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, FFilters.sizeInBytes);
  inc(result, FNext.sizeInBytes);
  inc(result, FNextOp.sizeInBytes);
  inc(result, FGroup.sizeInBytes);
  inc(result, FParams.sizeInBytes);
end;

{ TMXmlDocument }

function TMXmlDocument.contains(list: TFslList<TMXmlNode>; item: TMXmlNode): boolean;
var
  n : TMXmlNode;
begin
  result := false;
  for n in list do
    if n.equal(item) then
      exit(true);
end;

constructor TMXmlDocument.Create;
begin
  inherited Create(ntDocument);
  FNamespaceAbbreviations := TFslStringDictionary.create;
end;

constructor TMXmlDocument.Create(nodeType: TMXmlElementType; name: String);
begin
  inherited create(ntDocument);
  addElement(name);
end;

destructor TMXmlDocument.Destroy;
begin
  FNamespaceAbbreviations.Free;
  inherited;
end;

function TMXmlDocument.evaluate(expr: TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  fwork, work, work2 : TFslList<TMXmlNode>;
  item : TMXmlNode;
  list : TFslList<TMXmlNode>;
  next, last : TMXPathExpressionNode;
  i : integer;
  f : TMXPathExpressionNode;
begin
  work := TFslList<TMXmlNode>.create;
  fwork := TFslList<TMXmlNode>.create;
  list := TFslList<TMXmlNode>.create;
  try
    for item in focus do
    begin
      fwork.Clear;
      case expr.NodeType of
        xentName : evaluateName(expr, atEntry, item, fwork);
        xentFunction : evaluateFunction(expr, atEntry, variables, position, item, fwork);
        xentConstant : fwork.add(expr.buildConstant);
        xentGroup :
          begin
            list.Clear;
            list.Add(item.Link);
            work2 := evaluate(expr.Group, atEntry, variables, position, list);
            try
              fwork.AddAll(work2);
            finally
              work2.Free;
            end;
          end;
        xentRoot : fwork.add(self.link);
        xentIterator : evaluateIterator(expr, atEntry, variables, position, focus, fwork);
        xentVariable : raise EXmlTodo.create('TMXmlDocument.evaluate');
      else
        raise EXmlException.Create('Unsupported Expression Type');
      end;
      if expr.hasFilters then
      begin
        for f in expr.filters do
        begin
          list.Clear;
          for i := 0 to fwork.Count - 1 do
            if passesFilter(i, atEntry, variables, i+1, fwork[i], f) then
              list.add(fwork[i].link);
          fwork.clear;
          fwork.addAll(list);
        end;
      end;
      work.AddAll(fwork);
    end;

    if expr.next <> nil then
    begin
      result := evaluate(expr.next, false, variables, 0, work);
      work.free;
      work := result;
    end;

    if (expr.Op <> xeoNull) then
    begin
      next := expr.NextOp;
      last := expr;
      while (next <> nil) do
      begin
        // and and or - may be able to avoid executing the right side
        work2 := preOperate(work, last.Op);
        if work2 <> nil then
        begin
          work.Free;
          work := work2;
        end
        else
        begin
          work2 := evaluate(next, atEntry, variables, position, focus);
          try
            result := operate(work, last.Op, work2);
            work.Free;
            work := result;
          finally
            work2.Free;
          end;
        end;
        last := next;
        next := next.NextOp;
      end;
    end;
    result := work.link;
  finally
    work.Free;
    list.free;
    fwork.Free;
  end;
end;

function TMXmlDocument.evaluate(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position: integer; item: TMXmlNode): TFslList<TMXmlNode>;
var
  work : TFslList<TMXmlNode>;
begin
  work := TFslList<TMXmlNode>.create;
  try
    work.add(item.Link);
    result := evaluate(expr, atEntry, variables, position, work);
  finally
    work.Free;
  end;
end;

function TMXmlDocument.evaluateBoolean(nodes: TFslList<TMXmlNode>): boolean;
begin
  if (nodes = nil) or (nodes.Empty) then
    result := false
  else if (nodes.Count = 1) then
  begin
    if nodes[0] is TMXmlBoolean then
      result := TMXmlBoolean(nodes[0]).value
    else if nodes[0] is TMXmlNumber then
      result := TMXmlNumber(nodes[0]).value > 0
    else
      result := true;
  end
  else
    result := true;
end;

procedure TMXmlDocument.evaluateFunction(expr: TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; item : TMXmlNode; work: TFslList<TMXmlNode>);
begin
  case StringArrayIndexOfSensitive(['exists', 'count', 'not', 'text', 'comment', 'distinct-values', 'position',
      'starts-with', 'local-name', 'true', 'false', 'contains', 'concat', 'string', 'number', 'name', 'matches',
      'substring-after', 'normalize-space', 'translate'], expr.value) of
    0: work.Add(funcExists(expr, atEntry, variables, position, item));
    1: work.Add(funcCount(expr, atEntry, variables, position, item));
    2: work.Add(funcNot(expr, atEntry, variables, position, item));
    3: funcText(expr, item, work);
    4: funcComment(item, work);
    5: funcDistinctValues(expr, atEntry, variables, position, item, work);
    6: funcPosition(position, item, work);
    7: funcStartsWith(expr, atEntry, variables, position, item, work);
    8: funcLocalName(item, work);
    9: work.Add(TMXmlBoolean.Create(true));
    10: work.Add(TMXmlBoolean.Create(false));
    11: work.Add(funcContains(expr, atEntry, variables, position, item));
    12: work.Add(funcConcat(expr, atEntry, variables, position, item));
    13: work.Add(funcString(expr, atEntry, variables, position, item));
    14: work.Add(funcNumber(expr, atEntry, variables, position, item));
    15: funcName(item, work);
    16: funcMatches(expr, atEntry, variables, position, item, work);
    17: funcSubStringAfter(expr, atEntry, variables, position, item, work);
    18: work.Add(funcNormalizeSpace(expr, atEntry, variables, position, item));
    19: work.Add(funcTranslate(expr, atEntry, variables, position, item));
  else
    raise EXmlException.create('The function "'+expr.FValue+'" is not supported');
  end;
end;

procedure TMXmlDocument.evaluateIterator(expr: TMXPathExpressionNode; atEntry: boolean; variables : TXPathVariables; position : integer; focus, work: TFslList<TMXmlNode>);
var
  list, res : TFslList<TMXmlNode>;
  vars : TXPathVariables;
  n : TMXmlNode;
begin
  vars := TXPathVariables.create;
  try
    list := evaluate(expr.Params[0], atEntry, variables, position, focus);
    try
      for n in list do
      begin
        vars := variables.add(expr.value, n);
        try
          res := evaluate(expr.Params[1], atEntry, vars, position, focus);
          try
            work.AddAll(res);
          finally
            res.Free;
          end;
        finally
          vars.Free;
        end;
      end;
    finally
      list.Free;
    end;
  finally
    vars.Free;
  end;
end;

function TMXmlDocument.findLocation(loc : TSourceLocation) : TFslList<TMXmlNamedNode>;
begin
  result := TFslList<TMXmlNamedNode>.create;
  try
    docElement.findLocation(loc, result);
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.describePath(path : TFslList<TMXmlNamedNode>) : string;
var
  n : TMXmlNamedNode;
  e : TMXmlElement;
begin
  result := '';
  for n in path do
  begin
    if n is TMXmlElement then
    begin
      e := n as TMXmlElement;
      case e.NodeType of
        ntDocument, ntElement : result := result + '/' + n.FName;
        ntText : result := result + '/text()';
        ntComment : result := result + '/comment()';
        ntAttribute : result := result + '/@' + n.FName;
        ntProcessingInstruction : result := result + '/pi()';
        ntDocumentDeclaration : result := result + '/decl()';
        ntCData : result := result + '/cdata()';
      end
    end
    else if n is TMXmlAttribute then
      result := result + '/@' + n.FName
    else
      raise Exception.create('unexpected');
  end;
end;

function TMXmlDocument.funcCount(expr: TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  work : TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for count: expected 1 but found '+inttostr(expr.Params.Count));
  work := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    result := TMXmlNumber.create(work.Count);
  finally
    work.Free;
  end;
end;

procedure TMXmlDocument.funcDistinctValues(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
var
  i, j : integer;
  matched : boolean;
  list : TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for distinct-values: expected 1 but found '+inttostr(expr.Params.Count));
  list := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    for i := 0 to list.Count - 1 do
    begin
      matched := false;
      for j := 0 to i - 1 do
        if list[i].equal(list[j]) then
        begin
          matched := true;
          break;
        end;
      if not matched then
        work.Add(list[i].Link);
    end;
  finally
    list.Free;
  end;
end;

function TMXmlDocument.opEqual(left, right : TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  i : integer;
begin
  if left.Count <> right.Count then
    exit(list(false));
  for i := 0 to left.Count - 1 do
    if not left[i].equal(right[i]) then
      exit(list(false));
  result := list(true);
end;

function TMXmlDocument.funcExists(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  work : TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for exists: expected 1 but found '+inttostr(expr.Params.Count));
  work := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    result := TMXmlBoolean.create(not work.Empty);
  finally
    work.Free;
  end;
end;

procedure TMXmlDocument.funcLocalName(focus: TMXmlNode; work: TFslList<TMXmlNode>);
begin
  if focus is TMXmlNamedNode then
    work.Add(TMXmlString.Create(TMXmlNamedNode(focus).LocalName))
  else
    work.Add(TMXmlString.Create(''))
end;

procedure TMXmlDocument.funcMatches(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
var
  p1, p2 : TFslList<TMXmlNode>;
  reg : TRegEx;
begin
  if expr.Params.Count <> 2 then
    raise EXmlException.Create('Wrong number of parameters for starts-with: expected 2 but found '+inttostr(expr.Params.Count));
  p1 := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    p2 := evaluate(expr.Params[1], atEntry, variables, position, focus);
    try
      if (p1.Count <> 1) or (p2.Count <> 1) then
        work.Add(TMXmlBoolean.Create(false))
      else
      begin
        reg := TRegEx.create(p2[0].ToString, [roCompiled]);
        work.Add(TMXmlBoolean.Create(reg.IsMatch(p1[0].ToString)));
      end;
    finally
      p2.free;
    end;
  finally
    p1.Free;
  end;

end;

procedure TMXmlDocument.funcName(focus: TMXmlNode; work: TFslList<TMXmlNode>);
begin
  if focus is TMXmlNamedNode then
    work.Add(TMXmlString.Create(TMXmlNamedNode(focus).Name))
  else
    work.Add(TMXmlString.Create(''))
end;


function TMXmlDocument.funcNormalizeSpace(expr : TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus : TMXmlNode) : TMXmlNode;
var
  b : TStringBuilder;
  n : TMXmlNode;
  ch : char;
  ws : boolean;
  work: TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for normalize-space: expected at least 1 but found '+inttostr(expr.Params.Count));
  b := TStringBuilder.Create;
  try
    work := evaluate(expr.Params[0], atEntry, variables, position, focus);
    try
      ws := false;
      for n in work do
      begin
        for ch in n.ToString do
        begin
          if ch.IsWhiteSpace then
          begin
            if not ws then
            begin
              ws := true;
              b.Append(' ');
            end;
          end
          else
          begin
            b.Append(ch);
            ws := false;
          end;
        end;
      end;
    finally
      work.Free;
    end;
    result := TMXmlString.Create(b.ToString.Trim);
  finally
    b.Free;
  end;
end;

function TMXmlDocument.funcNot(expr: TMXPathExpressionNode; atEntry : boolean; variables : TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  work : TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for not: expected 1 but found '+inttostr(expr.Params.Count));
  work := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    result := TMXmlBoolean.create(not evaluateBoolean(work));
  finally
    work.Free;
  end;
end;


function TMXmlDocument.funcNumber(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  b : TStringBuilder;
  work: TFslList<TMXmlNode>;
  n : TMXmlNode;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for number: expected 1 but found 0');
  b := TStringBuilder.Create;
  try
    work := evaluate(expr.Params[0], atEntry, variables, position, focus);
    try
      for n in work do
        b.Append(n.ToString);
    finally
      work.Free;
    end;
    if not StringIsInteger32(b.ToString) then
      raise EXmlException.Create('The string "'+b.toString+'" is not a valid number');
    result := TMXmlNumber.Create(StringToInteger32(b.ToString));
  finally
    b.Free;
  end;

end;

procedure TMXmlDocument.funcPosition(position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
begin
  work.Add(TMXmlNumber.Create(position));
end;

procedure TMXmlDocument.funcStartsWith(expr : TMXPathExpressionNode; atEntry : boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
var
  p1, p2 : TFslList<TMXmlNode>;
begin
  if expr.Params.Count <> 2 then
    raise EXmlException.Create('Wrong number of parameters for starts-with: expected 2 but found '+inttostr(expr.Params.Count));
  p1 := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    p2 := evaluate(expr.Params[1], atEntry, variables, position, focus);
    try
      if (p1.Count <> 1) or (p2.Count <> 1) then
        work.Add(TMXmlBoolean.Create(false))
      else
        work.Add(TMXmlBoolean.Create(p1[0].ToString.StartsWith(p2[0].ToString)));
    finally
      p2.free;
    end;
  finally
    p1.Free;
  end;
end;

function TMXmlDocument.funcString(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  b : TStringBuilder;
  work: TFslList<TMXmlNode>;
  n : TMXmlNode;
begin
  if expr.Params.Count <> 1 then
    raise EXmlException.Create('Wrong number of parameters for string: expected 1 but found 0');
  b := TStringBuilder.Create;
  try
    work := evaluate(expr.Params[0], atEntry, variables, position, focus);
    try
      for n in work do
        b.Append(n.ToString);
    finally
      work.Free;
    end;
    result := TMXmlString.Create(b.ToString);
  finally
    b.Free;
  end;
end;

procedure TMXmlDocument.funcSubStringAfter(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode; work: TFslList<TMXmlNode>);
var
  p1, p2 : TFslList<TMXmlNode>;
  s1, s2 : String;
begin
  if expr.Params.Count <> 2 then
    raise EXmlException.Create('Wrong number of parameters for starts-with: expected 2 but found '+inttostr(expr.Params.Count));
  p1 := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    p2 := evaluate(expr.Params[1], atEntry, variables, position, focus);
    try
      if (p1.Count <> 1) or (p2.Count <> 1) then
        work.Add(TMXmlBoolean.Create(false))
      else
      begin
        s1 := p1[0].ToString;
        s2 := p2[0].ToString;
        if (s1.Contains(s2)) then
          work.Add(TMXmlString.Create(s1.Substring(s1.IndexOf(s2)+length(s2))))
        else
          work.Add(TMXmlString.Create(s1))
      end;
    finally
      p2.free;
    end;
  finally
    p1.Free;
  end;
end;

procedure TMXmlDocument.funcText(expr: TMXPathExpressionNode; focus: TMXmlNode; work: TFslList<TMXmlNode>);
begin
  iterate(focus, expr.axis, work, nil, funcTextTest);
end;

function TMXmlDocument.funcTextTest(Context: TObject; node: TMXmlNamedNode): boolean;
begin
  result := node.NodeType = ntText;
end;

function StringTranslate(s1,s2,s3 : string) : String;
var
  b : TStringBuilder;
  ch : char;
begin
  b := TStringBuilder.Create;
  try
    if (s3.Length > 1) then
      raise EXmlException.Create('3rd parameter to translate has more than a single character');
    for ch in s1 do
      if s2.Contains(ch) then
        b.Append(s3[1])
      else
        b.Append(ch);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TMXmlDocument.funcTranslate(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  p1, p2, p3 : TFslList<TMXmlNode>;
begin
  result := nil;
  if expr.Params.Count <> 3 then
    raise EXmlException.Create('Wrong number of parameters for translate: expected 3 but found '+inttostr(expr.Params.Count));
  p1 := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    p2 := evaluate(expr.Params[1], atEntry, variables, position, focus);
    try
      p3 := evaluate(expr.Params[2], atEntry, variables, position, focus);
      try
        if (p2.Count = 1) and (p1.Count = 1) and (p3.Count = 1) then
          result := TMXmlString.Create(StringTranslate(p1[0].toString, p2[0].toString, p3[0].toString));
      finally
        p3.Free;
      end;
    finally
      p2.free;
    end;
  finally
    p1.Free;
  end;
end;

function TMXmlDocument.GetDocElement: TMXmlElement;
var
  t : TMXmlElement;
begin
  result := nil;
  for t in Children do
    if t.NodeType = ntElement then
      exit(t);
end;

procedure TMXmlDocument.funcComment(focus: TMXmlNode; work: TFslList<TMXmlNode>);
var
  child : TMXmlElement;
begin
  if (focus is TMXmlElement) and (TMXmlElement(focus).NodeType = ntElement) then
    for child in TMXmlElement(focus).Children do
      if child.NodeType = ntComment then
        work.Add(child.Link);
end;

function TMXmlDocument.funcConcat(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus : TMXmlNode) : TMXmlNode;
var
  b : TStringBuilder;
  p : TMXPathExpressionNode;
  work: TFslList<TMXmlNode>;
  n : TMXmlNode;
begin
  if expr.Params.Count = 0 then
    raise EXmlException.Create('Wrong number of parameters for concat: expected at least 1 but found 0');
  b := TStringBuilder.Create;
  try
    for p in expr.Params do
    begin
      work := evaluate(p, atEntry, variables, position, focus);
      try
        for n in work do
          b.Append(n.ToString);
      finally
        work.Free;
      end;
    end;
    result := TMXmlString.Create(b.ToString);
  finally
    b.Free;
  end;
end;

function TMXmlDocument.funcContains(expr: TMXPathExpressionNode; atEntry: boolean; variables: TXPathVariables; position : integer; focus: TMXmlNode): TMXmlNode;
var
  p1, p2 : TFslList<TMXmlNode>;
  n : TMXmlNode;
begin
  if expr.Params.Count <> 2 then
    raise EXmlException.Create('Wrong number of parameters for contains: expected 2 but found '+inttostr(expr.Params.Count));
  p1 := evaluate(expr.Params[0], atEntry, variables, position, focus);
  try
    p2 := evaluate(expr.Params[1], atEntry, variables, position, focus);
    try
      if (p2.Count <> 1) then
        result := TMXmlBoolean.Create(false)
      else
      begin
        for n in p1 do
          if n.equal(p2[0]) then
            exit(TMXmlBoolean.Create(true));
        result := TMXmlBoolean.Create(false)
      end;
    finally
      p2.free;
    end;
  finally
    p1.Free;
  end;
end;

function TMXmlDocument.list(value: boolean): TFslList<TMXmlNode>;
begin
  result := TFslList<TMXmlNode>.create;
  result.Add(TMXmlBoolean.Create(value));
end;


function TMXmlDocument.passesFilter(index: integer; atEntry : boolean; variables : TXPathVariables; position : integer; item: TMXmlNode; filter: TMXPathExpressionNode): boolean;
var
  focus, list : TFslList<TMXmlNode>;
begin
  if (filter.NodeType = xentConstant) and StringIsInteger32(filter.value) and (filter.FOp = xeoNull) then
    result := StringToInteger32(filter.value) = position
  else
  begin
    focus := TFslList<TMXmlNode>.create;
    try
      focus.Add(item.Link);
      list := evaluate(filter, atEntry, variables, position, focus);
      try
        result := evaluateBoolean(list);
      finally
        list.Free;
      end;
    finally
      focus.Free;
    end;
  end;
end;

function isBoolean(list : TFslList<TMXmlNode>; b : boolean) : boolean;
begin
  result := (list.count = 1) and (list[0] is TMXmlBoolean) and (TMXmlBoolean(list[0]).value = b);
end;


function TMXmlDocument.preOperate(left: TFslList<TMXmlNode>; op: TMXPathExpressionOperation): TFslList<TMXmlNode>;
begin
  result := nil;
  case op of
    xeoAnd: if isBoolean(left, false) then
        result := list(false);
    xeoOr: if isBoolean(left, true) then
        result := list(true);
  end;
end;

function TMXmlDocument.opAnd(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Empty) and (right.empty) then
      // nothing
    else if (isBoolean(left, false)) or (isBoolean(right, false)) then
      result.Add(TMXmlBoolean.Create(false))
    else if (left.Empty) or (right.Empty) then
      // nothing
    else if (evaluateBoolean(left)) and (evaluateBoolean(right)) then
      result.Add(TMXmlBoolean.Create(true))
    else
      result.Add(TMXmlBoolean.Create(false));
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.operate(left: TFslList<TMXmlNode>; op: TMXPathExpressionOperation; right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
begin
  case op of
    xeoEquals: result := opEqual(left, right);
    xeoPlus: result := opPlus(left, right);
    xeoMinus: result := opMinus(left, right);
    xeoAnd: result := opAnd(left, right);
    xeoOr: result := opOr(left, right);
    xeoGreaterThan: result := opGreaterThan(left, right);
    xeoGreaterEquals: result := opGreaterThanEqual(left, right);
    xeoNotEquals: result := opNotEqual(left, right);
    xeoUnion: result := opUnion(left, right);
    xeoLessThan: result := opLessThan(left, right);
    xeoLessEquals: result := opLessThanEqual(left, right);
    xeoSequence: raise EXmlException.create('Operation , not done yet');
  else
    raise EXmlException.create('Unknown operation (internal error');
  end;
end;

function TMXmlDocument.opGreaterThan(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr: String;
  o : boolean;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        o := StringToInteger32(sl) > StringToInteger32(sr)
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        o := TFslDecimal.ValueOf(sl).Compares(TFslDecimal.ValueOf(sr)) > 0
      else
        o := sl > sr;
      result.Add(TMXmlBoolean.Create(o));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opGreaterThanEqual(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr: String;
  o : boolean;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        o := StringToInteger32(sl) >= StringToInteger32(sr)
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        o := TFslDecimal.ValueOf(sl).Compares(TFslDecimal.ValueOf(sr)) >= 0
      else
        o := sl >= sr;
      result.Add(TMXmlBoolean.Create(o));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opLessThan(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr: String;
  o : boolean;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        o := StringToInteger32(sl) < StringToInteger32(sr)
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        o := TFslDecimal.ValueOf(sl).Compares(TFslDecimal.ValueOf(sr)) < 0
      else
        o := sl < sr;
      result.Add(TMXmlBoolean.Create(o));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opLessThanEqual(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr: String;
  o : boolean;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        o := StringToInteger32(sl) <= StringToInteger32(sr)
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        o := TFslDecimal.ValueOf(sl).Compares(TFslDecimal.ValueOf(sr)) <= 0
      else
        o := sl <= sr;
      result.Add(TMXmlBoolean.Create(o));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opMinus(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr, so: String;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        so := IntToStr(StringToInteger32(sl) - StringToInteger32(sr))
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        so := TFslDecimal.ValueOf(sl).Subtract(TFslDecimal.ValueOf(sr)).AsString
      else
        so := sl.Replace(sr, '');
    end;
    result.Add(TMXmlString.Create(so));
    result.link;
  finally
    result.free;
  end;

end;

function TMXmlDocument.opNotEqual(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  i : integer;
begin
  if left.Count <> right.Count then
    exit(list(true));
  for i := 0 to left.Count - 1 do
    if not left[i].equal(right[i]) then
      exit(list(true));
  result := list(false);
end;

function TMXmlDocument.opOr(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Empty) and (right.Empty) then
      // nothing
    else if (evaluateBoolean(left)) or (evaluateBoolean(right)) then
      result.Add(TMxmlBoolean.Create(true))
    else if (left.Empty) or (right.Empty) then
      // nothing
    else
      result.Add(TMXmlBoolean.Create(false));
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opPlus(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  sl, sr, so: String;
begin
  result := TFslList<TMXmlNode>.Create;
  try
    if (left.Count <> 1) or (right.Count <> 1) then
      // nothing
    else
    begin
      sl := evaluateString(left);
      sr := evaluateString(right);
      if StringIsInteger32(sl) and StringIsInteger32(sr) then
        so := IntToStr(StringToInteger32(sl) + StringToInteger32(sr))
      else if StringIsDecimal(sl) and StringIsDecimal(sr) then
        so := TFslDecimal.ValueOf(sl).Add(TFslDecimal.ValueOf(sr)).AsString
      else
        so := sl+sr;
    end;
    result.Add(TMXmlString.Create(so));
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.opUnion(left, right: TFslList<TMXmlNode>): TFslList<TMXmlNode>;
var
  item : TMXmlNode;
begin
  result := TFslList<TMXmlNode>.create;
  try
    for item in left do
      if not contains(result, item) then
        result.Add(item.link);
    for item in right do
      if not contains(result, item) then
        result.Add(item.link);
    result.link;
  finally
    result.Free;
  end;
end;

procedure TMXmlDocument.iterate(node : TMXMLNode; axis : TMXPathExpressionAxis; list : TFslList<TMXmlNode>; context : TObject; event : TMXMLIterationEvent);
var
  count : integer;
  procedure iterateChildren(children : TFslList<TMXmlElement>; recurse : boolean);
  var
    e : TMXmlElement;
  begin
    for e in children do
    begin
      if event(context, e) then
      begin
        inc(count);
        // todo: check filter
        list.Add(e.Link);
      end;
      if recurse and e.HasChildren then
        iterateChildren(e.Children, true);
    end;
end;
var
  a : TMXmlAttribute;
begin
  count := 0;
  case axis of
    axisSelf: raise EXmlTodo.create('TMXmlDocument.iterate#1');
    axisChild:
      if node is TMXmlElement then
        iterateChildren(TMXmlElement(node).Children, false);
    axisDescendants:
      if node is TMXmlElement then
        iterateChildren(TMXmlElement(node).Children, true);
    axisDescendantsAndSelf:
      if node is TMXmlElement then
      begin
        if event(context, TMXmlElement(node)) then
          list.Add(node.Link);
        iterateChildren(TMXmlElement(node).Children, true);
      end;
    axisAttribute:
      if node is TMXmlElement then
        for a in TMXmlElement(node).Attributes do
          if event(context, a) then
            list.Add(a.Link);

    axisNamespace: raise EXmlTodo.create('TMXmlDocument.iterate#2');
    axisParent: raise EXmlTodo.create('TMXmlDocument.iterate#3');
    axisAncestor: raise EXmlTodo.create('TMXmlDocument.iterate#4');
    axisAncestorOrSelf: raise EXmlTodo.create('TMXmlDocument.iterate#5');
    axisFollowing: raise EXmlTodo.create('TMXmlDocument.iterate#6');
    axisFollowingSibling: raise EXmlTodo.create('TMXmlDocument.iterate#7');
    axisPreceding: raise EXmlTodo.create('TMXmlDocument.iterate#8');
    axisPrecedingSibling: raise EXmlTodo.create('TMXmlDocument.iterate#9');
  else
    raise EXmlException.Create('unknown xpath axis')
  end;
end;

function TMXmlDocument.evaluateNameForNode(Context : TObject; node : TMXmlNamedNode) : boolean;
var
  expr : TMXPathExpressionNode;
  p : TArray<String>;
  ns : String;
begin
  expr := Context as TMXPathExpressionNode;
  result := false;
  if (node.name <> '') then
  begin
    if (expr.value = '*') then
      result := true
    else if (node.name = expr.value) and (node.NamespaceURI = '') then
      result := true
    else if (node.name <> '') and expr.value.Contains(':') then
    begin
      p := expr.value.Split([':']);
      if NamespaceAbbreviations.TryGetValue(p[0], ns) then
        result := (node.NamespaceURI = ns) and ((node.localName = p[1]) or (p[1]= '*'))
      else
        result := false;
    end;
  end;
end;

function TMXmlDocument.evaluateString(nodes: TFslList<TMXmlNode>): String;
var
  i : integer;
begin
  if (nodes = nil) or (nodes.Empty) then
    result := ''
  else
  begin
    result := nodes[0].ToString;
    if (nodes.Count > 1) then
      for i := 1 to nodes.Count - 1 do
      begin
        result := result +','+nodes[i].ToString;
      end;
  end;
end;

procedure TMXmlDocument.evaluateName(expr: TMXPathExpressionNode; atEntry : boolean; item: TMXmlNode; focus: TFslList<TMXmlNode>);
begin
  iterate(item, expr.FAxis, focus, expr, evaluateNameForNode);
end;
//    procedure (expr: TMXPathExpressionNode; atEntry : boolean; item : TMXmlNode; focus: TFslList<TMXmlNode>); overload;
{function TMXmlDocument.matchesXPathName(name, namespace, localName, pattern: String; wildcards : boolean): boolean;
var
begin
end;
}
//  procedure checkChildren(children : TFslList<TMXmlElement>; recurse : boolean);
//  var
//    c : TMXmlElement;
//  begin
//    for c in Children do
//      if c.NodeType = ntElement then
//      begin
//        if matchesXPathName(c.Name, c.NamespaceURI, c.LocalName, expr.value, true) then
//          focus.Add(c.Link);
//        if (recurse and c.HasChildren) then
//          checkChildren(c.Children, true);
//      end;
//  end;
//var
//  e : TMXmlElement;
//  a : TMXmlAttribute;
//  s : String;
//  if item is TMXmlElement then
//  begin
//
//    end;
//    if expr.value = '.' then
//      focus.Add(item.Link)
//    else if (expr.value[1] = '@') then
//    begin
//      if TMXmlElement(item).HasAttributes then
//        for s in TMXmlElement(item).Attributes.Keys do
//        begin
//          a := TMXmlElement(item).Attributes[s];
//          if matchesXPathName(s, a.NamespaceURI, a.LocalName, expr.value.Substring(1), true) then
//            focus.Add(a.Link);
//        end;
//    end
//    else if expr.value.Contains('**') then
//    begin
//      if TMXmlElement(item).HasChildren then
//        checkChildren(TMXmlElement(item).Children, true);
//    end
//    else
//    begin
//      e := TMXmlElement(item);
//      if atEntry and matchesXPathName(e.Name, e.NamespaceURI, e.LocalName, expr.value, false) then
//        focus.Add(item.Link);
//      if not (atEntry and matchesXPathName(e.Name, e.NamespaceURI, e.LocalName, expr.value, false)) and e.HasChildren then
//        checkChildren(e.Children, false);
//    end;
//  end
//  else
//    raise EXmlTodo.create();

function TMXmlDocument.select(xpath: String; focus: TMXmlElement): TFslList<TMXmlNode>;
var
  expr : TMXPathExpressionNode;
begin
  expr := TMXmlParser.parseXPath(xpath);
  try
    result := select(expr, focus);
  finally
    expr.Free;
  end;
end;

function TMXmlDocument.selectElements(xpath: String; focus: TMXmlElement): TFslList<TMXmlElement>;
var
  list : TFslList<TMXmlNode>;
  item : TMXmlNode;
begin
  result := TFslList<TMXmlElement>.create;
  try
    list := select(xpath, focus);
    try
      for item in list do
        if item is TMXmlElement then
          result.Add(TMXmlElement(item).Link)
        else
          raise EXmlException.create('Error Message');
    finally
      list.Free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TMXmlDocument.select(xpath: TMXPathExpressionNode; focus: TMXmlElement): TFslList<TMXmlNode>;
var
  work : TFslList<TMXmlNode>;
  variables : TXPathVariables;
begin
  if NO_SELECT then
    exit(TFslList<TMXmlNode>.create);

  variables := TXPathVariables.Create('%current', focus.Link);
  try
    work := TFslList<TMXmlNode>.create;
    try
      work.Add(focus.Link);
      result := evaluate(xpath, true, variables, 0, work);
    finally
      work.Free;
    end;
  finally
    variables.Free;
  end;
end;

function TMXmlDocument.selectElements(xpath: TMXPathExpressionNode; focus: TMXmlElement): TFslList<TMXmlElement>;
var
  list : TFslList<TMXmlNode>;
  item : TMXmlNode;
begin
  result := TFslList<TMXmlElement>.create;
  try
    list := select(xpath, focus);
    try
      for item in list do
        if item is TMXmlElement then
          result.Add(TMXmlElement(item).Link)
        else
          raise EXmlException.create('Error Message');
    finally
      list.Free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

procedure TMXmlDocument.ToXml(stream: TFslStream; pretty, xmlHeader: boolean);
var
  s : String;
  b : TBytes;
begin
  s := ToXml(pretty, xmlheader);
  b := TEncoding.UTF8.GetBytes(s);
  stream.Write(b, length(b));
end;

procedure TMXmlDocument.ToXml(stream: TStream; pretty, xmlHeader: boolean);
var
  s : String;
  b : TBytes;
begin
  s := ToXml(pretty, xmlheader);
  b := TEncoding.UTF8.GetBytes(s);
  stream.Write(b, length(b));
end;

function TMXmlDocument.ToXml(pretty, xmlHeader: boolean): String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.create;
  try
    if xmlHeader then
    begin
      b.Append('<?xml version="1.0" encoding="UTF-8"?>');
      if pretty then
        b.Append(#13#10);
    end;
    writeToXml(b, pretty, 0);
    result := b.toString();
  finally
    b.Free;
  end;
end;

constructor TMXmlDocument.CreateNS(nodeType: TMXmlElementType; ns, local: String);
begin
  inherited create(ntDocument);
  addElementNS(ns, local);
end;

function TMXmlDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FNamespaceAbbreviations.sizeInBytes);
end;

{ TMXmlNode }

function TMXmlNode.equal(other: TMXmlNode): boolean;
begin
  result := other = self;
end;

function TMXmlNode.link: TMXmlNode;
begin
  result := TMXmlNode(inherited Link);
end;

{ TMXmlBoolean }

constructor TMXmlBoolean.Create(value: boolean);
begin
  inherited Create;
  FValue := value;
end;

function TMXmlBoolean.ToString: String;
begin
  result := BooleanToString(value);
end;

function TMXmlBoolean.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TMXmlNumber }

constructor TMXmlNumber.Create(value: integer);
begin
  inherited Create;
  FValue := value;
end;

function TMXmlNumber.ToString: String;
begin
  result := IntToStr(value);
end;

function TMXmlNumber.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TXPathVariables }

constructor TXPathVariables.Create;
begin
  inherited;
  FMap := TFslMap<TMXmlNode>.create('xpath vars');
end;

constructor TXPathVariables.Create(name: String; value: TMXmlNode);
begin
  Create;
  FMap.AddOrSetValue(name, value);
end;

destructor TXPathVariables.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TXPathVariables.get(name: string): TMXmlNode;
begin
  if not FMap.TryGetValue(name, result) then
    result := nil;
end;

function TXPathVariables.link: TXPathVariables;
begin
  result := TXPathVariables(inherited link);
end;

function TXPathVariables.add(name: String; value: TMXmlNode): TXPathVariables;
begin
  if FMap.ContainsKey(name) and not name.StartsWith('%') then
    raise EXmlException.create('Nested re-use of variable name '+name);
  result := TXPathVariables.Create;
  try
    result.FMap.addAll(FMap);
    result.FMap.AddOrSetValue(name, value.Link);
    result.link;
  finally
    result.Free;
  end;
end;

function TXPathVariables.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMap.sizeInBytes);
end;

{ TMXmlString }

constructor TMXmlString.Create(value: String);
begin
  inherited Create;
  FValue := value;
end;

function TMXmlString.ToString: String;
begin
  result := FValue;
end;

function TMXmlString.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TMXmlPrimitive }

function TMXmlPrimitive.equal(other: TMXmlNode): boolean;
begin
  result := other.ToString = toString;
end;

{ TFslXmlBuilder }

Procedure TFslXmlBuilder.Start;
var
  i: Integer;
begin
  buf := TFslBuffer.Create;
  mem := TFslMemoryStream.Create;
  mem.Buffer := buf.Link;
  xml := TFslXMLFormatter.Create;
  xml.CanonicalEntities := CanonicalEntities;
  xml.HasWhitespace := IsPretty;
  xml.Stream := mem.Link;
  if Canonicalise <> [] then
  begin
    NoHeader := true;
    IsPretty := false;
    CharEncoding := 'UTF-8';
    xml.NoDense := true;
    xml.Attributes.SortedBySortKey;
  end;
  if not NoHeader then
  begin
    xml.AddAttribute('encoding', 'UTF-8');
    xml.ProduceHeader;
  end;
  depth := 0;

  if CurrentNamespaces.DefaultNS <> '' then
    xml.AddNamespace('', CurrentNamespaces.DefaultNS);

  for i := 0 to CurrentNamespaces.Count - 1 do
    xml.AddNamespace(CurrentNamespaces.ValueByIndex[i], CurrentNamespaces.KeyByIndex[i]);
end;

Procedure TFslXmlBuilder.StartFragment;
begin
  raise EXmlException.Create('Not Supported yet');
end;

Procedure TFslXmlBuilder.Finish;
begin
  xml.Free;
  xml := nil;
  mem.Free;
  mem := nil;
end;

function TFslXmlBuilder.getNSRep(uri, name: String): String;
begin
  if (uri = CurrentNamespaces.DefaultNS) then
    result := name
  else if CurrentNamespaces.ExistsByKey(uri) then
    result := CurrentNamespaces.Matches[uri]+':'+name
  else
    raise EXmlException.Create('Unregistered namespace '+uri);
end;

procedure TFslXmlBuilder.inject(const bytes: TBytes);
begin
  xml.ProduceBytes(bytes);
end;

Procedure TFslXmlBuilder.Build(oStream: TStream);
begin
  buf.SaveToStream(oStream);
end;

Procedure TFslXmlBuilder.Build(oStream: TFslStream);
begin
  buf.SaveToStream(oStream);
end;

Function TFslXmlBuilder.Build : String;
begin
  result := buf.AsText;
end;

procedure TFslXmlBuilder.SetCanonicalEntities(const Value: boolean);
begin
  FCanonicalEntities := Value;
  if assigned(xml) then
    xml.CanonicalEntities := value;
end;

Function TFslXmlBuilder.SourceLocation : TSourceLocation;
begin
  result := xml.sourceLocationForPending;
end;

Procedure TFslXmlBuilder.Comment(Const sContent : String);
begin
  if not (xcmCanonicalise in FCanonicalise) or (xcmComments in FCanonicalise) then
  begin
    if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
      xml.Produce(#10);
    xml.ProduceComment(sContent);
    started := true;
  end;
end;


procedure TFslXmlBuilder.defineNS(abbrev, uri: String);
begin
  CurrentNamespaces.Add(uri, abbrev);
end;

destructor TFslXmlBuilder.destroy;
begin
  buf.Free;
  inherited;
end;

procedure TFslXmlBuilder.DocType(sText: String);
begin
  if not (xcmCanonicalise in FCanonicalise) then
    raise EXmlException.Create('Not supported');
end;

Procedure TFslXmlBuilder.AddAttribute(Const sName, sValue : String);
begin
  if (sName = 'xmlns') and CurrentNamespaces.DefaultSet then
  begin
    if sValue <> CurrentNamespaces.DefaultNS then
      raise EXmlException.Create('Namespace mismatch');
    CurrentNamespaces.DefaultSet := false;
  end;

  xml.AddAttribute(sName, sValue);
end;

Procedure TFslXmlBuilder.AddAttributeNS(Const sNamespace, sName, sValue : String);
begin
  xml.AddAttribute(getNSRep(sNamespace, sName), sValue, sNamespace);
end;

function TFslXmlBuilder.Tag(Const sName : String) : TSourceLocation;
begin
  if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
    xml.Produce(#10);
  if CurrentNamespaces.DefaultSet then
  begin
    xml.AddNamespace('', CurrentNamespaces.DefaultNS);
    CurrentNamespaces.DefaultSet := false;
  end;
  if xcmCanonicalise in FCanonicalise then
  begin
    xml.ProduceOpen(sName);
    xml.ProduceClose(sName);
  end
  else
    xml.ProduceTag(sName);
  started := true;
  result := xml.FLocation;
end;

function TFslXmlBuilder.Open(Const sName : String) : TSourceLocation;
begin
  if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
    xml.Produce(#10);
  if CurrentNamespaces.DefaultSet then
  begin
    if not xml.Attributes.ExistsByName('xmlns') then
      xml.AddNamespace('', CurrentNamespaces.DefaultNS)
    else if xml.Attributes.GetByName('xmlns').Value <> CurrentNamespaces.DefaultNS then
       raise EXmlException.Create('XML default namespce misalignment');
    CurrentNamespaces.DefaultSet := false;
  end;
  xml.ProduceOpen(sName);
  inc(depth);
  started := true;
  result := xml.FLocation;
end;

procedure TFslXmlBuilder.ProcessingInstruction(sName, sText: String);
begin
  if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
    xml.Produce(#10);
  xml.ProducePI(sName, sText);
  started := true;
end;

procedure TFslXmlBuilder.CData(text: String);
begin
  if xcmCanonicalise in FCanonicalise then
    xml.ProduceText(text, eolnCanonical)
  else
    xml.produceCData(text)
end;

Procedure TFslXmlBuilder.Close(Const sName : String);
begin
  xml.ProduceClose(sName);
  dec(depth);
end;

function HtmlTrim(s: String):String;
begin
  if s = '' then
    result := ''
  else
  begin
    result := StringTrimSet(s, [' ', #13, #10, #9]);
    if ((result = '') and (s <> '')) or (s.endsWith(' ')) then
      result := result + ' ';
  end;
end;

function TFslXmlBuilder.Text(Const sValue : String) : TSourceLocation;
begin
  if (xcmCanonicalise in FCanonicalise) then
  begin
    if (depth = 0) then
     // ignore
    else if xcmTrimWhitespace in FCanonicalise then
      xml.ProduceText(sValue.Trim, eolnCanonical)
    else
      xml.ProduceText(sValue, eolnCanonical);
  end
  else if IsPretty then
    xml.ProduceText(HtmlTrim(sValue))
  else
    xml.ProduceText(sValue);
  result := xml.FLocation;
end;


function TFslXmlBuilder.Entity(Const sValue : String) : TSourceLocation;
begin
  raise EXmlException.Create('entities not supported');
end;

function TFslXmlBuilder.TagText(Const sName, sValue : String) : TSourceLocation;
begin
  if CurrentNamespaces.DefaultSet then
  begin
    xml.AddNamespace('', CurrentNamespaces.DefaultNS);
    CurrentNamespaces.DefaultSet := false;
  end;

  if IsPretty then
    xml.ProduceText(sName, StringTrimWhitespace(sValue))
  else
    xml.ProduceText(sName, sValue);
  result := xml.FLocation;
end;

function TFslXmlBuilder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, mem.sizeInBytes);
  inc(result, buf.sizeInBytes);
  inc(result, xml.sizeInBytes);
end;

{ TXmlBuilderNamespaceList }

procedure TXmlBuilderNamespaceList.Assign(oObject: TFslObject);
begin
  inherited;

  DefaultNS := TXmlBuilderNamespaceList(oObject).DefaultNS;
  FDefaultSet := false;
end;

function TXmlBuilderNamespaceList.clone: TXmlBuilderNamespaceList;
begin
  result := TXmlBuilderNamespaceList(Inherited Clone);
end;

constructor TXmlBuilderNamespaceList.Create;
begin
  inherited;

end;

destructor TXmlBuilderNamespaceList.Destroy;
begin

  inherited;
end;

function TXmlBuilderNamespaceList.link: TXmlBuilderNamespaceList;
begin
  result := TXmlBuilderNamespaceList(Inherited Link);
end;

procedure TXmlBuilderNamespaceList.SetDefaultNS(const Value: String);
begin
  if FDefaultNS <> Value then
    FDefaultSet := true;
  FDefaultNS := Value;
end;

function TXmlBuilderNamespaceList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDefaultNS.length * sizeof(char)) + 12);
  inc(result, FNew.sizeInBytes);
end;

{ TXmlBuilder }

constructor TXmlBuilder.Create;
begin
  inherited;
  FNamespaces := TFslObjectList.create;
  FNamespaces.Add(TXmlBuilderNamespaceList.Create());
end;

destructor TXmlBuilder.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TXmlBuilder.GetCurrentNamespaces: TXmlBuilderNamespaceList;
begin
  result := FNamespaces[FNamespaces.Count - 1] as TXmlBuilderNamespaceList
end;

procedure TXmlBuilder.NSPop;
begin
  FNamespaces.DeleteByIndex(FNamespaces.Count - 1);
end;

procedure TXmlBuilder.NSPush;
begin
  FNamespaces.Add(CurrentNamespaces.clone);
end;

function TXmlBuilder.getNSAbbrev(iElement: TMXMLElement) : String;
var
  a : TMXmlAttribute;
  i : integer;
begin
  for a in iELement.Attributes do
    if a.Value = iElement.NamespaceURI then
    begin
      if (a.Name = 'xmlns') then
        exit('')
      else if a.Name.StartsWith('xmlns:') then
        exit(a.Name.Substring(6)+':');
    end;

  if CurrentNamespaces.DefaultNS = iElement.NamespaceURI then
    exit('');

  for i := 0 to CurrentNamespaces.Count - 1 do
  begin
    if CurrentNamespaces.Values[i] = iElement.NamespaceURI then
      exit(CurrentNamespaces.Keys[i]+':');
  end;
  if iElement.attribute['xmlns'] = '' then
  begin
    AddAttribute('xmlns', iElement.NamespaceURI);
    CurrentNamespaces.DefaultNS := iElement.NamespaceURI;
    exit('');
  end
  else
  begin
    AddAttribute('xmlns:ns', iElement.NamespaceURI);
    CurrentNamespaces.Add('ns', iElement.NamespaceURI);
    exit('ns:');
  end
end;

procedure TXmlBuilder.WriteXml(iElement: TMXMLElement);
var
  n : string;
  a : TMXmlAttribute;
  c : TMXmlElement;
begin
  n := iElement.name;
  if n = '' then
    n := getNSAbbrev(iElement)+iElement.LocalName;
  for a in iElement.Attributes do
    if a.NamespaceURI <> '' then
      AddAttributeNS(a.NamespaceURI, a.Name, a.Value)
    else
      AddAttribute(a.Name, a.Value);
  Open(n);
  for c in iElement.Children do
    case c.NodeType of
      ntElement: WriteXml(c);
      ntText: Text(c.Text);
      ntComment: Comment(c.Text);
      ntDocument: raise EXmlException.Create('Illegal XML - document inside element');
      ntAttribute: raise EXmlException.Create('Illegal XML - attribute in element chilren');
      ntProcessingInstruction: ProcessingInstruction(c.Name, c.Text);
      ntDocumentDeclaration: raise EXmlException.Create('Illegal DTD not supported');
      ntCData: raise EXmlException.Create('Illegal CDATA not supported');
    end;
  Close(n);
end;

{ TXmlPatchEngine }

class procedure TXmlPatchEngine.execute(doc : TMXmlDocument; target : TMXmlElement; patch: TMXmlElement);
begin
  if doc = nil then
    raise EXmlException.Create('No Target Document Root Found');
  if target = nil then
    raise EXmlException.Create('No Target Element Found');
  if patch = nil then
    raise EXmlException.Create('No Patch Operations Found');
  patch := patch.firstElement;
  if patch = nil then
    raise EXmlException.Create('No Patch Operations Found');

  doc.NamespaceAbbreviations.AddOrSetValue('f', 'http://hl7.org/fhir');
  doc.NamespaceAbbreviations.AddOrSetValue('h', 'http://www.w3.org/1999/xhtml');

  while (patch <> nil) do
  begin
    if (patch.localName = 'remove') then
      remove(doc, patch.attribute['sel'], target)
    else if (patch.localName = 'add') then
      add(doc, patch, target)
    else if (patch.localName = 'replace') then
      replace(doc, patch, target)
    else
      raise EXmlException.Create('Unknown Patch Operation "'+patch.localName+'"');
    patch := patch.nextElement;
  end;
end;

procedure checkEndsWithAttribute(var path, attrName : String);
var
  l, r : String;
begin
  StringSplitRight(path, '/', l, r);
  if (r.StartsWith('@')) then
  begin
    path := l;
    attrName := r.Substring(1);
  end
  else
    attrName := '';
end;

class procedure TXmlPatchEngine.add(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
var
  matches : TFslList<TMXmlNode>;
  elem : TMXmlElement;
  sel, typ, pos : String;
begin
  sel := op.attribute['sel'];
  typ := op.attribute['type'];
  pos := op.attribute['pos'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise EXmlException.Create('No match found for '+sel+' performing addition');
    if matches.count > 1 then
      raise EXmlException.Create('The xpath '+sel+' matched multiple nodes performing addition');

    if typ = '' then
      addChildNodes(doc, op, matches[0] as TMXmlElement, pos)
    else if typ.StartsWith('@') then
    begin
      elem := matches[0] as TMXmlElement;
      elem.attribute[typ.Substring(1)] := op.text;
    end
    else if typ.StartsWith('namespace::') then
    begin
      elem := matches[0] as TMXmlElement;
      elem.attribute['xmlns:'+typ.Substring(11)] := op.text;
    end
    else
      raise EXmlException.Create('Unknown value for type: '+typ);
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
var
  matches : TFslList<TMXmlNode>;
  n, ce : TMXmlElement;
  sel : String;
begin
  sel := op.attribute['sel'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise EXmlException.Create('No match found for '+sel+' performing replace');
    if matches.count > 1 then
      raise EXmlException.Create('The xpath '+sel+' matched multiple nodes performing replace');

    case TMXmlNamedNode(matches[0]).nodeType of
      ntElement :
        begin
          n := op.first;
          ce := TMXmlElement.CreateNSN(ntElement, TMXmlElement(n).Name, n.namespaceURI, n.localName);
          try
            ce.Attributes.addAll(n.Attributes);
            addChildNodes(doc, n, ce, '');
            TMXmlElement(matches[0]).parent.Children.replace(TMXmlElement(matches[0]), ce.Link);
            ce.Parent := matches[0].parent;
          finally
            ce.Free;
          end;
        end;
      ntText,
      ntComment : TMXmlElement(matches[0]).text := op.text;
      ntAttribute : TMXmlAttribute(matches[0]).value := op.text;
    else
      raise EXmlException.Create('Unsupported Node Type for replace');
    end;
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.remove(doc : TMXmlDocument; sel: String; target: TMXmlElement);
var
  matches : TFslList<TMXmlNode>;
  elem : TMXmlElement;
  attrName : String;
begin
  checkEndsWithAttribute(sel, attrName);

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise EXmlException.Create('Nothing to delete found for xpath '+sel);
    if matches.count > 1 then
      raise EXmlException.Create('The xpath '+sel+' matched multiple nodes');

    if attrName <> '' then
    begin
      elem := matches[0] as TMXmlElement;
      elem.RemoveAttribute(attrName)
    end
    else
      matches[0].parent.Children.remove(matches[0] as TMXmlElement);
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.addChildNodes(doc : TMXmlDocument; source, target: TMXmlElement; pos : String);
var
  n, c : TMXmlElement;
  ce, elem : TMXmlElement;
begin
  n := source.first;
  while n <> nil do
  begin
    case n.nodeType of
      ntElement :
        begin
        ce := TMXmlElement.createNSN(ntElement, n.Name, n.namespaceURI, n.localName);
        try
          elem := (n as TMXmlElement);
          ce.Attributes.addAll(elem.attributes);
          addChildNodes(doc, n, ce, '');
          c := ce.Link;
        finally
          ce.Free;
        end;
        end;
      ntText :
        c := TMXmlElement.createText(n.text);
      ntComment :
        c := TMXmlElement.createComment(n.text);
    else
      raise EXmlException.Create('Node type not supported '+inttostr(ord(n.nodeType)));
    end;
    if pos = '' then
    begin
      target.Children.add(c);
      c.Parent := target;
    end
    else if (pos = 'before') then
    begin
      target.parent.Children.insert(target.parent.Children.IndexOf(target), c);
      c.Parent := target.Parent;
    end
    else
      raise EXmlException.Create('Pos "'+pos+'" not supported');
    n := n.next;
  end;
end;


constructor TFslXMLFormatter.Create;
Begin
  Inherited;

  FBuilder := TFslStringBuilder.Create;
  FAttributes := TFslXMLAttributeList.Create;
  FLocation := TSourceLocation.Create;
  FLastText := true;
End;


destructor TFslXMLFormatter.Destroy;
Begin
  FAttributes.Free;
  FBuilder.Free;

  Inherited;
End;  


function TFslXMLFormatter.Clone: TFslXMLFormatter;
Begin 
  Result := TFslXMLFormatter(Inherited Clone);
End;  


procedure TFslXMLFormatter.commitPending;
begin
  if (FPending <> '') then
  begin
    LevelUp;
    ProducePretty('<'+FPending+'>');
    LevelDown;
    FPending := '';
  end;
end;

function TFslXMLFormatter.Link: TFslXMLFormatter;
Begin 
  Result := TFslXMLFormatter(Inherited Link);
End;  


procedure TFslXMLFormatter.ProduceHeader;
Begin 
  ProducePretty('<?xml version="1.0"' + UseAttributes + '?>');
  ProducePretty('');
End;  


procedure TFslXMLFormatter.ProduceOpen(const sName: String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceOpen', 'Open tag name must be specified.'));

  commitPending;

  FLastText := false;

  FPending := sName + UseAttributes;

  LevelDown;

  if FNoDense then
    CommitPending;
End;  


procedure TFslXMLFormatter.ProduceBytes(const aBytes: TBytes);
begin
  commitPending;
  inherited ProduceBytes(aBytes);
end;

procedure TFslXMLFormatter.ProduceCData(const sText: String);
begin
  commitPending;

  FLastText := false;
  ProducePretty('<![CDATA[' + sText + ']]>');
end;

procedure TFslXMLFormatter.ProduceClose(const sName: String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceClose', 'Close tag name must be specified.'));

  LevelUp;

  if FPending <> '' then
  begin
    ProducePretty('<' + FPending + '/>');
    FPending := '';
  end
  else
    ProducePretty('</' + sName + '>');
  FLastText := false;
End;  


procedure TFslXMLFormatter.ProduceTextNoEscapeEoln(const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));
  commitPending;

  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


function TFslXMLFormatter.updateForText(curr : TSourceLocation; s : String) : TSourceLocation;
var
  i : integer;
begin
  i := 1;
  result := curr;;
  while i <= length(s) do
  begin
    if CharInSet(s[i], [#10, #13]) then
    begin
      result.incLine;
      if (i < length(s)) and (s[i+1] <> s[i]) and CharInSet(s[i+1], [#10, #13]) then
        inc(i);
    end
    else
      result.incCol;
    inc(i);
  end;
End;


procedure TFslXMLFormatter.ProduceText(const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));

  commitPending;
  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


procedure TFslXMLFormatter.ProduceText(const sValue: String;
  processing: TEolnOption);
var
  s : String;
Begin
  if sValue = '' then
    exit;

  commitPending;

  if CanonicalEntities then
    s := FormatTextToXML(sValue, xmlCanonical)
  else
    s := FormatTextToXML(sValue, xmlText{, processing});
  Produce(s); // no pretty - might be a sequence of text
  FLocation := updateForText(FLocation, s);
  FLastText := true;
End;  


procedure TFslXMLFormatter.ProduceTag(const sName: String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceTag', 'Tag name must be specified.'));
  commitPending;

  FLastText := false;

  ProducePretty('<' + sName + UseAttributes + '/>');
End;  


procedure TFslXMLFormatter.ProduceComment(const sComment: String);
Begin
  commitPending;

  FLastText := false;
  if sComment.endsWith('-') then
    ProducePretty('<!--' + sComment + ' -->')
  else
    ProducePretty('<!--' + sComment + '-->')
End;


function TFslXMLFormatter.UseAttributes: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To FAttributes.Count - 1 Do
    if CanonicalEntities then
      Result := Result + SysUtils.Format(' %s="%s"', [FAttributes[iLoop].Name, FormatTextToXML(FAttributes[iLoop].Value, xmlCanonical)])
    else
      Result := Result + SysUtils.Format(' %s="%s"', [FAttributes[iLoop].Name, FormatTextToXML(FAttributes[iLoop].Value, xmlAttribute)]);
  FAttributes.Clear;
End;


procedure TFslXMLFormatter.AddAttribute(const sName, sValue: String; sNs: String);
var
  attr : TFslXMLAttribute;
Begin
  attr := TFslXMLAttribute.Create;
  try
    attr.Name := sName;
    attr.Value := sValue;
    if sNs <> '' then
      attr.SortKey := sNs+#1+sName;
    FAttributes.Add(Attr.Link);
  finally
    attr.Free;
  end;
End;


procedure TFslXMLFormatter.AddNamespace(const sAbbreviation, sURI: String);
Begin
  If sAbbreviation = '' Then
    AddAttribute('xmlns', sURI, #0)
  Else
    AddAttribute('xmlns:' + sAbbreviation, sURI, #0);
End;

function TFslXMLFormatter.sourceLocationForPending: TSourceLocation;
var
  s : String;
begin
  if FPending <> '' then
  begin
    LevelUp;
    if HasWhitespace and not FLastText then
      s := #13#10 + BeforeWhitespace+'<'+FPending+'>'
    else
      s := '<'+FPending+'>';
    LevelDown;
  end
  else
    s := '';
  result := updateForText(FLocation, s);
end;

procedure TFslXMLFormatter.ProducePI(const sName, sText: String);
begin
  Assert(CheckCondition(sName <> '', 'ProducePI', 'PI name must be specified.'));
  commitPending;

  FLastText := false;

  if sText <> '' then
    ProducePretty('<?' + sName + ' ' + sText+ '?>')
  else
    ProducePretty('<?' + sName + UseAttributes + sText+ '?>');
end;

procedure TFslXMLFormatter.ProducePretty(sValue: String);
var
  s : string;
begin
  if HasWhitespace and not FLastText then
    s := #13#10 + BeforeWhitespace+sValue
  else
    s := sValue;
  if (s <> '') then
  begin
    Produce(s);
    FLocation := UpdateForText(FLocation, s);
  end;
end;


function TFslXMLFormatter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAttributes.sizeInBytes);
  inc(result, FBuilder.sizeInBytes);
  inc(result, (FPending.length * sizeof(char)) + 12);
end;

Function TFslXMLAttribute.Link : TFslXMLAttribute;
Begin
  Result := TFslXMLAttribute(Inherited Link);
End;


Function TFslXMLAttribute.Clone : TFslXMLAttribute;
Begin
  Result := TFslXMLAttribute(Inherited Clone);
End;


Procedure TFslXMLAttribute.Assign(oObject : TFslObject);
Begin
  Inherited;

  Namespace := TFslXMLAttribute(oObject).Namespace;
  Name := TFslXMLAttribute(oObject).Name;
  Value := TFslXMLAttribute(oObject).Value;
  SortKey := TFslXMLAttribute(oObject).SortKey;
End;


Function TFslXMLAttribute.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslXMLObject;
End;


function TFslXMLAttribute.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FNamespace.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FSortKey.length * sizeof(char)) + 12);
end;

Function TFslXMLAttributeList.Link : TFslXMLAttributeList;
Begin
  Result := TFslXMLAttributeList(Inherited Link);
End;


procedure TFslXMLAttributeList.add(name, value: String);
var
  attr : TFslXMLAttribute;
begin
  attr := TFslXMLAttribute.Create;
  try
    attr.Namespace := '';
    attr.Name := name;
    attr.Value := value;
    add(attr.link);
  finally
    attr.Free;
  end;
end;

Function TFslXMLAttributeList.Clone : TFslXMLAttributeList;
Begin
  Result := TFslXMLAttributeList(Inherited Clone);
End;


Function TFslXMLAttributeList.New : TFslXMLAttribute;
Begin
  Result := TFslXMLAttribute(Inherited New);
End;


Function TFslXMLAttributeList.ItemClass : TFslObjectClass;
Begin
  Result := TFslXMLAttribute;
End;


Function TFslXMLAttributeList.GetElementByIndex(Const iIndex : Integer) : TFslXMLAttribute;
Begin
  Result := TFslXMLAttribute(ObjectByIndex[iIndex]);
End;


function TFslXMLAttributeList.GetPropName(name: String): String;
var
  a : TFslXMLAttribute;
begin
  a := GetByName(name);
  if a = nil then
    result := ''
  else
    result := a.Value;
end;

Procedure TFslXMLAttributeList.SetElementByIndex(Const iIndex : Integer; Const oValue : TFslXMLAttribute);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


procedure TFslXMLAttributeList.SetPropName(name: String; const Value: String);
var
  a : TFslXMLAttribute;
begin
  a := GetByName(name);
  if a = nil then
  begin
    a := TFslXMLAttribute.Create;
    add(a);
    a.Name := name;
  end;
  a.Value := value;
end;

Function TFslXMLAttributeList.CompareByNamespacedName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslXMLAttribute(pA).Namespace, TFslXMLAttribute(pB).Namespace);
  If Result = 0 Then
    Result := StringCompare(TFslXMLAttribute(pA).Name, TFslXMLAttribute(pB).Name);
End;


function TFslXMLAttributeList.CompareBySortKey(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(TFslXMLAttribute(pA).SortKey, TFslXMLAttribute(pB).SortKey);
end;

Function TFslXMLAttributeList.CompareByNamespace(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslXMLAttribute(pA).Namespace, TFslXMLAttribute(pB).Namespace);
End;


Function TFslXMLAttributeList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslXMLAttribute(pA).Name, TFslXMLAttribute(pB).Name);
End;


Function TFslXMLAttributeList.CompareByValue(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslXMLAttribute(pA).Value, TFslXMLAttribute(pB).Value);
End;


Function TFslXMLAttributeList.IndexByNamespacedName(Const aNamespace, aName: String) : Integer;
Var
  oElement : TFslXMLAttribute;
Begin
  oElement := New;
  Try
    oElement.Namespace := aNamespace;
    oElement.Name := aName;

    If Not Find(oElement, Result, CompareByNamespacedName) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLAttributeList.IndexByNamespace(Const aValue : String) : Integer;
Var
  oElement : TFslXMLAttribute;
Begin
  oElement := New;
  Try
    oElement.Namespace := aValue;

    If Not Find(oElement, Result, CompareByNamespace) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLAttributeList.IndexByName(Const aValue : String) : Integer;
Var
  oElement : TFslXMLAttribute;
Begin
  oElement := New;
  Try
    oElement.Name := aValue;

    If Not Find(oElement, Result, CompareByName) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLAttributeList.IndexByValue(Const aValue : String) : Integer;
Var
  oElement : TFslXMLAttribute;
Begin
  oElement := New;
  Try
    oElement.Value := aValue;

    If Not Find(oElement, Result, CompareByValue) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLAttributeList.Get(Const aValue : Integer) : TFslXMLAttribute;
Begin
  Result := TFslXMLAttribute(Inherited Get(aValue));
End;


Function TFslXMLAttributeList.GetByNamespacedName(Const aNamespace, aName : String) : TFslXMLAttribute;
Begin
  Result := Get(IndexByNamespacedName(aNamespace, aName));
End;


Function TFslXMLAttributeList.GetByNamespace(Const aValue : String) : TFslXMLAttribute;
Begin
  Result := Get(IndexByNamespace(aValue));
End;


Function TFslXMLAttributeList.GetByName(Const aValue : String) : TFslXMLAttribute;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TFslXMLAttributeList.GetByValue(Const aValue : String) : TFslXMLAttribute;
Begin
  Result := Get(IndexByValue(aValue));
End;


Function TFslXMLAttributeList.ExistsByNamespacedName(Const aNamespace, aName : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByNamespacedName(aNamespace, aName));
End;


Function TFslXMLAttributeList.ExistsByNamespace(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByNamespace(aValue));
End;


Function TFslXMLAttributeList.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Function TFslXMLAttributeList.ExistsByValue(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Procedure TFslXMLAttributeList.SortedByNamespacedName;
Begin
  SortedBy(CompareByNamespacedName);
End;


procedure TFslXMLAttributeList.SortedBySortKey;
begin
  SortedBy(CompareBySortKey);
end;

Procedure TFslXMLAttributeList.SortedByNamespace;
Begin
  SortedBy(CompareByNamespace);
End;


Procedure TFslXMLAttributeList.SortedByName;
Begin
  SortedBy(CompareByName);
End;


Procedure TFslXMLAttributeList.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;


Function TFslXMLAttributeList.IsSortedByNamespacedName : Boolean;
Begin
  Result := IsSortedBy(CompareByNamespacedName);
End;


function TFslXMLAttributeList.IsSortedBySortKey: Boolean;
begin
  Result := IsSortedBy(CompareBySortKey);
end;

Function TFslXMLAttributeList.IsSortedByNamespace : Boolean;
Begin
  Result := IsSortedBy(CompareByNamespace);
End;


Function TFslXMLAttributeList.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;


Function TFslXMLAttributeList.IsSortedByValue : Boolean;
Begin
  Result := IsSortedBy(CompareByValue);
End;


Constructor TFslXMLAttributeMatch.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TFslXMLAttributeMatch.GetAttribute(Const sKey: String): String;
Begin
  Result := GetValueByKey(sKey);
End;


Procedure TFslXMLAttributeMatch.SetAttribute(Const sKey, sValue: String);
Begin
  SetValueByKey(sKey, sValue);
End;

Constructor TFslXMLNamespaceEntry.Create;
Begin
  Inherited;

  FValues := TFslStringList.Create;
End;


Destructor TFslXMLNamespaceEntry.Destroy;
Begin
  FValues.Free;

  Inherited;
End;


Function TFslXMLNamespaceEntry.GetValue: String;
Begin
  Result := FValues.StringByIndex[FValues.Count - 1];
End;


Function TFslXMLNamespaceEntry.HasValue : Boolean;
Begin
  Result := Not FValues.IsEmpty;
End;


Procedure TFslXMLNamespaceEntry.Pop;
Begin
  FValues.DeleteByIndex(FValues.Count - 1);
End;


Procedure TFslXMLNamespaceEntry.Push(Const Value: String);
Begin
  FValues.Add(Value);
End;


function TFslXMLNamespaceEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, FValues.sizeInBytes);
end;

Function TFslXMLNamespaceEntryList.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TFslXMLNamespaceEntry(pA).Key, TFslXMLNamespaceEntry(pB).Key);
End;


Function TFslXMLNamespaceEntryList.IndexByKey(Const sKey: String): Integer;
Var
  oEntry : TFslXMLNamespaceEntry;
Begin
  oEntry := TFslXMLNamespaceEntry(ItemNew);
  Try
    oEntry.Key := sKey;

    Result := IndexBy(oEntry, CompareByKey);
  Finally
    oEntry.Free;
  End;
End;


Procedure TFslXMLNamespaceEntryList.SortedByKey;
Begin
  SortedBy(CompareByKey);
End;


Function TFslXMLNamespaceEntryList.ItemClass: TFslObjectClass;
Begin
  Result := TFslXMLNamespaceEntry;
End;


Function TFslXMLNamespaceEntryList.GetEntryByIndex(Const iIndex: Integer): TFslXMLNamespaceEntry;
Begin
  Result := TFslXMLNamespaceEntry(ObjectByIndex[iIndex]);
End;


Constructor TFslXMLNamespaceLevel.Create;
Begin
  Inherited;

  FEntryList := TFslXMLNamespaceEntryList.Create;
  FEntryList.SortedByKey;
End;


Destructor TFslXMLNamespaceLevel.Destroy;
Begin
  FEntryList.Free;

  Inherited;
End;


function TFslXMLNamespaceLevel.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FEntryList.sizeInBytes);
end;

Function TFslXMLNamespaceLevelList.ItemClass: TFslObjectClass;
Begin
  Result := TFslXMLNamespaceLevel;
End;


Function TFslXMLNamespaceLevelList.GetLevelByIndex(Const iIndex: Integer): TFslXMLNamespaceLevel;
Begin
  Result := TFslXMLNamespaceLevel(ObjectByIndex[iIndex]);
End;


Constructor TFslXMLNamespaceManager.Create;
Var
  oDefaultEntry : TFslXMLNamespaceEntry;
Begin
  Inherited;

  FLevelList := TFslXMLNamespaceLevelList.Create;
  FEntryList := TFslXMLNamespaceEntryList.Create;

  // Add default namespace entry.
  oDefaultEntry := TFslXMLNamespaceEntry.Create;
  oDefaultEntry.Key := '';
  oDefaultEntry.Push('');
  FEntryList.Add(oDefaultEntry);

  // Add "xml" default entry
  oDefaultEntry := TFslXMLNamespaceEntry.Create;
  oDefaultEntry.Key := 'xml';
  oDefaultEntry.Push('http://www.w3.org/XML/1998/namespace');
  FEntryList.Add(oDefaultEntry);

  FEntryList.SortedByKey;
  FEntryList.PreventDuplicates;
End;


Destructor TFslXMLNamespaceManager.Destroy;
Begin
  FEntryList.Free;
  FLevelList.Free;

  Inherited;
End;


Function TFslXMLNamespaceManager.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslXMLNamespaceManager;
End;


Function TFslXMLNamespaceManager.DefaultNamespace: String;
Begin
  Result := NamespaceOfPrefix('');
End;


Procedure TFslXMLNamespaceManager.ListPrefixes(Const oPrefixNamespaces: TFslStringMatch);
Var
  iEntryIndex : Integer;
  oEntry : TFslXMLNamespaceEntry;
Begin
  oPrefixNamespaces.Clear;

  For iEntryIndex := 0 To FEntryList.Count - 1 Do
  Begin
    oEntry := FEntryList[iEntryIndex];

    oPrefixNamespaces.Add(oEntry.Key, oEntry.Value);
  End;
End;


Function TFslXMLNamespaceManager.LocalNameOf(Const sElementName: String): String;
Var
  iColonIndex : Integer;
Begin
  iColonIndex := StringFind(sElementName, ':');

  If iColonIndex <= 0  Then
    Result := sElementName
  Else
    Result := StringCopy(sElementName, iColonIndex + 1, MaxInt);
End;


Function TFslXMLNamespaceManager.NamespaceOf(Const sElementName: String): String;
Begin
  Result := NamespaceOfPrefix(PrefixOf(sElementName));
End;


Function TFslXMLNamespaceManager.NamespaceOfPrefix(Const sPrefix: String) : String;
Var
  iEntryIndex : Integer;
Begin
  iEntryIndex := FEntryList.IndexByKey(sPrefix);

  If iEntryIndex < 0 Then
    RaiseError('NamespaceOfPrefix', StringFormat('The namespace prefix ''%s'' has not beed defined.', [sPrefix]));

  Result := FEntryList[iEntryIndex].Value;
End;


Procedure TFslXMLNamespaceManager.Pop;
Var
  oLevel : TFslXMLNamespaceLevel;
  oEntry : TFslXMLNamespaceEntry;
  iEntryIndex : Integer;
Begin
  oLevel := TFslXMLNamespaceLevel(FLevelList.RemoveLast);
  Try
    For iEntryIndex := 0 To oLevel.EntryList.Count - 1  Do
    Begin
      oEntry := oLevel.EntryList[iEntryIndex];

      oEntry.Pop;

      If Not oEntry.HasValue Then
        FEntryList.DeleteByReference(oEntry);
    End;
  Finally
    oLevel.Free;
  End;
End;


Function TFslXMLNamespaceManager.PrefixOf(Const sElementName: String): String;
Var
  iColonIndex : Integer;
Begin
  iColonIndex := StringFind(sElementName, ':');

  If iColonIndex <= 0 Then
    Result := ''
  Else
    Result := StringCopy(sElementName, 1, iColonIndex - 1);
End;


Procedure TFslXMLNamespaceManager.Push(Const oAttributes: TFslStringMatch);
Const
  XMLNS_STRING = 'xmlns';
  XMLNS_LENGTH = Length(XMLNS_STRING);
Var
  oLevel : TFslXMLNamespaceLevel;
  iAttributeIndex : Integer;
  sName : String;
  sPrefix : String;
  sNamespace : String;
  iEntryIndex : Integer;
  oEntry : TFslXMLNamespaceEntry;
Begin
  oLevel := TFslXMLNamespaceLevel.Create;
  Try
    For iAttributeIndex := 0 To oAttributes.Count - 1 Do
    Begin
      // Determine if this is a namespace name
      sName := oAttributes.KeyByIndex[iAttributeIndex];

      If StringStartsWith(sName, XMLNS_STRING) Then
      Begin
        // Obtain the prefix
        If Length(sName) = XMLNS_LENGTH Then
          sPrefix := ''
        Else
          sPrefix := StringCopy(sName, XMLNS_LENGTH + 2, MaxInt);

        // Obtain the value
        sNamespace := oAttributes.ValueByIndex[iAttributeIndex];

        // Add the name-value pair
        // Obtain a namespace value object...
        iEntryIndex := FEntryList.IndexByKey(sPrefix);

        If FEntryList.ExistsByIndex(iEntryIndex) Then
        Begin
          // ...either existing one...
          oEntry := FEntryList[iEntryIndex];
        End
        Else
        Begin
          // ...or new one.
          oEntry := TFslXMLNamespaceEntry.Create;
          oEntry.Key := sPrefix;
          FEntryList.Add(oEntry);
        End;

        // Add the namespace to the level
        oEntry.Push(sNamespace);

        oLevel.EntryList.Add(oEntry.Link);
      End;
    End;

    FLevelList.Add(oLevel.Link);
  Finally
    oLevel.Free;
  End;
End;

function TFslXMLNamespaceManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FEntryList.sizeInBytes);
  inc(result, FLevelList.sizeInBytes);
end;

{ TMXmlBuilder }

function TMXmlBuilder.SourceLocation: TSourceLocation;
begin
  result := TSourceLocation.Create;
end;

procedure TMXmlBuilder.Start(oNode : TMXmlElement);
begin
  if oNode = nil Then
  Begin
    FDoc := TMXmlDocument.create;
    if CharEncoding <> '' Then
      FDoc.addChild(TMXmlElement.createProcessingInstruction(CharEncoding), true);
    FStack.Add(FDoc.Link);
  End
  else
    FStack.Add(oNode.Link);
  FSourceLocation := TSourceLocation.Create
end;

procedure TMXmlBuilder.Build(oStream: TFslStream);
var
  oVCL : TVCLStream;
Begin
  oVCL := TVCLStream.Create;
  Try
    oVCL.Stream := oStream.Link;
    Build(oVCL);
  Finally
    oVCL.Free;
  End;
End;

procedure TMXmlBuilder.Finish;
Begin
  if FStack.Count > 1 Then
    RaiseError('Close', 'Document is not finished');
  FDoc.Free;
End;

procedure TMXmlBuilder.inject(const bytes: TBytes);
begin
  raise EXmlException.Create('Inject is not supported on the fsl_xml Builder');
end;

procedure TMXmlBuilder.Build(oStream: TStream);
Var
  b : TBytes;
begin
  assert(FAttributes.empty);
  assert(not FExternal);
  b := TEncoding.UTF8.GetBytes(FStack[0].ToXml(false));
  oStream.Write(b[0], length(b));
end;

function TMXmlBuilder.Open(const sName: String) : TSourceLocation;
var
  oElem : TMXmlElement;
  oParent : TMXmlElement;
  iLoop : integer;
  len : integer;
begin
  oElem := nil;
  try
    if CurrentNamespaces.DefaultNS <> '' Then
    begin
      oElem := TMXmlElement.CreateNS(ntElement, CurrentNamespaces.DefaultNS, sName);
      len := length(sName)+3
    end
    Else
    begin
      oElem := TMXmlElement.Create(ntElement, sName);
      len := length(sName);
    end;
    oParent := FStack.Last;
    if IsPretty and (oParent.NodeType = ntElement) Then
      oParent.addChild(TMXmlElement.createText(ReadTextLength(#13#10+pad)), true);
    oParent.addChild(oElem.Link, true);
    inc(FSourceLocation.col, len+2);
    for iLoop := 0 to FAttributes.Count - 1 Do
      oElem.attributes.addAll(FAttributes);
    FAttributes.Clear;
    FStack.Add(oElem.Link);
    result := FSourceLocation;
  finally
    oElem.Free;
  end;
end;

procedure TMXmlBuilder.Close(const sName: String);
begin
  if IsPretty Then
  Begin
    If FStack.Last.HasChildren and not FStack.Last.allChildrenAreText Then
      FStack.Last.addChild(TMXmlElement.createText(readTextLength(#13#10+pad(-1))), true);
  End;
  FStack.Delete(FStack.Count - 1)
end;


procedure TMXmlBuilder.AddAttribute(const sName, sValue: String);
var
  a : TMXmlAttribute;
  b : boolean;
begin
  b := false;
  for a in FAttributes do
    if a.Name = sName then
    begin
      b := true;
      inc(FSourceLocation.col, sValue.Length - a.Value.Length);
      a.Value := sValue;
    end;
  if not b then
  begin
    FAttributes.Add(TMXmlAttribute.Create(sName, sValue));
    ReadTextLengthWithEscapes(sName+'="', sValue, '"');
  end;
end;

function TMXmlBuilder.Text(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addChild(TMXmlElement.createText(ReadTextLengthWithEscapes('', sValue, '')), true);
  result := FSourceLocation;
end;


function TMXmlBuilder.Entity(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addChild(TMXmlElement.createText('&'+sValue+';'), true);
  inc(FSourceLocation.col, length(sValue)+2);
  result := FSourceLocation;
end;

function TMXmlBuilder.Tag(const sName: String) : TSourceLocation;
begin
  Open(sName);
  Close(sName);
  result := FSourceLocation;
end;

function TMXmlBuilder.TagText(const sName, sValue: String) : TSourceLocation;
begin
  result := Open(sName);
  if (sValue <> '') Then
    Text(sValue);
  Close(sName);
end;

function TMXmlBuilder.Pad(offset : integer = 0): String;
var
  iLoop : integer;
begin
  Setlength(result, ((FStack.Count - 1) + offset) * 2);
  For iLoop := 1 to Length(Result) Do
    result[iLoop] := ' ';
end;


procedure TMXmlBuilder.ProcessingInstruction(sName, sText: String);
begin
  raise EXmlException.Create('Not supported yet');
end;

function TMXmlBuilder.ReadTextLength(s: string): String;
var
  i : integer;
begin
  i := 1;
  while i <= length(s) do
  begin
    if CharInSet(s[i], [#10, #13]) then
    begin
      FSourceLocation.incLine;
      if (i < length(s)) and (s[i+1] <> s[i]) and CharInSet(s[i+1], [#10, #13]) then
        inc(i);
    end
    else
      FSourceLocation.incCol;
    inc(i);
  end;
  result := s;
end;

function TMXmlBuilder.ReadTextLengthWithEscapes(pfx, s, sfx: string): String;
begin
 ReadTextLength(pfx + FormatTextToXML(s, xmlText) + sfx);
  result := pfx + s + sfx;
end;

Procedure TMXmlBuilder.Comment(Const sContent : String);
begin
  if IsPretty and (FStack.Last.nodeType = ntElement) Then
    FStack.Last.addChild(TMXmlElement.createText(ReadTextLength(#13#10+pad)), true);
  FStack.Last.addChild(TMXmlElement.createComment(sContent), true);
  ReadTextLength('<!-- '+sContent+' -->');
End;


function TMXmlBuilder.Build: String;
var
  oStream : TStringStream;
begin
  oStream := TStringStream.Create('');
  Try
    Build(oStream);
    Result := oStream.DataString;
  Finally
    oStream.Free;
  End;
end;

constructor TMXmlBuilder.Create;
begin
  inherited;
  CurrentNamespaces.DefaultNS := 'urn:hl7-org:v3';
  CharEncoding := 'UTF-8';
  FStack := TFslList<TMXmlElement>.Create;
  FAttributes := TFslList<TMXmlAttribute>.Create;
end;

destructor TMXmlBuilder.Destroy;
begin
  FStack.Free;
  FStack := nil;
  FAttributes.Free;
  FAttributes := nil;
  inherited;
end;

procedure TMXmlBuilder.DocType(sText: String);
begin
  raise EXmlException.Create('Not supported yet');
end;

procedure TMXmlBuilder.AddAttributeNS(const sNamespace, sName, sValue: String);
var
  a : TMXmlAttribute;
  b : boolean;
begin
  b := false;
  for a in FAttributes do
    if a.LocalName = sName then
    begin
      b := true;
      inc(FSourceLocation.col, sValue.Length - a.Value.Length);
      a.Value := sValue;
    end;
  if not b then
  begin
    a := TMXmlAttribute.Create(sName, sValue);
    try
      a.NamespaceURI := sNamespace;
      a.LocalName := sName;
      FAttributes.Add(a.link);
    finally
      a.free;
    end;
    ReadTextLengthWithEscapes(sName+'="', sValue, '"');
  end;
end;

procedure TMXmlBuilder.Start;
begin
  Start(nil);
end;

procedure TMXmlBuilder.StartFragment;
begin
  Start(nil);
end;

function TMXmlBuilder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStack.sizeInBytes);
  inc(result, FDoc.sizeInBytes);
  inc(result, FAttributes.sizeInBytes);
end;

{ TFslXMLExtractor }

Constructor TFslXMLExtractor.Create;
Begin
  Inherited;

  FElement := '';
  FAttributes := TFslXMLAttributeMatch.Create;
  FNamespaceManager := TFslXMLNamespaceManager.Create;
End;


Destructor TFslXMLExtractor.Destroy;
Begin
  FNamespaceManager.Free;
  FAttributes.Free;

  Inherited;
End;


Function TFslXMLExtractor.Link: TFslXMLExtractor;
Begin
  Result := TFslXMLExtractor(Inherited Link);
End;


Procedure TFslXMLExtractor.Expected(Const sMethod, sExpected : String);
Begin
  RaiseError(sMethod, StringFormat('Expected %s but found ''%s''', [sExpected, FElement]));
End;


Function TFslXMLExtractor.ConsumeIdentifier(Const sValue: String; Const setTerminal : TCharSet) : String;
Begin
  Result := ConsumeIdentifier(setTerminal);

  If Not StringEquals(sValue, Result) Then
    RaiseError('ConsumeIdentifier', StringFormat('Expected ''%s'' but found ''%s''', [sValue, Result]));
End;


Function TFslXMLExtractor.ConsumeIdentifier(Const setTerminal : TCharSet) : String;
Begin
  Result := ConsumeUntilCharacterSet(setTerminal);
End;


Procedure TFslXMLExtractor.ConsumeAttributes(Const setTerminal : TCharSet);
Var
  sKey   : String;
  sValue : String;
  cQuote : Char;
Begin
  FAttributes.Clear;

  While Not CharInSet(NextCharacter, setTerminal) Do
  Begin
    sKey := ConsumeIdentifier(['=', '>'] + setWhitespace);

    ConsumeWhitespace;

    ConsumeCharacter('=');

    ConsumeWhitespace;

    If NextCharacter = '''' Then
      cQuote := ''''
    Else
      cQuote := '"';

    ConsumeCharacter(cQuote);

    sValue := ConsumeIdentifier([cQuote]);

    ConsumeCharacter(cQuote);

    If cQuote = '''' Then
      sValue := StringReplace(sValue, '&apos;', '''')
    Else
      sValue := StringReplace(sValue, '&quot;', '"');

    FAttributes.Add(sKey, sValue);

    ConsumeWhitespace;
  End;
End;


Function TFslXMLExtractor.PeekXML : String;
Const
  setTag = ['<', '>'] + setWhitespace;
Begin
  // Assume valid xml
  // All elements must have names. Element names are case-sensitive and must start with
  // a letter or underscore.
  // An element name can contain letters, digits, hyphens, underscores, and periods.

  FNodeName := '';

  If FElement = '' Then
  Begin
    ConsumeWhitespace;

    If Not More Then
    Begin
      //Nothing left to read
      FElement := '';
    End
    Else
    Begin
      ConsumeCharacter('<');

      If NextCharacter = '!' Then
      Begin
        // Comment
        ConsumeString('!--');

        If More Then
          FElement := '!' + ConsumeUntilString('-->')
        Else
          FElement := '!';

        If More Then
          ConsumeString('--');

        FAttributes.Clear;
      End
      Else If NextCharacter = '/' Then
      Begin
        // Close tag

        FElement := ConsumeIdentifier(setTag);
        ConsumeWhitespace;
        FAttributes.Clear;
        FNamespaceManager.Pop;
      End
      Else
      Begin
        // Open tag or header

        FElement := ConsumeIdentifier(setTag);
        FNodeName := FElement;

        ConsumeWhitespace;

        ConsumeAttributes(['/', '>', '?']);

        FNamespaceManager.Push(Attributes);

        If NextCharacter <> '>' Then
          FElement := FElement + ConsumeCharacter;
      End;

      ConsumeCharacter('>');
    End;
  End
  Else
    FNodeName := FElement;

  Result := FElement;
End;


Function TFslXMLExtractor.PeekIsClose : Boolean;
Begin
  PeekXML;

  Result := StringGet(FElement, 1) = '/';
End;


Function TFslXMLExtractor.PeekIsOpen : Boolean;
Begin
  PeekXML;

  Result := (FElement <> '') And Not CharInSet(StringGet(FElement, 1), ['/', '?', '!']);
End;


Function TFslXMLExtractor.PeekIsEmptyNode: Boolean;
Begin
  Result := PeekIsOpen And (StringGet(FElement, Length(FElement)) = '/');
End;


Function TFslXMLExtractor.PeekIsHeader : Boolean;
Begin
  PeekXML;

  Result := StringGet(FElement, 1) = '?';
End;


Function TFslXMLExtractor.PeekIsComment : Boolean;
Begin
  PeekXML;

  Result := StringGet(FElement, 1) = '!';
End;


Function TFslXMLExtractor.PeekIsText: Boolean;
Begin
  Result := NextCharacter <> '<';
  If Not Result Then
    PeekXML;
End;


Function TFslXMLExtractor.ConsumeOpen : String;
Begin
  Result := PeekXML;

  If StringEquals(Result, '/', 1) Then
    Expected('ConsumeComment', 'open');

  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeOpen(Const sTag: String);
Begin
  PeekXML;

  if StringEquals(sTag+'/', FElement) then
  begin
    FElement := '/'+sTag;
  end
  else
  begin
  If (sTag <> '') And Not StringEquals(sTag, FElement) Then
    Expected('ConsumeOpen', '''' + sTag + '''');

  FElement := '';
End;
End;


Procedure TFslXMLExtractor.ConsumeOpen(Const sTag, sNamespace : String);
Begin
  PeekXML;

  If (sTag <> '') And Not SameLocalAndNamespace(FElement, sNamespace, sTag) Then
    Expected('ConsumeOpen', '''' + sTag + ''' in namespace '''+sNamespace+'''');

  FElement := '';
End;


Function TFslXMLExtractor.ConsumeClose : String;
Begin
  Result := PeekXML;

  If Not StringEquals(FElement, '/', 1) Then
    Expected('ConsumeComment', 'close (/)');

  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeClose(Const sTag: String);
Begin
  PeekXML;

  If (sTag <> '') And Not StringEquals('/' + sTag, FElement) Then
    Expected('ConsumeClose', '''/' + sTag + '''');

  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeClose(Const sTag, sNamespace: String);
Begin
  PeekXML;

  If (sTag <> '') And (sTag[1] = '/') And
    Not SameLocalAndNamespace(Copy(FElement, 2, MaxInt), sNamespace, sTag) Then
    Expected('ConsumeClose', '''/' + sTag + ''' in namespace '''+sNamespace+'''');

  FElement := '';
End;


Function TFslXMLExtractor.ConsumeBody : String;
Begin
  Result := DecodeXML(ConsumeUntilCharacterSet(['<', '>']));
End;


Function TFslXMLExtractor.ConsumeTextBody : String;
Begin
  Result := DecodeXML(ConsumeUntilCharacterSet(['<']));
End;


Function TFslXMLExtractor.ConsumeText(Const sTag: String): String;
Begin
  ConsumeOpen(sTag);

  Result := ConsumeTextBody;

  ConsumeClose(sTag);
End;


Function TFslXMLExtractor.ConsumeText(Const sTag, sNamespace: String): String;
Begin
  ConsumeOpen(sTag, sNamespace);

  Result := ConsumeTextBody;

  ConsumeClose(sTag, sNamespace);
End;


Function TFslXMLExtractor.ConsumeComment : String;
Begin
  Result := StringTrimSet(PeekXML, ['!']);

  If Not StringEquals(FElement, '!', 1) Then
    Expected('ConsumeComment', 'comment (!)');

  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeComment(Const sComment: String);
Begin
  PeekXML;

  If Not StringEquals(sComment, StringTrimSet(FElement, ['!'])) Then
    Expected('ConsumeComment', sComment);

  FElement := '';
End;


Function TFslXMLExtractor.ConsumeHeader : String;
Begin
  Result := StringTrimSet(PeekXML, ['?']);

  If Not StringEquals(FElement, '?', 1) Then
    Expected('ConsumeHeader', 'header (?)');

  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeHeader(Const sHeader: String);
Begin
  PeekXML;

  If Not StringEquals(sHeader, StringTrimSet(FElement, ['?'])) Then
    Expected('ConsumeHeader', sHeader);

  FElement := '';
End;


Function TFslXMLExtractor.ConsumeWhitespace : String;
Begin
  Result := ConsumeWhileCharacterSet(setWhitespace);
End;


Function TFslXMLExtractor.ConsumeElement : String;
Begin
  Result := PeekXML;
  FElement := '';
End;


Procedure TFslXMLExtractor.ConsumeDocumentType;
Var
  sName : String;
Begin
  PeekXML;

  If StringGet(FElement, 1) <> '!' Then
    RaiseError('ConsumeDocumentType', 'Document Type not found.');

  sName := ConsumeOpen;

  ConsumeBody;

  ConsumeClose(sName);
End;


Function TFslXMLExtractor.NamespaceOf(Const sNodeName: String): String;
Begin
  Result := FNamespaceManager.NamespaceOf(sNodeName);
End;


Function TFslXMLExtractor.PrefixOf(Const sNodeName: String): String;
Begin
  Result := FNamespaceManager.PrefixOf(sNodeName);
End;


Function TFslXMLExtractor.DefaultNamespace: String;
Begin
  Result := FNamespaceManager.DefaultNamespace;
End;


Function TFslXMLExtractor.NodeNamespace: String;
Begin
  Result := FNamespaceManager.NamespaceOf(FNodeName);
End;


Function TFslXMLExtractor.NodeLocalName: String;
Begin
  Result := FNamespaceManager.LocalNameOf(FNodeName);
End;


Function TFslXMLExtractor.SameLocalAndNamespace(Const sTag, sNamespace, sLocal: String): Boolean;
Var
  sPrefix : String;
Begin
  // Result := (FNamespaceManager.NamespaceOf(sTag) = sNamespace) And (FNamespaceManager.LocalNameOf(sTag) = sLocal);
  // We want a more finer check, as prefix may not exist (cause error in namespace manager)
  // However, when a prefix is missing, default namespace should be tested instead
  sPrefix := FNamespaceManager.PrefixOf(sTag);
  If sPrefix = '' Then
    Result := (FNamespaceManager.DefaultNamespace = sNamespace) And (sTag = sLocal)
  Else
    Result := (FNamespaceManager.NamespaceOfPrefix(sPrefix) = sNamespace) And (FNamespaceManager.LocalNameOf(sTag) = sLocal);
End;


Procedure TFslXMLExtractor.ListPrefixes(Const oPrefixNamespaces: TFslStringMatch);
Begin
  FNamespaceManager.ListPrefixes(oPrefixNamespaces);
End;


Function TFslXMLExtractor.IsNode(Const sNamespace, sLocalName: String): Boolean;
Begin
  Result  := SameLocalAndNamespace(FNodeName, sNamespace, sLocalName);
End;


Function TFslXMLExtractor.GetAttribute(Const sNamespace, sLocalName: String): String;
Begin
  Result := GetAttribute(sNamespace, sLocalName, '');
End;


Function TFslXMLExtractor.GetAttribute(Const sNamespace, sLocalName, sDefault: String): String;
Var
  iCount : Integer;
  sAttrName : String;
Begin
  // default result
  Result := sDefault;

  For iCount := 0 To FAttributes.Count - 1 Do
  Begin
    sAttrName := FAttributes.KeyByIndex[iCount];
    If SameLocalAndNamespace(sAttrName, sNamespace, sLocalName) Then
    Begin
      Result := FAttributes.ValueByIndex[iCount];
      Break;
    End;
  End;
End;


Function TFslXMLExtractor.PeekIsOpenTag(Const sElement: String): Boolean;
Begin
  Result := PeekIsOpen And StringEquals(PeekXML, sElement);
End;


Procedure TFslXMLExtractor.Skip(oSkipTypes: TFslXMLKnownTypes);
Var
  bContinue: Boolean;
Begin
  // we assume well-formed XML node
  bContinue := True;
  While bContinue Do
  Begin
    If Self.PeekIsText And (TFslXMLKnownTextType In oSkipTypes) Then
      Self.ConsumeBody
    Else If Self.PeekIsHeader And (TFslXMLKnownHeaderType In oSkipTypes) Then
      Self.ConsumeHeader
    Else If Self.PeekIsComment And (TFslXMLKnownCommentType In oSkipTypes) Then
      Self.ConsumeComment
    Else If Self.PeekIsOpen And (TFslXMLKnownElementType In oSkipTypes) Then
    Begin
      If FElement[Length(FElement)] = '/' Then
        Self.ConsumeElement     // element with no body
      Else
      Begin
        Self.ConsumeOpen;
        Skip(ALL_XML_KNOWN_TYPE);
        Self.ConsumeClose;
      End;
    End
    Else
      bContinue := False;    // Reach non-skip node, or end of element's body
  End;
End;


Procedure TFslXMLExtractor.SkipNext;
Begin
  // Simply skip the next 'consumable'
  If Self.PeekIsHeader Then
    Self.ConsumeHeader
  Else If Self.PeekIsComment Then
    Self.ConsumeComment
  Else If Self.PeekIsOpen Then
  Begin
    If FElement[Length(FElement)] = '/' Then
      Self.ConsumeElement     // element with no body
    Else
    Begin
      Self.ConsumeOpen;
      Skip(ALL_XML_KNOWN_TYPE);
      Self.ConsumeClose;
    End;
  End
  Else If Self.PeekIsClose Then
    Self.ConsumeClose
End;

function TFslXMLExtractor.PeekString: String;
begin
  result := PeekXml;
end;

function TFslXMLExtractor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FElement.length * sizeof(char)) + 12);
  inc(result, FAttributes.sizeInBytes);
  inc(result, FNamespaceManager.sizeInBytes);
  inc(result, (FNodeName.length * sizeof(char)) + 12);
end;

end.
