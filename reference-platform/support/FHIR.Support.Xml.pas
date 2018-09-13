unit FHIR.Support.Xml;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

interface

Uses
  SysUtils, Classes,
  Xml.xmlintf,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Collections, FHIR.Support.MXml;


function getChildNode(node : IXMLNode; name, ns : String) : IXMLNode; overload;
function getChildNode(node : IXMLNode; name : String) : IXMLNode; overload;

Type

  TXmlCanonicalisationMethod = (xcmCanonicalise, xcmComments, xcmTrimWhitespace, {xcmPrefixRewrite, } xcmQNameAware);
  TXmlCanonicalisationMethodSet = set of TXmlCanonicalisationMethod;

  TXmlBuilderNamespaceList = class (TFslStringMatch)
  private
    FDefaultNS : String;
    FNew: TStringList;
    FDefaultSet: boolean;
    procedure SetDefaultNS(const Value: String);
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
{
    Procedure WriteXml(iElement : XmlIntf.IXMLNode; first : boolean); overload; virtual; abstract;
    Procedure WriteXmlNode(iNode : XmlIntf.IXMLNode; first : boolean); overload; virtual; abstract;
    Procedure WriteXmlDocument(iDoc : XmlIntf.IXMLDocument); overload; virtual; abstract;
}

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

  TFslXMLParserNamespaces = Class (TFslStringMatch)
    Private
      FDefaultNamespace : String;

    Public
      constructor Create; Override;

      Function Clone : TFslXMLParserNamespaces; Overload;

      Procedure Assign(oObject : TFslObject); Override;

      Property DefaultNamespace : String Read FDefaultNamespace Write FDefaultNamespace;
  End;

  TFslXMLAttribute = Class(TFslObject)
    Private
      FNamespace : String;
      FName : String;
      FValue : String;
      FSortKey : String;

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      Function Link : TFslXMLAttribute;
      Function Clone : TFslXMLAttribute;

      Procedure Assign(oObject : TFslObject); Override;

      Property Namespace : String Read FNamespace Write FNamespace;
      Property Name : String Read FName Write FName;
      Property Value : String Read FValue Write FValue;
      Property SortKey : String read FSortKey write FSortKey;
  End;

  TFslXMLElementType = (FslXMLElementTypeUnknown, FslXMLElementTypeNode, FslXMLElementTypeText, FslXMLElementTypeComment);

  TFslXMLElementIterator = Class;
  TFslXMLElementList = Class;

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

  TFslXMLElement = Class(TFslObject)
    Private
      FElementType : TFslXMLElementType;

      // if element
      FNamespace : String;
      FName : String;
      FID : String;
      FChildrenElementList : TFslXMLElementList;
      FAttributeList : TFslXMLAttributeList;

      // if comment or Text
      FContent : String;

      Function GetNamespace : String;
      Function GetName : String;
      Function GetId : String;
      Function GetChildren : TFslXMLElementList;
      Function GetAttributes : TFslXMLAttributeList;
      Function GetContent : String;

      Procedure SetElementType(Const Value : TFslXMLElementType);
      Procedure SetNamespace(Const Value : String);
      Procedure SetName(Const Value : String);
      Procedure SetId(Const Value : String);
      Procedure SetChildren(Const Value : TFslXMLElementList);
      Procedure SetAttributes(Const Value : TFslXMLAttributeList);
      Procedure SetContent(Const Value : String);

    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslXMLElement;
      Function Clone : TFslXMLElement;

      Procedure Assign(oObject : TFslObject); Override;

      Function Iterator(Const sNamespace, sName : String) : TFslXMLElementIterator; Overload;
      Function Iterator(Const sName : String) : TFslXMLElementIterator; Overload;
      Function Iterator(aElementType : TFslXMLElementType) : TFslXMLElementIterator; Overload;

      Procedure Clear;

      Function HasNamespace : Boolean;
      Function HasName : Boolean;
      Function HasId : Boolean;
      Function HasChildren : Boolean;
      Function HasAttributes : Boolean;
      Function HasContent : Boolean;
      Function HasText : Boolean;
      Function HasComment : Boolean;

      Property ElementType : TFslXMLElementType Read FElementType Write SetElementType;
      Property Namespace : String Read GetNamespace Write SetNamespace;
      Property Name : String Read GetName Write SetName;
      Property Id : String Read GetId Write SetId;
      Property Children : TFslXMLElementList Read GetChildren Write SetChildren;
      Property Attributes : TFslXMLAttributeList Read GetAttributes Write SetAttributes;
      Property Content : String Read GetContent Write SetContent;
  End;

  TFslXMLElementList = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TFslXMLElement;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TFslXMLElement);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareById(pA, pB : Pointer) : Integer;
      Function CompareByName(pA, pB : Pointer) : Integer;

      Function Get(Const aValue : Integer) : TFslXMLElement; Reintroduce;

    Public
      Function Link : TFslXMLElementList;
      Function Clone : TFslXMLElementList;

      Function New : TFslXMLElement; Reintroduce;

      Function IndexById(Const aValue : String) : Integer;
      Function GetById(Const aValue : String) : TFslXMLElement;
      Function ExistsById(Const aValue : String) : Boolean;

      Function IndexByName(Const aValue : String) : Integer;
      Function GetByName(Const aValue : String) : TFslXMLElement;
      Function ExistsByName(Const aValue : String) : Boolean;

      Property Elements[Const iIndex : Integer] : TFslXMLElement Read GetElement Write SetElement; Default;
  End;

  TFslXMLElementIterator = Class(TFslObjectListIterator)
    Private
      FElementType : TFslXMLElementType;
      FNamespace : String;
      FName : String;

    Protected
      Function Skip : Boolean; Overload; Override;

    Public
      Function Current : TFslXMLElement; Reintroduce;
  End;

  TFslXMLDocument = Class(TFslObject)
    Private
      FRootElement : TFslXMLElement;

      Function GetRootElement : TFslXMLElement;
      Procedure SetRootElement(Const Value : TFslXMLElement);

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslXMLDocument;
      Function Clone : TFslXMLDocument;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Clear;

      Function HasRootElement : Boolean;
      Property RootElement : TFslXMLElement Read GetRootElement Write SetRootElement;
  End;

  TFslXMLDocumentList = Class(TFslObjectList)
    Private
      Function GetElementByIndex(Const iIndex : Integer) : TFslXMLDocument;
      Procedure SetElementByIndex(Const iIndex : Integer; Const oValue : TFslXMLDocument);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TFslXMLDocument; Reintroduce;

    Public
      Function Link : TFslXMLDocumentList;
      Function Clone : TFslXMLDocumentList;

      Function New : TFslXMLDocument; Reintroduce;

      Property ElementByIndex[Const iIndex : Integer] : TFslXMLDocument Read GetElementByIndex Write SetElementByIndex; Default;
  End;

  TFslXMLNamespaceEntry = Class(TFslObject)
    Private
      FKey : String;
      FValues : TFslStringList;

      Function GetValue: String;

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

  TFslXMLFormatter = Class(TFslTextFormatter)
    Private
      FAttributes : TFslXMLAttributeList;
      FBuilder : TFslStringBuilder;
      FLine : integer;
      FCol : integer;
      FLastText : boolean;
      FPending : string;
      FNoDense : Boolean;
    FCanonicalEntities: boolean;

      procedure updateForText(s : String);
      procedure commitPending;
    Protected
      Function UseAttributes : String;

      Procedure ProducePretty(sValue : String);
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
      property Line : integer read FLine;
      property Col : integer read FCol;
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

  // http://www.w3.org/TR/2012/WD-xml-c14n2-testcases-20120105/
  TFslXmlBuilderCanonicalizationTests = class (TFslObject)
  private
    class procedure check(source : String; can : TXmlCanonicalisationMethodSet; target : String);
    class procedure Test1;
    class procedure Test2;
    class procedure Test3;
    class procedure Test4;
    class procedure Test5;

  public
    class procedure Test;
  end;

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


Const
  NAMES_ADVXMLELEMENTTYPE : Array [TFslXMLElementType] Of String = ('Unknown', 'Element', 'Text', 'Comment');

Type
  TMXmlBuilder = class (TXmlBuilder)
  private
    FExternal : Boolean;
    FStack : TFslList<TMXmlElement>;
    FDoc : TMXmlDocument;
    FAttributes : TFslMap<TMXmlAttribute>;
    FSourceLocation : TSourceLocation;
    Function Pad(offset : integer = 0) : String;
    function ReadTextLength(s : string):String;
    function ReadTextLengthWithEscapes(pfx, s, sfx : string):String;
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

{function TFslXmlBuilder.nsIsUsed(elem: IXmlNode; ns: String): boolean;
var
  i : integer;
begin
  result := false;
  if elem.NamespaceURI = ns then
    result := true
  else
    for i := 0 to elem.AttributeNodes.Count - 1 do
      result := result or (elem.AttributeNodes[i].NamespaceURI = ns);

  for i := 0 to elem.ChildNodes.Count - 1 do
    if elem.ChildNodes[i].NodeType = ntElement then
      result := result or nsIsUsed(elem.ChildNodes[i], ns);
end;
}

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
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
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


{procedure TFslXmlBuilder.defineNamespace(element, attribute: IXMLNode);
var
  ns : String;
begin
  if attribute.NodeValue = Null then
    ns := ''
  else
    ns := attribute.NodeValue;

  if not (xcmCanonicalise in FCanonicalise) then
    xml.AddAttribute(attribute.NodeName, ns)
  else if attribute.NodeName = 'xmlns' then
  begin
    if CurrentNamespaces.DefaultNS <> ns then
    begin
      CurrentNamespaces.DefaultNS := ns;
      CurrentNamespaces.DefaultSet := false; // duck the hook
      xml.AddAttribute(attribute.NodeName, ns);
    end;
  end
  else if nsIsUsed(element, ns) then
    xml.AddAttribute(attribute.NodeName, ns)
end;
}
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
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
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
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
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
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
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
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
end;

{ TFslXmlBuilderCanonicalizationTests }

class procedure TFslXmlBuilderCanonicalizationTests.check(source: String; can: TXmlCanonicalisationMethodSet; target: String);
{var
  doc : IXMLDocument;
  dom : TMXMLDocument;
  xb : TFslXmlBuilder;
  s : String;}
begin
(*  dom := TMXMLDocument.Create;
  doc := dom;
  dom.DOMVendor := OpenXML4Factory;
  dom.ParseOptions := [poPreserveWhiteSpace];
  dom.Options := [{doNodeAutoCreate, doNodeAutoIndent, doAttrNull,  doAutoPrefix, doAutoSave} doNamespaceDecl];
  doc.LoadFromXML(source);

  xb := TFslXmlBuilder.Create;
  try
    xb.Canonicalise := can;
    xb.Start;
    xb.WriteXmlDocument(doc);
    xb.Finish;
    s := xb.Build;
  finally
    xb.Free;
  end;
  if s <> target then
    raise EXmlException.Create('Mismatch');
    *)
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test;
begin
  Test1;
  Test2;
  Test3;
  Test4;
  Test5;
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test1;
var
  s : String;
begin
  s :=
'<?xml version="1.0"?>'+#13#10+
''+#13#10+
'<?xml-stylesheet   href="doc.xsl"'+#13#10+
'   type="text/xsl"   ?>'+#13#10+
''+#13#10+
'<!DOCTYPE doc SYSTEM "doc.dtd">'+#13#10+
''+#13#10+
'<doc>Hello, world!<!-- Comment 1 --></doc>'+#13#10+
''+#13#10+
'<?pi-without-data     ?>'+#13#10+
''+#13#10+
'<!-- Comment 2 -->'+#13#10+
''+#13#10+
'<!-- Comment 3 -->'+#13#10+
''+#13#10;

check(s, [xcmCanonicalise],
'<?xml-stylesheet href="doc.xsl"'+#10+
'   type="text/xsl"   ?>'+#10+
'<doc>Hello, world!</doc>'+#10+
'<?pi-without-data?>');

check(s, [xcmCanonicalise, xcmComments],
'<?xml-stylesheet href="doc.xsl"'+#10+
'   type="text/xsl"   ?>'+#10+
'<doc>Hello, world!<!-- Comment 1 --></doc>'+#10+
'<?pi-without-data?>'+#10+
'<!-- Comment 2 -->'+#10+
'<!-- Comment 3 -->');
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test2;
var
  s : String;
begin
  s :=
'<doc>'+#13#10+
'   <clean>   </clean>'+#13#10+
'   <dirty>   A   B   </dirty>'+#13#10+
'   <mixed>'+#13#10+
'      A'+#13#10+
'      <clean>   </clean>'+#13#10+
'      B'+#13#10+
'      <dirty>   A   B   </dirty>'+#13#10+
'      C'+#13#10+
'   </mixed>'+#13#10+
'</doc>'+#13#10;

check(s, [xcmCanonicalise],
'<doc>'+#10+
'   <clean>   </clean>'+#10+
'   <dirty>   A   B   </dirty>'+#10+
'   <mixed>'+#10+
'      A'+#10+
'      <clean>   </clean>'+#10+
'      B'+#10+
'      <dirty>   A   B   </dirty>'+#10+
'      C'+#10+
'   </mixed>'+#10+
'</doc>');

check(s, [xcmCanonicalise, xcmTrimWhitespace],
'<doc><clean></clean><dirty>A   B</dirty><mixed>A<clean></clean>B<dirty>A   B</dirty>C</mixed></doc>');
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test3;
var
  s : String;
begin
  s :=
'<doc>'+#13#10+
'   <e1   />'+#13#10+
'   <e2   ></e2>'+#13#10+
'   <e3   name = "elem3"   id="elem3"   />'+#13#10+
'   <e4   name="elem4"   id="elem4"   ></e4>'+#13#10+
'   <e5 a:attr="out" b:attr="sorted" attr2="all" attr="I''m"'+#13#10+
'      xmlns:b="http://www.ietf.org"'+#13#10+
'      xmlns:a="http://www.w3.org"'+#13#10+
'      xmlns="http://example.org"/>'+#13#10+
'   <e6 xmlns="" xmlns:a="http://www.w3.org">'+#13#10+
'      <e7 xmlns="http://www.ietf.org">'+#13#10+
'         <e8 xmlns="" xmlns:a="http://www.w3.org">'+#13#10+
'            <e9 xmlns="" xmlns:a="http://www.ietf.org"/>'+#13#10+
'         </e8>'+#13#10+
'      </e7>'+#13#10+
'   </e6>'+#13#10+
'</doc>'+#13#10;

  check(s, [xcmCanonicalise],
'<doc>'+#10+
'   <e1></e1>'+#10+
'   <e2></e2>'+#10+
'   <e3 id="elem3" name="elem3"></e3>'+#10+
'   <e4 id="elem4" name="elem4"></e4>'+#10+
'   <e5 xmlns="http://example.org" xmlns:a="http://www.w3.org" xmlns:b="http://www.ietf.org" attr="I''m" attr2="all" b:attr="sorted" a:attr="out"></e5>'+#10+
'   <e6>'+#10+
'      <e7 xmlns="http://www.ietf.org">'+#10+
'         <e8 xmlns="">'+#10+
'            <e9></e9>'+#10+
'         </e8>'+#10+
'      </e7>'+#10+
'   </e6>'+#10+
'</doc>'
  );
//  check(s, [xcmCanonicalise, xcmPrefixRewrite],
//'<n0:doc xmlns:n0="">'+#10+
//'   <n0:e1></n0:e1>'+#10+
//'   <n0:e2></n0:e2>'+#10+
//'   <n0:e3 id="elem3" name="elem3"></n0:e3>'+#10+
//'   <n0:e4 id="elem4" name="elem4"></n0:e4>'+#10+
//'   <n1:e5 xmlns:n1="http://example.org" xmlns:n2="http://www.ietf.org" xmlns:n3="http://www.w3.org" attr="I''m" attr2="all" n2:attr="sorted" n3:attr="out"></n1:e5>'+#10+
//'   <n0:e6>'+#10+
//'      <n2:e7 xmlns:n2="http://www.ietf.org">'+#10+
//'         <n0:e8>'+#10+
//'            <n0:e9></n0:e9>'+#10+
//'         </n0:e8>'+#10+
//'      </n2:e7>'+#10+
//'   </n0:e6>'+#10+
//'</n0:doc>'+#10
//  );

check(s, [xcmCanonicalise, xcmTrimWhitespace],
'<doc><e1></e1><e2></e2><e3 id="elem3" name="elem3"></e3><e4 id="elem4" name="elem4"></e4><e5 xmlns="http://example.org" xmlns:a="http://www.w3.org" xmlns:b="http://www.ietf.org" attr="I''m" attr2="all" '+'b:attr="sorted" a:attr="out"></e5><e6><e7 xmlns="http://www.ietf.org"><e8 xmlns=""><e9></e9></e8></e7></e6></doc>'
);
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test4;
var
  s : String;
begin
  s :=
'<doc>'+#13#10+
'   <text>First line&#x0d;&#10;Second line</text>'+#13#10+
'   <value>&#x32;</value>'+#13#10+
'   <compute><![CDATA[value>"0" && value<"10" ?"valid":"error"]]></compute>'+#13#10+
'   <compute expr=''value>"0" &amp;&amp; value&lt;"10" ?"valid":"error"''>valid</compute>'+#13#10+
'   <norm attr='' &apos;   &#x20;&#13;&#xa;&#9;   &apos; ''/>'+#13#10+
'   <normNames attr=''   A   &#x20;&#13;&#xa;&#9;   B   ''/>'+#13#10+
'   <normId id='' &apos;&#x20;&#13;&#xa;&#9; &apos; ''/>'+#13#10+
'</doc>'+#13#10;

  check(s, [xcmCanonicalise],
'<doc>'+#10+
'   <text>First line&#xD;'+#10+
'Second line</text>'+#10+
'   <value>2</value>'+#10+
'   <compute>value&gt;"0" &amp;&amp; value&lt;"10" ?"valid":"error"</compute>'+#10+
'   <compute expr="value>&quot;0&quot; &amp;&amp; value&lt;&quot;10&quot; ?&quot;valid&quot;:&quot;error&quot;">valid</compute>'+#10+
'   <norm attr=" ''    &#xD;&#xA;&#x9;   '' "></norm>'+#10+
'   <normNames attr="   A    &#xD;&#xA;&#x9;   B   "></normNames>'+#10+
'   <normId id=" '' &#xD;&#xA;&#x9; '' "></normId>'+#10+
'</doc>');
check(s, [xcmCanonicalise, xcmTrimWhitespace],
'<doc><text>First line&#xD;'+#10+
'Second line</text><value>2</value><compute>value&gt;"0" &amp;&amp; value&lt;"10" ?"valid":"error"</compute><compute expr="value>&quot;0&quot; &amp;&amp; value&lt;&quot;10&quot; ?&quot;valid&quot;:&quot;error&quot;">'+
  'valid</compute><norm attr=" ''    &#xD;&#xA;&#x9;   '' "></norm><normNames attr="   A    &#xD;&#xA;&#x9;   B   "></normNames><normId id=" '' &#xD;&#xA;&#x9; '' "></normId></doc>'
);
end;

class procedure TFslXmlBuilderCanonicalizationTests.Test5;
var
  s : String;
begin
  s :=
'<?xml version="1.0" encoding="ISO-8859-1"?>'+#13#10+
'<doc>&#169;</doc>'+#13#10;

  check(s, [xcmCanonicalise],
'<doc>©</doc>');

end;

(*procedure TFslXmlBuilder.WriteXml(iElement: IXMLNode; first : boolean);
var
  attr : IXMLNode;
  a: Integer;
begin
  NSPush;

  if first then
  begin
    if (iELement.NamespaceURI <> '') and (iELement.LocalName = iELement.NodeName) then
      // we are inheriting a default namespaces
      CurrentNamespaces.DefaultNS := iELement.NamespaceURI;
  end;


  if iElement.AttributeNodes <> nil then
    for a := 0 to iElement.AttributeNodes.Count - 1 do
    begin
      attr := iElement.attributeNodes[a];
      if attr.nodeName.StartsWith('xmlns') then
        DefineNamespace(iElement, attr)
      else if attr.nodeValue = Null then
        AddAttribute(attr.nodeName, '')
      else

        AddAttribute(attr.nodeName, DecodeXML(attr.nodeValue));
    end;

  if iElement.childNodes.count = 0 then
    Tag(iElement.localName)
  else
  begin
    Open(iElement.localName);
    writeXmlNode(iElement, false);
    Close(iElement.localName);
  end;
  NSPop;
end;

procedure TFslXmlBuilder.WriteXmlDocument(iDoc: IXMLDocument);
var
  n : IXMLNode;
  i : integer;
begin
  for i := 0 to iDoc.childNodes.count - 1 do
  begin
    n := iDoc.childNodes[i];
    case n.nodeType of
      ntElement : WriteXml(n, false);
      ntComment : Comment(n.text);
      ntText : Text(n.text);
      ntDocType : DocType(n.text);
      ntProcessingInstr : ProcessingInstruction(n.nodeName, n.text);
    else
      raise EXmlException.Create('Unhandled node type on document: '+inttostr(ord(n.nodeType)));
    end;
  end;
end;

{ntReserved, ntElement, ntAttribute, ntText, ntCData,
    ntEntityRef, ntEntity, ntProcessingInstr, ntComment, ntDocument,
    ntDocType, ntDocFragment, ntNotation);}


procedure TFslXmlBuilder.WriteXmlNode(iNode : IXMLNode; first : boolean);
var
  n : IXMLNode;
  i : integer;
begin
  for i := 0 to inode.childNodes.count - 1 do
  begin
    n := inode.childNodes[i];
    case n.nodeType of
      ntElement : WriteXml(n, first);
      ntComment : Comment(n.text);
      ntText : Text(n.text);
      ntProcessingInstr : ProcessingInstruction(n.nodeName, n.Text);
      ntCData : CData(n.text);
    else
      raise EXmlException.Create('Unhandled node type on document: '+inttostr(ord(n.nodeType)));
    end;
  end;
end;
 *)

 
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
  for a in iELement.Attributes.Values do
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
  for a in iElement.Attributes.Values do
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

function getChildNode(node : IXMLNode; name, ns : String) : IXMLNode;
var
  i : integer;
  child : IXMLNode;
begin
  result := nil;
  for i := 0 to node.ChildNodes.Count - 1 do
  begin
    child  := node.ChildNodes[i];
    if (child.NamespaceURI = ns) and (child.NodeName = name) then
    begin
      result := child;
      exit;
    end;
  end;
end;

function getChildNode(node : IXMLNode; name : String) : IXMLNode;
var
  i : integer;
  child : IXMLNode;
begin
  result := nil;
  for i := 0 to node.ChildNodes.Count - 1 do
  begin
    child  := node.ChildNodes[i];
    if (child.NodeName = name) then
    begin
      result := child;
      exit;
    end;
  end;
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
      elem.Attributes.Remove(attrName)
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


Constructor TFslXMLFormatter.Create;
Begin
  Inherited;

  FBuilder := TFslStringBuilder.Create;
  FAttributes := TFslXMLAttributeList.Create;
  FLine := 1;
  FCol := 0;
  FLastText := true;
End;


Destructor TFslXMLFormatter.Destroy;
Begin
  FAttributes.Free;
  FBuilder.Free;

  Inherited;
End;  


Function TFslXMLFormatter.Clone : TFslXMLFormatter;
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

Function TFslXMLFormatter.Link : TFslXMLFormatter;
Begin 
  Result := TFslXMLFormatter(Inherited Link);
End;  


Procedure TFslXMLFormatter.ProduceHeader;
Begin 
  ProducePretty('<?xml version="1.0"' + UseAttributes + '?>');
  ProducePretty('');
End;  


Procedure TFslXMLFormatter.ProduceOpen(Const sName : String);
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

Procedure TFslXMLFormatter.ProduceClose(Const sName: String);
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


Procedure TFslXMLFormatter.ProduceTextNoEscapeEoln(Const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));
  commitPending;

  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


procedure TFslXMLFormatter.updateForText(s: String);
var
  i : integer;
begin
  i := 1;
  while i <= length(s) do
  begin
    if CharInSet(s[i], [#10, #13]) then
    begin
      inc(Fline);
      Fcol := 0;
      if (i < length(s)) and (s[i+1] <> s[i]) and CharInSet(s[i+1], [#10, #13]) then
        inc(i);
    end
    else
      inc(Fcol);
    inc(i);
  end;
End;


Procedure TFslXMLFormatter.ProduceText(Const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));

  commitPending;
  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


Procedure TFslXMLFormatter.ProduceText(Const sValue: String; processing : TEolnOption = eolnEscape);
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
  updateForText(s);
  FLastText := true;
End;  


Procedure TFslXMLFormatter.ProduceTag(Const sName: String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceTag', 'Tag name must be specified.'));
  commitPending;

  FLastText := false;

  ProducePretty('<' + sName + UseAttributes + ' />');
End;  


Procedure TFslXMLFormatter.ProduceComment(Const sComment: String);
Begin
  commitPending;

  FLastText := false;
  if sComment.endsWith('-') then
    ProducePretty('<!--' + sComment + ' -->')
  else
    ProducePretty('<!--' + sComment + '-->')
End;


Function TFslXMLFormatter.UseAttributes : String;
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


Procedure TFslXMLFormatter.AddAttribute(Const sName, sValue : String; sNs : String = '');
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


Procedure TFslXMLFormatter.AddNamespace(Const sAbbreviation, sURI : String);
Begin
  If sAbbreviation = '' Then
    AddAttribute('xmlns', sURI, #0)
  Else
    AddAttribute('xmlns:' + sAbbreviation, sURI, #0);
End;

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
    UpdateForText(s);
  end;
end;


Constructor TFslXMLParserNamespaces.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TFslXMLParserNamespaces.Clone : TFslXMLParserNamespaces;
Begin
  Result := TFslXMLParserNamespaces(Inherited Clone);
End;


Procedure TFslXMLParserNamespaces.Assign(oObject : TFslObject);
Begin
  Inherited;

  DefaultNamespace := TFslXMLParserNamespaces(oObject).DefaultNamespace;
End;


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


Constructor TFslXMLElement.Create;
Begin
  Inherited;

  FChildrenElementList := Nil;
  FAttributeList := Nil;
End;


Destructor TFslXMLElement.Destroy;
Begin
  FChildrenElementList.Free;
  FAttributeList.Free;

  Inherited;
End;


Function TFslXMLElement.Link : TFslXMLElement;
Begin
  Result := TFslXMLElement(Inherited Link);
End;


Function TFslXMLElement.Clone : TFslXMLElement;
Begin
  Result := TFslXMLElement(Inherited Clone);
End;


Procedure TFslXMLElement.Assign(oObject : TFslObject);
Begin
  Inherited;

  ElementType := TFslXMLElement(oObject).ElementType;

  If HasNamespace Then
    Namespace := TFslXMLElement(oObject).Namespace;

  If HasName Then
    Name := TFslXMLElement(oObject).Name;

  If HasId Then
    Id := TFslXMLElement(oObject).Id;

  If HasChildren Then
    Children := TFslXMLElement(oObject).Children.Clone;

  If HasAttributes Then
    Attributes := TFslXMLElement(oObject).Attributes.Clone;

  If HasContent Then
    Content := TFslXMLElement(oObject).Content;
End;



Procedure TFslXMLElement.Clear;
Begin
  FNamespace := '';
  FName := '';
  FId := '';

  If Assigned(FChildrenElementList) Then
  Begin
    If HasChildren Then
      FChildrenElementList.Clear
    Else
    Begin
      FChildrenElementList.Free;
      FChildrenElementList := Nil;
    End;
  End
  Else If HasChildren Then
    FChildrenElementList := TFslXMLElementList.Create;

  If Assigned(FAttributeList) Then
  Begin
    If HasAttributes Then
      FAttributeList.Clear
    Else
    Begin
      FAttributeList.Free;
      FAttributeList := Nil;
    End;
  End
  Else If HasAttributes Then
    FAttributeList := TFslXMLAttributeList.Create;

  FContent := '';
End;


Procedure TFslXMLElement.SetElementType(Const Value : TFslXMLElementType);
Begin
  If Value <> FElementType Then
  Begin
    Assert(CheckCondition(Value <> FslXMLElementTypeUnknown, 'SetElementType', 'ElementType must not be unknown'));
    FElementType := Value;
    Clear;
  End;
End;


Function TFslXMLElement.HasComment: Boolean;
Begin
  Result := FElementType = FslXMLElementTypeComment;
End;


Function TFslXMLElement.HasText: Boolean;
Begin
  Result := FElementType = FslXMLElementTypeText;
End;


Function TFslXMLElement.HasNamespace : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = FslXMLElementTypeNode;
End;


Function TFslXMLElement.HasName : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = FslXMLElementTypeNode;
End;


Function TFslXMLElement.HasId : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = FslXMLElementTypeNode;
End;


Function TFslXMLElement.HasChildren : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = FslXMLElementTypeNode;
End;


Function TFslXMLElement.HasAttributes : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = FslXMLElementTypeNode;
End;


Function TFslXMLElement.HasContent : Boolean;
Begin
  Assert(CheckCondition(FElementType <> FslXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType In [FslXMLElementTypeText, FslXMLElementTypeComment];
End;


Function TFslXMLElement.GetNamespace : String;
Begin
  Assert(CheckCondition(HasNamespace, 'GetNamespace', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Namespace'])));
  Result := FNamespace;
End;


Procedure TFslXMLElement.SetNamespace(Const Value : String);
Begin
  Assert(CheckCondition(HasNamespace, 'SetNamespace', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Namespace'])));
  FNamespace := Value;
End;


Function TFslXMLElement.GetName : String;
Begin
  Assert(CheckCondition(HasName, 'GetName', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Name'])));
  Result := FName;
End;


Procedure TFslXMLElement.SetName(Const Value : String);
Begin
  Assert(CheckCondition(HasName, 'SetName', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Name'])));
  FName := Value;
End;


Function TFslXMLElement.GetId : String;
Begin
  Assert(CheckCondition(HasId, 'GetId', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Id'])));
  Result := FId;
End;


Procedure TFslXMLElement.SetId(Const Value : String);
Begin
  Assert(CheckCondition(HasId, 'SetId', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Id'])));
  FId := Value;
End;


Function TFslXMLElement.GetChildren : TFslXMLElementList;
Begin
  Assert(CheckCondition(HasChildren, 'GetChildren', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Children'])));
  Result := FChildrenElementList;
End;


Procedure TFslXMLElement.SetChildren(Const Value : TFslXMLElementList);
Begin
  Assert(CheckCondition(HasChildren, 'SetChildren', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Children'])));

  FChildrenElementList.Free;
  FChildrenElementList := Value;
End;


Function TFslXMLElement.GetAttributes : TFslXMLAttributeList;
Begin
  Assert(CheckCondition(HasAttributes, 'GetAttributes', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Attributes'])));
  Result := FAttributeList;
End;


Procedure TFslXMLElement.SetAttributes(Const Value : TFslXMLAttributeList);
Begin
  Assert(CheckCondition(HasAttributes, 'SetAttributes', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Attributes'])));
  FAttributeList.Free;
  FAttributeList := Value;
End;


Function TFslXMLElement.GetContent : String;
Var
  iLoop : Integer;
Begin
  If HasContent Then
    Result := FContent
  Else If Not HasChildren Or (Children.Count = 0) Then
    Result := ''
  Else
  Begin
    Result := Children[0].Content;
    For iLoop := 1 To Children.Count - 1 Do
      Result := Result + Children[iLoop].Content;
  End;
End;


Procedure TFslXMLElement.SetContent(Const Value : String);
Begin
  Assert(CheckCondition(HasContent, 'SetContent', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Content'])));
  FContent := Value;
End;


Function TFslXMLElement.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslXMLObject;
End;


Function TFslXMLElement.Iterator(Const sNamespace, sName : String) : TFslXMLElementIterator;
Begin
  Result := Iterator(sName);
  Result.FNamespace := sNamespace;
End;


Function TFslXMLElement.Iterator(Const sName : String) : TFslXMLElementIterator;
Begin
  Result := Iterator(FslXMLElementTypeNode);
  Result.FName := sName;
End;


Function TFslXMLElement.Iterator(aElementType : TFslXMLElementType) : TFslXMLElementIterator;
Begin
  Result := TFslXMLElementIterator.Create;
  Result.List := Children.Link;
  Result.FElementType := aElementType;
End;


Function TFslXMLElementIterator.Current : TFslXMLElement;
Begin
  Assert(Invariants('Current', Inherited Current, TFslXMLElement, 'Current'));
  Result := TFslXMLElement(Inherited Current);
End;


Function TFslXMLElementIterator.Skip : Boolean;
Begin
  Result := False;
  If FElementType <> FslXMLElementTypeUnknown Then
    Result := Current.ElementType <> FElementType;
  If FNamespace <> '' Then
    Result := Result Or (Current.Namespace <> FNamespace);
  If FName <> '' Then
    Result := Result Or (Current.Name <> FName);
End;


Function TFslXMLElementList.Link : TFslXMLElementList;
Begin
  Result := TFslXMLElementList(Inherited Link);
End;


Function TFslXMLElementList.Clone : TFslXMLElementList;
Begin
  Result := TFslXMLElementList(Inherited Clone);
End;


Function TFslXMLElementList.New : TFslXMLElement;
Begin
  Result := TFslXMLElement(Inherited New);
End;


Function TFslXMLElementList.ItemClass : TFslObjectClass;
Begin
  Result := TFslXMLElement;
End;


Function TFslXMLElementList.GetElement(Const iIndex : Integer) : TFslXMLElement;
Begin
  Result := TFslXMLElement(ObjectByIndex[iIndex]);
End;


Procedure TFslXMLElementList.SetElement(Const iIndex : Integer; Const oValue : TFslXMLElement);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TFslXMLElementList.CompareById(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslXMLElement(pA).Id, TFslXMLElement(pB).Id);
End;


Function TFslXMLElementList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(Integer(TFslXMLElement(pA).ElementType), Integer(TFslXMLElement(pB).ElementType));

  If Result = 0 Then
    Result := StringCompare(TFslXMLElement(pA).Name, TFslXMLElement(pB).Name);
End;


Function TFslXMLElementList.Get(Const aValue : Integer) : TFslXMLElement;
Begin
  Result := TFslXMLElement(Inherited Get(aValue));
End;


Function TFslXMLElementList.IndexById(Const aValue : String) : Integer;
Var
  oElement : TFslXMLElement;
Begin
  oElement := New;
  Try
    oElement.Id := aValue;

    If Not Find(oElement, Result, CompareById) Then
      Result := -1;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLElementList.GetById(Const aValue : String) : TFslXMLElement;
Begin
  Result := Get(IndexById(aValue));
End;


Function TFslXMLElementList.ExistsById(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexById(aValue));
End;


Function TFslXMLElementList.IndexByName(Const aValue : String) : Integer;
Var
  oElement : TFslXMLElement;
Begin
  oElement := New;
  Try
    oElement.ElementType := FslXMLElementTypeNode;
    oElement.Name := aValue;

    If Not Find(oElement, Result, CompareByName) Then
      Result := -1;
  Finally
    oElement.Free;
  End;
End;


Function TFslXMLElementList.GetByName(Const aValue : String) : TFslXMLElement;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TFslXMLElementList.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Constructor TFslXMLDocument.Create;
Begin
  Inherited;

  FRootElement := Nil;
End;


Destructor TFslXMLDocument.Destroy;
Begin
  FRootElement.Free;

  Inherited;
End;


Function TFslXMLDocument.HasRootElement : Boolean;
Begin
  Result := Assigned(FRootElement);
End;


Function TFslXMLDocument.GetRootElement : TFslXMLElement;
Begin
  Assert(Invariants('GetRootElement', FRootElement, TFslXMLElement, 'FRootElement'));

  Result := FRootElement;
End;


Procedure TFslXMLDocument.SetRootElement(Const Value : TFslXMLElement);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetRootElement', Value, TFslXMLElement, 'Value'));

  FRootElement.Free;
  FRootElement := Value;
End;


Function TFslXMLDocument.Link : TFslXMLDocument;
Begin
  Result := TFslXMLDocument(Inherited Link);
End;


Function TFslXMLDocument.Clone : TFslXMLDocument;
Begin
  Result := TFslXMLDocument(Inherited Clone);
End;


Procedure TFslXMLDocument.Assign(oObject : TFslObject);
Begin
  Inherited;

  RootElement := TFslXMLDocument(oObject).RootElement.Clone;
End;



Function TFslXMLDocument.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslXMLObject;
End;


Procedure TFslXMLDocument.Clear;
Begin
  RootElement := Nil;
End;


Function TFslXMLDocumentList.Link : TFslXMLDocumentList;
Begin
  Result := TFslXMLDocumentList(Inherited Link);
End;


Function TFslXMLDocumentList.Clone : TFslXMLDocumentList;
Begin
  Result := TFslXMLDocumentList(Inherited Clone);
End;


Function TFslXMLDocumentList.New : TFslXMLDocument;
Begin
  Result := TFslXMLDocument(Inherited New);
End;


Function TFslXMLDocumentList.ItemClass : TFslObjectClass;
Begin
  Result := TFslXMLDocument;
End;


Function TFslXMLDocumentList.GetElementByIndex(Const iIndex : Integer) : TFslXMLDocument;
Begin
  Result := TFslXMLDocument(ObjectByIndex[iIndex]);
End;


Procedure TFslXMLDocumentList.SetElementByIndex(Const iIndex : Integer; Const oValue : TFslXMLDocument);
Begin
  ObjectByIndex[iIndex] := oValue;
End;


Function TFslXMLDocumentList.Get(Const aValue : Integer) : TFslXMLDocument;
Begin
  Result := TFslXMLDocument(Inherited Get(aValue));
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

{ TMXmlBuilder }

function TMXmlBuilder.SourceLocation: TSourceLocation;
begin
  result.line := 0;
  result.col := 0;
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
  FSourceLocation.line := 0;
  FSourceLocation.col := 0;
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
  raise EXmlException.Create('Inject is not supported on the FHIR.Support.MXml Builder');
end;

procedure TMXmlBuilder.Build(oStream: TStream);
Var
  b : TBytes;
begin
  assert(FAttributes = nil);
  assert(not FExternal);
  b := TEncoding.UTF8.GetBytes(FStack[0].ToXml(true));
  oStream.Write(b[0], length(b));
end;


{function HasElements(oElem : IXMLDOMElement) : Boolean;
var
  oChild : IXMLDOMNode;
Begin
  Result := False;
  oChild := oElem.firstChild;
  While Not result and (oChild <> nil) Do
  Begin
    result := oChild.nodeType = NODE_ELEMENT;
    oChild := oChild.nextSibling;
  End;
End;
}
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
    oParent.addChild(oElem, true);
    inc(FSourceLocation.col, len+2);
    for iLoop := 0 to FAttributes.Count - 1 Do
      oElem.attributes.addAll(FAttributes);
    FAttributes.Clear;
    FStack.Add(oElem.Link);
    result.line := FSourceLocation.line;
    result.col := FSourceLocation.col;
  finally
    oElem.Free;
  end;
end;

procedure TMXmlBuilder.Close(const sName: String);
begin
  if IsPretty Then
  Begin
    If FStack.Last.HasChildren Then
      FStack.Last.addChild(TMXmlElement.createText(readTextLength(#13#10+pad(-1))), true);
  End;
  FStack.Delete(FStack.Count - 1)
end;


procedure TMXmlBuilder.AddAttribute(const sName, sValue: String);
begin
  FAttributes.AddOrSetValue(sName, TMXmlAttribute.Create(sName, sValue));
  ReadTextLengthWithEscapes(sName+'="', sValue, '"');
end;

function TMXmlBuilder.Text(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addChild(TMXmlElement.createText(ReadTextLengthWithEscapes('', sValue, '')), true);
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
end;


function TMXmlBuilder.Entity(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addChild(TMXmlElement.createText('&'+sValue+';'), true);
  inc(FSourceLocation.col, length(sValue)+2);
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
end;

function TMXmlBuilder.Tag(const sName: String) : TSourceLocation;
begin
  Open(sName);
  Close(sName);
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
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
      inc(FSourceLocation.line);
      FSourceLocation.col := 0;
      if (i < length(s)) and (s[i+1] <> s[i]) and CharInSet(s[i+1], [#10, #13]) then
        inc(i);
    end
    else
      inc(FSourceLocation.col);
    inc(i);
  end;
end;

function TMXmlBuilder.ReadTextLengthWithEscapes(pfx, s, sfx: string): String;
begin
  ReadTextLength(pfx);
  ReadTextLength(FormatTextToXML(s, xmlText));
  ReadTextLength(sfx);
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
  FAttributes := TFslMap<TMXmlAttribute>.Create;
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
  attr : TMXmlAttribute;
begin
  attr := TMXmlAttribute.Create(sName, sValue);
  try
    attr.NamespaceURI := sNamespace;
    attr.LocalName := sName;
    FAttributes.AddOrSetValue(sName, attr.link);
  finally
    attr.free;
  end;
  ReadTextLengthWithEscapes(sName+'="', sValue, '"');
end;

procedure TMXmlBuilder.Start;
begin
  Start(nil);
end;

procedure TMXmlBuilder.StartFragment;
begin
  Start(nil);
end;


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


end.
