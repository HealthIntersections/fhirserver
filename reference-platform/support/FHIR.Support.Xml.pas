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
  FHIR.Support.Strings, FHIR.Support.Text, FHIR.Support.Math,
  FHIR.Support.Objects, FHIR.Support.Stream, FHIR.Support.Collections, FHIR.Support.Filers, FHIR.Support.Exceptions, FHIR.Support.Generics,
  FHIR.Support.MXml;

  
function getChildNode(node : IXMLNode; name, ns : String) : IXMLNode; overload;
function getChildNode(node : IXMLNode; name : String) : IXMLNode; overload;
  
Type
  TXmlCanonicalisationMethod = (xcmCanonicalise, xcmComments, xcmTrimWhitespace, {xcmPrefixRewrite, } xcmQNameAware);
  TXmlCanonicalisationMethodSet = set of TXmlCanonicalisationMethod;

  TXmlBuilderNamespaceList = class (TAdvStringMatch)
  private
    FDefaultNS : String;
    FNew: TStringList;
    FDefaultSet: boolean;
    procedure SetDefaultNS(const Value: String);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function link : TXmlBuilderNamespaceList; overload;
    function clone : TXmlBuilderNamespaceList; overload;

    Procedure Assign(oObject : TAdvObject); Override;

    Property DefaultNS : String read FDefaultNS write SetDefaultNS;
    Property DefaultSet : boolean read FDefaultSet write FDefaultSet;
    Property new : TStringList read FNew;
  end;

  TXmlBuilder = {abstract} class (TAdvObject)
  private
    function GetCurrentNamespaces: TXmlBuilderNamespaceList;
    function getNSAbbrev(iElement: TMXMLElement): String;
  Protected
    FNoHeader: Boolean;
    FCanonicalise: TXmlCanonicalisationMethodSet;
    FIsPretty: Boolean;
    FCharEncoding: String;
    FNamespaces : TAdvObjectList;
  Public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start; overload; virtual; abstract;
    procedure StartFragment; overload; virtual; abstract;
    procedure Finish; overload; virtual; abstract;
    procedure Build(oStream: TStream);   overload; virtual; abstract;
    procedure Build(oStream: TAdvStream);   overload; virtual; abstract;
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
  EAdvXMLObject = Class(EAdvException);

  TAdvXMLParserNamespaces = Class (TAdvStringMatch)
    Private
      FDefaultNamespace : String;

    Public
      Constructor Create; Override;

      Function Clone : TAdvXMLParserNamespaces; Overload;

      Procedure Assign(oObject : TAdvObject); Override;

      Property DefaultNamespace : String Read FDefaultNamespace Write FDefaultNamespace;
  End;

  TAdvXMLAttribute = Class(TAdvPersistent)
    Private
      FNamespace : String;
      FName : String;
      FValue : String;
      FSortKey : String;

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

    Public
      Function Link : TAdvXMLAttribute;
      Function Clone : TAdvXMLAttribute;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Namespace : String Read FNamespace Write FNamespace;
      Property Name : String Read FName Write FName;
      Property Value : String Read FValue Write FValue;
      Property SortKey : String read FSortKey write FSortKey;
  End;

  TAdvXMLElementType = (AdvXMLElementTypeUnknown, AdvXMLElementTypeNode, AdvXMLElementTypeText, AdvXMLElementTypeComment);

  TAdvXMLElementIterator = Class;
  TAdvXMLElementList = Class;

  TAdvXMLAttributeList = Class(TAdvPersistentList)
    Private
      Function GetElementByIndex(Const iIndex : Integer) : TAdvXMLAttribute;
      Procedure SetElementByIndex(Const iIndex : Integer; Const oValue : TAdvXMLAttribute);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Function CompareByNamespacedName(pA, pB : Pointer) : Integer;
      Function CompareByNamespace(pA, pB : Pointer) : Integer;
      Function CompareByName(pA, pB : Pointer) : Integer;
      Function CompareByValue(pA, pB : Pointer) : Integer;
      Function CompareBySortKey(pA, pB : Pointer) : Integer;

      Function Get(Const aValue : Integer) : TAdvXMLAttribute; Reintroduce;

    Public
      Function Link : TAdvXMLAttributeList;
      Function Clone : TAdvXMLAttributeList;

      Function New : TAdvXMLAttribute; Reintroduce;

      Function IndexByNamespacedName(Const aNamespace, aName : String) : Integer;
      Function IndexByNamespace(Const aValue : String) : Integer;
      Function IndexByName(Const aValue : String) : Integer;
      Function IndexByValue(Const aValue : String) : Integer;

      Function GetByNamespacedName(Const aNamespace, aName : String) : TAdvXMLAttribute;
      Function GetByNamespace(Const aValue : String) : TAdvXMLAttribute;
      Function GetByName(Const aValue : String) : TAdvXMLAttribute;
      Function GetByValue(Const aValue : String) : TAdvXMLAttribute;

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

      Property ElementByIndex[Const iIndex : Integer] : TAdvXMLAttribute Read GetElementByIndex Write SetElementByIndex; Default;
  End;

  TAdvXMLAttributeMatch = Class(TAdvStringMatch)
    Private
      Function GetAttribute(Const sKey: String): String;
      Procedure SetAttribute(Const sKey, sValue: String);

    Public
      Constructor Create; Override;

      Property Attribute[Const sKey : String] : String Read GetAttribute Write SetAttribute; Default;
  End;

  TAdvXMLElement = Class(TAdvPersistent)
    Private
      FElementType : TAdvXMLElementType;

      // if element
      FNamespace : String;
      FName : String;
      FID : String;
      FChildrenElementList : TAdvXMLElementList;
      FAttributeList : TAdvXMLAttributeList;

      // if comment or Text
      FContent : String;

      Function GetNamespace : String;
      Function GetName : String;
      Function GetId : String;
      Function GetChildren : TAdvXMLElementList;
      Function GetAttributes : TAdvXMLAttributeList;
      Function GetContent : String;

      Procedure SetElementType(Const Value : TAdvXMLElementType);
      Procedure SetNamespace(Const Value : String);
      Procedure SetName(Const Value : String);
      Procedure SetId(Const Value : String);
      Procedure SetChildren(Const Value : TAdvXMLElementList);
      Procedure SetAttributes(Const Value : TAdvXMLAttributeList);
      Procedure SetContent(Const Value : String);

    Protected
      Function ErrorClass : EAdvExceptionClass; Overload; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvXMLElement;
      Function Clone : TAdvXMLElement;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Function Iterator(Const sNamespace, sName : String) : TAdvXMLElementIterator; Overload;
      Function Iterator(Const sName : String) : TAdvXMLElementIterator; Overload;
      Function Iterator(aElementType : TAdvXMLElementType) : TAdvXMLElementIterator; Overload;

      Procedure Clear;

      Function HasNamespace : Boolean;
      Function HasName : Boolean;
      Function HasId : Boolean;
      Function HasChildren : Boolean;
      Function HasAttributes : Boolean;
      Function HasContent : Boolean;
      Function HasText : Boolean;
      Function HasComment : Boolean;

      Property ElementType : TAdvXMLElementType Read FElementType Write SetElementType;
      Property Namespace : String Read GetNamespace Write SetNamespace;
      Property Name : String Read GetName Write SetName;
      Property Id : String Read GetId Write SetId;
      Property Children : TAdvXMLElementList Read GetChildren Write SetChildren;
      Property Attributes : TAdvXMLAttributeList Read GetAttributes Write SetAttributes;
      Property Content : String Read GetContent Write SetContent;
  End;

  TAdvXMLElementList = Class(TAdvPersistentList)
    Private
      Function GetElement(Const iIndex : Integer) : TAdvXMLElement;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TAdvXMLElement);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Function CompareById(pA, pB : Pointer) : Integer;
      Function CompareByName(pA, pB : Pointer) : Integer;

      Function Get(Const aValue : Integer) : TAdvXMLElement; Reintroduce;

    Public
      Function Link : TAdvXMLElementList;
      Function Clone : TAdvXMLElementList;

      Function New : TAdvXMLElement; Reintroduce;

      Function IndexById(Const aValue : String) : Integer;
      Function GetById(Const aValue : String) : TAdvXMLElement;
      Function ExistsById(Const aValue : String) : Boolean;

      Function IndexByName(Const aValue : String) : Integer;
      Function GetByName(Const aValue : String) : TAdvXMLElement;
      Function ExistsByName(Const aValue : String) : Boolean;

      Property Elements[Const iIndex : Integer] : TAdvXMLElement Read GetElement Write SetElement; Default;
  End;

  TAdvXMLElementIterator = Class(TAdvPersistentListIterator)
    Private
      FElementType : TAdvXMLElementType;
      FNamespace : String;
      FName : String;

    Protected
      Function Skip : Boolean; Overload; Override;

    Public
      Function Current : TAdvXMLElement; Reintroduce;
  End;

  TAdvXMLDocument = Class(TAdvPersistent)
    Private
      FRootElement : TAdvXMLElement;

      Function GetRootElement : TAdvXMLElement;
      Procedure SetRootElement(Const Value : TAdvXMLElement);

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvXMLDocument;
      Function Clone : TAdvXMLDocument;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Procedure Clear;

      Function HasRootElement : Boolean;
      Property RootElement : TAdvXMLElement Read GetRootElement Write SetRootElement;
  End;

  TAdvXMLDocumentList = Class(TAdvPersistentList)
    Private
      Function GetElementByIndex(Const iIndex : Integer) : TAdvXMLDocument;
      Procedure SetElementByIndex(Const iIndex : Integer; Const oValue : TAdvXMLDocument);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Function Get(Const aValue : Integer) : TAdvXMLDocument; Reintroduce;

    Public
      Function Link : TAdvXMLDocumentList;
      Function Clone : TAdvXMLDocumentList;

      Function New : TAdvXMLDocument; Reintroduce;

      Property ElementByIndex[Const iIndex : Integer] : TAdvXMLDocument Read GetElementByIndex Write SetElementByIndex; Default;
  End;

  TAdvXMLNamespaceEntry = Class(TAdvObject)
    Private
      FKey : String;
      FValues : TAdvStringList;

      Function GetValue: String;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Push(Const Value: String);
      Procedure Pop;

      Function HasValue : Boolean;

      Property Key : String Read FKey Write FKey;
      Property Value : String Read GetValue;
  End;

  TAdvXMLNamespaceEntryList = Class(TAdvObjectList)
    Private
      Function GetEntryByIndex(Const iIndex: Integer): TAdvXMLNamespaceEntry;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Function CompareByKey(pA, pB : Pointer) : Integer;

    Public
      Function IndexByKey(Const sKey : String) : Integer;

      Procedure SortedByKey;

      Property EntryByIndex[Const iIndex : Integer] : TAdvXMLNamespaceEntry Read GetEntryByIndex; Default;
  End;

  TAdvXMLNamespaceLevel = Class(TAdvObject)
    Private
      FEntryList : TAdvXMLNamespaceEntryList;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Property EntryList : TAdvXMLNamespaceEntryList Read FEntryList;
  End;

  TAdvXMLNamespaceLevelList = Class(TAdvObjectList)
    Private
      Function GetLevelByIndex(Const iIndex: Integer): TAdvXMLNamespaceLevel;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property LevelByIndex[Const iIndex : Integer] : TAdvXMLNamespaceLevel Read GetLevelByIndex; Default;
  End;

  TAdvXMLNamespaceManager = Class(TAdvObject)
    Private
      FEntryList : TAdvXMLNamespaceEntryList;
      FLevelList : TAdvXMLNamespaceLevelList;

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Push(Const oAttributes : TAdvStringMatch);
      Procedure Pop;

      Function DefaultNamespace : String;
      Function NamespaceOfPrefix(Const sPrefix: String): String;
      Function LocalNameOf(Const sElementName: String) : String;
      Function PrefixOf(Const sElementName: String) : String;
      Function NamespaceOf(Const sElementName: String) : String;

      Procedure ListPrefixes(Const oPrefixNamespaces: TAdvStringMatch);
  End;

  TAdvXMLFormatter = Class(TAdvTextFormatter)
    Private
      FAttributes : TAdvXMLAttributeList;
      FBuilder : TAdvStringBuilder;
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
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvXMLFormatter;
      Function Clone : TAdvXMLFormatter;

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

      Property Attributes : TAdvXMLAttributeList Read FAttributes;
      property Line : integer read FLine;
      property Col : integer read FCol;
      property NoDense : Boolean read FNoDense write FNoDense;
      property CanonicalEntities : boolean read FCanonicalEntities write FCanonicalEntities;
  End;

  TAdvXmlBuilder = class (TXmlBuilder)
  private
    mem : TAdvMemoryStream;
    buf : TAdvBuffer;
    xml : TAdvXMLFormatter;

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
    Procedure Build(oStream: TAdvStream);  Overload; override;
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
  TAdvXmlBuilderCanonicalizationTests = class (TAdvObject)
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

  TXmlPatchEngine = class (TAdvObject)
  private
    class procedure remove(doc : TMXmlDocument; sel : String; target : TMXmlElement);
    class procedure add(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure addChildNodes(doc : TMXmlDocument; source, target : TMXmlElement; pos : String);
  public
    class procedure execute(doc : TMXmlDocument; target : TMXmlElement; patch : TMXmlElement);
  end;

  EAdvXMLNamespaceManager = Class(EAdvException)
  End;


Const
  NAMES_ADVXMLELEMENTTYPE : Array [TAdvXMLElementType] Of String = ('Unknown', 'Element', 'Text', 'Comment');

Type
  TMXmlBuilder = class (TXmlBuilder)
  private
    FExternal : Boolean;
    FStack : TAdvList<TMXmlElement>;
    FDoc : TMXmlDocument;
    FAttributes : TAdvMap<TMXmlAttribute>;
    FSourceLocation : TSourceLocation;
    Function Pad(offset : integer = 0) : String;
    function ReadTextLength(s : string):String;
    function ReadTextLengthWithEscapes(pfx, s, sfx : string):String;
  Public
    Constructor Create; Override;
    Destructor Destroy; override;
    Procedure Start(oNode : TMXmlElement); overload;
    Procedure Start(); overload; override;
    Procedure StartFragment; override;
    Procedure Finish; override;
    Procedure Build(oStream: TStream);  Overload; override;
    Procedure Build(oStream: TAdvStream);  Overload; override;
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

implementation

Procedure TAdvXmlBuilder.Start;
var
  i: Integer;
begin
  buf := TAdvBuffer.Create;
  mem := TAdvMemoryStream.Create;
  mem.Buffer := buf.Link;
  xml := TAdvXMLFormatter.Create;
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

Procedure TAdvXmlBuilder.StartFragment;
begin
  raise Exception.Create('Not Supported yet');
end;

Procedure TAdvXmlBuilder.Finish;
begin
  xml.Free;
  xml := nil;
  mem.Free;
  mem := nil;
end;

function TAdvXmlBuilder.getNSRep(uri, name: String): String;
begin
  if (uri = CurrentNamespaces.DefaultNS) then
    result := name
  else if CurrentNamespaces.ExistsByKey(uri) then
    result := CurrentNamespaces.Matches[uri]+':'+name
  else
    raise Exception.Create('Unregistered namespace '+uri);
end;

procedure TAdvXmlBuilder.inject(const bytes: TBytes);
begin
  xml.ProduceBytes(bytes);
end;

{function TAdvXmlBuilder.nsIsUsed(elem: IXmlNode; ns: String): boolean;
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

Procedure TAdvXmlBuilder.Build(oStream: TStream);
begin
  buf.SaveToStream(oStream);
end;

Procedure TAdvXmlBuilder.Build(oStream: TAdvStream);
begin
  buf.SaveToStream(oStream);
end;

Function TAdvXmlBuilder.Build : String;
begin
  result := buf.AsUnicode;
end;

procedure TAdvXmlBuilder.SetCanonicalEntities(const Value: boolean);
begin
  FCanonicalEntities := Value;
  if assigned(xml) then
    xml.CanonicalEntities := value;
end;

Function TAdvXmlBuilder.SourceLocation : TSourceLocation;
begin
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
end;

Procedure TAdvXmlBuilder.Comment(Const sContent : String);
begin
  if not (xcmCanonicalise in FCanonicalise) or (xcmComments in FCanonicalise) then
  begin
    if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
      xml.Produce(#10);
    xml.ProduceComment(sContent);
    started := true;
  end;
end;


{procedure TAdvXmlBuilder.defineNamespace(element, attribute: IXMLNode);
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
procedure TAdvXmlBuilder.defineNS(abbrev, uri: String);
begin
  CurrentNamespaces.Add(uri, abbrev);
end;

destructor TAdvXmlBuilder.destroy;
begin
  buf.Free;
  inherited;
end;

procedure TAdvXmlBuilder.DocType(sText: String);
begin
  if not (xcmCanonicalise in FCanonicalise) then
    raise Exception.Create('Not supported');
end;

Procedure TAdvXmlBuilder.AddAttribute(Const sName, sValue : String);
begin
  if (sName = 'xmlns') and CurrentNamespaces.DefaultSet then
  begin
    if sValue <> CurrentNamespaces.DefaultNS then
      raise Exception.Create('Namespace mismatch');
    CurrentNamespaces.DefaultSet := false;
  end;

  xml.AddAttribute(sName, sValue);
end;

Procedure TAdvXmlBuilder.AddAttributeNS(Const sNamespace, sName, sValue : String);
begin
  xml.AddAttribute(getNSRep(sNamespace, sName), sValue, sNamespace);
end;

function TAdvXmlBuilder.Tag(Const sName : String) : TSourceLocation;
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

function TAdvXmlBuilder.Open(Const sName : String) : TSourceLocation;
begin
  if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
    xml.Produce(#10);
  if CurrentNamespaces.DefaultSet then
  begin
    if not xml.Attributes.ExistsByName('xmlns') then
      xml.AddNamespace('', CurrentNamespaces.DefaultNS)
    else if xml.Attributes.GetByName('xmlns').Value <> CurrentNamespaces.DefaultNS then
       raise Exception.Create('XML default namespce misalignment');
    CurrentNamespaces.DefaultSet := false;
  end;
  xml.ProduceOpen(sName);
  inc(depth);
  started := true;
  result.line := 0; //xml.line;
  result.col := 0; //xml.col;
end;

procedure TAdvXmlBuilder.ProcessingInstruction(sName, sText: String);
begin
  if (xcmCanonicalise in FCanonicalise) and started and (depth = 0) then
    xml.Produce(#10);
  xml.ProducePI(sName, sText);
  started := true;
end;

procedure TAdvXmlBuilder.CData(text: String);
begin
  if xcmCanonicalise in FCanonicalise then
    xml.ProduceText(text, eolnCanonical)
  else
    xml.produceCData(text)
end;

Procedure TAdvXmlBuilder.Close(Const sName : String);
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

function TAdvXmlBuilder.Text(Const sValue : String) : TSourceLocation;
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


function TAdvXmlBuilder.Entity(Const sValue : String) : TSourceLocation;
begin
  raise Exception.Create('entities not supported');
end;

function TAdvXmlBuilder.TagText(Const sName, sValue : String) : TSourceLocation;
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

{ TAdvXmlBuilderCanonicalizationTests }

class procedure TAdvXmlBuilderCanonicalizationTests.check(source: String; can: TXmlCanonicalisationMethodSet; target: String);
{var
  doc : IXMLDocument;
  dom : TMXMLDocument;
  xb : TAdvXmlBuilder;
  s : String;}
begin
(*  dom := TMXMLDocument.Create;
  doc := dom;
  dom.DOMVendor := OpenXML4Factory;
  dom.ParseOptions := [poPreserveWhiteSpace];
  dom.Options := [{doNodeAutoCreate, doNodeAutoIndent, doAttrNull,  doAutoPrefix, doAutoSave} doNamespaceDecl];
  doc.LoadFromXML(source);

  xb := TAdvXmlBuilder.Create;
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
    raise Exception.Create('Mismatch');
    *)
end;

class procedure TAdvXmlBuilderCanonicalizationTests.Test;
begin
  Test1;
  Test2;
  Test3;
  Test4;
  Test5;
end;

class procedure TAdvXmlBuilderCanonicalizationTests.Test1;
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

class procedure TAdvXmlBuilderCanonicalizationTests.Test2;
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

class procedure TAdvXmlBuilderCanonicalizationTests.Test3;
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

class procedure TAdvXmlBuilderCanonicalizationTests.Test4;
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

class procedure TAdvXmlBuilderCanonicalizationTests.Test5;
var
  s : String;
begin
  s :=
'<?xml version="1.0" encoding="ISO-8859-1"?>'+#13#10+
'<doc>&#169;</doc>'+#13#10;

  check(s, [xcmCanonicalise],
'<doc>©</doc>');

end;

(*procedure TAdvXmlBuilder.WriteXml(iElement: IXMLNode; first : boolean);
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

procedure TAdvXmlBuilder.WriteXmlDocument(iDoc: IXMLDocument);
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
      raise Exception.Create('Unhandled node type on document: '+inttostr(ord(n.nodeType)));
    end;
  end;
end;

{ntReserved, ntElement, ntAttribute, ntText, ntCData,
    ntEntityRef, ntEntity, ntProcessingInstr, ntComment, ntDocument,
    ntDocType, ntDocFragment, ntNotation);}


procedure TAdvXmlBuilder.WriteXmlNode(iNode : IXMLNode; first : boolean);
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
      raise Exception.Create('Unhandled node type on document: '+inttostr(ord(n.nodeType)));
    end;
  end;
end;
 *)

 
{ TXmlBuilderNamespaceList }

procedure TXmlBuilderNamespaceList.Assign(oObject: TAdvObject);
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
  FNamespaces := TAdvObjectList.create;
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
      ntDocument: raise Exception.Create('Illegal XML - document inside element');
      ntAttribute: raise Exception.Create('Illegal XML - attribute in element chilren');
      ntProcessingInstruction: ProcessingInstruction(c.Name, c.Text);
      ntDocumentDeclaration: raise Exception.Create('Illegal DTD not supported');
      ntCData: raise Exception.Create('Illegal CDATA not supported');
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
    raise Exception.Create('No Target Document Root Found');
  if target = nil then
    raise Exception.Create('No Target Element Found');
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');
  patch := patch.firstElement;
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');

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
      raise Exception.Create('Unknown Patch Operation "'+patch.localName+'"');
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
  matches : TAdvList<TMXmlNode>;
  elem : TMXmlElement;
  sel, typ, pos : String;
begin
  sel := op.attribute['sel'];
  typ := op.attribute['type'];
  pos := op.attribute['pos'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('No match found for '+sel+' performing addition');
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes performing addition');

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
      raise Exception.Create('Unknown value for type: '+typ);
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
var
  matches : TAdvList<TMXmlNode>;
  n, ce : TMXmlElement;
  sel : String;
begin
  sel := op.attribute['sel'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('No match found for '+sel+' performing replace');
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes performing replace');

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
      raise Exception.Create('Unsupported Node Type for replace');
    end;
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.remove(doc : TMXmlDocument; sel: String; target: TMXmlElement);
var
  matches : TAdvList<TMXmlNode>;
  elem : TMXmlElement;
  attrName : String;
begin
  checkEndsWithAttribute(sel, attrName);

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('Nothing to delete found for xpath '+sel);
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes');

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
      raise Exception.Create('Node type not supported '+inttostr(ord(n.nodeType)));
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
      raise Exception.Create('Pos "'+pos+'" not supported');
    n := n.next;
  end;
end;


Constructor TAdvXMLFormatter.Create;
Begin
  Inherited;

  FBuilder := TAdvStringBuilder.Create;
  FAttributes := TAdvXMLAttributeList.Create;
  FLine := 1;
  FCol := 0;
  FLastText := true;
End;


Destructor TAdvXMLFormatter.Destroy;
Begin
  FAttributes.Free;
  FBuilder.Free;

  Inherited;
End;  


Function TAdvXMLFormatter.Clone : TAdvXMLFormatter;
Begin 
  Result := TAdvXMLFormatter(Inherited Clone);
End;  


procedure TAdvXMLFormatter.commitPending;
begin
  if (FPending <> '') then
  begin
    LevelUp;
    ProducePretty('<'+FPending+'>');
    LevelDown;
    FPending := '';
  end;
end;

Function TAdvXMLFormatter.Link : TAdvXMLFormatter;
Begin 
  Result := TAdvXMLFormatter(Inherited Link);
End;  


Procedure TAdvXMLFormatter.ProduceHeader;
Begin 
  ProducePretty('<?xml version="1.0"' + UseAttributes + '?>');
  ProducePretty('');
End;  


Procedure TAdvXMLFormatter.ProduceOpen(Const sName : String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceOpen', 'Open tag name must be specified.'));

  commitPending;

  FLastText := false;

  FPending := sName + UseAttributes;

  LevelDown;

  if FNoDense then
    CommitPending;
End;  


procedure TAdvXMLFormatter.ProduceBytes(const aBytes: TBytes);
begin
  commitPending;
  inherited ProduceBytes(aBytes);
end;

procedure TAdvXMLFormatter.ProduceCData(const sText: String);
begin
  commitPending;

  FLastText := false;
  ProducePretty('<![CDATA[' + sText + ']]>');
end;

Procedure TAdvXMLFormatter.ProduceClose(Const sName: String);
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


Procedure TAdvXMLFormatter.ProduceTextNoEscapeEoln(Const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));
  commitPending;

  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


procedure TAdvXMLFormatter.updateForText(s: String);
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


Procedure TAdvXMLFormatter.ProduceText(Const sName, sValue: String);
Begin
  Assert(CheckCondition(sName <> '', 'ProduceText', 'Tag name for text must be specified.'));

  commitPending;
  FLastText := false;
  ProducePretty('<' + sName + UseAttributes + '>' + FormatTextToXML(sValue, xmlText) + '</' + sName + '>');
End;


Procedure TAdvXMLFormatter.ProduceText(Const sValue: String; processing : TEolnOption = eolnEscape);
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


Procedure TAdvXMLFormatter.ProduceTag(Const sName: String);
Begin 
  Assert(CheckCondition(sName <> '', 'ProduceTag', 'Tag name must be specified.'));
  commitPending;

  FLastText := false;

  ProducePretty('<' + sName + UseAttributes + ' />');
End;  


Procedure TAdvXMLFormatter.ProduceComment(Const sComment: String);
Begin
  commitPending;

  FLastText := false;
  if sComment.endsWith('-') then
    ProducePretty('<!--' + sComment + ' -->')
  else
    ProducePretty('<!--' + sComment + '-->')
End;


Function TAdvXMLFormatter.UseAttributes : String;
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


Procedure TAdvXMLFormatter.AddAttribute(Const sName, sValue : String; sNs : String = '');
var
  attr : TAdvXMLAttribute;
Begin
  attr := TAdvXMLAttribute.Create;
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


Procedure TAdvXMLFormatter.AddNamespace(Const sAbbreviation, sURI : String);
Begin
  If sAbbreviation = '' Then
    AddAttribute('xmlns', sURI, #0)
  Else
    AddAttribute('xmlns:' + sAbbreviation, sURI, #0);
End;

procedure TAdvXMLFormatter.ProducePI(const sName, sText: String);
begin
  Assert(CheckCondition(sName <> '', 'ProducePI', 'PI name must be specified.'));
  commitPending;

  FLastText := false;

  if sText <> '' then
    ProducePretty('<?' + sName + ' ' + sText+ '?>')
  else
    ProducePretty('<?' + sName + UseAttributes + sText+ '?>');
end;

procedure TAdvXMLFormatter.ProducePretty(sValue: String);
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


Constructor TAdvXMLParserNamespaces.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TAdvXMLParserNamespaces.Clone : TAdvXMLParserNamespaces;
Begin
  Result := TAdvXMLParserNamespaces(Inherited Clone);
End;


Procedure TAdvXMLParserNamespaces.Assign(oObject : TAdvObject);
Begin
  Inherited;

  DefaultNamespace := TAdvXMLParserNamespaces(oObject).DefaultNamespace;
End;


Function TAdvXMLAttribute.Link : TAdvXMLAttribute;
Begin
  Result := TAdvXMLAttribute(Inherited Link);
End;


Function TAdvXMLAttribute.Clone : TAdvXMLAttribute;
Begin
  Result := TAdvXMLAttribute(Inherited Clone);
End;


Procedure TAdvXMLAttribute.Assign(oObject : TAdvObject);
Begin
  Inherited;

  Namespace := TAdvXMLAttribute(oObject).Namespace;
  Name := TAdvXMLAttribute(oObject).Name;
  Value := TAdvXMLAttribute(oObject).Value;
  SortKey := TAdvXMLAttribute(oObject).SortKey;
End;


Procedure TAdvXMLAttribute.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['Namespace'].DefineString(FNamespace);
  oFiler['Name'].DefineString(FName);
  oFiler['Value'].DefineString(FValue);
  oFiler['SortKey'].DefineString(FSortKey);
End;


Function TAdvXMLAttribute.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvXMLObject;
End;


Function TAdvXMLAttributeList.Link : TAdvXMLAttributeList;
Begin
  Result := TAdvXMLAttributeList(Inherited Link);
End;


Function TAdvXMLAttributeList.Clone : TAdvXMLAttributeList;
Begin
  Result := TAdvXMLAttributeList(Inherited Clone);
End;


Function TAdvXMLAttributeList.New : TAdvXMLAttribute;
Begin
  Result := TAdvXMLAttribute(Inherited New);
End;


Function TAdvXMLAttributeList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvXMLAttribute;
End;


Function TAdvXMLAttributeList.GetElementByIndex(Const iIndex : Integer) : TAdvXMLAttribute;
Begin
  Result := TAdvXMLAttribute(ObjectByIndex[iIndex]);
End;


Procedure TAdvXMLAttributeList.SetElementByIndex(Const iIndex : Integer; Const oValue : TAdvXMLAttribute);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TAdvXMLAttributeList.CompareByNamespacedName(pA, pB : Pointer) : Integer;
Begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).Namespace, TAdvXMLAttribute(pB).Namespace);
  If Result = 0 Then
    Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).Name, TAdvXMLAttribute(pB).Name);
End;


function TAdvXMLAttributeList.CompareBySortKey(pA, pB: Pointer): Integer;
begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).SortKey, TAdvXMLAttribute(pB).SortKey);
end;

Function TAdvXMLAttributeList.CompareByNamespace(pA, pB : Pointer) : Integer;
Begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).Namespace, TAdvXMLAttribute(pB).Namespace);
End;


Function TAdvXMLAttributeList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).Name, TAdvXMLAttribute(pB).Name);
End;


Function TAdvXMLAttributeList.CompareByValue(pA, pB : Pointer) : Integer;
Begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLAttribute(pA).Value, TAdvXMLAttribute(pB).Value);
End;


Function TAdvXMLAttributeList.IndexByNamespacedName(Const aNamespace, aName: String) : Integer;
Var
  oElement : TAdvXMLAttribute;
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


Function TAdvXMLAttributeList.IndexByNamespace(Const aValue : String) : Integer;
Var
  oElement : TAdvXMLAttribute;
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


Function TAdvXMLAttributeList.IndexByName(Const aValue : String) : Integer;
Var
  oElement : TAdvXMLAttribute;
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


Function TAdvXMLAttributeList.IndexByValue(Const aValue : String) : Integer;
Var
  oElement : TAdvXMLAttribute;
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


Function TAdvXMLAttributeList.Get(Const aValue : Integer) : TAdvXMLAttribute;
Begin
  Result := TAdvXMLAttribute(Inherited Get(aValue));
End;


Function TAdvXMLAttributeList.GetByNamespacedName(Const aNamespace, aName : String) : TAdvXMLAttribute;
Begin
  Result := Get(IndexByNamespacedName(aNamespace, aName));
End;


Function TAdvXMLAttributeList.GetByNamespace(Const aValue : String) : TAdvXMLAttribute;
Begin
  Result := Get(IndexByNamespace(aValue));
End;


Function TAdvXMLAttributeList.GetByName(Const aValue : String) : TAdvXMLAttribute;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TAdvXMLAttributeList.GetByValue(Const aValue : String) : TAdvXMLAttribute;
Begin
  Result := Get(IndexByValue(aValue));
End;


Function TAdvXMLAttributeList.ExistsByNamespacedName(Const aNamespace, aName : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByNamespacedName(aNamespace, aName));
End;


Function TAdvXMLAttributeList.ExistsByNamespace(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByNamespace(aValue));
End;


Function TAdvXMLAttributeList.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Function TAdvXMLAttributeList.ExistsByValue(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Procedure TAdvXMLAttributeList.SortedByNamespacedName;
Begin
  SortedBy(CompareByNamespacedName);
End;


procedure TAdvXMLAttributeList.SortedBySortKey;
begin
  SortedBy(CompareBySortKey);
end;

Procedure TAdvXMLAttributeList.SortedByNamespace;
Begin
  SortedBy(CompareByNamespace);
End;


Procedure TAdvXMLAttributeList.SortedByName;
Begin
  SortedBy(CompareByName);
End;


Procedure TAdvXMLAttributeList.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;


Function TAdvXMLAttributeList.IsSortedByNamespacedName : Boolean;
Begin
  Result := IsSortedBy(CompareByNamespacedName);
End;


function TAdvXMLAttributeList.IsSortedBySortKey: Boolean;
begin
  Result := IsSortedBy(CompareBySortKey);
end;

Function TAdvXMLAttributeList.IsSortedByNamespace : Boolean;
Begin
  Result := IsSortedBy(CompareByNamespace);
End;


Function TAdvXMLAttributeList.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;


Function TAdvXMLAttributeList.IsSortedByValue : Boolean;
Begin
  Result := IsSortedBy(CompareByValue);
End;


Constructor TAdvXMLAttributeMatch.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TAdvXMLAttributeMatch.GetAttribute(Const sKey: String): String;
Begin
  Result := GetValueByKey(sKey);
End;


Procedure TAdvXMLAttributeMatch.SetAttribute(Const sKey, sValue: String);
Begin
  SetValueByKey(sKey, sValue);
End;


Constructor TAdvXMLElement.Create;
Begin
  Inherited;

  FChildrenElementList := Nil;
  FAttributeList := Nil;
End;


Destructor TAdvXMLElement.Destroy;
Begin
  FChildrenElementList.Free;
  FAttributeList.Free;

  Inherited;
End;


Function TAdvXMLElement.Link : TAdvXMLElement;
Begin
  Result := TAdvXMLElement(Inherited Link);
End;


Function TAdvXMLElement.Clone : TAdvXMLElement;
Begin
  Result := TAdvXMLElement(Inherited Clone);
End;


Procedure TAdvXMLElement.Assign(oObject : TAdvObject);
Begin
  Inherited;

  ElementType := TAdvXMLElement(oObject).ElementType;

  If HasNamespace Then
    Namespace := TAdvXMLElement(oObject).Namespace;

  If HasName Then
    Name := TAdvXMLElement(oObject).Name;

  If HasId Then
    Id := TAdvXMLElement(oObject).Id;

  If HasChildren Then
    Children := TAdvXMLElement(oObject).Children.Clone;

  If HasAttributes Then
    Attributes := TAdvXMLElement(oObject).Attributes.Clone;

  If HasContent Then
    Content := TAdvXMLElement(oObject).Content;
End;


Procedure TAdvXMLElement.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['ElementType'].DefineEnumerated(FElementType, NAMES_ADVXMLELEMENTTYPE);
  oFiler['Namespace'].DefineString(FNamespace);
  oFiler['Name'].DefineString(FName);
  oFiler['Id'].DefineString(FId);
  oFiler['Children'].DefineObject(FChildrenElementList, TAdvXMLElementList);
  oFiler['Attributes'].DefineObject(FAttributeList, TAdvXMLAttributeList);
  oFiler['Content'].DefineString(FContent);
End;


Procedure TAdvXMLElement.Clear;
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
    FChildrenElementList := TAdvXMLElementList.Create;

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
    FAttributeList := TAdvXMLAttributeList.Create;

  FContent := '';
End;


Procedure TAdvXMLElement.SetElementType(Const Value : TAdvXMLElementType);
Begin
  If Value <> FElementType Then
  Begin
    Assert(CheckCondition(Value <> AdvXMLElementTypeUnknown, 'SetElementType', 'ElementType must not be unknown'));
    FElementType := Value;
    Clear;
  End;
End;


Function TAdvXMLElement.HasComment: Boolean;
Begin
  Result := FElementType = AdvXMLElementTypeComment;
End;


Function TAdvXMLElement.HasText: Boolean;
Begin
  Result := FElementType = AdvXMLElementTypeText;
End;


Function TAdvXMLElement.HasNamespace : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = AdvXMLElementTypeNode;
End;


Function TAdvXMLElement.HasName : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = AdvXMLElementTypeNode;
End;


Function TAdvXMLElement.HasId : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = AdvXMLElementTypeNode;
End;


Function TAdvXMLElement.HasChildren : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = AdvXMLElementTypeNode;
End;


Function TAdvXMLElement.HasAttributes : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType = AdvXMLElementTypeNode;
End;


Function TAdvXMLElement.HasContent : Boolean;
Begin
  Assert(CheckCondition(FElementType <> AdvXMLElementTypeUnknown, 'GetNamespace', 'Element has no type assigned'));
  Result := FElementType In [AdvXMLElementTypeText, AdvXMLElementTypeComment];
End;


Function TAdvXMLElement.GetNamespace : String;
Begin
  Assert(CheckCondition(HasNamespace, 'GetNamespace', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Namespace'])));
  Result := FNamespace;
End;


Procedure TAdvXMLElement.SetNamespace(Const Value : String);
Begin
  Assert(CheckCondition(HasNamespace, 'SetNamespace', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Namespace'])));
  FNamespace := Value;
End;


Function TAdvXMLElement.GetName : String;
Begin
  Assert(CheckCondition(HasName, 'GetName', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Name'])));
  Result := FName;
End;


Procedure TAdvXMLElement.SetName(Const Value : String);
Begin
  Assert(CheckCondition(HasName, 'SetName', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Name'])));
  FName := Value;
End;


Function TAdvXMLElement.GetId : String;
Begin
  Assert(CheckCondition(HasId, 'GetId', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Id'])));
  Result := FId;
End;


Procedure TAdvXMLElement.SetId(Const Value : String);
Begin
  Assert(CheckCondition(HasId, 'SetId', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Id'])));
  FId := Value;
End;


Function TAdvXMLElement.GetChildren : TAdvXMLElementList;
Begin
  Assert(CheckCondition(HasChildren, 'GetChildren', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Children'])));
  Result := FChildrenElementList;
End;


Procedure TAdvXMLElement.SetChildren(Const Value : TAdvXMLElementList);
Begin
  Assert(CheckCondition(HasChildren, 'SetChildren', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Children'])));

  FChildrenElementList.Free;
  FChildrenElementList := Value;
End;


Function TAdvXMLElement.GetAttributes : TAdvXMLAttributeList;
Begin
  Assert(CheckCondition(HasAttributes, 'GetAttributes', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Attributes'])));
  Result := FAttributeList;
End;


Procedure TAdvXMLElement.SetAttributes(Const Value : TAdvXMLAttributeList);
Begin
  Assert(CheckCondition(HasAttributes, 'SetAttributes', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Attributes'])));
  FAttributeList.Free;
  FAttributeList := Value;
End;


Function TAdvXMLElement.GetContent : String;
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


Procedure TAdvXMLElement.SetContent(Const Value : String);
Begin
  Assert(CheckCondition(HasContent, 'SetContent', StringFormat('Element of Type %s does not have property %s', [NAMES_ADVXMLELEMENTTYPE[FElementType], 'Content'])));
  FContent := Value;
End;


Function TAdvXMLElement.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvXMLObject;
End;


Function TAdvXMLElement.Iterator(Const sNamespace, sName : String) : TAdvXMLElementIterator;
Begin
  Result := Iterator(sName);
  Result.FNamespace := sNamespace;
End;


Function TAdvXMLElement.Iterator(Const sName : String) : TAdvXMLElementIterator;
Begin
  Result := Iterator(AdvXMLElementTypeNode);
  Result.FName := sName;
End;


Function TAdvXMLElement.Iterator(aElementType : TAdvXMLElementType) : TAdvXMLElementIterator;
Begin
  Result := TAdvXMLElementIterator.Create;
  Result.List := Children.Link;
  Result.FElementType := aElementType;
End;


Function TAdvXMLElementIterator.Current : TAdvXMLElement;
Begin
  Assert(Invariants('Current', Inherited Current, TAdvXMLElement, 'Current'));
  Result := TAdvXMLElement(Inherited Current);
End;


Function TAdvXMLElementIterator.Skip : Boolean;
Begin
  Result := False;
  If FElementType <> AdvXMLElementTypeUnknown Then
    Result := Current.ElementType <> FElementType;
  If FNamespace <> '' Then
    Result := Result Or (Current.Namespace <> FNamespace);
  If FName <> '' Then
    Result := Result Or (Current.Name <> FName);
End;


Function TAdvXMLElementList.Link : TAdvXMLElementList;
Begin
  Result := TAdvXMLElementList(Inherited Link);
End;


Function TAdvXMLElementList.Clone : TAdvXMLElementList;
Begin
  Result := TAdvXMLElementList(Inherited Clone);
End;


Function TAdvXMLElementList.New : TAdvXMLElement;
Begin
  Result := TAdvXMLElement(Inherited New);
End;


Function TAdvXMLElementList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvXMLElement;
End;


Function TAdvXMLElementList.GetElement(Const iIndex : Integer) : TAdvXMLElement;
Begin
  Result := TAdvXMLElement(ObjectByIndex[iIndex]);
End;


Procedure TAdvXMLElementList.SetElement(Const iIndex : Integer; Const oValue : TAdvXMLElement);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TAdvXMLElementList.CompareById(pA, pB : Pointer) : Integer;
Begin
  Result := FHIR.Support.Strings.StringCompare(TAdvXMLElement(pA).Id, TAdvXMLElement(pB).Id);
End;


Function TAdvXMLElementList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(Integer(TAdvXMLElement(pA).ElementType), Integer(TAdvXMLElement(pB).ElementType));

  If Result = 0 Then
    Result := StringCompare(TAdvXMLElement(pA).Name, TAdvXMLElement(pB).Name);
End;


Function TAdvXMLElementList.Get(Const aValue : Integer) : TAdvXMLElement;
Begin
  Result := TAdvXMLElement(Inherited Get(aValue));
End;


Function TAdvXMLElementList.IndexById(Const aValue : String) : Integer;
Var
  oElement : TAdvXMLElement;
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


Function TAdvXMLElementList.GetById(Const aValue : String) : TAdvXMLElement;
Begin
  Result := Get(IndexById(aValue));
End;


Function TAdvXMLElementList.ExistsById(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexById(aValue));
End;


Function TAdvXMLElementList.IndexByName(Const aValue : String) : Integer;
Var
  oElement : TAdvXMLElement;
Begin
  oElement := New;
  Try
    oElement.ElementType := AdvXMLElementTypeNode;
    oElement.Name := aValue;

    If Not Find(oElement, Result, CompareByName) Then
      Result := -1;
  Finally
    oElement.Free;
  End;
End;


Function TAdvXMLElementList.GetByName(Const aValue : String) : TAdvXMLElement;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TAdvXMLElementList.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Constructor TAdvXMLDocument.Create;
Begin
  Inherited;

  FRootElement := Nil;
End;


Destructor TAdvXMLDocument.Destroy;
Begin
  FRootElement.Free;

  Inherited;
End;


Function TAdvXMLDocument.HasRootElement : Boolean;
Begin
  Result := Assigned(FRootElement);
End;


Function TAdvXMLDocument.GetRootElement : TAdvXMLElement;
Begin
  Assert(Invariants('GetRootElement', FRootElement, TAdvXMLElement, 'FRootElement'));

  Result := FRootElement;
End;


Procedure TAdvXMLDocument.SetRootElement(Const Value : TAdvXMLElement);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetRootElement', Value, TAdvXMLElement, 'Value'));

  FRootElement.Free;
  FRootElement := Value;
End;


Function TAdvXMLDocument.Link : TAdvXMLDocument;
Begin
  Result := TAdvXMLDocument(Inherited Link);
End;


Function TAdvXMLDocument.Clone : TAdvXMLDocument;
Begin
  Result := TAdvXMLDocument(Inherited Clone);
End;


Procedure TAdvXMLDocument.Assign(oObject : TAdvObject);
Begin
  Inherited;

  RootElement := TAdvXMLDocument(oObject).RootElement.Clone;
End;


Procedure TAdvXMLDocument.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['RootElement'].DefineObject(FRootElement, TAdvXMLElement);
End;


Function TAdvXMLDocument.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvXMLObject;
End;


Procedure TAdvXMLDocument.Clear;
Begin
  RootElement := Nil;
End;


Function TAdvXMLDocumentList.Link : TAdvXMLDocumentList;
Begin
  Result := TAdvXMLDocumentList(Inherited Link);
End;


Function TAdvXMLDocumentList.Clone : TAdvXMLDocumentList;
Begin
  Result := TAdvXMLDocumentList(Inherited Clone);
End;


Function TAdvXMLDocumentList.New : TAdvXMLDocument;
Begin
  Result := TAdvXMLDocument(Inherited New);
End;


Function TAdvXMLDocumentList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvXMLDocument;
End;


Function TAdvXMLDocumentList.GetElementByIndex(Const iIndex : Integer) : TAdvXMLDocument;
Begin
  Result := TAdvXMLDocument(ObjectByIndex[iIndex]);
End;


Procedure TAdvXMLDocumentList.SetElementByIndex(Const iIndex : Integer; Const oValue : TAdvXMLDocument);
Begin
  ObjectByIndex[iIndex] := oValue;
End;


Function TAdvXMLDocumentList.Get(Const aValue : Integer) : TAdvXMLDocument;
Begin
  Result := TAdvXMLDocument(Inherited Get(aValue));
End;


Constructor TAdvXMLNamespaceEntry.Create;
Begin
  Inherited;

  FValues := TAdvStringList.Create;
End;


Destructor TAdvXMLNamespaceEntry.Destroy;
Begin
  FValues.Free;

  Inherited;
End;


Function TAdvXMLNamespaceEntry.GetValue: String;
Begin
  Result := FValues.StringByIndex[FValues.Count - 1];
End;


Function TAdvXMLNamespaceEntry.HasValue : Boolean;
Begin
  Result := Not FValues.IsEmpty;
End;


Procedure TAdvXMLNamespaceEntry.Pop;
Begin
  FValues.DeleteByIndex(FValues.Count - 1);
End;


Procedure TAdvXMLNamespaceEntry.Push(Const Value: String);
Begin
  FValues.Add(Value);
End;


Function TAdvXMLNamespaceEntryList.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TAdvXMLNamespaceEntry(pA).Key, TAdvXMLNamespaceEntry(pB).Key);
End;


Function TAdvXMLNamespaceEntryList.IndexByKey(Const sKey: String): Integer;
Var
  oEntry : TAdvXMLNamespaceEntry;
Begin
  oEntry := TAdvXMLNamespaceEntry(ItemNew);
  Try
    oEntry.Key := sKey;

    Result := IndexBy(oEntry, CompareByKey);
  Finally
    oEntry.Free;
  End;
End;


Procedure TAdvXMLNamespaceEntryList.SortedByKey;
Begin
  SortedBy(CompareByKey);
End;


Function TAdvXMLNamespaceEntryList.ItemClass: TAdvObjectClass;
Begin
  Result := TAdvXMLNamespaceEntry;
End;


Function TAdvXMLNamespaceEntryList.GetEntryByIndex(Const iIndex: Integer): TAdvXMLNamespaceEntry;
Begin
  Result := TAdvXMLNamespaceEntry(ObjectByIndex[iIndex]);
End;


Constructor TAdvXMLNamespaceLevel.Create;
Begin
  Inherited;

  FEntryList := TAdvXMLNamespaceEntryList.Create;
  FEntryList.SortedByKey;
End;


Destructor TAdvXMLNamespaceLevel.Destroy;
Begin
  FEntryList.Free;

  Inherited;
End;


Function TAdvXMLNamespaceLevelList.ItemClass: TAdvObjectClass;
Begin
  Result := TAdvXMLNamespaceLevel;
End;


Function TAdvXMLNamespaceLevelList.GetLevelByIndex(Const iIndex: Integer): TAdvXMLNamespaceLevel;
Begin
  Result := TAdvXMLNamespaceLevel(ObjectByIndex[iIndex]);
End;


Constructor TAdvXMLNamespaceManager.Create;
Var
  oDefaultEntry : TAdvXMLNamespaceEntry;
Begin
  Inherited;

  FLevelList := TAdvXMLNamespaceLevelList.Create;
  FEntryList := TAdvXMLNamespaceEntryList.Create;

  // Add default namespace entry.
  oDefaultEntry := TAdvXMLNamespaceEntry.Create;
  oDefaultEntry.Key := '';
  oDefaultEntry.Push('');
  FEntryList.Add(oDefaultEntry);

  // Add "xml" default entry
  oDefaultEntry := TAdvXMLNamespaceEntry.Create;
  oDefaultEntry.Key := 'xml';
  oDefaultEntry.Push('http://www.w3.org/XML/1998/namespace');
  FEntryList.Add(oDefaultEntry);

  FEntryList.SortedByKey;
  FEntryList.PreventDuplicates;
End;


Destructor TAdvXMLNamespaceManager.Destroy;
Begin
  FEntryList.Free;
  FLevelList.Free;

  Inherited;
End;


Function TAdvXMLNamespaceManager.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvXMLNamespaceManager;
End;


Function TAdvXMLNamespaceManager.DefaultNamespace: String;
Begin
  Result := NamespaceOfPrefix('');
End;


Procedure TAdvXMLNamespaceManager.ListPrefixes(Const oPrefixNamespaces: TAdvStringMatch);
Var
  iEntryIndex : Integer;
  oEntry : TAdvXMLNamespaceEntry;
Begin
  oPrefixNamespaces.Clear;

  For iEntryIndex := 0 To FEntryList.Count - 1 Do
  Begin
    oEntry := FEntryList[iEntryIndex];

    oPrefixNamespaces.Add(oEntry.Key, oEntry.Value);
  End;
End;


Function TAdvXMLNamespaceManager.LocalNameOf(Const sElementName: String): String;
Var
  iColonIndex : Integer;
Begin
  iColonIndex := FHIR.Support.Strings.StringFind(sElementName, ':');

  If iColonIndex <= 0  Then
    Result := sElementName
  Else
    Result := StringCopy(sElementName, iColonIndex + 1, MaxInt);
End;


Function TAdvXMLNamespaceManager.NamespaceOf(Const sElementName: String): String;
Begin
  Result := NamespaceOfPrefix(PrefixOf(sElementName));
End;


Function TAdvXMLNamespaceManager.NamespaceOfPrefix(Const sPrefix: String) : String;
Var
  iEntryIndex : Integer;
Begin
  iEntryIndex := FEntryList.IndexByKey(sPrefix);

  If iEntryIndex < 0 Then
    RaiseError('NamespaceOfPrefix', StringFormat('The namespace prefix ''%s'' has not beed defined.', [sPrefix]));

  Result := FEntryList[iEntryIndex].Value;
End;


Procedure TAdvXMLNamespaceManager.Pop;
Var
  oLevel : TAdvXMLNamespaceLevel;
  oEntry : TAdvXMLNamespaceEntry;
  iEntryIndex : Integer;
Begin
  oLevel := TAdvXMLNamespaceLevel(FLevelList.RemoveLast);
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


Function TAdvXMLNamespaceManager.PrefixOf(Const sElementName: String): String;
Var
  iColonIndex : Integer;
Begin
  iColonIndex := FHIR.Support.Strings.StringFind(sElementName, ':');

  If iColonIndex <= 0 Then
    Result := ''
  Else
    Result := StringCopy(sElementName, 1, iColonIndex - 1);
End;


Procedure TAdvXMLNamespaceManager.Push(Const oAttributes: TAdvStringMatch);
Const
  XMLNS_STRING = 'xmlns';
  XMLNS_LENGTH = Length(XMLNS_STRING);
Var
  oLevel : TAdvXMLNamespaceLevel;
  iAttributeIndex : Integer;
  sName : String;
  sPrefix : String;
  sNamespace : String;
  iEntryIndex : Integer;
  oEntry : TAdvXMLNamespaceEntry;
Begin
  oLevel := TAdvXMLNamespaceLevel.Create;
  Try
    For iAttributeIndex := 0 To oAttributes.Count - 1 Do
    Begin
      // Determine if this is a namespace name
      sName := oAttributes.KeyByIndex[iAttributeIndex];

      If FHIR.Support.Strings.StringStartsWith(sName, XMLNS_STRING) Then
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
          oEntry := TAdvXMLNamespaceEntry.Create;
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

procedure TMXmlBuilder.Build(oStream: TAdvStream);
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
  raise Exception.Create('Inject is not supported on the FHIR.Support.MXml Builder');
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
  raise Exception.Create('Not supported yet');
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
  FStack := TAdvList<TMXmlElement>.Create;
  FAttributes := TAdvMap<TMXmlAttribute>.Create;
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
  raise Exception.Create('Not supported yet');
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



end.
