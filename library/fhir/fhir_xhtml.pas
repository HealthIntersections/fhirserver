unit fhir_xhtml;

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
  SysUtils, Classes, Character,
  fsl_base, fsl_utilities, fsl_fpc, fsl_stream, fsl_xml, fsl_http,
  fhir_objects;

const
  XHTML_NS = 'http://www.w3.org/1999/xhtml';

Type
  TFHIRAttributeList = class;
  TFHIRAttribute = class (TFHIRObject)
  private
    FName : String;
    FValue : String;
  protected
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(Name : String; Value : String); Overload;

    function Link : TFHIRAttribute; Overload;
    function Clone : TFHIRAttribute; Overload;
    procedure Assign(oSource : TFslObject); override;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;

    property Name : String read FName write FName;
    property Value : String read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function Equals(other : TObject) : boolean; override;
  end;

  TFHIRAttributeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFHIRAttributeList;
    function GetCurrent : TFHIRAttribute;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFHIRAttributeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRAttribute read GetCurrent;
  end;

  TFHIRAttributeList = class (TFHIRObjectList)
  private
    Function GetItemN(index : Integer) : TFHIRAttribute;
  public
    function Link : TFHIRAttributeList; Overload;
    Function IndexOf(value : TFHIRAttribute) : Integer;
    Function Item(index : Integer) : TFHIRAttribute;
    Function Count : Integer; Overload;
    Property Segments[index : Integer] : TFHIRAttribute read GetItemN; default;
    Function Get(name : String):String;
    function GetEnumerator : TFHIRAttributeListEnumerator;
    Procedure SetValue(name : String; value :String);
    Procedure Add(name : String; value :String); overload;
  End;



  TFhirXHtmlNodeList = class;

  {
    An xhtml node. Has a type - is either an element, with a name and children,
    or a different type of node with text (usually text or comment)
  }
  TFhirXHtmlNode = class (TFHIRObject)
  private
    FLocation : TSourceLocation;
    FNodeType : TFHIRHtmlNodeType;
    FName : String;
    FAttributes : TFHIRAttributeList;
    FChildNodes : TFhirXHtmlNodeList;
    FContent : String;
    procedure SetNodeType(const Value: TFHIRHtmlNodeType);
    function GetChildNodes: TFhirXHtmlNodeList;
    function GetAttributes: TFHIRAttributeList;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    constructor Create(nodeType : TFHIRHtmlNodeType); Overload;
    constructor Create(name : String); Overload;
    destructor Destroy; Override;
    function Link : TFhirXHtmlNode; Overload;
    function Clone : TFhirXHtmlNode; Overload;
    procedure Assign(oSource : TFslObject); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;

    function HasAttributes : boolean;
    property Attributes : TFHIRAttributeList read GetAttributes;
    function allChildrenAreText : boolean;
    function isPrimitive : boolean; override;
    function primitiveValue : string; override;
    function fhirType : String; override;
    function NsDecl : String; virtual;
    function hasAttribute(name : String): boolean;
    procedure attribute(name, value : String);
    property Location : TSourceLocation read FLocation write FLocation;

    {
      plain text content of html
    }
    function AsPlainText : String;
    function AsHtmlPage : String;
    function isEmpty : boolean; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;

    function Equals(other : TObject) : boolean; override;
  published
    {
      The type of the node - fhntElement, fhntText, fhntComment, fhntDocument

      Note that documents are not encountered in FHIR resources
    }
    property NodeType : TFHIRHtmlNodeType read FNodeType write SetNodeType;

    {
      The name of the element, if the node is an element

      Note that namespaces are not supported in FHIR xhtml
    }
    property Name : String read FName write FName;

    {
      The content of the element if it is a text or comment node
    }
    property Content : String read FContent write FContent;

    {
      The children of the node, if it is an element
    }
    property ChildNodes : TFhirXHtmlNodeList read GetChildNodes;

    {
      Add a text node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddText(content : String) : TFhirXHtmlNode;

    {
      Add a comment node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddComment(content : String) : TFhirXHtmlNode;

    {
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddChild(name : String) : TFhirXHtmlNode;

    {
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddTag(name : String) : TFhirXHtmlNode;

    {
      Get an attribute by it's name

      Note that namespaces are not supported in FHIR xhtml
    }
    Function GetAttribute(name : String) : String;

    {
      Set the value of an attribute. Create it if it doesn't exist

      Note that namespaces are not supported in FHIR xhtml
    }
    function SetAttribute(name, value : String) : TFhirXHtmlNode;
  end;

  TFHIRXhtmlNodeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFHIRXhtmlNodeList;
    function GetCurrent : TFHIRXhtmlNode;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFHIRXhtmlNodeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRXhtmlNode read GetCurrent;
  end;

  {
    A list of Xhtml Nodes
  }
  TFHIRXHtmlNodeList = class (TFHIRObjectList)
  private
    Function GetItemN(index : Integer) : TFHIRXHtmlNode;
    Procedure SetItemN(index : Integer; value : TFHIRXHtmlNode);
  public
      Function Link : TFHIRXHtmlNodeList; Overload;
    Function Clone : TFHIRXHtmlNodeList; Overload;
    function GetEnumerator : TFHIRXhtmlNodeListEnumerator;
      {
      Add an Xhtml Node to the end of the list.
    }
    Function Append : TFHIRXHtmlNode;
    {
      Add an already existing Xhtml Node to the end of the list.
    }
    Procedure AddItem(value : TFHIRXHtmlNode);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TFHIRXHtmlNode) : Integer;
    {
       Insert an Xhtml node before the designated index (0 = first item)
    }
    Function Insert(index : Integer) : TFHIRXHtmlNode;
    {
       Insert an existing Xhtml Node before the designated index (0 = first item)
    }
    Procedure InsertItem(index : Integer; value : TFHIRXHtmlNode);
    {
       Get the indexth Xhtml Node. (0 = first item)
    }
    Function Item(index : Integer) : TFHIRXHtmlNode;
    {
       Set the indexth Xhtml Node. (0 = first item)
    }
    Procedure SetItemByIndex(index : Integer; value : TFHIRXHtmlNode);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the indexth item. The first item is index 0.
    }
    Procedure Remove(index : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Nodes[index : Integer] : TFHIRXHtmlNode read GetItemN write SetItemN; default;
  End;


  TFHIRXhtmlParserOption = (xopTrimWhitspace, xopValidatorMode);
  TFHIRXhtmlParserOptions = set of TFHIRXhtmlParserOption;

  TFHIRXhtmlParser = class
  private
    class Function checkNS(options: TFHIRXhtmlParserOptions; focus : TFhirXHtmlNode; node : TMXmlElement; defaultNS : String)  : String;
    class procedure doCompose(node: TFhirXHtmlNode; xml : TXmlBuilder);
    class function doParse(const lang : THTTPLanguages; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode; static;
  public
    class Function parse(const lang : THTTPLanguages; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; node : TMXmlElement; path : String; defaultNS : String) : TFhirXHtmlNode; overload;
    class Function parse(const lang : THTTPLanguages; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode; Overload;

    class procedure compose(node: TFhirXHtmlNode; xml : TXmlBuilder); overload;
    class procedure compose(node: TFhirXHtmlNode; s : TFslStringBuilder; canonicalise : boolean; indent : integer = 0; relativeReferenceAdjustment : integer = 0); overload;
    class function  compose(node: TFhirXHtmlNode; canonicalise : boolean = false) : String; overload;

    class Function attributeIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name, attr, value : String) : boolean;
    class Function elementIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name : String) : boolean;
  end;

  TCDANarrativeParser = class (TFslObject)
  private
    class procedure processAttributes(ed : TMXmlElement; x : TFHIRXhtmlNode; names : Array of String); overload;
    class procedure processChildren(ed : TMXmlElement; x : TFHIRXhtmlNode); overload;
    class procedure processChildNode(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processBreak(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processCaption(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processCol(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processColGroup(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processContent(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processFootNote(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processFootNodeRef(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processItem(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processlinkHtml(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processList(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processParagraph(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processRenderMultiMedia(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processSub(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processSup(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTable(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTBody(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTd(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTFoot(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTh(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTHead(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;
    class procedure processTr(n : TMXmlElement; xn : TFHIRXhtmlNode); overload;

    class procedure processAttributes(xml : TXmlBuilder; x : TFHIRXHtmlNode; names : array of String); overload;
    class procedure processChildren(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processChildNode(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processBreak(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processCaption(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processCol(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processColGroup(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processContent(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processFootNote(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processFootNodeRef(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processItem(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processlinkHtml(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processList(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processParagraph(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processRenderMultiMedia(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processSub(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processSup(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTable(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTBody(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTd(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTFoot(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTh(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTHead(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
    class procedure processTr(xml : TXmlBuilder; x : TFHIRXHtmlNode); overload;
  public
    class function parse(element : TMXmlElement) : TFhirXHtmlNode;
    class procedure render(xml : TXmlBuilder; node: TFhirXHtmlNode);
  end;

function compareDeep(div1, div2 : TFhirXHtmlNode; allowNull : boolean) : boolean; overload;


implementation

{ TFHIRAttributeList }
procedure TFHIRAttributeList.Add(name, value: String);
begin
  SetValue(name, value);
end;

function TFHIRAttributeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFHIRAttributeList.Get(name: String): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    if GetItemN(i).Name = name then
      result := GetItemN(i).Value;
end;

function TFHIRAttributeList.GetEnumerator: TFHIRAttributeListEnumerator;
begin
  result := TFHIRAttributeListEnumerator.Create(self.Link);
end;

function TFHIRAttributeList.GetItemN(index: Integer): TFHIRAttribute;
begin
  result := TFHIRAttribute(ObjectByIndex[index]);
end;

function TFHIRAttributeList.IndexOf(value: TFHIRAttribute): Integer;
begin
  result := IndexByReference(value);
end;

function TFHIRAttributeList.Item(index: Integer): TFHIRAttribute;
begin
  result := TFHIRAttribute(ObjectByIndex[index]);
end;

function TFHIRAttributeList.Link: TFHIRAttributeList;
begin
  result := TFHIRAttributeList(inherited Link);
end;

procedure TFHIRAttributeList.SetValue(name, value: String);
var
  i : integer;
  b : boolean;
  attr : TFHIRAttribute;
begin
  b := false;
  for i := 0 to Count - 1 do
    if GetItemN(i).Name = name then
    begin
      b := true;
      GetItemN(i).Value := value;
    end;
  if not b then
  begin
    attr := TFHIRAttribute.create;
    try
      attr.name := name;
      attr.value := value;
      add(attr.link);
    finally
      attr.free;
    end;
  end;
end;

{ TFHIRAttribute }

procedure TFHIRAttribute.Assign(oSource: TFslObject);
begin
  inherited;
  FName := TFHIRAttribute(oSource).FName;
  FValue := TFHIRAttribute(oSource).FValue;
end;

function TFHIRAttribute.Clone: TFHIRAttribute;
begin
  result := TFHIRAttribute(inherited Clone);
end;

constructor TFHIRAttribute.Create(Name, Value: String);
begin
  Create;
  FName := Name;
  FValue := Value;
end;

function TFHIRAttribute.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.equals(other : TObject): boolean;
begin
  result := inherited equals(other) and (FName = TFHIRAttribute(other).FName) and (FValue = TFHIRAttribute(other).FValue);
end;

function TFHIRAttribute.fhirType: String;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.getId: String;
begin
  result := '';
end;

function TFHIRAttribute.getTypesForProperty(propName : string): String;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRAttribute.isEmpty: boolean;
begin
  result := inherited isEmpty and (FName = '') and (FValue = '');
end;

function TFHIRAttribute.Link: TFHIRAttribute;
begin
  result := TFHIRAttribute(inherited Link);
end;

procedure TFHIRAttribute.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, nil, FName));
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, nil, FValue));
end;

function TFHIRAttribute.makeCodeValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.makeIntValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.makeStringValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

procedure TFHIRAttribute.setIdValue(id: String);
begin
end;

function TFHIRAttribute.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFHIRAttribute.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFhirAttributeListEnumerator }

Constructor TFhirAttributeListEnumerator.Create(list : TFhirAttributeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirAttributeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAttributeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirAttributeListEnumerator.GetCurrent : TFhirAttribute;
begin
  Result := FList[FIndex];
end;

{ TFHIRXHtmlNodeList }

procedure TFHIRXHtmlNodeList.AddItem(value: TFHIRXHtmlNode);
begin
  add(value.Link);
end;

function TFHIRXHtmlNodeList.Append: TFHIRXHtmlNode;
begin
  result := TFhirXHtmlNode.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFHIRXHtmlNodeList.ClearItems;
begin
  Clear;
end;

function TFHIRXHtmlNodeList.Clone: TFHIRXHtmlNodeList;
begin
  result := TFHIRXHtmlNodeList(inherited Clone);
end;

function TFHIRXHtmlNodeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFHIRXHtmlNodeList.GetEnumerator: TFHIRXhtmlNodeListEnumerator;
begin
  result := TFHIRXhtmlNodeListEnumerator.Create(self.Link);
end;

function TFHIRXHtmlNodeList.GetItemN(index: Integer): TFHIRXHtmlNode;
begin
  result := TFHIRXHtmlNode(ObjectByIndex[index]);
end;

function TFHIRXHtmlNodeList.IndexOf(value: TFHIRXHtmlNode): Integer;
begin
  result := IndexByReference(value);
end;

function TFHIRXHtmlNodeList.Insert(index: Integer): TFHIRXHtmlNode;
begin
  result := TFhirXHtmlNode.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFHIRXHtmlNodeList.InsertItem(index: Integer; value: TFHIRXHtmlNode);
begin
  Inherited Insert(index, value.Link);
end;

function TFHIRXHtmlNodeList.Item(index: Integer): TFHIRXHtmlNode;
begin
  result := TFHIRXHtmlNode(ObjectByIndex[index]);
end;

function TFHIRXHtmlNodeList.Link: TFHIRXHtmlNodeList;
begin
  result := TFHIRXHtmlNodeList(inherited Link);
end;

procedure TFHIRXHtmlNodeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFHIRXHtmlNodeList.SetItemByIndex(index: Integer; value: TFHIRXHtmlNode);
begin
  Nodes[index] := value.Link;
end;

procedure TFHIRXHtmlNodeList.SetItemN(index: Integer; value: TFHIRXHtmlNode);
begin
  ObjectByIndex[index] := value;
end;

{ TFhirXHtmlNode }

function TFhirXHtmlNode.AddChild(name: String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  node := TFhirXHtmlNode.create;
  try
    node.NodeType := fhntElement;
    node.FName := name;
    ChildNodes.add(node.Link);
    result := node;
  finally
    node.free;
  end;
end;

function TFhirXHtmlNode.AddComment(content: String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  node := TFhirXHtmlNode.create;
  try
    node.NodeType := fhntComment;
    node.FContent := content;
    ChildNodes.add(node.Link);
    result := node;
  finally
    node.free;
  end;
end;

function TFhirXHtmlNode.AddTag(name: String): TFhirXHtmlNode;
begin
  result := AddChild(name);
end;

function TFhirXHtmlNode.AddText(content : String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  if content = '' then
    result := nil
  else
  begin
    node := TFhirXHtmlNode.create;
    try
      node.NodeType := fhntText;
      node.FContent := content;
      ChildNodes.add(node.Link);
      result := node;
    finally
      node.free;
    end;
  end;
end;

function TFhirXHtmlNode.allChildrenAreText: boolean;
var
  i : integer;
begin
  result := FChildNodes.Count > 0;
  for i := 0 to FChildNodes.Count - 1 do
    result := result and (FChildNodes[i].FNodeType = fhntText);
end;

function TFhirXHtmlNode.AsHtmlPage: String;
begin
  if (self = nil) then
    result := '<html><body>No Narrative</body></html>'
  else
    result := '<html><body>'+TFHIRXhtmlParser.Compose(self)+'</body></html>'
end;

function TFhirXHtmlNode.AsPlainText: String;
var
  s : String;
  i : integer;
begin
  case NodeType of
    fhntText : result := Content;
    fhntComment : result := '';
  else // fhntElement, fhntDocumenk
    s := '';
    for i := 0 to ChildNodes.count - 1 do
      s := s + ChildNodes[i].AsPlainText;
    if (Name = 'p') or (Name = 'h2') or (Name = 'h3') or (Name = 'h4') or (Name = 'h5') or (Name = 'h6') or (name='div') then
      result := s + #13#10
    else if Name = 'li' then
      result := '* '+ s +#13#10
    else
      result := s;
  end;
end;

procedure TFhirXHtmlNode.Assign(oSource: TFslObject);
begin
  inherited;
  NodeType := TFhirXHtmlNode(oSource).FNodeType;
  FName := TFhirXHtmlNode(oSource).FName;
  FContent := TFhirXHtmlNode(oSource).FContent;
  if TFhirXHtmlNode(oSource).HasAttributes Then
    Attributes.assign(TFhirXHtmlNode(oSource).Attributes);
  if TFhirXHtmlNode(oSource).FChildNodes <> nil then
    ChildNodes.assign(TFhirXHtmlNode(oSource).FChildNodes);
end;

procedure TFhirXHtmlNode.attribute(name, value: String);
var
  attr : TFHIRAttribute;
begin
  if FAttributes = nil then
    FAttributes := TFHIRAttributeList.Create;

  for attr in FAttributes do
    if attr.Name = name then
    begin
      attr.Value := value;
      exit;
    end;
  FAttributes.Add(name, value);
end;

function TFhirXHtmlNode.Clone: TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode(inherited Clone);
end;

constructor TFhirXHtmlNode.Create(nodeType: TFHIRHtmlNodeType);
begin
  Create;
  self.NodeType := fhntElement;
end;

constructor TFhirXHtmlNode.Create(name: String);
begin
  Create;
  NodeType := fhntElement;
  FName := name;
end;

function TFhirXHtmlNode.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

constructor TFhirXHtmlNode.Create;
begin
  inherited;
end;

destructor TFhirXHtmlNode.Destroy;
begin
  FChildNodes.Free;
  FAttributes.Free;
  inherited;
end;

function TFhirXHtmlNode.equals(other : TObject): boolean;
var
  o : TFhirXHtmlNode;
  i : integer;
begin
  result := inherited equals(other);
  if result then
  begin
    o := TFhirXHtmlNode(other);
    if FNodeType <> o.FNodeType then
      exit(false);
    if FName <> o.FName then
      exit(false);

    if (FAttributes <> nil) <> (o.FAttributes <> nil) then
      exit(false);
    if (FAttributes <> nil) then
    begin
      if FAttributes.Count <> o.FAttributes.Count then
        exit(false);
      for i := 0 to FAttributes.Count - 1 do
        if FAttributes[i].Value <> o.FAttributes.Get(FAttributes[i].Name) then
          exit(false);
    end;
    if (FChildNodes <> nil) <> (o.FChildNodes <> nil) then
      exit(false);
    if (FChildNodes <> nil) then
    begin
      if FChildNodes.Count <> o.FChildNodes.Count then
        exit(false);
      for i := 0 to FChildNodes.Count - 1 do
        if not FChildNodes[i].equals(o.FChildNodes[i]) then
          exit(false);
    end;
    if FContent <> o.FContent then
      exit(false);
  end;
end;

function TFhirXHtmlNode.FhirType: String;
begin
  result := 'xhtml';
end;

function TFhirXHtmlNode.GetAttribute(name: String): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to FAttributes.Count - 1 Do
    if FAttributes[i].Name = name then
    begin
      result := FAttributes[i].Value;
      exit;
    end;
end;

function TFhirXHtmlNode.GetAttributes: TFHIRAttributeList;
begin
  if FAttributes = nil then
    FAttributes := TFHIRAttributeList.create;
  result := FAttributes;
end;

function TFhirXHtmlNode.GetChildNodes: TFhirXHtmlNodeList;
begin
  if FChildNodes = nil then
    FChildNodes := TFhirXHtmlNodeList.create;
  result := FChildNodes;
end;

procedure TFhirXHtmlNode.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  i : integer;
begin
  inherited;
  if FAttributes <> Nil then
    for i := 0 to FAttributes.Count - 1 do
      if name = '@'+FAttributes[i].Name then
        list.add(FAttributes[i].Link);
  if FChildNodes <> Nil then
    for i := 0 to FChildNodes.Count - 1 do
      if name = FChildNodes[i].FName then
        list.add(FChildNodes[i].Link);
  if name = 'text()' then
    list.add(TFHIRObjectText.create(FContent));
end;

function TFhirXHtmlNode.getId: String;
begin
  result := '';
end;

function TFhirXHtmlNode.getTypesForProperty(propName : string): String;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFhirXHtmlNode.hasAttribute(name: String): boolean;
var
  attr : TFHIRAttribute;
begin
  result := false;
  if FAttributes <> nil then
    for attr in FAttributes do
      if attr.Name = name then
        exit(true);
end;

function TFhirXHtmlNode.HasAttributes: boolean;
begin
  result := (FAttributes <> nil) and (FAttributes.Count > 0);
end;

function TFhirXHtmlNode.hasExtensions: boolean;
begin
  result := false;
end;

function TFhirXHtmlNode.isEmpty: boolean;
begin
  result := inherited isEmpty and (FName = '') and FAttributes.IsEmpty and FChildNodes.IsEmpty and (FContent = '');
end;

function TFhirXHtmlNode.isPrimitive: boolean;
begin
  result := true;
end;

function TFhirXHtmlNode.Link: TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode(inherited Link);
end;

procedure TFhirXHtmlNode.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  if (bPrimitiveValues) then
  begin
    oList.add(TFHIRProperty.create(self, 'type', 'string', false, nil, CODES_TFHIRHtmlNodeType[FNodeType]));
    oList.add(TFHIRProperty.create(self, 'name', 'string', false, nil, FName));
    oList.add(TFHIRProperty.create(self, 'attribute', 'Attribute', true, nil, FAttributes.Link));
    oList.add(TFHIRProperty.create(self, 'childNode', 'Node', true, nil, FChildNodes.Link));
    oList.add(TFHIRProperty.create(self, 'content', 'string', false, nil, FContent));
  end;
end;

function TFhirXHtmlNode.makeCodeValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFhirXHtmlNode.makeIntValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFhirXHtmlNode.makeStringValue(v: String): TFHIRObject;
begin
   result := TFHIRObjectText.create(TFHIRXhtmlParser.Compose(self));
end;

function TFhirXHtmlNode.NsDecl: String;
var
  attr : TFHIRAttribute;
begin
  result := '';
  if FAttributes <> nil then
     for attr in FAttributes do
       if attr.Name = 'xmlns' then
         exit(attr.value);
end;

function TFhirXHtmlNode.primitiveValue: string;
begin
  result := AsPlainText;
end;

function TFhirXHtmlNode.SetAttribute(name, value: String) : TFhirXHtmlNode;
var
  i : integer;
begin
  result := self;
  for i := 0 to FAttributes.Count - 1 Do
    if FAttributes[i].Name = name then
    begin
      FAttributes[i].Value := value;
      exit;
    end;
  FAttributes.add(TFHIRAttribute.create(name, value));
end;

procedure TFhirXHtmlNode.setIdValue(id: String);
begin
end;

procedure TFhirXHtmlNode.SetNodeType(const Value: TFHIRHtmlNodeType);
begin
  FNodeType := Value;
  if FNodeType = fhntElement then
  begin
    FChildNodes := TFhirXHtmlNodeList.create;
    FAttributes := TFHIRAttributeList.create;
  end;
end;

function TFhirXHtmlNode.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRException.create('TFHIRAttribute.createPropertyValue: not sure how to implement this?');
end;

function TFhirXHtmlNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FAttributes.sizeInBytes);
  inc(result, FChildNodes.sizeInBytes);
  inc(result, (FContent.length * sizeof(char)) + 12);
end;

{ TFhirXhtmlNodeListEnumerator }

Constructor TFhirXhtmlNodeListEnumerator.Create(list : TFhirXhtmlNodeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirXhtmlNodeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirXhtmlNodeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirXhtmlNodeListEnumerator.GetCurrent : TFhirXhtmlNode;
begin
  Result := FList[FIndex];
end;

{ TFHIRXhtmlParser }

class Function TFHIRXhtmlParser.parse(const lang : THTTPLanguages; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode;
var
  doc : TMXmlDocument;
begin
  doc := TMXmlParser.Parse(content, [xpResolveNamespaces, xpHTMLEntities]);
  try
    result := parse(lang, policy, options, doc.document, '', '');
  finally
    doc.Free;
  end;
end;

class function TFHIRXhtmlParser.parse(const lang : THTTPLanguages; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
begin
  result := doParse(lang, policy, options, node, path, defaultNS);
  if (result.NsDecl = '') and not (xopValidatorMode in options) then
    result.Attributes.Add('xmlns', XHTML_NS);
end;

class function TFHIRXhtmlParser.doParse(const lang : THTTPLanguages; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
var
  attr : TMXmlAttribute;
  child : TMXmlElement;
begin
  result := TFhirXHtmlNode.create(fhntElement);
  try
    result.Location := node.Start;
    result.Name := node.localName;
    defaultNS := checkNS(options, result, node, defaultNS);
    path := path + '/h:'+result.Name;
    if node.HasAttributes then
    begin
      for attr in node.Attributes do
      begin
        if not attr.Name.startsWith('xmlns') and attributeIsOk(policy, options, result.Name, attr.Name, attr.Value) then
          result.Attributes.add(attr.Name, attr.value);
      end;
    end;
    child := node.First;
    while (child <> nil) do
    begin
      if (child.NodeType = ntText) then
        result.addText(child.text)
      else if (child.NodeType = ntComment) then
        result.addComment(child.text)
      else if (child.NodeType = ntElement) then
      begin
        if (elementIsOk(policy, options, child.localName)) then
          result.ChildNodes.add(doParse(lang, policy, options, child as TMXmlElement, path, defaultNS));
      end
      else
        raise EFHIRException.create('Unhandled XHTML feature: '+inttostr(ord(child.NodeType))+path);
      child := child.Next;
    end;
    result.link;
  finally
    result.free;
  end;
end;


class Function TFHIRXhtmlParser.elementIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name : String) : boolean;
begin
  if (xopValidatorMode in options) then
    exit(true);
  result := (policy = xppAllow) or StringArrayExistsInsensitive(['p', 'br', 'div', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'a', 'span', 'b', 'em', 'i', 'strong',
    'small', 'big', 'tt', 'small', 'dfn', 'q', 'var', 'abbr', 'acronym', 'cite', 'blockquote', 'hr', 'address', 'bdo', 'kbd', 'q', 'sub', 'sup',
    'ul', 'ol', 'li', 'dl', 'dt', 'dd', 'pre', 'table', 'caption', 'colgroup', 'col', 'thead', 'tr', 'tfoot', 'tbody', 'th', 'td',
    'code', 'samp', 'img', 'map', 'area'], name);
  if (not result) and (policy = xppReject) then
    raise EFHIRException.create('Illegal HTML element '+name);
end;


class Function TFHIRXhtmlParser.attributeIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name, attr, value : String) : boolean;
begin
  if (xopValidatorMode in options) then
    exit(true);
  if (attr = 'xmlns') or (attr.StartsWith('xmlns:')) then
    exit(true);

  result := (policy = xppAllow) or
         StringArrayExistsInsensitive(['title', 'style', 'class', 'id', 'lang', 'xml:lang', 'dir', 'accesskey', 'tabindex',
                    // tables
                   'span', 'width', 'align', 'valign', 'char', 'charoff', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan'], attr) or
         StringArrayExistsInsensitive(['a.href', 'a.name', 'img.src', 'img.border', 'div.xmlns', 'blockquote.cite', 'q.cite',
             'a.charset', 'a.type', 'a.name', 'a.href', 'a.hreflang', 'a.rel', 'a.rev', 'a.shape', 'a.coords', 'img.src',
             'img.alt', 'img.longdesc', 'img.height', 'img.width', 'img.usemap', 'img.ismap', 'map.name', 'area.shape',
             'area.coords', 'area.href', 'area.nohref', 'area.alt', 'table.summary', 'table.width', 'table.border',
             'table.frame', 'table.rules', 'table.cellspacing', 'table.cellpadding'], name+'.'+attr);

  if result and (name+'.'+attr = 'img.src') then
    result := (policy = xppAllow) or (StringStartsWith(value, '#') or StringStartsWith(value, 'data:') or StringStartsWith(value, 'http:') or StringStartsWith(value, 'https:'));

  if (not result) and (policy = xppReject) then
    raise EFHIRException.create('Illegal Attribute name '+name+'.'+attr);
end;

class Function TFHIRXhtmlParser.checkNS(options: TFHIRXhtmlParserOptions; focus : TFhirXHtmlNode; node : TMXmlElement; defaultNS : String)  : String;
var
  ns : String;
begin
  if not (xopValidatorMode in options) then
    exit('');

  ns := node.NamespaceURI;
  if (ns = '') then
     exit('');
  if (ns <> defaultNS) then
  begin
    focus.Attributes.add('xmlns', ns);
     exit(ns);
  end;
  exit(defaultNS);
end;

class function TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; canonicalise : boolean): String;
var
  b : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    compose(node, b, canonicalise);
    result := b.AsString;
  finally
    b.Free;
  end;
end;

function isRelativeReference(s : string) : boolean;
begin
  if s.StartsWith('http') then
    result := false
  else if s.StartsWith('https') then
    result := false
  else if s.StartsWith('/') then
    result := false
  else
    result := true;
end;

function FixRelativeReference(s : string; indent : integer) : String;
var
  i : integer;
begin
  result := '';
  for i := 1 to indent do
    result := result + '../';
  result := result + s;
end;

function normaliseWhitespace(s : String) : String;
var
  w : boolean;
  b : TStringBuilder;
  c : Char;
begin
  w := false;
  b := TStringBuilder.Create;
  try
    for c in s do
    begin
      if not c.isWhitespace or (c = #$a0) then
      begin
        b.Append(c);
        w := false;
      end
      else if not w then
      begin
        b.append(' ');
        w := true;
      end;
      // else
        // ignore


    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;


class procedure TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; s: TFslStringBuilder; canonicalise : boolean; indent, relativeReferenceAdjustment: integer);
var
  i : Integer;
  function canonical(s: String) : String;
  begin
    if canonicalise then
      result := normaliseWhitespace(s)
    else
      result := s;
  end;
begin
  if node = nil then
    exit;
  case node.NodeType of
    fhntText : s.append(FormatTexttoXml(canonical(node.Content), xmlText));
    fhntComment : s.append('<!-- '+FormatTexttoXml(node.Content, xmlText)+' -->');
    fhntElement :
      begin
      s.append('<'+node.name);
      if node.HasAttributes then
      begin
        for i := 0 to node.Attributes.count - 1 do
          if (node.name = 'a') and (node.Attributes[i].Name = 'href') and isRelativeReference(node.Attributes[i].Value) then
            s.append(' '+node.Attributes[i].Name+'="'+FixRelativeReference(node.Attributes[i].Value, relativeReferenceAdjustment)+'"')
          else
            s.append(' '+node.Attributes[i].Name+'="'+FormatTexttoXml(node.Attributes[i].Value, xmlAttribute)+'"');
      end;
      if node.ChildNodes.Count > 0 then
      begin
        s.append('>');
        for i := 0 to node.ChildNodes.count - 1 do
          compose(node.ChildNodes[i], s, canonicalise, indent+2, relativeReferenceAdjustment);
        s.append('</'+node.name+'>');
      end
      else
        s.append('/>');
      end;
    fhntDocument:
      for i := 0 to node.ChildNodes.count - 1 do
        compose(node.ChildNodes[i], s, canonicalise, indent, relativeReferenceAdjustment);
  else
    raise EFHIRException.create('not supported');
  End;
end;

class procedure TFHIRXhtmlParser.docompose(node: TFhirXHtmlNode; xml: TXmlBuilder);
var
  i : Integer;
begin
  case node.NodeType of
    fhntText : xml.Text(normaliseWhitespace(node.Content));
    fhntComment : xml.Comment(node.Content);
    fhntElement :
      begin
      if node.HasAttributes then
        for i := 0 to node.Attributes.count - 1 do
          xml.AddAttribute(node.Attributes[i].Name, node.Attributes[i].Value);
      xml.Open(node.name);
      for i := 0 to node.ChildNodes.count - 1 do
        docompose(node.ChildNodes[i], xml);
      xml.Close(node.Name);
      end;
    fhntDocument:
      for i := 0 to node.ChildNodes.count - 1 do
        docompose(node.ChildNodes[i], xml);
  else
    raise EFHIRException.create('not supported: '+CODES_TFHIRHtmlNodeType[node.NodeType]);
  end;
end;

class procedure TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; xml: TXmlBuilder);
begin
  if node = nil then
    exit;

  xml.NSPush;
  xml.CurrentNamespaces.DefaultNS := XHTML_NS;
  doCompose(node, xml);
  xml.NSPop;
end;

function compareDeep(div1, div2 : TFhirXHtmlNode; allowNull : boolean) : boolean;
begin
  if (div1 = nil) and (div2 = nil) and (allowNull) then
    result := true
  else if (div1 = nil) or (div2 = nil) then
    result := false
  else
  begin
//    result := false; //div1.equals(div2);
    raise EFHIRTodo.create('compareDeep');
  end;
end;

{ TCDANarrativeParser }

class procedure TCDANarrativeParser.processChildren(ed : TMXmlElement; x : TFHIRXhtmlNode);
var
  n : TMXmlElement;
begin
  for n in ed.Children do
    processChildNode(n, x);
end;

class procedure TCDANarrativeParser.processChildNode(n : TMXmlElement; xn : TFHIRXhtmlNode);
begin
  case n.NodeType of
    ntText: if not StringIsWhitespace(n.Text) then xn.AddText(n.Text);
    ntComment: xn.AddComment(n.Text);
    ntDocument: raise ELibraryException.create('Not supported yet');
    ntAttribute: raise ELibraryException.create('Not supported yet'); // should never happen
    ntProcessingInstruction: raise ELibraryException.create('Not supported yet');
    ntDocumentDeclaration: raise ELibraryException.create('Not supported yet');
    ntCData: raise ELibraryException.create('Not supported yet');
    ntElement:
      begin
      if (n.Name = 'br') then
        processBreak(n, xn)
      else if (n.name = 'caption') then
        processCaption(n, xn)
      else if (n.name = 'col') then
        processCol(n, xn)
      else if (n.name = 'colgroup') then
        processColGroup(n, xn)
      else if (n.name = 'content') then
        processContent(n, xn)
      else if (n.name = 'footnote') then
        processFootNote(n, xn)
      else if (n.name = 'footnoteRef') then
        processFootNodeRef(n, xn)
      else if (n.name = 'item') then
        processItem(n, xn)
      else if (n.name = 'linkHtml') then
        processlinkHtml(n, xn)
      else if (n.name = 'list') then
        processList(n, xn)
      else if (n.name = 'paragraph') then
        processParagraph(n, xn)
      else if (n.name = 'renderMultiMedia') then
        processRenderMultiMedia(n, xn)
      else if (n.name = 'sub') then
        processSub(n, xn)
      else if (n.name = 'sup') then
        processSup(n, xn)
      else if (n.name = 'table') then
        processTable(n, xn)
      else if (n.name = 'tbody') then
        processTBody(n, xn)
      else if (n.name = 'td') then
        processTd(n, xn)
      else if (n.name = 'tfoot') then
        processTFoot(n, xn)
      else if (n.name = 'th') then
        processTh(n, xn)
      else if (n.name = 'thead') then
        processTHead(n, xn)
      else if (n.name = 'tr') then
        processTr(n, xn)
      else
        raise EFHIRException.Create('Unknown element '+n.name);
    end;
  end;
end;

class procedure TCDANarrativeParser.processBreak(n : TMXmlElement; xn : TFHIRXhtmlNode);
begin
  xn.addTag('br');
end;

class procedure TCDANarrativeParser.processCaption(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('h2');
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processCol(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('col');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'span', 'width', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processColGroup(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('colgroup');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'span', 'width', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processContent(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('span');
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  // todo: do something with revised..., 'revised'
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processFootNote(n : TMXmlElement; xn : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+n.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processFootNodeRef(n : TMXmlElement; xn : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+n.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processItem(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('li');
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processlinkHtml(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('a');
  processAttributes(n, xc, ['name', 'href', 'rel', 'rev', 'title', 'ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processList(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
  lt : String;
begin
  lt := n.Attribute['listType'];
  if (lt = 'ordered') then
    xc := xn.addTag('ol')
  else
    xc := xn.addTag('ul');
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processParagraph(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('p');
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processRenderMultiMedia(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
  v : String;
begin
  xc := xn.addTag('img');
  v := n.Attribute['referencedObject'];
  xn.attribute('src', v);
  processAttributes(n, xc, ['ID', 'language', 'styleCode']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processSub(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('sub');
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processSup(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('sup');
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTable(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('table');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'summary', 'width', 'border', 'frame', 'rules', 'cellspacing', 'cellpadding']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTBody(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('tbody');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTd(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('td');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTFoot(n : TMXmlElement; xn : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+n.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processTh(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('th');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTHead(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('thead');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processTr(n : TMXmlElement; xn : TFHIRXhtmlNode);
var
  xc : TFHIRXhtmlNode;
begin
  xc := xn.addTag('tr');
  processAttributes(n, xc, ['ID', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  processChildren(n, xc);
end;

class procedure TCDANarrativeParser.processAttributes(ed : TMXmlElement; x : TFHIRXhtmlNode; names : Array of String);
var
  n, v : String;
begin
  for n in names do
  begin
    if (ed.hasAttribute[n]) then
    begin
      v := ed.Attribute[n];
      if (n = 'ID') then
        x.attribute('id', v)
      else
        x.attribute(n, v);
    end;
  end;
end;

class function TCDANarrativeParser.parse(element: TMXmlElement): TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode.Create('div');
  try
    processAttributes(element, result, ['ID', 'language', 'styleCode']);
    processChildren(element, result);
    result.link;
  finally
    result.Free;
  end;
end;

class procedure TCDANarrativeParser.processChildren(xml : TXmlBuilder; x : TFHIRXHtmlNode);
var
  n : TFHIRXHtmlNode;
begin
  for n in x.ChildNodes do
    processChildNode(xml, n);
end;

class procedure TCDANarrativeParser.processChildNode(xml : TXmlBuilder; x : TFHIRXHtmlNode);
begin
  case x.NodeType of
    fhntText : xml.Text(x.Content);
    fhntComment: xml.Comment(x.Content);
    fhntDocument:;
    fhntElement:
      if (x.name = 'br') then
        processBreak(xml, x)
      else if (x.name = 'h2') then
        processCaption(xml, x)
      else if (x.name = 'col') then
        processCol(xml, x)
      else if (x.name = 'colgroup') then
        processColGroup(xml, x)
      else if (x.name = 'span') then
        processContent(xml, x)
      else if (x.name = 'footnote') then
        processFootNote(xml, x)
      else if (x.name = 'footnoteRef') then
        processFootNodeRef(xml, x)
      else if (x.name = 'li') then
        processItem(xml, x)
      else if (x.name = 'linkHtml') then
        processlinkHtml(xml, x)
      else if (x.name = 'ul') or (x.name = 'ol') then
        processList(xml, x)
      else if (x.name = 'p') then
        processParagraph(xml, x)
      else if (x.name = 'img') then
        processRenderMultiMedia(xml, x)
      else if (x.name = 'sub') then
        processSub(xml, x)
      else if (x.name = 'sup') then
        processSup(xml, x)
      else if (x.name = 'table') then
        processTable(xml, x)
      else if (x.name = 'tbody') then
        processTBody(xml, x)
      else if (x.name = 'td') then
        processTd(xml, x)
      else if (x.name = 'tfoot') then
        processTFoot(xml, x)
      else if (x.name = 'th') then
        processTh(xml, x)
      else if (x.name = 'thead') then
        processTHead(xml, x)
      else if (x.name = 'tr') then
        processTr(xml, x)
      else
        raise EFHIRException.create('Unknown element '+x.name);
  end;
end;

class procedure TCDANarrativeParser.processBreak(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  xml.Tag('br');
end;

class procedure TCDANarrativeParser.processCaption(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.Open('caption');
  processChildren(xml, x);
  xml.Close('caption');
end;

class procedure TCDANarrativeParser.processCol(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'span', 'width', 'align', 'char', 'charoff', 'valign']);
  xml.open('col');
  processChildren(xml, x);
  xml.close('col');
end;

class procedure TCDANarrativeParser.processColGroup(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'span', 'width', 'align', 'char', 'charoff', 'valign']);
  xml.open('colgroup');
  processChildren(xml, x);
  xml.close('colgroup');
end;

class procedure TCDANarrativeParser.processContent(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.open('content');
  // todo: do something with revised..., 'revised'
  processChildren(xml, x);
  xml.close('content');
end;

class procedure TCDANarrativeParser.processFootNote(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+x.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processFootNodeRef(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+x.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processItem(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.open('item');
  processChildren(xml, x);
  xml.close('item');
end;

class procedure TCDANarrativeParser.processlinkHtml(xml : TXmlBuilder; x : TFHIRXhtmlNode);
var
  v : String;
begin
  v := x.GetAttribute('src');
  xml.addAttribute('referencedObject', v);
  processAttributes(xml, x, ['name', 'href', 'rel', 'rev', 'title', 'id', 'language', 'styleCode']);
  xml.open('linkHtml');
  processChildren(xml, x);
  xml.close('linkHtml');
end;

class procedure TCDANarrativeParser.processList(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  if (x.name = 'ol') then
    xml.addAttribute('listType', 'ordered')
  else
    xml.addAttribute('listType', 'unordered');
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.open('list');
  processChildren(xml, x);
  xml.close('list');
end;

class procedure TCDANarrativeParser.processParagraph(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.open('paragraph');
  processChildren(xml, x);
  xml.close('paragraph');
end;

class procedure TCDANarrativeParser.processRenderMultiMedia(xml : TXmlBuilder; x : TFHIRXhtmlNode);
var
  v : String;
begin
  v := x.GetAttribute('src');
  xml.addAttribute('referencedObject', v);
  processAttributes(xml, x, ['id', 'language', 'styleCode']);
  xml.open('renderMultiMedia');
  processChildren(xml, x);
  xml.close('renderMultiMedia');
end;

class procedure TCDANarrativeParser.processSub(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  xml.open('sub');
  processChildren(xml, x);
  xml.close('sub');
end;

class procedure TCDANarrativeParser.processSup(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  xml.open('sup');
  processChildren(xml, x);
  xml.close('sup');
end;

class procedure TCDANarrativeParser.processTable(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'summary', 'width', 'border', 'frame', 'rules', 'cellspacing', 'cellpadding']);
  xml.open('table');
  processChildren(xml, x);
  xml.close('table');
end;

class procedure TCDANarrativeParser.processTBody(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  xml.open('tbody');
  processChildren(xml, x);
  xml.close('tbody');
end;

class procedure TCDANarrativeParser.processTd(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan', 'align', 'char', 'charoff', 'valign']);
  xml.open('td');
  processChildren(xml, x);
  xml.close('td');
end;

class procedure TCDANarrativeParser.processTFoot(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  raise ELibraryException.create('element '+x.name+' not handled yet');
end;

class procedure TCDANarrativeParser.processTh(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan', 'align', 'char', 'charoff', 'valign']);
  xml.open('th');
  processChildren(xml, x);
  xml.close('th');
end;

class procedure TCDANarrativeParser.processTHead(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  xml.open('thead');
  processChildren(xml, x);
  xml.close('thead');
end;

class procedure TCDANarrativeParser.processTr(xml : TXmlBuilder; x : TFHIRXhtmlNode);
begin
  processAttributes(xml, x, ['id', 'language', 'styleCode', 'align', 'char', 'charoff', 'valign']);
  xml.open('tr');
  processChildren(xml, x);
  xml.close('tr');
end;

class procedure TCDANarrativeParser.processAttributes(xml : TXmlBuilder; x : TFHIRXHtmlNode; names : array of String);
var
  n, v : String;
begin
  for n in names do
  begin
    if (x.hasAttribute(n)) then
    begin
      v := x.getAttribute(n);
      if (n = 'id') then
        xml.addAttribute('ID', v)
      else
        xml.addAttribute(n, v);
    end;
  end;
end;

class procedure TCDANarrativeParser.render(xml: TXmlBuilder; node: TFhirXHtmlNode);
begin
  processAttributes(xml, node, ['ID', 'language', 'styleCode']);
  xml.open('text');
  processChildren(xml, node);
  xml.close('text');
end;

function TFHIRAttributeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

function TFHIRXhtmlNodeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

end.

