unit FHIRXhtml;
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
  SysUtils, Classes, System.Character,
  StringSupport, TextUtilities,
  AdvObjects, AdvStringBuilders,
  MXML, XmlBuilder,
  FHIRBase;

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
  public
    Constructor Create(Name : String; Value : String); Overload;

    function Link : TFHIRAttribute; Overload;
    function Clone : TFHIRAttribute; Overload;
    procedure Assign(oSource : TAdvObject); override;
    property Name : String read FName write FName;
    property Value : String read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  end;

  TFHIRAttributeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFHIRAttributeList;
    function GetCurrent : TFHIRAttribute;
  public
    Constructor Create(list : TFHIRAttributeList);
    Destructor Destroy; override;
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

  {@Class TFhirXHtmlNode
    An xhtml node. Has a type - is either an element, with a name and children,
    or a different type of node with text (usually text or comment)
  }
  {!.Net HL7Connect.Fhir.XhtmlNode}
  TFhirXHtmlNode = class (TFHIRObject)
  private
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
  public
    Constructor Create; Override;
    Constructor Create(nodeType : TFHIRHtmlNodeType) ; Overload;
    Constructor Create(name : String) ; Overload;
    Destructor Destroy; Override;
    {!script hide}
    function Link : TFhirXHtmlNode; Overload;
    function Clone : TFhirXHtmlNode; Overload;
    procedure Assign(oSource : TAdvObject); override;
    property Attributes : TFHIRAttributeList read GetAttributes;
    function allChildrenAreText : boolean;
    function isPrimitive : boolean; override;
    function primitiveValue : string; override;
    function fhirType : String; override;
    function NsDecl : String; virtual;
    {!script show}

    {@member AsPlainText
      plain text content of html
    }
    function AsPlainText : String;
    function isEmpty : boolean; override;
    function getId : String; override;

    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  published
    {@member NodeType
      The type of the node - fhntElement, fhntText, fhntComment, fhntDocument

      Note that documents are not encountered in FHIR resources
    }
    property NodeType : TFHIRHtmlNodeType read FNodeType write SetNodeType;

    {@member Name
      The name of the element, if the node is an element

      Note that namespaces are not supported in FHIR xhtml
    }
    property Name : String read FName write FName;

    {@member Content
      The content of the element if it is a text or comment node
    }
    property Content : String read FContent write FContent;

    {@member ChildNodes
      The children of the node, if it is an element
    }
    property ChildNodes : TFhirXHtmlNodeList read GetChildNodes;

    {@member AddText
      Add a text node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddText(content : String) : TFhirXHtmlNode;

    {@member AddComment
      Add a comment node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddComment(content : String) : TFhirXHtmlNode;

    {@member AddChild
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddChild(name : String) : TFhirXHtmlNode;

    {@member AddTag
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddTag(name : String) : TFhirXHtmlNode;

    {@member GetAttribute
      Get an attribute by it's name

      Note that namespaces are not supported in FHIR xhtml
    }
    Function GetAttribute(name : String) : String;

    {@member SetAttribute
      Set the value of an attribute. Create it if it doesn't exist

      Note that namespaces are not supported in FHIR xhtml
    }
    function SetAttribute(name, value : String) : TFhirXHtmlNode;
  end;

  TFHIRXhtmlNodeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFHIRXhtmlNodeList;
    function GetCurrent : TFHIRXhtmlNode;
  public
    Constructor Create(list : TFHIRXhtmlNodeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRXhtmlNode read GetCurrent;
  end;

  {@Class TFHIRXHtmlNodeList
    A list of Xhtml Nodes
  }
  {!.Net HL7Connect.Fhir.XHtmlNodeList}
  TFHIRXHtmlNodeList = class (TFHIRObjectList)
  private
    Function GetItemN(index : Integer) : TFHIRXHtmlNode;
    Procedure SetItemN(index : Integer; value : TFHIRXHtmlNode);
  public
    {!script hide}
    Function Link : TFHIRXHtmlNodeList; Overload;
    Function Clone : TFHIRXHtmlNodeList; Overload;
    function GetEnumerator : TFHIRXhtmlNodeListEnumerator;
    {!script show}
    {@member Append
      Add an Xhtml Node to the end of the list.
    }
    Function Append : TFHIRXHtmlNode;
    {@member AddItem
      Add an already existing Xhtml Node to the end of the list.
    }
    Procedure AddItem(value : TFHIRXHtmlNode);
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TFHIRXHtmlNode) : Integer;
    {@member Insert
       Insert an Xhtml node before the designated index (0 = first item)
    }
    Function Insert(index : Integer) : TFHIRXHtmlNode;
    {@member InsertItem
       Insert an existing Xhtml Node before the designated index (0 = first item)
    }
    Procedure InsertItem(index : Integer; value : TFHIRXHtmlNode);
    {@member Item
       Get the indexth Xhtml Node. (0 = first item)
    }
    Function Item(index : Integer) : TFHIRXHtmlNode;
    {@member SetItemByIndex
       Set the indexth Xhtml Node. (0 = first item)
    }
    Procedure SetItemByIndex(index : Integer; value : TFHIRXHtmlNode);
    {@member Count
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    Procedure Remove(index : Integer);
    {@member ClearItems
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
    class function doParse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode; static;
  public
    class Function parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; node : TMXmlElement; path : String; defaultNS : String) : TFhirXHtmlNode; overload;
    class Function parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode; Overload;

    class procedure compose(node: TFhirXHtmlNode; xml : TXmlBuilder); overload;
    class procedure compose(node: TFhirXHtmlNode; s : TAdvStringBuilder; canonicalise : boolean; indent : integer = 0; relativeReferenceAdjustment : integer = 0); overload;
    class function  compose(node: TFhirXHtmlNode; canonicalise : boolean = false) : String; overload;

    class Function attributeIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name, attr, value : String) : boolean;
    class Function elementIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name : String) : boolean;
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

procedure TFHIRAttribute.Assign(oSource: TAdvObject);
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

function TFHIRAttribute.equalsDeep(other: TFHIRObject): boolean;
begin
  result := inherited equalsDeep(other) and (FName = TFHIRAttribute(other).FName) and (FValue = TFHIRAttribute(other).FValue);
end;

function TFHIRAttribute.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
end;

function TFHIRAttribute.getId: String;
begin
  result := '';
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

procedure TFhirXHtmlNode.Assign(oSource: TAdvObject);
begin
  inherited;
  NodeType := TFhirXHtmlNode(oSource).FNodeType;
  FName := TFhirXHtmlNode(oSource).FName;
  FContent := TFhirXHtmlNode(oSource).FContent;
  if TFhirXHtmlNode(oSource).Attributes <> nil Then
    Attributes.assign(TFhirXHtmlNode(oSource).Attributes);
  if TFhirXHtmlNode(oSource).FChildNodes <> nil then
    ChildNodes.assign(TFhirXHtmlNode(oSource).FChildNodes);
end;

function TFhirXHtmlNode.Clone: TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode(inherited Clone);
end;

constructor TFhirXHtmlNode.Create(nodeType: TFHIRHtmlNodeType);
begin
  Create;
  NodeType := fhntElement;
end;

constructor TFhirXHtmlNode.Create(name: String);
begin
  Create;
  NodeType := fhntElement;
  FName := name;
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

function TFhirXHtmlNode.equalsDeep(other: TFHIRObject): boolean;
var
  o : TFhirXHtmlNode;
  i : integer;
begin
  result := inherited equalsDeep(other);
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
      if Attributes.Count <> o.FAttributes.Count then
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
        if not FChildNodes[i].equalsDeep(o.FChildNodes[i]) then
          exit(false);
    end;
    if FContent <> o.FContent then
      exit(false);
  end;
end;

function TFhirXHtmlNode.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
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

function TFhirXHtmlNode.NsDecl: String;
var
  attr : TFHIRAttribute;
begin
  result := '';
  if FAttributes <> nil then
   	for attr in Attributes do
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

procedure TFhirXHtmlNode.SetNodeType(const Value: TFHIRHtmlNodeType);
begin
  FNodeType := Value;
  if FNodeType = fhntElement then
  begin
    FChildNodes := TFhirXHtmlNodeList.create;
    FAttributes := TFHIRAttributeList.create;
  end;
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

class Function TFHIRXhtmlParser.parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode;
var
  doc : TMXmlDocument;
begin
  doc := TMXmlParser.Parse(content, [xpResolveNamespaces]);
  try
    result := parse(lang, policy, options, doc.document, '', '');
  finally
    doc.Free;
  end;
end;

class function TFHIRXhtmlParser.parse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
begin
  result := doParse(lang, policy, options, node, path, defaultNS);
  if (result.NsDecl = '') and not (xopValidatorMode in options) then
    result.Attributes.Add('xmlns', XHTML_NS);
end;

class function TFHIRXhtmlParser.doParse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
var
  attr : TMXmlAttribute;
  n : String;
  child : TMXmlElement;
begin
  result := TFhirXHtmlNode.create(fhntElement);
  try
    result.Name := node.localName;
    defaultNS := checkNS(options, result, node, defaultNS);
    path := path + '/h:'+result.Name;
    for n in node.Attributes.Keys do
    begin
      attr := node.attributes[n];
      if not n.startsWith('xmlns') and attributeIsOk(policy, options, result.Name, n, attr.Value) then
        result.Attributes.add(n, attr.value);
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
        raise Exception.create('Unhandled XHTML feature: '+inttostr(ord(child.NodeType))+path);
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
  	raise Exception.create('Illegal HTML element '+name);
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
  	raise Exception.create('Illegal Attribute name '+name+'.'+attr);
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
  b : TAdvStringBuilder;
begin
  b := TAdvStringBuilder.Create;
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


class procedure TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; s: TAdvStringBuilder; canonicalise : boolean; indent, relativeReferenceAdjustment: integer);
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
      for i := 0 to node.Attributes.count - 1 do
        if (node.name = 'a') and (node.Attributes[i].Name = 'href') and isRelativeReference(node.Attributes[i].Value) then
          s.append(' '+node.Attributes[i].Name+'="'+FixRelativeReference(node.Attributes[i].Value, relativeReferenceAdjustment)+'"')
        else
          s.append(' '+node.Attributes[i].Name+'="'+FormatTexttoXml(node.Attributes[i].Value, xmlAttribute)+'"');
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
    raise exception.create('not supported');
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
    raise exception.create('not supported');
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
    result := false; //div1.equalsDeep(div2);
    raise Exception.Create('Not done yet');
  end;
end;

end.

