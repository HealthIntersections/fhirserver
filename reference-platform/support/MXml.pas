unit MXML;

{
Originally Based on MicroXML, but grew somewhat to handle minimal necessities.

work remains necessary on XPath.

}
interface

uses
  SysUtils, Classes, Generics.Collections,
  StringSupport, TextUtilities,
  AdvObjects, AdvGenerics, AdvStreams, AdvStreamReaders, AdvFiles, AdvVclStreams,
  ParserSupport;

const
  DEF_BUF_SIZE = 128;

type
  TMXmlElement = class;

  TMXmlNode = class (TAdvObject)
  private
    FParent : TMXmlElement; // no own
  public
    function Link : TMXmlNode; overload;
    property Parent : TMXmlElement read FParent write FParent;
  end;

  TXPathExpressionOperation = (xeoNull, xeoEquals, xeoPlus, xeoAnd, xeoOr, xeoGreaterThan, xeoGreaterEquals, xeoNotEquals, xeoUnion, xeoLessThan, xeoLessEquals);

  TXPathExpressionNodeType = (xentName, xentFunction, xentConstant, xentGroup, xentRoot);

  TXPathExpressionNode = class (TAdvObject)
  private
    FNodeType : TXPathExpressionNodeType;
    FValue : String;
    FFilters: TAdvList<TXPathExpressionNode>;
    FNext: TXPathExpressionNode;
    FOp : TXPathExpressionOperation;
    FNextOp : TXPathExpressionNode;
    FGroup : TXPathExpressionNode;
    FParams : TAdvList<TXPathExpressionNode>;
    procedure SetNext(const Value: TXPathExpressionNode);
    procedure SetNextOp(const Value: TXPathExpressionNode);
    procedure SetGroup(const Value: TXPathExpressionNode);
    function GetParams: TAdvList<TXPathExpressionNode>;
    function GetFilters: TAdvList<TXPathExpressionNode>;
  public
    Destructor Destroy; override;
    Function Link : TXPathExpressionNode; overload;
    property NodeType : TXPathExpressionNodeType read FNodeType write FNodeType;
    property filters : TAdvList<TXPathExpressionNode> read GetFilters;
    function hasFilters : Boolean;
    property next : TXPathExpressionNode read FNext write SetNext;
    property op : TXPathExpressionOperation read FOp write FOp;
    property NextOp : TXPathExpressionNode read FNextOp write SetNextOp;
    property Group : TXPathExpressionNode read FGroup write SetGroup;
    property Params : TAdvList<TXPathExpressionNode> read GetParams;
    property value : String read FValue write FValue;
  end;

  TMXmlElementType = (ntElement, ntText, ntComment, ntDocument, ntAttribute, ntProcessingInstruction, ntDocumentDeclaration, ntCData);

  TMXmlNamedNode = class (TMXmlNode)
  private
    FNodeType : TMXmlElementType;
    FNamespaceURI : String;
    FLocalName : String;
    FStart : TSourceLocation;
    FStop : TSourceLocation;
  public
    Constructor Create(nodeType : TMXmlElementType); overload;
    property NodeType : TMXmlElementType read FNodeType write FNodeType;
    property NamespaceURI : String read FNamespaceURI write FNamespaceURI;
    property LocalName : String read FLocalName write FLocalName;
    property Start : TSourceLocation read FStart write FStart;
    property Stop : TSourceLocation read FStop write FStop;
  end;

  TMXmlAttribute = class (TMXmlNamedNode)
  private
    FValue : String;
  public
    Constructor Create(); overload;
    Constructor Create(value : String); overload;

    Function Link : TMXmlAttribute; overload;
    property Value : String read FValue write FValue;
  end;


  TMXmlElement = class (TMXmlNamedNode)
  private
    FName : String;
    FAttributes : TAdvMap<TMXmlAttribute>;
    FChildren : TAdvList<TMXmlElement>;
    FText : string;
    FNext : TMXmlElement;
    function GetAttributes: TAdvMap<TMXmlAttribute>;
    function GetChildren: TAdvList<TMXmlElement>;
    function GetHasAttributes: boolean;
    function GetHasChildren: boolean;
    function GetHasText: boolean;
    procedure SetNext(const Value: TMXmlElement);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetAttribute(name: String): String;
    procedure SetAttribute(name: String; const Value: String);
    function getAbbreviation(ns : String; first : boolean) : String;
    procedure writeToXml(b : TStringBuilder; pretty : boolean; indent : integer);
    function GetAttributeNS(ns, name: String): String;
    procedure SetAttributeNS(ns, name: String; const Value: String);
    function GetAllText: String;
    procedure fixChildren;
  public
    Constructor Create(nodeType : TMXmlElementType; name : String); overload;
    Constructor Create(nodeType : TMXmlElementType; local, ns : String); overload;
    Constructor Create(nodeType : TMXmlElementType; name, local, ns : String); overload;
    Destructor Destroy; override;
    Function Link : TMXmlElement; overload;

    Property Name : String read FName write FName;
    property Attributes : TAdvMap<TMXmlAttribute> read GetAttributes;
    property HasAttributes  : boolean read GetHasAttributes;
    property Children : TAdvList<TMXmlElement> read GetChildren;
    property HasChildren : boolean read GetHasChildren;
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
    function element(name : String) : TMXmlElement;
    procedure listElements(name : String; list : TAdvList<TMXmlElement>);
    procedure addC(node : TMXmlElement);
    function ToXml(pretty : boolean = false) : String;
  end;

  TMXmlDocument = class(TMXmlElement)
  private
    FNamespaceAbbreviations : TDictionary<String, String>;
    function funcEquals(a : TMXmlAttribute; op : TXPathExpressionNode) : boolean;
    function matchesXPathNode(element : TMXmlElement; expr : TXPathExpressionNode) : boolean;
    function matchesXPathName(name, namespace, localName : string; pattern : String) : boolean;
    function passesFilter(i : integer; item : TMXmlNode; filter : TXPathExpressionNode) : boolean;
    procedure evaluate(expr: TXPathExpressionNode; item : TMXmlNode; focus: TAdvList<TMXmlNode>); overload;
    function evaluate(expr : TXPathExpressionNode; focus : TAdvList<TMXmlNode>) : TAdvList<TMXmlNode>; overload;
  public
    Constructor Create; overload;
    Destructor Destroy; override;
    function select(xpath : String; focus : TMXmlElement) : TAdvList<TMXmlNode>;
    function selectElements(xpath : String; focus : TMXmlElement) : TAdvList<TMXmlElement>;
    property NamespaceAbbreviations : TDictionary<String, String> read FNamespaceAbbreviations;
  end;

  TMXmlParserOption = (xpResolveNamespaces, xpDropWhitespace, xpDropComments);
  TMXmlParserOptions = set of TMXmlParserOption;

  EMXmlParser = class (Exception);

  TMXmlParser = class (TAdvObject)
  private
    reader : TAdvTextReader;
    options : TMXmlParserOptions;
    b : TStringBuilder;

    FLocation, FStartLocation : TSourceLocation;

    procedure rule(test : boolean; message : String);

    Function xmlToText(s: String): String;
    function peek : char;
    function read : char;
    Function ReadToken(skipWhitespace : Boolean; allowEmpty : boolean = false): String;
    Function ReadXPathToken(skipWhitespace : Boolean; allowEmpty : boolean = false): String;
    Function ReadToNextChar(ch : Char): String;
    function ReadAttribute : TMXmlAttribute;
    procedure ReadElement(parent : TMXmlElement);
    procedure ReadText(parent : TMXmlElement; text : String);
    procedure ReadComment(parent : TMXmlElement);
    function parse : TMXmlDocument; overload;
    function resolveNamespace(element : TMXmlElement; abbrev : String) : String;
    procedure resolveNamespaces(element : TMXmlElement; defNs : String);
    function readXpathExpression(node : TXPathExpressionNode; endTokens : Array of String; alreadyRead : String = '') : string;
    function parseXPath : TXPathExpressionNode; overload;
  public
    class function parse(content : String; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parse(content : TStream; options : TMXmlParserOptions) :  TMXmlDocument; overload;
    class function parse(content : TAdvStream; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parse(content : TBytes; options : TMXmlParserOptions) : TMXmlDocument; overload;
    class function parseFile(name : String; options : TMXmlParserOptions) : TMXmlDocument; overload;

    class function isXmlNameChar(const ch: Char): Boolean;
    class function isXmlWhiteSpace(const ch: Char): Boolean;
    class function isXmlName(name : String) : boolean;
    class function isXPathName(name : String) : boolean;

    class function parseXPath(content : String) : TXPathExpressionNode; overload;
  end;


const
  CODES_TMXmlElementType : Array [TMXmlElementType] of String = ('Element', 'Text', 'Comment', 'Document', 'Attribute', 'ProcessingInstruction', 'DocumentDeclaration', 'CData');

implementation

{ TMXmlAttribute }

constructor TMXmlAttribute.Create(value: String);
begin
  Create;
  FValue := value;
end;

constructor TMXmlAttribute.Create;
begin
  inherited Create(ntAttribute);
end;

function TMXmlAttribute.Link: TMXmlAttribute;
begin
  result := TMXmlAttribute(inherited Link);
end;

{ TMXmlNamedNode }

constructor TMXmlNamedNode.Create(nodeType: TMXmlElementType);
begin
  inherited Create;
  FNodeType := nodeType;
end;

{ TMXmlElement }

constructor TMXmlElement.Create(nodeType: TMXmlElementType; name: String);
begin
  Create(nodeType);
  FName := name;
end;

constructor TMXmlElement.Create(nodeType: TMXmlElementType; local, ns: String);
begin
  Create(nodeType);
  FNamespaceURI := ns;
  FLocalName := local;
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
        raise Exception.Create('Multiple matches found for '+name+' at Row '+inttostr(FStart.line)+' column '+inttostr(FStart.col));
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
    for a in Attributes.Values do
      a.Parent := self;
end;

function TMXmlElement.getAbbreviation(ns: String; first : boolean): String;
var
  s, v : String;
  hasdef : boolean;
  i : integer;
begin
  hasdef := false;
  if HasAttributes then
  begin
    for s in FAttributes.Keys do
    begin
      v := FAttributes[s].Value;
      if (s = 'xmlns') then
        hasdef := true;
      if (s = 'xmlns') and (v = ns) then
        exit('');
      if (s.StartsWith('xmlns:')) and (v = ns) then
        exit(s.Substring(6));
    end;
  end;
  if (Parent <> nil) then
    result := parent.getAbbreviation(ns, false)
  else
    result := '';
  if (first and (result = '')) then
  begin
    if not hasdef then
      attribute['xmlns'] := ns
    else
    begin
      i := 0;
      while attributes.ContainsKey('xmlns:n'+inttostr(i)) do
        inc(i);
      result := 'n'+inttostr(i);
      attribute['xmlns:'+result] := ns;
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
        b.Append(c.FText);
      result := b.ToString;
    finally
      b.Free;
    end;
  end;
end;

function TMXmlElement.GetAttribute(name: String): String;
var
  attr : TMXmlAttribute;
begin
  result := '';
  if HasAttributes and FAttributes.TryGetValue(name, attr) then
    result := attr.Value;
end;

function TMXmlElement.GetAttributeNS(ns, name: String): String;
var
  attr : TMXmlAttribute;
begin
  result := '';
  if HasAttributes then
    for attr in FAttributes.Values do
      if (attr.NamespaceURI = ns) and (attr.LocalName = name) then
        exit(attr.Value);
end;

function TMXmlElement.GetAttributes: TAdvMap<TMXmlAttribute>;
begin
  if (FAttributes = nil) then
    FAttributes := TAdvMap<TMXmlAttribute>.create;
  result := FAttributes;
end;

function TMXmlElement.GetChildren: TAdvList<TMXmlElement>;
begin
  if (FChildren = nil) then
    FChildren := TAdvList<TMXmlElement>.create;
  result := FChildren;
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

procedure TMXmlElement.listElements(name: String; list: TAdvList<TMXmlElement>);
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
  parent.Children.IndexOf(self);
  if i = 0 then
    result := nil
  else
    result := parent.Children[i-1];
end;

procedure TMXmlElement.SetAttribute(name: String; const Value: String);
var
  attr : TMXmlAttribute;
begin
  if not Attributes.TryGetValue(name, attr) then
  begin
    attr := TMXmlAttribute.Create(Value);
    Attributes.Add(name, attr);
  end;
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
    for attr in FAttributes.Values do
      if (attr.NamespaceURI = ns) and (attr.LocalName = name) then
      begin
        attr.Value := Value;
        done := true;
      end;
  if not done then
  begin
    s := getAbbreviation(ns, true);
    attr := TMXmlAttribute.Create;
    try
      attr.NamespaceURI := ns;
      attr.LocalName := name;
      attr.Value := Value;
      if s = '' then
        Attributes.Add(name, attr.Link)
      else
        Attributes.Add(s+':'+name, attr.Link);
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
  if NodeType in [ntText, ntComment] then
    FText := Value
  else
    raise Exception.Create('Unable to set text at Row '+inttostr(FStart.line)+' column '+inttostr(FStart.col));
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
  s : String;
  c : TMXmlElement;
begin
  case FNodeType of
    ntElement, ntDocument:
      begin
      if (Name = '') then
      begin
        if (LocalName <> '') and (NamespaceURI <> '') then
        begin
          s := getAbbreviation(NamespaceURI, true);
          if s = '' then
            Name := LocalName
          else
            Name := s+':'+LocalName
        end
        else
          raise Exception.Create('no Name or QName provided');
      end;
      b.Append('<');
      b.Append(Name);
      if HasAttributes then
      begin
        for s in attributes.Keys do
        begin
          b.Append(' ');
          b.Append(s);
          b.Append('="');
          b.Append(FormatTextToXML(Attributes[s].Value));
          b.Append('"');
        end;
      end;
      if HasChildren then
      begin
        b.append('>');
        for c in Children do
          c.writeToXml(b, pretty, indent + 1);
        b.Append('</');
        b.Append(Name);
        b.append('>');
      end
      else
        b.append('/>');
      end;
    ntText:
      b.Append(FormatXMLForTextArea(Text));
    ntComment:
      begin
      b.Append('<!-- ');
      b.Append(Text);
      b.Append(' -->');
      end;
  end;
end;

procedure TMXmlElement.addC(node: TMXmlElement);
begin
  Children.Add(node);
  node.Parent := self;
  fixChildren;
end;

constructor TMXmlElement.Create(nodeType: TMXmlElementType; name, local, ns: String);
begin
  Create(nodeType);
  FName := name;
  FNamespaceURI := ns;
  FLocalName := local;
end;

{ TMXmlParser }

class function TMXmlParser.parse(content: TStream; options : TMXmlParserOptions): TMXmlDocument;
var
  s : TAdvVCLStream;
begin
  s := TAdvVCLStream.create;
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
    this.reader := TAdvStringReader.create(content);
    try
      result := this.parse();
    finally
      this.reader.Free;
    end;
  finally
    this.free;
  end;
end;

class function TMXmlParser.parse(content: TAdvStream; options : TMXmlParserOptions): TMXmlDocument;
var
  this : TMXmlParser;
begin
  this := TMXmlParser.create;
  try
    this.options := options;
    this.reader := TAdvStreamReader.create(content.Link, TEncoding.UTF8, true);
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
  s : TAdvFile;
begin
  s := TAdvFile.create;
  try
    s.name := name;
    s.OpenRead;
    result := parse(s, options);
  finally
    s.Free;
  end;
end;

function TMXmlParser.parseXPath: TXPathExpressionNode;
var
  s : String;
begin
  FLocation.line := 1;
  FLocation.col := 1;
  FStartLocation := FLocation;

  b := TStringBuilder.Create;
  try
    result := TXPathExpressionNode.Create;
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

class function TMXmlParser.parseXPath(content: String): TXPathExpressionNode;
var
  this : TMXmlParser;
begin
  this := TMXmlParser.Create;
  try
    this.reader := TAdvStringReader.Create(content);
    try
      result := this.parseXPath;
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
    inc(FLocation.col)
  else if (result = #10) or (peek <> #10) then
  begin
    inc(FLocation.line);
    FLocation.col := 1;
  end;
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
//        rule(IsHexString(v.Substring(1)), 'Illegal Entity in "'+s+'"');
        b.Append(chr(StrToInt(v.Substring(1))));
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
      else
        rule(false, 'Illegal Entity "'+v+'" in "'+s+'"');
    end
    else
    begin
      If (s[i] = #13) Then
      begin
        b.Append(#10);
        If (i <= s.Length) And (s[i+1] = #10) Then
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
  FLocation.line := 1;
  FLocation.col := 1;
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
    rule(s.StartsWith('<'), 'Unable to read XML  - starts with "'+s+'"');
    result := TMXmlDocument.Create;
    try
      while s = '<!' do
      begin
        ReadComment(result);
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
  e, c : TMXmlElement;
  a : TMXmlAttribute;
  i : integer;
begin
  e := TMXmlElement.Create(ntElement);
  try
    e.Start := FStartLocation;
    e.Name := ReadToken(xpDropWhitespace in options);
    s := ReadToken(true);
    while (s <> '/>') And (s <> '>') Do
      begin
      rule(isXmlName(s), 'The attribute name '+s+' is illegal');
      e.Attributes.Add(s, ReadAttribute);
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
        else  if s = '<!' then
          readComment(e);
      end;
      s := ReadToken(false);
      rule(s = e.Name, 'Element name mismatch (start: "'+e.Name+'"/ end: "'+s+'")');
      s := Readtoken(true);
      rule(s = '>', 'Element "'+e.Name+'" not terminated properly');
    end;
    e.Stop := FLocation;
    parent.addC(e.Link);
    e.fixChildren;
  finally
    e.Free;
  end;
end;


function TMXmlParser.ReadAttribute;
Var
  s : String;
begin
  result := TMXmlAttribute.Create;
  try
    result.Start := FStartLocation;
    s := ReadToken(True);
    rule(s = '=', 'Found "'+s+'" looking for "=" in attribute');
    s := ReadToken(xpDropWhitespace in options);
    rule((s = '"') Or (s = ''''), 'Found "'+s+'" looking for " or '' at start of attribute');
    result.Value := XmlToText(ReadToNextChar(s[1]));
    s := ReadToken(False);
    rule((s = '"') Or (s = ''''), 'Found "'+s+'" looking for " or '' at end of attribute');
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
  LStart : Integer;
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
    raise EMXmlParser.Create(message + ' at Row '+inttostr(FLocation.line)+' column '+inttostr(FLocation.col));
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
function TMXmlParser.readXpathExpression(node: TXPathExpressionNode; endTokens : Array of String; alreadyRead : String = '') : String;
var
  p, f : TXPathExpressionNode;
  done, readNext : boolean;
  s : String;
begin
  readNext := true;
  if alreadyRead = '' then
    s := readXPathToken(true)
  else
    s := alreadyRead;
  node.NodeType := xentName;
  if isXPathName(s) or StringArrayExistsSensitive(['*', '**'], s) then
    node.Value := s
  else if (s = '@') then
    node.Value := '@'+readXPathToken(false)
  else if (s = '''') then
  begin
    node.NodeType := xentConstant;
    node.value := ReadToNextChar('''');
    s := readXPathToken(false);
  end
  else if (s = '(') then
  begin
    node.nodeType := xentGroup;
    node.group := TXPathExpressionNode.Create;
    s := readXpathExpression(node.Group, [')']);
    rule(s = ')', 'Expected ''('' at this point but found '+s);
  end
  else if (s = '/') then
  begin
    // starting at the root...
    readNext := false;
    node.NodeType := xentRoot; // no value in this case
  end
  else
    rule(false, 'Unknown XPath name '+s);
  if readNext then
    s := readXPathToken(true, true);
  if (s = '(') then
  begin
    s := readXPathToken(true);
    done := s = ')';
    while not done do
    begin
      p := TXPathExpressionNode.Create;
      try
        s := readXpathExpression(p, [',', ')'], s);
        node.Params.Add(p.Link);
      finally
        p.Free;
      end;
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
    f := TXPathExpressionNode.Create;
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
    node.Next := TXPathExpressionNode.Create;
    s := readXpathExpression(node.next, endTokens);
  end;
  if StringArrayExistsSensitive(['=', '+', 'and', 'or', '>', '>=', '!=', '|', '<', '<='], s) then
  begin
    node.Op := TXPathExpressionOperation(StringArrayIndexOfSensitive(['', '=', '+', 'and', 'or', '>', '>=', '!=', '|', '<', '<='], s));
    node.NextOp := TXPathExpressionNode.Create;
    s := readXpathExpression(node.NextOp, endTokens);
  end;
  if s = '' then
    rule(length(endTokens) = 0, 'Unexpected end of expression expecting '+describeTokens(endTokens))
  else
    rule(StringArrayExistsSensitive(endTokens, s), 'Found '+s+' expecting '+describeTokens(endTokens));
  result := s;
end;

function TMXmlParser.ReadXPathToken(skipWhitespace, allowEmpty: boolean): String;
Var
  ch : Char;
  LStart : Integer;
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
  s : String;
begin
  if element.hasAttributes then
    for s in element.Attributes.Keys do
      if (s = 'xmlns:'+abbrev) then
        exit(element.Attributes[s].Value);
  if element.Parent <> nil then
    result := resolveNamespace(element.Parent, abbrev)
  else
    raise Exception.Create('Unable to resolve namespace abbreviation "'+abbrev+'"');
end;

procedure TMXmlParser.resolveNamespaces(element: TMXmlElement; defNs : String);
var
  s : String;
  p : TArray<String>;
  c : TMXmlElement;
begin
  if element.hasAttributes then
    for s in element.Attributes.Keys do
      if (s = 'xmlns') then
        defNs := element.Attributes[s].Value;
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

{ TXPathExpressionNode }

destructor TXPathExpressionNode.Destroy;
begin
  FFilters.Free;
  FNext.Free;
  FNextOp.Free;
  FGroup.Free;
  FParams.Free;
  inherited;
end;

function TXPathExpressionNode.GetFilters: TAdvList<TXPathExpressionNode>;
begin
  if FFilters = nil then
    FFilters := TAdvList<TXPathExpressionNode>.create;
  result := FFilters;
end;

function TXPathExpressionNode.GetParams: TAdvList<TXPathExpressionNode>;
begin
  if FParams = nil then
    FParams := TAdvList<TXPathExpressionNode>.create;
  result := FParams;
end;

function TXPathExpressionNode.hasFilters: Boolean;
begin
  result := (FFilters <> nil) and (FFilters.Count > 0);
end;

function TXPathExpressionNode.Link: TXPathExpressionNode;
begin
  result := TXPathExpressionNode(inherited Link);
end;

procedure TXPathExpressionNode.SetGroup(const Value: TXPathExpressionNode);
begin
  FGroup.Free;
  FGroup := Value;
end;

procedure TXPathExpressionNode.SetNext(const Value: TXPathExpressionNode);
begin
  FNext.Free;
  FNext := Value;
end;

procedure TXPathExpressionNode.SetNextOp(const Value: TXPathExpressionNode);
begin
  FNextOp.Free;
  FNextOp := Value;
end;

{ TMXmlDocument }

constructor TMXmlDocument.Create;
begin
  inherited Create(ntDocument);
  FNamespaceAbbreviations := TDictionary<String, String>.create;
end;

destructor TMXmlDocument.Destroy;
begin
  FNamespaceAbbreviations.Free;
  inherited;
end;

function TMXmlDocument.evaluate(expr: TXPathExpressionNode; focus: TAdvList<TMXmlNode>): TAdvList<TMXmlNode>;
var
  work : TAdvList<TMXmlNode>;
  item : TMXmlNode;
begin
  work := TAdvList<TMXmlNode>.create;
  try
    for item in focus do
      evaluate(expr, item, work);
    if expr.next = nil then
      result := work.link
    else
      result := evaluate(expr.next, work);
  finally
    work.Free;
  end;
end;

function TMXmlDocument.funcEquals(a: TMXmlAttribute; op: TXPathExpressionNode): boolean;
begin
  if op.value <> '' then
    result := a.Value = op.value
  else
    result := false;
end;

function TMXmlDocument.matchesXPathName(name, namespace, localName, pattern: String): boolean;
var
  p : TArray<String>;
begin
  if name = pattern then
    result := true
  else if pattern.Contains(':') and (name <> '') then
  begin
    p := pattern.Split([':']);
    if NamespaceAbbreviations.ContainsKey(p[0]) then
      result := (Namespace = FNamespaceAbbreviations[p[0]]) and (localName = p[1])
    else
      result := false;
  end
  else
    result := false;
end;

function TMXmlDocument.matchesXPathNode(element : TMXmlElement; expr: TXPathExpressionNode): boolean;
begin
  case expr.NodeType of
    xentName: result := matchesXPathName(element.Name, element.NamespaceURI, element.LocalName, expr.value);
    xentFunction :
      case StringArrayIndexOfSensitive(['text', 'comment'], expr.value) of
        0 : result := element.NodeType = ntText;
        1 : result := element.NodeType = ntComment;
      else
        raise Exception.Create('Unknown function "'+expr.value+'"');
      end;
    xentConstant :
      raise Exception.Create('Not supported yet');
  else
      raise Exception.Create('Not supported yet');
  end;
end;

function TMXmlDocument.passesFilter(i: integer; item: TMXmlNode; filter: TXPathExpressionNode): boolean;
var
  a : TMXmlAttribute;
begin
  case filter.FNodeType of
    xentName :
      if IsNumericString(filter.Value) then
        result := i + 1 = StrToInt(filter.Value)
      else if filter.value.startsWith('@') then
      begin
        a := (item as TMXmlElement).attributes[filter.value.Substring(1)];
        if filter.NextOp <> nil then
        begin
          case filter.op of
            xeoEquals : result := funcEquals(a, filter.NextOp);
          else
            raise EMXmlParser.Create('Unsupported operation');
          end;
        end
        else
          result := a <> nil;
      end;
    xentFunction :
      raise Exception.Create('Not supported yet');
    xentConstant :
      raise Exception.Create('Not supported yet');
  else
      raise Exception.Create('Not supported yet');
  end;
end;

procedure TMXmlDocument.evaluate(expr: TXPathExpressionNode; item: TMXmlNode; focus: TAdvList<TMXmlNode>);
var
  c : TMXmlElement;
  a : TMXmlAttribute;
  s : String;
  filterList : TAdvList<TMXmlNode>;
  i : integer;
  f : TXPathExpressionNode;
begin
  if item is TMXmlElement then
  begin
    if (expr.value[1] = '@') then
    begin
      if TMXmlElement(item).HasAttributes then
        for s in TMXmlElement(item).Attributes.Keys do
        begin
          a := TMXmlElement(item).Attributes[s];
          if matchesXPathName(s, a.NamespaceURI, a.LocalName, expr.value.Substring(1)) then
            focus.Add(a.Link);
        end;
    end
    else if TMXmlElement(item).HasChildren then
    begin
      for c in TMXmlElement(item).Children do
        if matchesXPathNode(c, expr) then
          focus.Add(c.Link);
    end;
  end
  else
    raise Exception.Create('Not done yet');
  if expr.hasFilters then
  begin
    for f in expr.filters do
    begin
      filterList := TAdvList<TMXmlNode>.create;
      try
        for i := 0 to focus.Count - 1 do
          if passesFilter(i, focus[i], f) then
            filterList.add(focus[i].link);
        focus.clear;
        focus.addAll(filterList);
      finally
        filterList.free;
      end;
    end;
  end;
end;

function TMXmlDocument.select(xpath: String; focus: TMXmlElement): TAdvList<TMXmlNode>;
var
  expr : TXPathExpressionNode;
  work : TAdvList<TMXmlNode>;
begin
  expr := TMXmlParser.parseXPath(xpath);
  try
    work := TAdvList<TMXmlNode>.create;
    try
      work.Add(focus.Link);
      result := evaluate(expr, work);
    finally
      work.Free;
    end;
  finally
    expr.Free;
  end;
end;

function TMXmlDocument.selectElements(xpath: String; focus: TMXmlElement): TAdvList<TMXmlElement>;
var
  list : TAdvList<TMXmlNode>;
  item : TMXmlNode;
begin
  result := TAdvList<TMXmlElement>.create;
  try
    list := select(xpath, focus);
    try
      for item in list do
        if item is TMXmlElement then
          result.Add(TMXmlElement(item).Link)
        else
          raise Exception.Create('Error Message');
    finally
      list.Free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

{ TMXmlNode }

function TMXmlNode.link: TMXmlNode;
begin
  result := TMXmlNode(inherited Link);
end;

end.
