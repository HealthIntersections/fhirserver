unit FHIRMetaModel;

interface

uses
  SysUtils, Classes,
  AdvObjects, AdvGenerics,
  MsXml, MsXmlParser, AdvJson,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities;

function isPrimitiveType(name : String):boolean;

type
  TFHIRMMProperty = class (TAdvObject)
  private
    FDefinition : TFHIRElementDefinition;
    FStructure : TFHIRStructureDefinition;
    function GetName: string;
  public
    Constructor create(definition : TFHIRElementDefinition; structure : TFHIRStructureDefinition);
    Destructor Destroy; override;
    function link : TFHIRMMProperty; overload;
    property definition : TFHIRElementDefinition read FDefinition;
    property structure : TFHIRStructureDefinition read FStructure;
    property name : string read GetName;
    function getType : string; overload;
    function getType(elementName : String) : string; overload;
    function isPrimitive : boolean;
    function isResource : boolean;
    function isList : boolean;
    function getScopedPropertyName : String;
  end;

   {* This class represents the reference model of FHIR
   *
   * A resource is nothing but a set of elements, where every element has a
   * name, maybe a stated type, maybe an id, and either a value or child elements
   * (one or the other, but not both or neither)
   *}
  TFHIRMMElement = class (TAdvObject)
  private
	  FComments : TStringList;// not relevant for production, but useful in documentation
	  FName : String;
	  FType : String;
	  FValue : String;
  	FIndex : integer;
  	FChildren : TAdvList<TFHIRMMElement>;
	  FProperty : TFHIRMMProperty;
    function GetType: String;
    function GetChildren: TAdvList<TFHIRMMElement>;
    function GetComments: TStringList;
  public
    constructor Create(name : String); overload;
    constructor Create(name : String; prop : TFHIRMMProperty); overload;
    constructor Create(name : String; prop : TFHIRMMProperty; type_, value : String); overload;
    Destructor Destroy; override;
    function link : TFHIRMMElement; overload;

    property name : String read FName;
    property type_ : String read GetType write FType;
    property value : String read FValue write FValue;
    property children : TAdvList<TFHIRMMElement> read GetChildren;
    property comments : TStringList read GetComments;
    property prop : TFHIRMMProperty read FProperty;
    property index : integer read FIndex write FIndex;

    function hasChildren : boolean;
    function hasComments : boolean;
    function hasValue : boolean;
    function hasIndex : boolean;
    procedure getChildrenByName(name : String; children : TAdvList<TFHIRMMElement>);
    procedure numberChildren;

    function getChildValue(name : String) : string;
  end;

  IWorkerContext = class (TAdvObject)
  public
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual; abstract;
    function getStructure(url : String) : TFHIRStructureDefinition; virtual; abstract;
  end;

  TFHIRMMParserBase = class (TAdvObject)
	protected
    FContext : IWorkerContext;
    FCheck : boolean;
	  function getChildProperties(prop : TFHIRMMProperty; elementName : String) : TAdvList<TFHIRMMProperty>;
  public
    constructor create(context : IWorkerContext; check : boolean);
    function parse(stream : TStream) : TFHIRMMElement; virtual; abstract;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String);  virtual; abstract;
  end;

  TFHIRMMFhirFormat = (fmfXML, fmfJSON, fmfJSONLD, fmfTURTLE);

  TFHIRMMManager = class (TAdvObject)
  private
    class function makeParser(context : IWorkerContext; format : TFHIRMMFhirFormat; check : boolean) : TFHIRMMParserBase;
  public
    class function parse(context : IWorkerContext; source : TStream; inputFormat : TFHIRMMFhirFormat; check : boolean) : TFHIRMMElement;
    class procedure compose(context : IWorkerContext; e : TFHIRMMElement; destination : TStream; outputFormat : TFHIRMMFhirFormat; pretty : boolean; base : String);
  end;

  TFHIRMMXmlParser = class (TFHIRMMParserBase)
  private
    function parseElement(base : IXMLDomElement; fixedType : String) : TFHIRMMElement;
    procedure parseChildren(path : String; node : IXMLDomElement; context : TFHIRMMElement);
    procedure parseResource(s : String; container : IXMLDomElement; parent : TFHIRMMElement);
  	procedure reapComments(element : IXMLDomElement; context : TFHIRMMElement);
    function isAttr(prop : TFHIRMMProperty) : boolean;
  public
    function parse(stream : TStream) : TFHIRMMElement; override;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); override;
  end;

  TFHIRMMJsonParser = class (TFHIRMMParserBase)
  private
    json : TJSONWriter;
    procedure prop(name, value : string);
    procedure open(name : String);
    procedure close;
    procedure openArray(name : String);
    procedure closeArray();
    procedure composeChild(path : String; e : TFHIRMMElement; done : TStringList; child : TFHIRMMElement);
    procedure composeList(path : String; list : TAdvList<TFHIRMMElement>);
    procedure primitiveValue(name : String; item : TFHIRMMElement);
    procedure composeElement(path : String; element : TFHIRMMElement);
    procedure composeComments(element : TFHIRMMElement);
  public
    function parse(stream : TStream) : TFHIRMMElement; override;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); override;
  end;


implementation

uses
  StringSupport;

function isPrimitiveType(name : String) : boolean;
begin
  result := StringArrayExistsSensitive(['integer', 'unsignedInt', 'positiveInt', 'decimal', 'dateTime', 'date', 'time', 'instant', 'string', 'uri', 'oid', 'uuid', 'id', 'boolean', 'code', 'markdown'], name);
end;

function tail(path : String) : string;
begin
  if path.contains('.') then
    result := path.substring(path.lastIndexOf('.')+1)
  else
    result := path;
end;

function lowFirst(t: String): String;
begin
  result := t.ToLower.substring(0, 1)+t.substring(1);
end;

{ TFHIRMMProperty }

constructor TFHIRMMProperty.create(definition: TFHIRElementDefinition; structure: TFHIRStructureDefinition);
begin
  inherited create;
  FDefinition := definition;
  FStructure := structure;
end;

destructor TFHIRMMProperty.Destroy;
begin
  FDefinition.Free;
  FStructure.Free;
  inherited;
end;

function TFHIRMMProperty.GetName: string;
begin
  result := definition.Path.substring(definition.Path.lastIndexOf('.')+1);
end;

function TFHIRMMProperty.getScopedPropertyName: String;
begin
  result := definition.Base.Path;
end;

function TFHIRMMProperty.getType: string;
begin
  if (definition.Type_List.count() = 0) then
    result := ''
  else if (definition.type_List.count() > 1) then
    raise Exception.create('logic error, gettype when types > 1')
  else
    result := CODES_TFhirDefinedTypesEnum[definition.type_List[0].Code];
end;

function TFHIRMMProperty.getType(elementName: String): string;
var
  t, name, tail : String;
  all : boolean;
  tr : TFhirElementDefinitionType;
begin
  if (definition.type_List.count() = 0) then
    result := ''
  else if (definition.type_list.count() > 1) then
  begin
    t := CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code];
    all := true;
    for tr in definition.type_list do
    begin
      if (t <> CODES_TFhirDefinedTypesEnum[tr.Code]) then
        all := false;
    end;
    if (all) then
      result := t;
    tail := definition.Path.substring(definition.Path.lastIndexOf('.')+1);
    if (tail.endsWith('[x]') and elementName.startsWith(tail.substring(0, tail.length-3))) then
    begin
      name := elementName.substring(tail.length-3);
      if isPrimitiveType(lowFirst(name)) then
        result := lowFirst(name)
      else
        result := name;
    end
    else
      raise Exception.create('logic error, gettype when types > 1, name mismatch');
  end
  else
    result := CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code];
end;

function TFHIRMMProperty.isList: boolean;
begin
  result := definition.Max <> '1'
end;

function TFHIRMMProperty.isPrimitive: boolean;
begin
	result := (definition.type_list.count() = 1) and isPrimitiveType(CODES_TFhirDefinedTypesEnum[definition.type_list[0].code]);
end;

function TFHIRMMProperty.isResource: boolean;
begin
  result := (definition.type_list.count() = 1) and (CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code] = 'Resource');
end;

function TFHIRMMProperty.link: TFHIRMMProperty;
begin
  result := TFHIRMMProperty(inherited link);
end;

{ TFHIRMMElement }

constructor TFHIRMMElement.Create(name: String);
begin
  inherited Create;
  self.Fname := name;
  FIndex := -1;
end;

constructor TFHIRMMElement.Create(name: String; prop: TFHIRMMProperty);
begin
  inherited Create;
  self.Fname := name;
  self.fproperty := prop;
  FIndex := -1;
end;

constructor TFHIRMMElement.Create(name: String; prop: TFHIRMMProperty; type_, value: String);
begin
  inherited Create;
  self.Fname := name;
  self.fproperty := prop;
  self.Ftype := type_;
  self.Fvalue := value;
  FIndex := -1;
end;

destructor TFHIRMMElement.Destroy;
begin
  FComments.Free;
  FChildren.Free;
  FProperty.Free;
  inherited;
end;

function TFHIRMMElement.GetChildren: TAdvList<TFHIRMMElement>;
begin
  if FChildren = nil then
    FChildren := TAdvList<TFHIRMMElement>.create;
  result := FChildren;
end;

procedure TFHIRMMElement.getChildrenByName(name: String; children: TAdvList<TFHIRMMElement>);
var
  child : TFHIRMMElement;
begin
  if (hasChildren()) then
  begin
    for child in self.children do
      if (name = child.Name) then
        children.add(child);
  end;
end;

function TFHIRMMElement.getChildValue(name: String): string;
var
  child : TFHIRMMElement;
begin
  result := '';
  if (hasChildren()) then
  begin
    for child in self.children do
      if (name = child.Name) then
				result := child.Value;
  end;
end;

function TFHIRMMElement.GetComments: TStringList;
begin
  if FComments = nil then
    FComments := TStringList.Create;
  result := FComments;
end;

function TFHIRMMElement.GetType: String;
begin
  if (Ftype = '') then
    result := FProperty.getType(name)
  else
    result := Ftype;
end;

function TFHIRMMElement.hasChildren: boolean;
begin
  result := (FChildren <> nil) and (FChildren.count > 0);
end;

function TFHIRMMElement.hasComments: boolean;
begin
  result := (FComments <> nil) and (FComments.count > 0);
end;

function TFHIRMMElement.hasIndex: boolean;
begin
  result := FIndex > -1;
end;

function TFHIRMMElement.hasValue: boolean;
begin
  result := FValue <> '';
end;

function TFHIRMMElement.link: TFHIRMMElement;
begin
  result := TFHIRMMElement(inherited Link);
end;

procedure TFHIRMMElement.numberChildren;
var
  last : string;
  index : integer;
  child : TFHIRMMElement;
begin
  if (haschildren) then
  begin
    last := '';
		index := 0;
    for child in self.children do
    begin
			if (child.prop.isList) then
      begin
			  if (last = child.Name) then
			  	inc(index)
			  else
        begin
			  	last := child.Name;
			  	index := 0;
			  end;
		  	child.index := index;
			end
      else
				child.index := -1;
			child.numberChildren();
    end;
  end;
end;


{ TFHIRMMParserBase }

constructor TFHIRMMParserBase.create(context: IWorkerContext; check: boolean);
begin
  inherited create;
  self.FContext := context;
  self.FCheck := check;
end;

function TFHIRMMParserBase.getChildProperties(prop: TFHIRMMProperty; elementName: String): TAdvList<TFHIRMMProperty>;
var
  ed : TFhirElementDefinition;
  sd : TFhirStructureDefinition;
  children : TFhirElementDefinitionList;
  child : TFhirElementDefinition;
  tr : TFhirElementDefinitionType;
  t : String;
  all : boolean;
begin
  ed := prop.Definition;
  sd := prop.Structure.Link;
  children := FContext.getChildMap(sd, ed);
  try
    if (children.isEmpty()) then
    begin
      // ok, find the right definitions
      t := '';
      if (ed.type_list.count() = 1) then
        t := CODES_TFhirDefinedTypesEnum[ed.type_list[0].Code]
      else if (ed.type_list.count() = 0) then
        raise Exception.create('types = 0, and no children found')
      else
      begin
        t := CODES_TFhirDefinedTypesEnum[ed.type_list[0].Code];
        all := true;
        for tr in ed.type_list do
          if (CODES_TFhirDefinedTypesEnum[tr.Code] <> t) then
            all := false;
        if (not all) then
        begin
          t := elementName.substring(tail(ed.Path).length - 3);
          if (isPrimitiveType(lowFirst(t))) then
            t := lowFirst(t);
        end;
      end;
      if (t <> 'xhtml') then
      begin
        sd.Free;
        sd := FContext.getStructure('http://hl7.org/fhir/StructureDefinition/'+t);
        if (sd = nil) then
          raise Exception.create('Unable to find '+t);
        children.Free;
        children := FContext.getChildMap(sd, sd.snapshot.elementList[0]);
      end;
    end;
    result := TAdvList<TFHIRMMProperty>.create;
    for child in children do
      result.add(TFHIRMMProperty.create(child.link, sd.link));
  finally
    children.Free;
    sd.Free;
  end;
end;

{ TFHIRMMManager }

class function TFHIRMMManager.makeParser(context: IWorkerContext; format: TFHIRMMFhirFormat; check: boolean): TFHIRMMParserBase;
begin
  case format of
    fmfJSON : result := TFHIRMMJsonParser.create(context, check);
//    fmfJSONLD : result := TFHIRMMJsonLDParser.create(context, check);
    fmfXML : result := TFHIRMMXmlParser.create(context, check);
//    fmfTURTLE : result := TFHIRMMTurtleParser.create(context, check);
  else
    result := nil;
  end;
end;

class procedure TFHIRMMManager.compose(context: IWorkerContext; e: TFHIRMMElement; destination: TStream; outputFormat: TFHIRMMFhirFormat; pretty: boolean; base: String);
var
  p : TFHIRMMParserBase;
begin
  p := makeParser(context, outputFormat, false);
  try
    p.compose(e, destination, pretty, base);
  finally
    p.Free;
  end;
end;

class function TFHIRMMManager.parse(context: IWorkerContext; source: TStream; inputFormat: TFHIRMMFhirFormat; check: boolean): TFHIRMMElement;
var
  p : TFHIRMMParserBase;
begin
  p := makeParser(context, inputFormat, check);
  try
    result := p.parse(source);
  finally
    p.Free;
  end;
end;

{ TFHIRMMXmlParser }

procedure TFHIRMMXmlParser.compose(e: TFHIRMMElement; stream: TStream; pretty: boolean; base: String);
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRMMXmlParser.isAttr(prop: TFHIRMMProperty): boolean;
begin
  result := PropertyRepresentationXmlAttr in prop.definition.representation;
end;

function TFHIRMMXmlParser.parse(stream: TStream): TFHIRMMElement;
var
  doc : IXMLDOMDocument2;
  base : IXMLDOMElement;
begin
  doc := TMsXmlParser.Parse(stream);
  base := doc.DocumentElement;
  if (base.NamespaceURI = '') then
    raise Exception.create('This does not appear to be a FHIR resource (no namespace "'+base.NamespaceURI+'")');
  if (base.NamespaceURI <> FHIR_NS) then
    raise Exception.create('This does not appear to be a FHIR resource (wrong namespace "'+base.NamespaceURI+'")');
  result := parseElement(base, '');
end;

function TFHIRMMXmlParser.parseElement(base: IXMLDomElement; fixedType: String): TFHIRMMElement;
var
  name : String;
  sd : TFhirStructureDefinition;
begin
  name := base.NodeName;
  if (fixedType <> '') then
    sd := FContext.getStructure('http://hl7.org/fhir/StructureDefinition/'+fixedType)
  else
    sd := FContext.getStructure('http://hl7.org/fhir/StructureDefinition/'+name);
  if (sd = nil) then
    raise Exception.create('This does not appear to be a FHIR resource (unknown name "'+base.NodeName+'")');
  result := TFHIRMMElement.create(base.NodeName, TFHIRMMProperty.create(sd.Snapshot.elementList[0].link, sd));
  try
    result.type_ := base.NodeName;
    parseChildren(base.nodeName, base, result);
    result.numberChildren();
    result.link;
  finally
    result.Free;
  end;
end;

procedure TFHIRMMXmlParser.parseChildren(path: String; node: IXMLDomElement; context: TFHIRMMElement);
var
  props : TAdvList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  processed : TInterfaceList;
  attr : IXMLDOMAttribute;
  e, div_ : IXMLDOMElement;
  xhtml : TFhirXHtmlNode;
  children : TInterfaceList;
  child : IInterface;
  n : TFHIRMMElement;
begin
  reapComments(node, context);
  processed := TInterfaceList.create;
  props := getChildProperties(context.Prop, context.Name);
  try
    for prop in props do
    begin
      if (isAttr(prop)) then
      begin
      	attr := node.getAttributeNode(prop.Name);
      	if (attr <> nil) then
        begin
      	  processed.add(attr);
      		if (prop.Name = 'value') then
      			context.Value := attr.Value
      		else
      	    context.getChildren().add(TFHIRMMElement.create(prop.Name, prop.Link, prop.getType, attr.Value));
        end;
      end
      else if prop.isPrimitive() and ('xhtml' = prop.getType()) then
      begin
      	div_ := TMsXmlParser.NamedChild(node, prop.Name);
      	processed.add(div_);
      	xhtml := ParseXhtml('en', div_.text, xppReject);
  	    context.getChildren().add(TFHIRMMElement.create('div', prop, 'xhtml', ComposeXHtml(xhtml)));
      end
      else
      begin
        children := TInterfaceList.Create;
        try
      	  TMsXmlParser.getNamedChildrenWithWildcard(node, prop.getName(), children);
        	for child in children do
          begin
            processed.Add(child);
            e := child as IXMLDOMElement;
            n := TFHIRMMElement.create(e.nodeName, prop);
            context.getChildren().add(n);
            if (prop.isResource()) then
              parseResource(path+'.'+prop.getName(), e, n)
            else
              parseChildren(path+'.'+prop.getName(), e, n);
          end;
        finally
          children.Free;
        end;
      end;
    end;
    if (Fcheck) then
    begin
      e := TMsXmlParser.FirstChild(node);
      while (e <> nil) do
      begin
        if (processed.IndexOf(e) = -1) then
          raise Exception.create('Unexpected element at '+path+'.'+e.NodeName);
        e := TMsXmlParser.NextSibling(e);
      end;
//      NamedNodeMap am = node.getAttributes();
//      for (int i = 0; i < am.getLength(); i++) {
//        if (!processed.contains(am.item(i)) && !am.item(i).getNodeName().startsWith("xmlns"))
//          throw new Exception("Unexpected element at "+path+".@"+am.item(i).getNodeName());
//      }
    end;
  finally
    processed.free;
    props.free;
  end;
end;

procedure TFHIRMMXmlParser.parseResource(s: String; container: IXMLDomElement; parent: TFHIRMMElement);
var
  res : IXMLDOMElement;
  name : String;
  sd : TFHIRStructureDefinition;
  result : TFHIRMMElement;
begin
  res := TMsXmlParser.FirstChild(container);
  name := res.NodeName;
  sd := Fcontext.getStructure('http://hl7.org/fhir/StructureDefinition/'+name);
  if (sd = nil) then
    raise Exception.create('Contained resource does not appear to be a FHIR resource (unknown name "'+res.NodeName+'")');
  result := TFHIRMMElement.create(res.NodeName, TFHIRMMProperty.create(sd.Snapshot.elementList[0], sd));
  try
    result.Type_ := res.NodeName;
    parseChildren(res.NodeName, res, result);
    parent.getChildren().add(result.link);
  finally
    result.Free;
  end;
end;

procedure TFHIRMMXmlParser.reapComments(element: IXMLDomElement; context: TFHIRMMElement);
var
  node : IXMLDOMNode;
begin
  node := element.PreviousSibling;
  while (node <> nil) and (node.NodeType <> NODE_ELEMENT) do
  begin
    if (node.NodeType = NODE_COMMENT) then
      context.getComments().insert(0, node.Text);
    node := node.PreviousSibling;
  end;
  node := element.LastChild;
  while (node <> nil) and (node.NodeType <> NODE_ELEMENT) do
    node := node.PreviousSibling;
  while (node <> nil) do
  begin
    if (node.NodeType = NODE_COMMENT) then
      context.getComments().insert(0, node.Text);
    node := node.NextSibling;
  end;
end;


{ TFHIRMMJsonParser }

procedure TFHIRMMJsonParser.close;
begin

end;

procedure TFHIRMMJsonParser.closeArray;
begin

end;

procedure TFHIRMMJsonParser.compose(e: TFHIRMMElement; stream: TStream;
  pretty: boolean; base: String);
begin

end;

procedure TFHIRMMJsonParser.composeChild(path: String; e: TFHIRMMElement;
  done: TStringList; child: TFHIRMMElement);
begin

end;

procedure TFHIRMMJsonParser.composeComments(element: TFHIRMMElement);
begin

end;

procedure TFHIRMMJsonParser.composeElement(path: String;
  element: TFHIRMMElement);
begin

end;

procedure TFHIRMMJsonParser.composeList(path: String;
  list: TAdvList<TFHIRMMElement>);
begin

end;

procedure TFHIRMMJsonParser.open(name: String);
begin

end;

procedure TFHIRMMJsonParser.openArray(name: String);
begin

end;

function TFHIRMMJsonParser.parse(stream: TStream): TFHIRMMElement;
begin

end;

procedure TFHIRMMJsonParser.primitiveValue(name: String; item: TFHIRMMElement);
begin

end;

procedure TFHIRMMJsonParser.prop(name, value: string);
begin

end;

end.
