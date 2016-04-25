unit FHIRMetaModel;

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}
interface

uses
  SysUtils, Classes, Variants, Math,
  AdvObjects, AdvGenerics, AdvStreams, AdvBuffers, AdvVclStreams,  AdvMemories,
  MsXml, MsXmlParser, XmlBuilder, AdvXmlBuilders, AdvJson, DateAndTime,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRProfileUtilities, {FHIRParserBase, }FHIRXHtml;


type
  TFHIRMMProperty = class (TAdvObject)
  private
    FContext : TWorkerContext;
    FDefinition : TFHIRElementDefinition;
    FStructure : TFHIRStructureDefinition;
    FCanBePrimitive : integer;

    function GetName: string;
  public
    Constructor create(context : TWorkerContext; definition : TFHIRElementDefinition; structure : TFHIRStructureDefinition);
    Destructor Destroy; override;
    function link : TFHIRMMProperty; overload;

    property context : TWorkerContext read FContext;
    property definition : TFHIRElementDefinition read FDefinition;
    property structure : TFHIRStructureDefinition read FStructure;
    property name : string read GetName;

    function getType : string; overload;
    function getType(elementName : String) : string; overload;
    function hasType(elementName : String) : boolean;
    function isPrimitive(elementName : String) : boolean;
    function isResource : boolean;
    function isList : boolean;
    function getScopedPropertyName : String;
    function getNamespace : string;
    function IsLogicalAndHasPrimitiveValue(name : String) : boolean;
    function isChoice : boolean;
  end;

  TFHIRMMSpecialElement = (fseNil, fseContained, fseBundleEntry);

   {* This class represents the reference model of FHIR
   *
   * A resource is nothing but a set of elements, where every element has a
   * name, maybe a stated type, maybe an id, and either a value or child elements
   * (one or the other, but not both or neither)
   *}
  TFHIRMMElement = class (TFHIRBase)
  private
	  FComments : TStringList;// not relevant for production, but useful in documentation
	  FName : String;
	  FType : String;
	  FValue : String;
  	FIndex : integer;
  	FChildren : TAdvList<TFHIRMMElement>;
	  FProperty : TFHIRMMProperty;
    FlocStart: TSourceLocation;
    FlocEnd: TSourceLocation;
    FSpecial : TFHIRMMSpecialElement;
    FXhtml : TFhirXHtmlNode;

    function GetType: String;
    function GetChildren: TAdvList<TFHIRMMElement>;
    function GetComments: TStringList;
    procedure SetXhtml(const Value: TFhirXHtmlNode);
  public
    constructor Create(name : String); overload;
    constructor Create(name : String; prop : TFHIRMMProperty); overload;
    constructor Create(name : String; prop : TFHIRMMProperty; type_, value : String); overload;
    Destructor Destroy; override;

    function link : TFHIRMMElement; overload;
    procedure updateProperty(prop : TFHIRMMProperty; special : TFHIRMMSpecialElement);

    property name : String read FName;
    property type_ : String read GetType write FType;
    property value : String read FValue write FValue;
    property children : TAdvList<TFHIRMMElement> read GetChildren;
    property comments : TStringList read GetComments;
    property prop : TFHIRMMProperty read FProperty;
    property index : integer read FIndex write FIndex;
    property special : TFHIRMMSpecialElement read FSpecial;
    property LocStart : TSourceLocation read FLocStart;
    property LocEnd : TSourceLocation read FLocEnd;
    property xhtml : TFhirXHtmlNode read FXhtml write SetXhtml;


    function hasChildren : boolean;
    function hasComments : boolean;
    function hasValue : boolean;
    function hasIndex : boolean;
    procedure getChildrenByName(name : String; children : TFHIRObjectList); override;
    function getNamedChild(name : String) : TFHIRMMElement;
    procedure getNamedChildren(name : String; list : TAdvList<TFHIRMMElement>);
    function getNamedChildValue(name : String) : string;

    procedure numberChildren;

    function getChildValue(name : String) : string;
    function hasType : boolean;
    function markLocation(start, end_ : TSourceLocation) : TFHIRMMElement;

    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function fhirType : String; override;
    function primitiveValue : String; override;
    procedure getProperty(hash : integer; name : String; checkValid : boolean; list : TAdvList<TFHIRBase>); override;
  end;

  TFHIRValidationPolicy = (fvpNONE, fvpQUICK, fvpEVERYTHING);

  TFHIRMMParserBase = class (TAdvObject)
	protected
    FContext : TWorkerContext;
   	FPolicy : TFHIRValidationPolicy;
    FErrors : TFhirOperationOutcomeIssueList;
	  function getChildProperties(prop : TFHIRMMProperty; elementName, statedType : String) : TAdvList<TFHIRMMProperty>;
    function getDefinition(line, col : integer; ns, name : String) : TFHIRStructureDefinition; overload;
    function getDefinition(line, col : integer; name : String) : TFHIRStructureDefinition; overload;
  public
    constructor create(context : TWorkerContext);
    destructor Destroy; override;

    procedure setupValidation(policy : TFHIRValidationPolicy; errors : TFhirOperationOutcomeIssueList);
    procedure logError(line, col : integer; path : String; type_ : TFhirIssueTypeEnum; message : String; level : TFhirIssueSeverityEnum);

    function parse(stream : TStream) : TFHIRMMElement; overload; virtual; abstract;
    function parse(stream : TAdvStream) : TFHIRMMElement; overload; virtual;
    function parse(buffer : TAdvBuffer) : TFHIRMMElement; overload; virtual;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String);  virtual; abstract;
  end;

  TFHIRMMManager = class (TAdvObject)
  public
    class function parseFile(context : TWorkerContext; filename : string; inputFormat : TFhirFormat) : TFHIRMMElement;
    class function parse(context : TWorkerContext; source : TStream; inputFormat : TFhirFormat) : TFHIRMMElement;
    class procedure compose(context : TWorkerContext; e : TFHIRMMElement; destination : TStream; outputFormat : TFhirFormat; pretty : boolean; base : String = '');
    class procedure composeFile(context : TWorkerContext; e : TFHIRMMElement; filename : String; outputFormat : TFhirFormat; pretty : boolean; base : String = '');
    class function makeParser(context : TWorkerContext; format : TFhirFormat) : TFHIRMMParserBase;
  end;

  TFHIRMMXmlParser = class (TFHIRMMParserBase)
  private
    FLocations : TAdvList<TSourceLocationObject>;

    function line(node : IXMLDomNode) : integer;
    function col(node : IXMLDomNode) : integer;
    function start(node : IXMLDomNode) : TSourceLocation;
    function end_(node : IXMLDomNode) : TSourceLocation;
    function pathPrefix(ns : String) : String;

    procedure checkRootNode(document : IXMLDOMDocument);
    function empty(element : IXMLDomElement) : boolean ;
    procedure checkElement(element : IXMLDomElement; path : String; prop : TFHIRMMProperty);
  	function convertForDateFormat(fmt, av : String) : String;
  	procedure reapComments(element : IXMLDomElement; context : TFHIRMMElement);

    function getElementProp(props : TAdvList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
  	function getAttrProp(props : TAdvList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
   	function getTextProp(props : TAdvList<TFHIRMMProperty>) : TFHIRMMProperty;
   	function isAttr(prop : TFHIRMMProperty) : boolean;
    function isText(prop : TFHIRMMProperty) : boolean;

    procedure parseChildren(path : String; node : IXMLDomElement; context : TFHIRMMElement);
    procedure parseResource(s : String; container : IXMLDomElement; parent : TFHIRMMElement);

    procedure composeElement(xml : TXmlBuilder; element : TFHIRMMElement; elementName : String);
  public
    destructor Destroy; override;

    function parse(stream : TStream) : TFHIRMMElement; overload; override;
    function parse(document : IXMLDOMDocument) : TFHIRMMElement; overload;
    function parse(element : IXMLDomElement) : TFHIRMMElement; overload;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); override;
  end;

  TFHIRMMJsonParser = class (TFHIRMMParserBase)
  private
    json : TJSONWriter;

    procedure checkObject(obj : TJsonObject; path : String);
    {no-comments procedure reapComments(obj : TJsonObject; context : TFHIRMMElement); }
  	procedure parseChildren(path : String; obj : TJsonObject; context : TFHIRMMElement; hasResourceType : boolean);

    procedure parseChildComplex(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String);
    procedure parseChildComplexInstance(path : String; obj: TJsonObject; context : TFHIRMMElement; prop : TFHIRMMProperty; name : String; e : TJsonNode);
    procedure parseChildPrimitive(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String);
    procedure parseChildPrimitiveInstance(npath : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String; main, fork : TJsonNode);
    procedure parseResource(path : String; obj: TJsonObject; context : TFHIRMMElement);

    procedure compose(e : TFHIRMMElement); overload;
    procedure compose(path : String; e : TFHIRMMElement; done : TAdvStringSet; child : TFHIRMMElement); overload;
    procedure composeList(path : String; list : TFHIRObjectList);
    procedure primitiveValue(name : String; item : TFHIRMMElement);
    procedure compose(path : String; element : TFHIRMMElement); overload;

  public
    function parse(stream : TStream) : TFHIRMMElement; overload; override;
    function parse(obj : TJsonObject) : TFHIRMMElement; overload;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); overload; override;
    procedure compose(e : TFHIRMMElement; stream : TAdvStream; pretty : boolean; base : String); overload;
  end;


implementation

uses
  StringSupport;

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

constructor TFHIRMMProperty.create(context : TWorkerContext; definition: TFHIRElementDefinition; structure: TFHIRStructureDefinition);
begin
  inherited create;
  FContext := context;
  FDefinition := definition;
  FStructure := structure;
end;

destructor TFHIRMMProperty.Destroy;
begin
  FContext.Free;
  FDefinition.Free;
  FStructure.Free;
  inherited;
end;

function TFHIRMMProperty.link: TFHIRMMProperty;
begin
  result := TFHIRMMProperty(inherited link);
end;

function TFHIRMMProperty.GetName: string;
begin
  result := definition.Path.substring(definition.Path.lastIndexOf('.')+1);
end;

function TFHIRMMProperty.getType: string;
var
  i : integer;
begin
  if (definition.Type_List.count() = 0) then
    result := ''
  else if (definition.type_List.count() > 1) then
  begin
    result := CODES_TFhirDefinedTypesEnum[definition.type_List[0].Code];
    for i := 1 to definition.type_List.count - 1 do
      if (result <> CODES_TFhirDefinedTypesEnum[definition.type_List[i].Code]) then
				raise Exception.create('logic error, gettype when types > 1');
  end
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
      result := t
    else
    begin
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
    end;
  end
  else
    result := CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code];
end;

function TFHIRMMProperty.hasType(elementName: String): boolean;
var
  t, tail, name : String;
  all : boolean;
  tr : TFhirElementDefinitionType;
begin
  if (definition.Type_list.count = 0) then
    result := false
  else if (definition.Type_list.count > 1) then
  begin
    t := CODES_TFhirDefinedTypesEnum[definition.Type_list[0].Code];
    all := true;
    for tr in definition.type_List do
    begin
      if (t <> CODES_TFhirDefinedTypesEnum[tr.Code]) then
        all := false;
    end;
    if (all) then
      result := true
    else
    begin
      tail := definition.Path.substring(definition.Path.lastIndexOf('.')+1);
      if (tail.endsWith('[x]') and elementName.startsWith(tail.substring(0, tail.length-3))) then
      begin
//        name := elementName.substring(tail.length-3);
        result := true;
      end
      else
        result := false;
    end;
  end
  else
    result := true;
end;


function TFHIRMMProperty.isPrimitive(elementName : String): boolean;
begin
	result := isPrimitiveType(getType(elementName));
end;

function TFHIRMMProperty.isResource: boolean;
begin
  result := (definition.type_list.count() = 1) and ((CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code] = 'Resource') or (CODES_TFhirDefinedTypesEnum[definition.type_list[0].Code] = 'DomainResource'));
end;

function TFHIRMMProperty.isList: boolean;
begin
  result := definition.Max <> '1'
end;

function TFHIRMMProperty.IsLogicalAndHasPrimitiveValue(name: String): boolean;
var
  sd : TFhirStructureDefinition;
  ed : TFhirElementDefinition;
begin
  if (FcanBePrimitive <> 0) then
    result := FcanBePrimitive > 0;

  FcanBePrimitive := -1;
  if (structure.Kind <> StructureDefinitionKindLOGICAL) then
    exit(false);
  if (not hasType(name)) then
    exit(false);
  sd := context.getStructure(structure.Url.substring(0, structure.Url.lastIndexOf('/')+1)+getType(name));
  if (sd = nil) or (sd.Kind <> StructureDefinitionKindLOGICAL) then
    exit(false);
  for ed in sd.Snapshot.elementList do
    if (ed.Path = sd.Id+'.value') and (ed.type_List.count = 1) and isPrimitive(CODES_TFhirDefinedTypesEnum[ed.type_List[0].Code]) then
    begin
      FcanBePrimitive := 1;
      exit(true);
    end;
  result := false;
end;

function TFHIRMMProperty.getScopedPropertyName: String;
begin
  result := definition.Base.Path;
end;

function TFHIRMMProperty.getNamespace: string;
begin
  if definition.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace') then
    result := definition.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace')
  else if structure.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace') then
    result := definition.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace')
  else
    result := FHIR_NS;
end;

function TFHIRMMProperty.isChoice: boolean;
var
  tn : TFhirDefinedTypesEnum;
  tr : TFhirElementDefinitionType;
begin
  result := false;
  if (definition.type_List.count > 1) then
  begin
    tn := definition.type_List[0].Code;
    for tr in definition.type_List do
      if (tr.Code <> tn) then
        exit(true);
  end;
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
  FXhtml.Free;
  inherited;
end;

procedure TFHIRMMElement.updateProperty(prop: TFHIRMMProperty; special: TFHIRMMSpecialElement);
begin
  FProperty.Free;
  FProperty := prop;
  FSpecial := special;
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

function TFHIRMMElement.GetComments: TStringList;
begin
  if FComments = nil then
    FComments := TStringList.Create;
  result := FComments;
end;

function TFHIRMMElement.hasValue: boolean;
begin
  result := FValue <> '';
end;

procedure TFHIRMMElement.getChildrenByName(name: String; children: TFHIRObjectList);
var
  child : TFHIRMMElement;
begin
  if (hasChildren()) then
  begin
    for child in self.Fchildren do
      if (name = child.Name) then
        children.add(child.link);
  end;
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
    for child in self.Fchildren do
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

function TFHIRMMElement.hasIndex: boolean;
begin
  result := FIndex > -1;
end;


function TFHIRMMElement.getChildValue(name: String): string;
var
  child : TFHIRMMElement;
begin
  result := '';
  if (hasChildren()) then
  begin
    for child in self.Fchildren do
      if (name = child.Name) then
				result := child.Value;
  end;
end;

function TFHIRMMElement.GetChildren: TAdvList<TFHIRMMElement>;
begin
  if FChildren = nil then
    FChildren := TAdvList<TFHIRMMElement>.create;
  result := FChildren;
end;

function TFHIRMMElement.hasType: boolean;
begin
  if (Ftype = '') then
    result := FProperty.hasType(name)
  else
    result := true;
end;

function TFHIRMMElement.fhirType: String;
begin
  result := GetType;
end;

procedure TFHIRMMElement.getProperty(hash: integer; name: String; checkValid: boolean; list: TAdvList<TFHIRBase>);
var
  child : TFHIRMMElement;
begin
// 	if isPrimitive() and (name = 'value') and (value <> '') then
//  		String tn = getType();
//  		throw new Error("not done yet");

  if FChildren <> nil then
  begin
   	for child in Fchildren do
    begin
  		if (child.Name = name) then
  			list.add(child);
  		if (child.Name.startsWith(name) and child.Prop.isChoice and (child.Prop.Name = name+'[x]')) then
  			list.add(child.link);
  	end;
  end;
end;

function TFHIRMMElement.isPrimitive: boolean;
begin
  if (Ftype <> '') then
    result := isPrimitiveType(Ftype)
  else
   result := prop.isPrimitive(name);
end;

function TFHIRMMElement.hasPrimitiveValue: boolean;
begin
  result := prop.isPrimitive(name) or prop.IsLogicalAndHasPrimitiveValue(name);
end;

function TFHIRMMElement.primitiveValue: String;
var
  c : TFHIRMMElement;
begin
  result := '';
	if (isPrimitive()) then
    result := value
  else if (hasPrimitiveValue() and hasChildren) then
  	for c in Fchildren do
      if (c.Name = 'value') then
				exit(c.primitiveValue());
end;

function TFHIRMMElement.markLocation(start, end_: TSourceLocation): TFHIRMMElement;
begin
  FLocStart := start;
  FLocEnd := end_;
  result := self;
end;

function TFHIRMMElement.getNamedChild(name: String): TFHIRMMElement;
var
  c : TFHIRMMElement;
begin
	result := nil;
  if (Fchildren <> nil) then
  begin
	  for c in Fchildren do
	  	if (c.Name = name) then
	  		if (result = nil) then
	  			result := c
	  		else
	  			raise Exception.create('Attempt to read a single element when there is more than one present ('+name+')');
  end;
end;

procedure TFHIRMMElement.getNamedChildren(name: String; list: TAdvList<TFHIRMMElement>);
var
  c : TFHIRMMElement;
begin
	if (Fchildren <> nil) then
		for c in Fchildren do
      if (c.Name = name) then
        list.add(c.link);
end;

function TFHIRMMElement.getNamedChildValue(name: String): string;
var
  child : TFHIRMMElement;
begin
  child := getNamedChild(name);
  if (child = nil) then
    result := ''
  else
    result := child.value;
end;

function TFHIRMMElement.hasComments: boolean;
begin
  result := (FComments <> nil) and (FComments.count > 0);
end;

function TFHIRMMElement.link: TFHIRMMElement;
begin
  result := TFHIRMMElement(inherited Link);
end;


procedure TFHIRMMElement.SetXhtml(const Value: TFhirXHtmlNode);
begin
  FXhtml.Free;
  FXhtml := Value;
end;

{ TFHIRMMParserBase }

constructor TFHIRMMParserBase.create(context: TWorkerContext);
begin
  inherited create;
  self.FContext := context;
end;

destructor TFHIRMMParserBase.Destroy;
begin
  FContext.Free;
  FErrors.Free;
  inherited;
end;

procedure TFHIRMMParserBase.setupValidation(policy: TFHIRValidationPolicy; errors: TFhirOperationOutcomeIssueList);
begin
  FPolicy := policy;
  FErrors.Free;
  FErrors := errors;
end;


procedure TFHIRMMParserBase.logError(line, col: integer; path: String; type_: TFhirIssueTypeEnum; message: String; level: TFhirIssueSeverityEnum);
var
  err : TFhirOperationOutcomeIssue;
begin
  if (Fpolicy = fvpEVERYTHING) then
  begin
    err := Ferrors.Append;
    err.locationList.add(path);
    err.code := type_;
    err.severity := level;
    err.details :=  TFhirCodeableConcept.Create;
    err.details.text := message+Stringformat(' at line %d col %d', [line, col]);
  end
	else if (level = IssueSeverityFatal) or ((level = IssueSeverityERROR) and (Fpolicy = fvpQUICK)) then
	 raise Exception.create(message+Stringformat(' at line %d col %d', [line, col]));
end;

function TFHIRMMParserBase.parse(stream: TAdvStream): TFHIRMMElement;
var
  vcl : TVCLStream;
begin
  vcl := TVCLStream.create;
  try
    vcl.Stream := stream.link;
    result := parse(vcl);
  finally
    vcl.Free;
  end;
end;

function TFHIRMMParserBase.parse(buffer: TAdvBuffer): TFHIRMMElement;
var
  mem : TAdvMemoryStream;
begin
  mem := TAdvMemoryStream.Create;
  try
    mem.Buffer := buffer.Link;
    result := parse(mem);
  finally
    mem.Free;
  end;
end;

function TFHIRMMParserBase.getDefinition(line, col: integer; ns, name: String): TFHIRStructureDefinition;
var
  sd : TFHIRStructureDefinition;
  sdl : TFHIRStructureDefinitionList;
  sns : String;
begin
  result := nil;
  if (ns = '') then
  begin
    logError(line, col, name, IssueTypeSTRUCTURE, 'This cannot be parsed as a FHIR object (no namespace)', IssueSeverityFATAL);
    exit(nil);
  end;
  if (name = '') then
  begin
    logError(line, col, name, IssueTypeSTRUCTURE, 'This cannot be parsed as a FHIR object (no name)', IssueSeverityFATAL);
    exit(nil);
  end;
  for sd in FContext.profiles.ProfilesByURL.values do
    if (name = sd.Id) then
    begin
      if ((ns = '') or (ns = FHIR_NS)) and not sd.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace') then
        exit(sd);
      sns := sd.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace');
      if (ns = sns) then
        exit(sd);
    end;
  logError(line, col, name, IssueTypeSTRUCTURE, 'This does not appear to be a FHIR resource (unknown namespace/name "'+ns+'::'+name+'")', IssueSeverityFATAL);
  result := nil;
end;

function TFHIRMMParserBase.getDefinition(line, col: integer; name: String): TFHIRStructureDefinition;
var
  sd : TFHIRStructureDefinition;
begin
  result := nil;
  if (name = '') then
  begin
    logError(line, col, name, IssueTypeSTRUCTURE, 'This cannot be parsed as a FHIR object (no name)', IssueSeverityFATAL);
    exit(nil);
  end;
	for sd in Fcontext.Profiles.ProfilesByURL.values do
    if (name = sd.Id) then
      exit(sd);
  logError(line, col, name, IssueTypeSTRUCTURE, 'This does not appear to be a FHIR resource (unknown name "'+name+'")', IssueSeverityFATAL);
  result := nil;
end;


function TFHIRMMParserBase.getChildProperties(prop: TFHIRMMProperty; elementName, statedType: String): TAdvList<TFHIRMMProperty>;
var
  ed : TFhirElementDefinition;
  sd : TFhirStructureDefinition;
  children : TFhirElementDefinitionList;
  child : TFhirElementDefinition;
  tr : TFhirElementDefinitionType;
  t : String;
  all, ok : boolean;
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
				  // ok, it's polymorphic
				  if (PropertyRepresentationTYPEATTR in ed.Representation) then
          begin
				    t := statedType;
				    if (t = '') and ed.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-defaultype') then
				      t := ed.GetExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-defaultype');
				    ok := false;
		        for tr in ed.type_List do
		          if (CODES_TFhirDefinedTypesEnum[tr.code] = t) then
		            ok := true;
            if (not ok) then
		           raise Exception.create('Type "'+t+'" is not an acceptable type for "'+elementName+'" on property '+prop.Definition.Path);
				  end
          else
          begin
            t := elementName.substring(tail(ed.Path).length - 3);
            if (isPrimitiveType(lowFirst(t))) then
              t := lowFirst(t);
          end;
        end;
      end;
      if (t <> 'xhtml') then
      begin
        sd.Free;
        sd := FContext.getStructure('http://hl7.org/fhir/StructureDefinition/'+t);
        if (sd = nil) then
          raise Exception.create('Unable to find class "'+t+'" for name "'+elementName+'" on property '+prop.Definition.Path);
        children.Free;
        children := FContext.getChildMap(sd, sd.snapshot.elementList[0]);
      end;
    end;
    result := TAdvList<TFHIRMMProperty>.create;
    for child in children do
      result.add(TFHIRMMProperty.create(FContext.link, child.link, sd.link));
  finally
    children.Free;
    sd.Free;
  end;
end;

{ TFHIRMMManager }

class procedure TFHIRMMManager.composeFile(context: TWorkerContext; e: TFHIRMMElement; filename: String; outputFormat: TFhirFormat; pretty: boolean; base: String);
var
  f : TFileStream;
begin
  f := TFileStream.create(filename, fmCreate);
  try
    compose(context, e, f, outputFormat, pretty, base);
  finally
    f.free;
  end;
end;

class function TFHIRMMManager.makeParser(context: TWorkerContext; format: TFhirFormat): TFHIRMMParserBase;
begin
  case format of
    ffXML : result := TFHIRMMXmlParser.create(context.Link);
    ffJSON : result := TFHIRMMJsonParser.create(context.Link);
//    fmfTURTLE : result := TFHIRMMTurtleParser.create(context);
//    fmfJSONLD : result := TFHIRMMJsonLDParser.create(context);
  else
    result := nil;
  end;
end;

class procedure TFHIRMMManager.compose(context: TWorkerContext; e: TFHIRMMElement; destination: TStream; outputFormat: TFhirFormat; pretty: boolean; base: String);
var
  p : TFHIRMMParserBase;
begin
  p := makeParser(context, outputFormat);
  try
    p.compose(e, destination, pretty, base);
  finally
    p.Free;
  end;
end;

class function TFHIRMMManager.parse(context: TWorkerContext; source: TStream; inputFormat: TFhirFormat): TFHIRMMElement;
var
  p : TFHIRMMParserBase;
begin
  p := makeParser(context, inputFormat);
  try
    result := p.parse(source);
  finally
    p.Free;
  end;
end;

class function TFHIRMMManager.parseFile(context: TWorkerContext; filename: string; inputFormat: TFhirFormat): TFHIRMMElement;
var
  f : TFileStream;
begin
  f := TFileStream.create(filename, fmOpenread + fmShareDenywrite);
  try
    result := parse(context, f, inputFormat);
  finally
    f.free;
  end;
end;

{ TFHIRMMXmlParser }

destructor TFHIRMMXmlParser.Destroy;
begin
  FLocations.Free;
  inherited;
end;

function TFHIRMMXmlParser.parse(stream: TStream): TFHIRMMElement;
var
  doc : IXMLDOMDocument2;
begin
  try
    if FPolicy = fvpEVERYTHING then
    begin
      FLocations := TAdvList<TSourceLocationObject>.create;
      doc := TMsXmlParser.Parse(stream, FLocations)
    end
    else
      doc := TMsXmlParser.Parse(stream);
  except
    on e : Exception do
    begin
      logError(0, 0, '(syntax)', IssueTypeINVALID, e.Message, IssueSeverityFATAL);
      exit(nil);
    end;
  end;
  result := parse(doc);
end;

procedure TFHIRMMXmlParser.checkRootNode(document : IXMLDOMDocument);
var
  node : IXMLDOMNode;
begin
   if (FPolicy = fvpEVERYTHING) then
   begin
    node := document.FirstChild;
    while (node <> nil) do
    begin
      if (node.NodeType = NODE_PROCESSING_INSTRUCTION) then
        logError(line(document), col(document), '(document)', IssueTypeINVALID, 'No processing instructions allowed in resources', IssueSeverityERROR);
      if (node.NodeType = NODE_DOCUMENT_TYPE) then
        logError(line(document), col(document), '(document)', IssueTypeINVALID, 'No document type declarations allowed in resources', IssueSeverityERROR);
      node := node.NextSibling;
    end;
  end;
end;

function TFHIRMMXmlParser.line(node : IXMLDomNode) : integer;
var
  e : IXMLDOMElement;
  i : integer;
  o : variant;
begin
  result := -1;
  if node.nodeType = NODE_ELEMENT then
  begin
    e := node as IXMLDOMElement;
    o := e.getAttribute(MAP_ATTR_NAME);
    if not varisnull(o) then
    begin
      i := StrToIntDef(o, -1);
      if i > -1 then
        result := FLocations[i].locationStart.line;
    end;
  end;
end;

function TFHIRMMXmlParser.col(node : IXMLDomNode) : integer;
var
  e : IXMLDOMElement;
  i : integer;
  o : variant;
begin
  result := -1;
  if node.nodeType = NODE_ELEMENT then
  begin
    e := node as IXMLDOMElement;
    o := e.getAttribute(MAP_ATTR_NAME);
    if not varisnull(o) then
    begin
      i := StrToIntDef(e.getAttribute(MAP_ATTR_NAME), -1);
      if i > -1 then
        result := FLocations[i].locationStart.col;
    end;
  end;
end;

function TFHIRMMXmlParser.parse(document : IXMLDOMDocument) : TFHIRMMElement;
var
  element : IXMLDOMElement;
begin
  checkRootNode(document);
  element := document.DocumentElement;
  result := parse(element);
end;

function TFHIRMMXmlParser.parse(element : IXMLDomElement) : TFHIRMMElement;
var
  ns, name, path : String;
  sd : TFhirStructureDefinition;
begin
  ns := element.NamespaceURI;
  name := element.baseName;
  path := '/'+pathPrefix(ns)+name;

  sd := getDefinition(line(element), col(element), ns, name);
  if (sd = nil) then
    exit(nil);

  result := TFHIRMMElement.create(element.baseName, TFHIRMMProperty.create(Fcontext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  checkElement(element, path, result.Prop);
  result.markLocation(start(element), end_(element));
  result.type_ := element.baseName;
  parseChildren(path, element, result);
  result.numberChildren();
end;

function TFHIRMMXmlParser.pathPrefix(ns : String) : String;
begin
  if (ns = '') then
    exit('');
  if (ns = FHIR_NS) then
    exit('f:');
  if (ns = XHTML_NS) then
    exit('h:');
  if (ns = 'urn:hl7-org:v3') then
    exit('v3:');
  exit('?:');
end;

function TFHIRMMXmlParser.empty(element : IXMLDomElement) : boolean ;
var
  i : integer;
  n : String;
  node : IXMLDOMNode;
begin
  for i := 0 to element.attributes.Length - 1 do
  begin
    n := element.Attributes[i].nodeName;
    if (n <> 'xmlns') and not n.startsWith('xmlns:') and (n <> MAP_ATTR_NAME) then
      exit(false);
  end;
  if ('' <> trim(element.text)) then
      exit(false);

  node := element.FirstChild;
  while (node <> nil) do
  begin
    if (node.NodeType = NODE_ELEMENT) then
      exit(false);
    node := node.NextSibling;
  end;
  exit(true);
end;


function TFHIRMMXmlParser.end_(node: IXMLDomNode): TSourceLocation;
var
  e : IXMLDOMElement;
  i : integer;
begin
  result.line := -1;
  result.col := -1;
  if node.nodeType = NODE_ELEMENT then
  begin
    e := node as IXMLDOMElement;
    i := StrToIntDef(VartoStr(e.getAttribute(MAP_ATTR_NAME)), -1);
    if i > -1 then
      result := FLocations[i].locationEnd;
  end;
end;

procedure TFHIRMMXmlParser.checkElement(element : IXMLDomElement; path : String; prop : TFHIRMMProperty);
var
  ns : String;
begin
  if (FPolicy = fvpEVERYTHING) then
  begin
    if (empty(element)) then
      logError(line(element), col(element), path, IssueTypeINVALID, 'Element must have some content', IssueSeverityERROR);
    ns := FHIR_NS;
    if (prop.Definition.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace')) then
      ns := prop.Definition.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace')
    else if (prop.Structure.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace')) then
      ns := prop.Structure.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-namespace');
    if (element.NamespaceURI <> ns) then
      logError(line(element), col(element), path, IssueTypeINVALID, 'Wrong namespace - expected "'+ns+'"', IssueSeverityERROR);
  end;
end;

function getXsiType(node : IXMLDomElement) : String;
begin
  result := TMsXmlParser.GetAttribute(node, 'http://www.w3.org/2001/XMLSchema-instance', 'type');
  if (result.contains(':')) then
    result := result.substring(result.indexOf(';')+1);
end;

procedure TFHIRMMXmlParser.parseChildren(path : String; node : IXMLDomElement; context : TFHIRMMElement);
var
  properties : TAdvList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  text : String;
  i : integer;
  attr, child : IXMLDOMNode;
  e : IXMLDOMElement;
  av: String;
  xhtml: TFhirXHtmlNode;
  n : TFHIRMMElement;
  npath, xsiType : String;
  ok : boolean;
begin
  // this parsing routine retains the original order in a the XML file, to support validation
  reapComments(node, context);
  properties := getChildProperties(context.Prop, context.Name, getXsiType(node));
  try
    text := TMsXmlParser.TextContent(node, ttTrim).trim();
    if ('' <> text) then
    begin
      prop := getTextProp(properties);
      if (prop <> nil) then
      begin
        context.getChildren().add(TFHIRMMElement.create(prop.Name, prop.Link, prop.getType(), text).markLocation(start(node), end_(node)));
      end
      else
      begin
        logError(line(node), col(node), path, IssueTypeSTRUCTURE, 'Text should not be present', IssueSeverityERROR);
      end;
    end;

    for i := 0 to node.Attributes.Length - 1 do
    begin
      attr := node.Attributes[i];
      if not ((attr.nodeName = 'xmlns') or StringStartsWith(attr.nodeName, 'xmlns:')) then
      begin
        prop := getAttrProp(properties, attr.nodeName);
        if (prop <> nil) then
        begin
          av := attr.NodeValue;
          if (prop.Definition.hasExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/elementdefinition-dateformat')) then
            av := convertForDateFormat(prop.Definition.getExtensionString('http://www.healthintersections.com.au/fhir/StructureDefinition/elementdefinition-dateformat'), av);
          if (prop.Name = 'value') and context.isPrimitive() then
            context.Value := av
          else
            context.getChildren().add(TFHIRMMElement.create(prop.Name, prop.Link, prop.getType(), av).markLocation(start(node), end_(node)));
        end
        else if attr.nodeName <> MAP_ATTR_NAME then
        begin
          logError(line(node), col(node), path, IssueTypeSTRUCTURE, 'Undefined attribute "@'+attr.nodeName+'"', IssueSeverityERROR);
        end;
      end;
    end;

    child := node.FirstChild;
    while (child <> nil) do
    begin
      if (child.NodeType = NODE_ELEMENT) then
      begin
        e := child as IXMLDOMElement;
        prop := getElementProp(properties, e.baseName);
        if (prop <> nil) then
        begin
          if (not prop.isChoice()) and ('xhtml' = prop.getType()) then
          begin
            xhtml := TFHIRXhtmlParser.parse('en', xppReject, [xopValidatorMode], e, path, FHIR_NS);
            n := TFHIRMMElement.create('div', prop.link, 'xhtml', TFHIRXhtmlParser.compose(xhtml));
            context.getChildren().add(n);
            n.Xhtml := xhtml;
            n.markLocation(start(e), end_(e));
          end
          else
          begin
            npath := path+'/'+pathPrefix(e.NamespaceURI)+e.baseName;
            n := TFHIRMMElement.create(e.baseName, prop.Link);
            context.getChildren().add(n);
            n.markLocation(start(e), end_(e));
            checkElement(e as IXMLDOMElement, npath, n.Prop);
            ok := true;
            if (prop.isChoice()) then
            begin
              if (PropertyRepresentationTYPEATTR in prop.Definition.Representation) then
              begin
                xsiType := getXsiType(e);
                if (xsiType = '') then
                begin
                  logError(line(e), col(e), path, IssueTypeSTRUCTURE, 'No type found on "'+e.baseName+'"', IssueSeverityERROR);
                  ok := false;
                end
                else
                begin
                  if (xsiType.contains(':')) then
                    xsiType := xsiType.substring(xsiType.indexOf(':')+1);
                  n.Type_ := xsiType;
                end;
              end
              else
                n.Type_ := n.getType();
            end;
            if (ok) then
            begin
              if (prop.isResource()) then
                parseResource(npath, e, n)
              else
                parseChildren(npath, e, n);
            end;
          end;
        end
        else
          logError(line(e), col(e), path, IssueTypeSTRUCTURE, 'Undefined element "'+e.baseName+'"', IssueSeverityERROR);
      end
      else if (child.NodeType = NODE_CDATA_SECTION)then
      begin
        logError(line(child), col(child), path, IssueTypeSTRUCTURE, 'CDATA is not allowed', IssueSeverityERROR);
      end
      else if not (child.NodeType in [NODE_TEXT, NODE_COMMENT]) then
      begin
        logError(line(child), col(child), path, IssueTypeSTRUCTURE, 'Node type '+Integer.toString(child.NodeType)+' is not allowed', IssueSeverityERROR);
      end;
      child := child.NextSibling;
    end;
  finally
    properties.free;
  end;
end;


function TFHIRMMXmlParser.getElementProp(props : TAdvList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
	for p in props do
    if not (PropertyRepresentationXMLATTR in p.Definition.Representation) and not (PropertyRepresentationXMLTEXT in p.Definition.Representation) then
    begin
      if (p.Name = nodeName) then
        exit(p);
      if (p.Name.endsWith('[x]')) and (nodeName.length > p.Name.length-3) and (p.Name.substring(0, p.Name.length-3) = nodeName.substring(0, p.Name.length-3)) then
        exit(p);
    end;
  exit(nil);
end;

function TFHIRMMXmlParser.getAttrProp(props : TAdvList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
  for p in props do
    if (p.Name = nodeName) and (PropertyRepresentationXMLATTR in p.Definition.Representation) then
      exit(p);
  exit(nil);
end;


function TFHIRMMXmlParser.getTextProp(props : TAdvList<TFHIRMMProperty>) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
  for p in props do
    if (PropertyRepresentationXMLTEXT in p.Definition.Representation) then
      exit(p);
  exit(nil);
end;

function TFHIRMMXmlParser.convertForDateFormat(fmt, av : String) : String;
var
  d : TDateAndTime;
begin
  if ('v3' = fmt) then
  begin
    d := TDateAndTime.CreateHL7(av);
    try
      result := d.AsXML;
    finally
      d.Free;
    end;
  end
  else
    raise Exception.create('Unknown Data format "'+fmt+'"');
end;

procedure TFHIRMMXmlParser.parseResource(s : String; container : IXMLDomElement; parent : TFHIRMMElement);
var
  res : IXMLDOMElement;
  name : String;
  sd : TFHIRStructureDefinition;
begin
  res := TMsXmlParser.FirstChild(container);
  name := res.baseName;
  sd := Fcontext.getStructure('http://hl7.org/fhir/StructureDefinition/'+name);
  if (sd = nil) then
    raise Exception.create('Contained resource does not appear to be a FHIR resource (unknown name "'+res.baseName+'")');
  if parent.Prop.Name = 'contained' then
    parent.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].Link, sd), fseContained)
  else
    parent.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].Link, sd), fseBundleEntry);
  parent.Type_ := name;
  parseChildren(res.baseName, res, parent);
end;


procedure TFHIRMMXmlParser.reapComments(element : IXMLDomElement; context : TFHIRMMElement);
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
  begin
    node := node.PreviousSibling;
  end;
  while (node <> nil) do
  begin
    if (node.NodeType = NODE_COMMENT) then
      context.getComments().add(node.Text);
    node := node.NextSibling;
  end;
end;

function TFHIRMMXmlParser.start(node: IXMLDomNode): TSourceLocation;
var
  e : IXMLDOMElement;
  i : integer;
begin
  result.line := -1;
  result.col := -1;
  if node.nodeType = NODE_ELEMENT then
  begin
    e := node as IXMLDOMElement;

    i := StrToIntDef(VartoStr(e.getAttribute(MAP_ATTR_NAME)), -1);
    if i > -1 then
      result := FLocations[i].locationStart;
  end;
end;

function TFHIRMMXmlParser.isAttr(prop : TFHIRMMProperty) : boolean;
begin
  result := PropertyRepresentationXMLATTR in prop.Definition.Representation;
end;

function TFHIRMMXmlParser.isText(prop : TFHIRMMProperty) : boolean;
begin
  result := PropertyRepresentationXMLTEXT in prop.Definition.Representation;
end;

procedure TFHIRMMXmlParser.compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String);
var
  xml : TXmlBuilder;
begin
  xml := TAdvXmlBuilder.Create;
  try
    xml.IsPretty := pretty;
    xml.NoHeader := true;
    xml.CurrentNamespaces.DefaultNS := e.Prop.getNamespace();
    xml.Start;
    composeElement(xml, e, e.getType());
    xml.Finish;
    xml.Build(stream);
  finally
    xml.Free;
  end;
end;

procedure TFHIRMMXmlParser.composeElement(xml : TXmlBuilder; element : TFHIRMMElement; elementName : String);
var
  s : String;
  child : TFHIRMMElement;
begin
  for s in element.Comments do
    xml.comment(s);
  if (isText(element.Prop)) then
  begin
    xml.Open(elementName);
    xml.text(element.Value);
    xml.close(elementName);
  end
  else if element.isPrimitive() or (element.hasType() and isPrimitiveType(element.getType())) then
  begin
    if (element.getType() = 'xhtml') then
    begin
      xml.inject(TEncoding.UTF8.getBytes(element.Value));
    end
    else if (isText(element.Prop)) then
    begin
      xml.text(element.Value);
    end
    else
    begin
      if element.value <> '' then
        xml.AddAttribute('value', element.Value);
      if element.hasChildren then
      begin
        xml.open(elementName);
        for child in element.Children do
          composeElement(xml, child, child.Name);
        xml.close(elementName);
      end
      else
        xml.Tag(elementName);
    end;
  end
  else
  begin

    for child in element.Children do
    begin
      if (isAttr(child.Prop)) then
        xml.AddAttribute(child.Name, child.Value);
    end;
    xml.open(elementName);
    if element.special <> fsenil then
      xml.open(element.type_);
    for child in element.Children do
    begin
      if (isText(child.Prop)) then
        xml.text(child.Value)
      else if (not isAttr(child.prop)) then
        composeElement(xml, child, child.Name);
    end;
    if element.special <> fsenil then
      xml.close(element.type_);
    xml.close(elementName);
  end;
end;



{ TFHIRMMJsonParser }

function TFHIRMMJsonParser.parse(stream: TStream): TFHIRMMElement;
var
  obj : TJsonObject;
begin
  try
    obj := TJSONParser.parse(stream);
  except
    on e : Exception do
    begin
      logError(0, 0, '(syntax)', IssueTypeINVALID, e.Message, IssueSeverityFATAL);
      exit(nil);
    end;
  end;
  try
    result := parse(obj);
  finally
    obj.free;
  end;
end;


function TFHIRMMJsonParser.parse(obj: TJsonObject): TFHIRMMElement;
var
  name, path : String;
  sd : TFHIRStructureDefinition;
begin
  if not obj.has('resourceType') then
  begin
    logError(obj.LocationStart.Line, obj.LocationStart.Col, '$', IssueTypeINVALID, 'Unable to find resourceType property', IssueSeverityFATAL);
    exit(nil);
  end;

  name := obj.str['resourceType'];
  path := '/'+name;

  sd := getDefinition(obj.LocationStart.Line, obj.LocationStart.Col, name);
  if (sd = nil) then
    exit(nil);
  result := TFHIRMMElement.create(name, TFHIRMMProperty.create(FContext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  try
    checkObject(obj, path);
    result.markLocation(obj.LocationStart, obj.LocationEnd);
    result.Type_ := name;
    parseChildren(path, obj, result, true);
    result.numberChildren();
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRMMJsonParser.checkObject(obj: TJsonObject; path : String);
var
  found : boolean;
begin
  if (FPolicy = fvpEVERYTHING) and (obj.properties.count = 0) then
    logError(obj.LocationStart.Line, obj.LocationStart.Col, path, IssueTypeINVALID, 'Object must have some content', IssueSeverityERROR);
end;

procedure TFHIRMMJsonParser.parseChildren(path : String; obj: TJsonObject; context : TFHIRMMElement; hasResourceType : boolean);
var
  properties : TAdvList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  processed : TAdvStringSet;
  tr : TFHIRElementDefinitionType;
  ename, name : String;
begin
  //no-comments reapComments(obj, context);
  properties := getChildProperties(context.Prop, context.Name, '');
  processed := TAdvStringSet.create;
  try
    if (hasResourceType) then
      processed.add('resourceType');
    {no-comments processed.add('fhir_comments'); }

    // note that we do not trouble ourselves to maintain the wire format order here - we don"t even know what it was anyway
    // first pass: process the properties
    for prop in properties do
    begin
      if (prop.isChoice()) then
      begin
        for tr in prop.Definition.Type_List do
        begin
          eName := prop.Name.substring(0, prop.Name.length-3) + capitalize(CODES_TFhirDefinedTypesEnum[tr.Code]);
          if (not isPrimitiveType(CODES_TFhirDefinedTypesEnum[tr.Code]) and obj.has(eName)) then
          begin
            parseChildComplex(path, obj, context, processed, prop, eName);
            break;
          end else if (isPrimitiveType(CODES_TFhirDefinedTypesEnum[tr.Code]) and (obj.has(eName) or obj.has('_'+eName))) then
          begin
            parseChildPrimitive(path, obj, context, processed, prop, eName);
            break;
          end;
        end;
      end 
      else if (prop.isPrimitive('')) then
      begin
        parseChildPrimitive(path, obj, context, processed, prop, prop.Name);
      end 
      else if (obj.has(prop.Name)) then
      begin
        parseChildComplex(path, obj, context, processed, prop, prop.Name);
      end;
    end;

    // second pass: check for things not processed
    if (FPolicy <> fvpNONE) then
    begin
      for name in obj.properties.keys do
      begin
        if (not processed.contains(name)) then
          logError(obj.properties[name].locationStart.line, obj.properties[name].locationStart.col, path, IssueTypeSTRUCTURE, 'Unrecognised prop "'+name+'"', IssueSeverityERROR);
      end;
    end;
  finally
    properties.Free;
    processed.Free;
  end;
end;

procedure TFHIRMMJsonParser.parseChildComplex(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String);
var
  npath : String;
  e : TJsonNode;
  arr : TJsonArray;
  am : TJsonObject;
begin
  processed.add(name);
  npath := path+'/'+prop.Name;
  e := obj.properties[name];
  if (prop.isList() and (e is TJsonArray)) then
  begin
    arr := e as TjsonArray;
    for am in arr do
      parseChildComplexInstance(npath, obj, context, prop, name, am);
  end
  else
    parseChildComplexInstance(npath, obj, context, prop, name, e);
end;

procedure TFHIRMMJsonParser.parseChildComplexInstance(path : String; obj: TJsonObject; context : TFHIRMMElement; prop : TFHIRMMProperty; name : String; e : TJsonNode);
var
  child : TJsonObject;
  n : TFHIRMMElement;
begin
  if (e is TJsonObject) then
  begin
    child := e as TJsonObject;
    n := TFHIRMMElement.create(name, prop.Link).markLocation(child.LocationStart, child.LocationEnd);
    checkObject(child, path);
    context.getChildren().add(n);
    if (prop.isResource()) then
      parseResource(path, child, n)
    else
      parseChildren(path, child, n, false);
  end
  else if prop.isList then
    logError(e.LocationStart.Line, e.LocationStart.Col, path, IssueTypeINVALID, 'This prop must be an Array not a '+e.ClassName, IssueSeverityERROR)
  else
    logError(e.LocationStart.Line, e.LocationStart.Col, path, IssueTypeINVALID, 'This prop must be an object, not a '+e.className, IssueSeverityERROR);
end;

function arrC(arr : TJsonArray) : integer;
begin
  if (arr = nil) then
    result := 0
  else
    result := arr.Count;
end;

function arrI(arr : TJsonArray; i : integer) : TJsonNode;
begin
  if (arr = nil) or (i >= arr.count) then
    result := nil
  else if arr.item[i] is TJsonNull then
    result := nil
  else
    result := arr.item[i];
end;

procedure TFHIRMMJsonParser.parseChildPrimitive(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String);
var
  npath : String;
  main, fork : TJsonNode;
  arr1, arr2 : TJsonArray;
  m, f : TJsonNode;
  i : integer;
begin
  npath := path+'/'+prop.Name;
  processed.add(name);
  processed.add('_'+name);
  if (obj.has(name)) then
    main := obj.properties[name]
  else
    main := nil;
  if (obj.has('_'+name)) then
    fork := obj.properties['_'+name]
  else
    fork := nil;
  if (main <> nil) or (fork <> nil) then
  begin
    if (prop.isList() and ((main = nil) or (main is TJsonArray)) and ((fork = nil) or (fork is TJsonArray)) ) then
    begin
      arr1 := main as TjsonArray;
      arr2 := fork as TjsonArray;
      for i := 0 to max(arrC(arr1), arrC(arr2)) - 1 do
      begin
        m := arrI(arr1, i);
        f := arrI(arr2, i);
        parseChildPrimitiveInstance(npath, obj, context, processed, prop, name, m, f);
      end;
    end
    else
      parseChildPrimitiveInstance(npath, obj, context, processed, prop, name, main, fork);
  end;
end;

procedure TFHIRMMJsonParser.parseChildPrimitiveInstance(npath : String; obj: TJsonObject; context : TFHIRMMElement; processed : TAdvStringSet; prop : TFHIRMMProperty; name : String; main, fork : TJsonNode);
var
  n : TFHIRMMElement;
  child : TJsonObject;
begin
  if (main <> nil) and not ((main is TJsonString) or (main is TJsonBoolean) or (main is TJsonNull) or (main is TJsonNumber)) then
    logError(main.LocationStart.Line, main.LocationStart.Col, npath, IssueTypeINVALID, 'This prop must be an simple value, not a '+main.className, IssueSeverityERROR)
  else if (fork <> nil) and (not (fork is TJsonObject)) then
    logError(fork.LocationStart.Line, fork.LocationStart.Col, npath, IssueTypeINVALID, 'This prop must be an obj, not a '+fork.className, IssueSeverityERROR)
  else
  begin
    n := TFHIRMMElement.create(name, prop.link);
    context.Children.add(n);
    if (main <> nil) then
      n.markLocation(main.LocationStart, main.LocationEnd)
    else
      n.markLocation(fork.LocationStart, fork.LocationEnd);
    if (main <> nil) then
    begin
      if (main is TJsonString) then
        n.value := (main as TJsonString).Value
      else if (main is TJsonNumber) then
        n.value := (main as TJsonNumber).Value
      else if (main is TJsonBoolean) then
        if (main as TJsonBoolean).Value then
          n.value := 'true'
        else
          n.value := 'false';

      if ( not n.Prop.isChoice()) and (n.Type_ = 'xhtml') then
      begin
        try
          n.Xhtml := TFHIRXhtmlParser.parse('en', xppAllow, [xopValidatorMode], n.value);
        Except
          on e : Exception do
            logError(main.LocationStart.Line, main.LocationStart.Col, npath, IssueTypeINVALID, 'Error parsing XHTML: '+e.Message, IssueSeverityERROR);
        end;
      end;
      if (FPolicy = fvpEVERYTHING) then
      begin
        // now we cross-check the primitive format against the stated type
        if (n.Type_ = 'boolean') then
        begin
          if not (main is TJsonBoolean) then
            logError(main.LocationStart.Line, main.LocationStart.Col, npath, IssueTypeINVALID, 'Error parsing JSON: the primitive value must be a boolean', IssueSeverityERROR);
        end
        else if (StringArrayExistsSensitive(['integer', 'unsignedInt', 'positiveInt', 'decimal'], n.Type_)) then
        begin
          if not (main is TJsonNumber) then
            logError(main.LocationStart.Line, main.LocationStart.Col, npath, IssueTypeINVALID, 'Error parsing JSON: the primitive value must be a number', IssueSeverityERROR);
        end
        else if not (main is TJsonString) then
          logError(main.LocationStart.Line, main.LocationStart.Col, npath, IssueTypeINVALID, 'Error parsing JSON: the primitive value must be a string', IssueSeverityERROR);
      end;
    end;
    if (fork <> nil) then
    begin
      child := fork as TJsonObject;
      checkObject(child, npath);
      parseChildren(npath, child, n, false);
    end;
  end;
end;


procedure TFHIRMMJsonParser.parseResource(path : String; obj: TJsonObject; context : TFHIRMMElement);
var
  name : String;
  sd : TFHIRStructureDefinition;
begin
  if not obj.has('resourceType') then
  begin
    logError(obj.LocationStart.Line, obj.LocationStart.Col, '$', IssueTypeINVALID, 'Unable to find resourceType property', IssueSeverityFATAL);
    exit;
  end;

  name := obj.str['resourceType'];
  sd := getDefinition(obj.LocationStart.Line, obj.LocationStart.Col, name);
  if (sd <> nil) then
  begin
    if context.prop.name = 'contained' then
      context.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].link, sd.Link), fseContained)
    else
      context.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].link, sd.Link), fseBundleEntry);
    context.Type_ := name;
    parseChildren(path, obj, context, true);
  end;
end;


{procedure TFHIRMMJsonParser.reapComments(obj : TJsonObject; context : TFHIRMMElement);
var
  arr : TJsonArray;
  i : integer;
begin
  if (obj.has('fhir_comments')) then
  begin
    arr := obj.Arr['fhir_comments'];
    for i := 0 to arr.count - 1 do
      context.getComments().add((arr.Item[i] as TJsonString).value);
  end;
end;
}
procedure TFHIRMMJsonParser.compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String);
var
  oStream : TAdvVCLStream;
begin
  oStream := TAdvVCLStream.Create;
  try
    oStream.Stream := stream;
    compose(e, oStream, pretty, base);
  finally
    oStream.free;
  end;
end;

procedure TFHIRMMJsonParser.compose(e : TFHIRMMElement; stream : TAdvStream; pretty : boolean; base : String);
begin
  json := TJSONWriter.Create;
  try
    json.Stream := stream.Link;
    json.Start;
    json.HasWhitespace := pretty;
    compose(e);
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRMMJsonParser.compose(e : TFHIRMMElement);
var
  done : TAdvStringSet;
  child : TFHIRMMElement;
begin
  json.value('resourceType', e.type_);
  done := TAdvStringSet.create;
  try
    {no-comments composeComments(e); }
    for child in e.Children do
      compose(e.Name, e, done, child);
  finally
    done.free;
  end;
end;

procedure TFHIRMMJsonParser.compose(path : String; e : TFHIRMMElement; done : TAdvStringSet; child : TFHIRMMElement);
var
  list : TFHIRObjectList;
begin
  if (child.special = fseBundleEntry) or not child.Prop.isList() then // for specials, ignore the cardinality of the stated type 
    compose(path, child)
  else if not (done.contains(child.Name)) then
  begin
    done.add(child.Name);
    list := TFHIRObjectList.create;
    try
      e.getChildrenByName(child.Name, list);
      composeList(path, list);
    finally
      list.free;
    end;
  end;
end;

procedure TFHIRMMJsonParser.composeList(path : String; list : TFHIRObjectList);
var
  item, child : TFHIRMMElement;
  name : String;
  complex, prim : boolean;
  o : TFHIRObject;
  done : TAdvStringSet;
begin
  item := list[0] as TFHIRMMElement;
  // there will be at least one element
  name := item.Name;
  complex := true;
  if (item.isPrimitive()) then
  begin
    prim := false;
    complex := false;
    for o in list do
    begin
      item := o as TFHIRMMElement;
      if (item.hasValue()) then
        prim := true;
      if (item.hasChildren()) {no-comments or (item.hasComments())} then
        complex := true;
    end;
    if (prim) then
    begin
      json.ValueArray(name);
      for o in list do
      begin
        item := o as TFHIRMMElement;
        if (item.hasValue()) then
          primitiveValue('', item)
        else
          json.ValueNullInArray;
      end;
      json.FinishArray();
    end;
    name := '_'+name;
  end;
  if (complex) then
  begin
    json.ValueArray(name);
    for o in list do
    begin
      item := o as TFHIRMMElement;
      if (item.hasChildren()) then
      begin
        json.ValueObject;
        {no-comments composeComments(item);}
        if (item.Prop.isResource()) then
        begin
          json.value('resourceType', item.type_);
        end;
        done := TAdvStringSet.create;
        try
          for child in item.Children do
            compose(path+'.'+name+'[]', item, done, child);
        finally
          done.free;
        end;
        json.FinishObject;
      end
      else
        json.ValueNullInArray();
    end;
    json.FinishArray();
  end;
end;

procedure TFHIRMMJsonParser.primitiveValue(name : String; item : TFHIRMMElement);
begin
  if (item.type_ = 'boolean') then
    json.value(name, item.Value.Trim = 'true')
  else if (StringArrayExists(['decimal', 'integer', 'unsignedInt', 'positiveInt'], item.type_)) then
    json.ValueNumber(name, item.Value)
  else
    json.value(name, item.Value);
end;

procedure TFHIRMMJsonParser.compose(path : String; element : TFHIRMMElement);
var
  name : string;
  done : TAdvStringSet;
  child :  TFHIRMMElement;
begin
  name := element.Name;
  if (element.isPrimitive()) or (isPrimitiveType(element.type_)) then
  begin
    if (element.hasValue()) then
      primitiveValue(name, element);
    name := '_'+name;
  end;
  if (element.hasChildren()) {no-comments or (element.hasComments())} then
  begin
    json.ValueObject(name);
    {no-comments composeComments(element);}
    if (element.Prop.isResource()) then
      json.value('resourceType', element.Type_);
    done := TAdvStringSet.create;
    try
      for child in element.Children do
        compose(path+'.'+element.Name, element, done, child);
    finally
      done.free;
    end;
    json.finishObject;
  end;
end;


end.
