unit FHIR.R2.ElementModel;

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
  SysUtils, Classes, Variants, Math,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.MXml, FHIR.Support.Xml, FHIR.Support.Json,
  FHIR.Base.Objects, FHIR.Base.Xhtml, FHIR.Base.Common, FHIR.Base.Lang, FHIR.Base.ElementModel,
  FHIR.R2.Base, FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Utilities, FHIR.R2.Context, FHIR.R2.Common;


type
  TFHIRMMProperty = class (TFslObject)
  private
    FContext : TFHIRWorkerContext;
    FDefinition : TFHIRElementDefinition;
    FStructure : TFHIRStructureDefinition;
    FCanBePrimitive : integer;

    function GetName: string;
  public
    constructor Create(context : TFHIRWorkerContext; definition : TFHIRElementDefinition; structure : TFHIRStructureDefinition);
    destructor Destroy; override;
    function link : TFHIRMMProperty; overload;

    property context : TFHIRWorkerContext read FContext;
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
  TFHIRMMElement = class (TFHIRObject2)
  private
	  FComments : TStringList;// not relevant for production, but useful in documentation
	  FName : String;
	  FType : String;
	  FValue : String;
  	FIndex : integer;
  	FChildren : TFslList<TFHIRMMElement>;
	  FProperty : TFHIRMMProperty;
    FlocStart: TSourceLocation;
    FlocEnd: TSourceLocation;
    FSpecial : TFHIRMMSpecialElement;
    FXhtml : TFhirXHtmlNode;

    function GetType: String;
    function GetChildren: TFslList<TFHIRMMElement>;
    function GetComments: TStringList;
    procedure SetXhtml(const Value: TFhirXHtmlNode);
  public
    constructor Create(name : String); overload;
    constructor Create(name : String; prop : TFHIRMMProperty); overload;
    constructor Create(name : String; prop : TFHIRMMProperty; type_, value : String); overload;
    destructor Destroy; override;
    function link : TFHIRMMElement; overload;
    procedure updateProperty(prop : TFHIRMMProperty; special : TFHIRMMSpecialElement);
    function createPropertyValue(propName : string): TFHIRObject; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function isMetadataBased : boolean; override;

    property name : String read FName;
    property type_ : String read GetType write FType;
    property value : String read FValue write FValue;
    property children : TFslList<TFHIRMMElement> read GetChildren;
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
    procedure GetChildrenByName(name : String; children : TFHIRSelectionList); override;
    function getNamedChild(name : String) : TFHIRMMElement;
    procedure getNamedChildren(name : String; list : TFslList<TFHIRMMElement>);
    function getNamedChildValue(name : String) : string;

    procedure numberChildren;

    function getChildValue(name : String) : string;
    function hasType : boolean;
    function markLocation(start, end_ : TSourceLocation) : TFHIRMMElement;

    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function primitiveValue : String; override;
    procedure getProperty(name : String; checkValid : boolean; list : TFslList<TFHIRObject>); override;
  end;

  TFHIRValidationPolicy = (fvpNONE, fvpQUICK, fvpEVERYTHING);

  TFHIRMMParserBase = class (TFslObject)
	protected
    FContext : TFHIRWorkerContext;
   	FPolicy : TFHIRValidationPolicy;
    FErrors : TFslList<TFhirOperationOutcomeIssueW>;
	  function getChildProperties(prop : TFHIRMMProperty; elementName, statedType : String) : TFslList<TFHIRMMProperty>;
    function getDefinition(line, col : integer; ns, name : String) : TFHIRStructureDefinition; overload;
    function getDefinition(line, col : integer; name : String) : TFHIRStructureDefinition; overload;
  public
    constructor Create(context : TFHIRWorkerContext);
    destructor Destroy; override;

    procedure setupValidation(policy : TFHIRValidationPolicy; errors : TFslList<TFhirOperationOutcomeIssueW>);
    procedure logError(line, col : integer; path : String; type_ : TFhirIssueTypeEnum; message : String; level : TFhirIssueSeverityEnum);

    function parse(stream : TStream) : TFHIRMMElement; overload; virtual; abstract;
    function parse(stream : TFslStream) : TFHIRMMElement; overload; virtual;
    function parse(buffer : TFslBuffer) : TFHIRMMElement; overload; virtual;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); overload; virtual; abstract;
  end;

  TFHIRMMManager = class (TFHIRBaseMMManager)
  public
    class function parseFile(context : TFHIRWorkerContext; filename : string; inputFormat : TFhirFormat) : TFHIRMMElement;
    class function parse(context : TFHIRWorkerContext; source : TStream; inputFormat : TFhirFormat) : TFHIRMMElement;
    class procedure compose(context : TFHIRWorkerContext; e : TFHIRMMElement; destination : TStream; outputFormat : TFhirFormat; pretty : boolean; base : String = '');
    class procedure composeFile(context : TFHIRWorkerContext; e : TFHIRMMElement; filename : String; outputFormat : TFhirFormat; pretty : boolean; base : String = '');
    class function makeParser(context : TFHIRWorkerContext; format : TFhirFormat) : TFHIRMMParserBase;
    function parseV(context : TFHIRWorkerContextV; source : TStream; inputFormat : TFhirFormat) : TFHIRObject; override;
    procedure composeV(context : TFHIRWorkerContextV; e : TFHIRObject; destination : TStream; outputFormat : TFhirFormat; style : TFHIROutputStyle; base : String = ''); override;
  end;

  TFHIRMMXmlParser = class (TFHIRMMParserBase)
  private
    function line(node : TMXmlElement) : integer;
    function col(node : TMXmlElement) : integer;
    function start(node : TMXmlElement) : TSourceLocation;
    function end_(node : TMXmlElement) : TSourceLocation;
    function pathPrefix(ns : String) : String;

    procedure checkRootNode(document : TMXmlDocument);
    function empty(element : TMXmlElement) : boolean ;
    procedure checkElement(element : TMXmlElement; path : String; prop : TFHIRMMProperty);
  	function convertForDateFormat(fmt, av : String) : String;
  	procedure reapComments(element : TMXmlElement; context : TFHIRMMElement);

    function getElementProp(props : TFslList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
  	function getAttrProp(props : TFslList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
   	function getTextProp(props : TFslList<TFHIRMMProperty>) : TFHIRMMProperty;
    function isAttr(prop : TFHIRMMProperty) : boolean;
    function isText(prop : TFHIRMMProperty) : boolean;

    procedure parseChildren(path : String; node : TMXmlElement; context : TFHIRMMElement);
    procedure parseResource(s : String; container : TMXmlElement; parent : TFHIRMMElement);

    procedure composeElement(xml : TXmlBuilder; element : TFHIRMMElement; elementName : String);
  public
    destructor Destroy; override;

    function parse(stream : TStream) : TFHIRMMElement; overload; override;
    function parse(document : TMXmlDocument) : TFHIRMMElement; overload;
    function parse(element : TMXmlElement) : TFHIRMMElement; overload;
    function parse(element : TMXmlElement; sd : TFHIRStructureDefinition) : TFHIRMMElement; overload;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); override;
  end;

  TFHIRMMJsonParser = class (TFHIRMMParserBase)
  private
    json : TJSONWriter;

    procedure checkObject(obj : TJsonObject; path : String);
  	procedure parseChildren(path : String; obj : TJsonObject; context : TFHIRMMElement; hasResourceType : boolean);

    procedure parseChildComplex(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String);
    procedure parseChildComplexInstance(path : String; obj: TJsonObject; context : TFHIRMMElement; prop : TFHIRMMProperty; name : String; e : TJsonNode);
    procedure parseChildPrimitive(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String);
    procedure parseChildPrimitiveInstance(npath : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String; main, fork : TJsonNode);
    procedure parseResource(path : String; obj: TJsonObject; context : TFHIRMMElement);

    procedure composeElement(e : TFHIRMMElement); overload;
    procedure composeElement(path : String; e : TFHIRMMElement; done : TFslStringSet; child : TFHIRMMElement); overload;
    procedure composeList(path : String; list : TFHIRObjectList); overload;
    procedure composeList(path : String; list : TFHIRSelectionList); overload;
    procedure primitiveValue(name : String; item : TFHIRMMElement);
    procedure composeElement(path : String; element : TFHIRMMElement); overload;

  public
    function parse(stream : TStream) : TFHIRMMElement; overload; override;
    function parse(obj : TJsonObject) : TFHIRMMElement; overload;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); overload; override;
    procedure compose(e : TFHIRMMElement; stream : TFslStream; pretty : boolean; base : String); overload;
  end;

  TFHIRMMResourceLoader = class (TFHIRMMParserBase)
  private
   	procedure parseChildren(path : String; obj : TFHIRObject; context : TFHIRMMElement);
  public
    function parse(r : TFHIRResource) : TFHIRMMElement; overload;
    function parse(r : TFHIRObject) : TFHIRMMElement; overload;
    procedure compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String); override;
  end;

  TFHIRCustomResource = class (TFHIRResource)
  private
    FRoot: TFHIRMMElement;
    procedure SetRoot(const Value: TFHIRMMElement);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create(root : TFHIRMMElement);
    destructor Destroy; override;

    class function CreateFromBase(context : TFHIRWorkerContext; base : TFHIRObject) : TFHIRCustomResource;

    function isMetaDataBased : boolean; override;
    function hasExtensions : boolean; override;
    property Root : TFHIRMMElement read FRoot write SetRoot;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFHIRCustomResource; overload;
    function Clone : TFHIRCustomResource; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function FhirType : string; override;
    function getId : string; override;
    function equals(other : TObject) : boolean; override;
    procedure getProperty(name : String; checkValid : boolean; list : TFslList<TFHIRObject>); override;
  end;



implementation

uses
  FHIR.R2.Constants,
  FHIR.R2.Profiles;


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

constructor TFHIRMMProperty.create(context : TFHIRWorkerContext; definition: TFHIRElementDefinition; structure: TFHIRStructureDefinition);
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
    result := definition.type_List[0].Code;
    for i := 1 to definition.type_List.count - 1 do
      if (result <> definition.type_List[i].Code) then
				raise EDefinitionException.create('logic error, gettype when types > 1');
  end
  else
    result := definition.type_List[0].Code;
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
    t := definition.type_list[0].Code;
    all := true;
    for tr in definition.type_list do
    begin
      if (t <> tr.Code) then
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
      raise EDefinitionException.create('logic error, gettype when types > 1, name mismatch');
    end;
  end
  else
    result := definition.type_list[0].Code;
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
    t := definition.Type_list[0].Code;
    all := true;
    for tr in definition.type_List do
    begin
      if (t <> tr.Code) then
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
  result := (definition.type_list.count() = 1) and ((definition.type_list[0].Code = 'Resource') or (definition.type_list[0].Code = 'DomainResource'));
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
    if (ed.Path = sd.Id+'.value') and (ed.type_List.count = 1) and isPrimitive(ed.type_List[0].Code) then
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
  tn : String;
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

function TFHIRMMElement.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRMMElement.createPropertyValue');
end;

function TFHIRMMElement.isMetadataBased: boolean;
begin
  result := true;
end;

function TFHIRMMElement.hasExtensions: boolean;
var
  c : TFHIRMMElement;
begin
  result := False;
  for c in FChildren do
    if c.name = 'extension' then
      exit(true);
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

procedure TFHIRMMElement.getChildrenByName(name: String; children: TFHIRSelectionList);
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

function TFHIRMMElement.GetChildren: TFslList<TFHIRMMElement>;
begin
  if FChildren = nil then
    FChildren := TFslList<TFHIRMMElement>.create;
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

procedure TFHIRMMElement.getProperty(name: String; checkValid: boolean; list: TFslList<TFHIRObject>);
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

procedure TFHIRMMElement.setIdValue(id: String);
begin
  raise EFHIRTodo.create('TFHIRMMElement.setIdValue');
end;

function TFHIRMMElement.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRMMElement.setProperty');
end;

function TFHIRMMElement.markLocation(start, end_: TSourceLocation): TFHIRMMElement;
  begin
  FLocStart := start;
  FLocEnd := end_;
  result := self;
end;

function TFHIRMMElement.getId: String;
begin
  raise EFHIRTodo.create('TFHIRMMElement.getId:');
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
	  			raise EFHIRException.create('Attempt to read a single element when there is more than one present ('+name+')');
  end;
end;

procedure TFHIRMMElement.getNamedChildren(name: String; list: TFslList<TFHIRMMElement>);
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

constructor TFHIRMMParserBase.create(context: TFHIRWorkerContext);
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

procedure TFHIRMMParserBase.setupValidation(policy: TFHIRValidationPolicy; errors: TFslList<TFhirOperationOutcomeIssueW>);
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
    err := TFhirOperationOutcomeIssue.create;
    try
      err.locationList.add(path);
      err.code := type_;
      err.severity := level;
      err.details :=  TFhirCodeableConcept.Create;
      err.details.text := message+Stringformat(' at line %d col %d', [line, col]);
      Ferrors.add(TFhirOperationOutcomeIssue2.create(err.Link));
    finally
      err.Free;
    end;
  end
	else if (level = IssueSeverityFatal) or ((level = IssueSeverityERROR) and (Fpolicy = fvpQUICK)) then
	 raise EFHIRException.create(message+Stringformat(' at line %d col %d', [line, col]));
end;

function TFHIRMMParserBase.parse(stream: TFslStream): TFHIRMMElement;
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

function TFHIRMMParserBase.parse(buffer: TFslBuffer): TFHIRMMElement;
var
  mem : TFslMemoryStream;
begin
  mem := TFslMemoryStream.Create;
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
  result := FContext.getStructure(ns, name);
  if result = nil then
  logError(line, col, name, IssueTypeSTRUCTURE, 'This does not appear to be a FHIR resource (unknown namespace/name "'+ns+'::'+name+'")', IssueSeverityFATAL);
end;

function TFHIRMMParserBase.getDefinition(line, col: integer; name: String): TFHIRStructureDefinition;
var
  sd : TFHIRStructureDefinition;
  list : TFslList<TFHIRStructureDefinition>;
begin
  result := nil;
  if (name = '') then
  begin
    logError(line, col, name, IssueTypeSTRUCTURE, 'This cannot be parsed as a FHIR object (no name)', IssueSeverityFATAL);
    exit(nil);
  end;
  list := TFslList<TFHIRStructureDefinition>.create;
  try
    FContext.listStructures(list);
    for sd in list do
      if (name = sd.Id) then
        exit(sd);
  finally
    list.Free;
  end;
  logError(line, col, name, IssueTypeSTRUCTURE, 'This does not appear to be a FHIR resource (unknown name "'+name+'")', IssueSeverityFATAL);
  result := nil;
end;


function TFHIRMMParserBase.getChildProperties(prop: TFHIRMMProperty; elementName, statedType: String): TFslList<TFHIRMMProperty>;
var
  ed : TFhirElementDefinition;
  sd : TFhirStructureDefinition;
  children : TFhirElementDefinitionList;
  child : TFhirElementDefinition;
  tr : TFhirElementDefinitionType;
  t : String;
  all, ok : boolean;
begin
  if (prop.isResource) and (statedType <> '') then
  begin
    sd := FContext.getStructure('http://hl7.org/fhir/StructureDefinition/'+statedType);
    ed := sd.snapshot.elementList[0];
  end
  else
  begin
  ed := prop.Definition;
  sd := prop.Structure.Link;
  end;

  children := FContext.getChildMap(sd, ed);
  try
    if (children.isEmpty()) then
    begin
      // ok, find the right definitions
      t := '';
      if (ed.type_list.count() = 1) then
        t := ed.type_list[0].Code
      else if (ed.type_list.count() = 0) then
        raise EDefinitionException.create('types = 0, and no children found')
      else
      begin
        t := ed.type_list[0].Code;
        all := true;
        for tr in ed.type_list do
          if (tr.Code <> t) then
            all := false;
        if (not all) then
        begin
				  // ok, it's polymorphic
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
          raise EDefinitionException.create('Unable to find class "'+t+'" for name "'+elementName+'" on property '+prop.Definition.Path);
        children.Free;
        children := FContext.getChildMap(sd, sd.snapshot.elementList[0]);
      end;
    end;
    result := TFslList<TFHIRMMProperty>.create;
    for child in children do
      result.add(TFHIRMMProperty.create(FContext.link, child.link, sd.link));
  finally
    children.Free;
    sd.Free;
  end;
end;

{ TFHIRMMManager }

class procedure TFHIRMMManager.composeFile(context: TFHIRWorkerContext; e: TFHIRMMElement; filename: String; outputFormat: TFhirFormat; pretty: boolean; base: String);
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

class function TFHIRMMManager.makeParser(context: TFHIRWorkerContext; format: TFhirFormat): TFHIRMMParserBase;
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

class procedure TFHIRMMManager.compose(context: TFHIRWorkerContext; e: TFHIRMMElement; destination: TStream; outputFormat: TFhirFormat; pretty: boolean; base: String);
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

class function TFHIRMMManager.parse(context: TFHIRWorkerContext; source: TStream; inputFormat: TFhirFormat): TFHIRMMElement;
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

class function TFHIRMMManager.parseFile(context: TFHIRWorkerContext; filename: string; inputFormat: TFhirFormat): TFHIRMMElement;
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

procedure TFHIRMMManager.composeV(context: TFHIRWorkerContextV; e: TFHIRObject; destination: TStream; outputFormat: TFhirFormat; style: TFHIROutputStyle; base: String);
begin
  compose(context as TFHIRWorkerContext, e as TFHIRMMElement, destination, outputFormat, style = OutputStylePretty);
end;

function TFHIRMMManager.parseV(context: TFHIRWorkerContextV; source: TStream; inputFormat: TFhirFormat): TFHIRObject;
begin
  result := parse(context as TFHIRWorkerContext, source, inputFormat);
end;

{ TFHIRMMXmlParser }

destructor TFHIRMMXmlParser.Destroy;
begin
  inherited;
end;

function TFHIRMMXmlParser.parse(stream: TStream): TFHIRMMElement;
var
  doc : TMXmlDocument;
begin
  doc := nil;
  try
    try
      doc := TMXmlParser.Parse(stream, [xpResolveNamespaces]);
  except
    on e : Exception do
    begin
      logError(0, 0, '(syntax)', IssueTypeINVALID, e.Message, IssueSeverityFATAL);
      exit(nil);
    end;
  end;
  result := parse(doc);
  finally
    doc.Free;
  end;
end;

procedure TFHIRMMXmlParser.checkRootNode(document : TMXmlDocument);
var
  node : TMXmlElement;
begin
   if (FPolicy = fvpEVERYTHING) then
   begin
    node := document.First;
    while (node <> nil) do
    begin
      if (node.NodeType = ntProcessingInstruction) then
        logError(line(document), col(document), '(document)', IssueTypeINVALID, 'No processing instructions allowed in resources', IssueSeverityERROR);
      if (node.NodeType = ntDocumentDeclaration) then
        logError(line(document), col(document), '(document)', IssueTypeINVALID, 'No document type declarations allowed in resources', IssueSeverityERROR);
      node := node.Next;
    end;
  end;
end;

function TFHIRMMXmlParser.line(node : TMXmlElement) : integer;
  begin
  result := node.Start.line;
end;

function TFHIRMMXmlParser.col(node : TMXmlElement) : integer;
    begin
  result := node.Start.col;
end;

function TFHIRMMXmlParser.parse(document : TMXmlDocument) : TFHIRMMElement;
var
  element : TMXmlElement;
begin
  checkRootNode(document);
  element := document.Document;
  result := parse(element);
end;

function TFHIRMMXmlParser.parse(element : TMXmlElement) : TFHIRMMElement;
var
  ns, name, path : String;
  sd : TFhirStructureDefinition;
begin
  ns := element.NamespaceURI;
  name := element.localName;
  path := '/'+pathPrefix(ns)+name;

  sd := getDefinition(line(element), col(element), ns, name);
  try
  if (sd = nil) then
    exit(nil);
    result := nil;
    result := TFHIRMMElement.create(element.localName, TFHIRMMProperty.create(Fcontext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  checkElement(element, path, result.Prop);
  result.markLocation(start(element), end_(element));
    result.type_ := element.localName;
  parseChildren(path, element, result);
    result.numberChildren();
  finally
    sd.free;
  end;
  end;

function TFHIRMMXmlParser.parse(element : TMXmlElement; sd : TFhirStructureDefinition) : TFHIRMMElement;
begin
  result := TFHIRMMElement.create(element.localName, TFHIRMMProperty.create(Fcontext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  checkElement(element, sd.id, result.Prop);
  result.markLocation(start(element), end_(element));
  result.type_ := element.localName;
  parseChildren(sd.id, element, result);
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

function TFHIRMMXmlParser.empty(element : TMXmlElement) : boolean ;
var
  i : integer;
  a : TMXmlAttribute;
  node : TMXmlElement;
begin
  for a in element.attributes do
  begin
    if (a.Name <> 'xmlns') and not a.name.startsWith('xmlns:') then
      exit(false);
  end;
  if ('' <> trim(element.text)) then
      exit(false);

  node := element.First;
  while (node <> nil) do
  begin
    if (node.NodeType = ntElement) then
      exit(false);
    node := node.Next;
  end;
  exit(true);
end;


function TFHIRMMXmlParser.end_(node: TMXmlElement): TSourceLocation;
  begin
  result := node.Stop;
end;

procedure TFHIRMMXmlParser.checkElement(element : TMXmlElement; path : String; prop : TFHIRMMProperty);
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

function getXsiType(node : TMXmlElement) : String;
begin
  result := node.attributeNS['http://www.w3.org/2001/XMLSchema-instance', 'type'];
  if (result.contains(':')) then
    result := result.substring(result.indexOf(';')+1);
end;

procedure TFHIRMMXmlParser.parseChildren(path : String; node : TMXmlElement; context : TFHIRMMElement);
var
  properties : TFslList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  s, text : String;
  i : integer;
  attr : TMXmlAttribute;
  child : TMXmlElement;
  e : TMXmlElement;
  av: String;
  xhtml : TFhirXHtmlNode;
  n : TFHIRMMElement;
  npath, xsiType : String;
  ok : boolean;
begin
  // this parsing routine retains the original order in a the XML file, to support validation
  reapComments(node, context);
  properties := getChildProperties(context.Prop, context.Name, getXsiType(node));
  try
    text := node.allText.trim();
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

    for attr in node.Attributes do
      begin
      if not ((s = 'xmlns') or StringStartsWith(s, 'xmlns:')) then
      begin
        prop := getAttrProp(properties, s);
        if (prop <> nil) then
        begin
          av := attr.Value;
          if (prop.Definition.hasExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/elementdefinition-dateformat')) then
            av := convertForDateFormat(prop.Definition.getExtensionString('http://www.healthintersections.com.au/fhir/StructureDefinition/elementdefinition-dateformat'), av);
          if (prop.Name = 'value') and context.isPrimitive() then
            context.Value := av
          else
            context.getChildren().add(TFHIRMMElement.create(prop.Name, prop.Link, prop.getType(), av).markLocation(start(node), end_(node)));
      end
        else
          logError(line(node), col(node), path, IssueTypeSTRUCTURE, 'Undefined attribute "@'+s+'"', IssueSeverityERROR);
      end;
    end;

    child := node.First;
    while (child <> nil) do
    begin
      if (child.NodeType = ntElement) then
          begin
        e := child as TMXmlElement;
        prop := getElementProp(properties, e.localName);
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
            npath := path+'/'+pathPrefix(e.NamespaceURI)+e.localName;
            n := TFHIRMMElement.create(e.localName, prop.Link);
            context.getChildren().add(n);
            n.markLocation(start(e), end_(e));
            checkElement(e as TMXmlElement, npath, n.Prop);
            ok := true;
            if (prop.isChoice()) then
            begin
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
          logError(line(e), col(e), path, IssueTypeSTRUCTURE, 'Undefined element "'+e.localName+'"', IssueSeverityERROR);
      end
      else if (child.NodeType = ntCData) then
        logError(line(child), col(child), path, IssueTypeSTRUCTURE, 'CDATA is not allowed', IssueSeverityERROR)
      else if not (child.NodeType in [ntText, ntComment]) then
        logError(line(child), col(child), path, IssueTypeSTRUCTURE, 'Node type '+CODES_TMXmlElementType[child.NodeType]+' is not allowed', IssueSeverityERROR);
      child := child.Next;
          end;
        finally
    properties.free;
        end;
      end;


function TFHIRMMXmlParser.getElementProp(props : TFslList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
	for p in props do
    if not (PropertyRepresentationXMLATTR in p.Definition.Representation) then
    begin
      if (p.Name = nodeName) then
        exit(p);
      if (p.Name.endsWith('[x]')) and (nodeName.length > p.Name.length-3) and (p.Name.substring(0, p.Name.length-3) = nodeName.substring(0, p.Name.length-3)) then
        exit(p);
    end;
  exit(nil);
    end;

function TFHIRMMXmlParser.getAttrProp(props : TFslList<TFHIRMMProperty>; nodeName : String) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
  for p in props do
    if (p.Name = nodeName) and (PropertyRepresentationXMLATTR in p.Definition.Representation) then
      exit(p);
  exit(nil);
end;


function TFHIRMMXmlParser.getTextProp(props : TFslList<TFHIRMMProperty>) : TFHIRMMProperty;
var
  p : TFHIRMMProperty;
begin
  exit(nil);
end;

function TFHIRMMXmlParser.convertForDateFormat(fmt, av : String) : String;
var
  d : TDateTimeEx;
begin
  if ('v3' = fmt) then
  begin
    d := TDateTimeEx.fromHL7(av);
    result := d.ToXML;
  end
  else
    raise EFHIRException.create('Unknown Data format "'+fmt+'"');
end;

procedure TFHIRMMXmlParser.parseResource(s: String; container: TMXmlElement; parent: TFHIRMMElement);
var
  res : TMXmlElement;
  name : String;
  sd : TFHIRStructureDefinition;
begin
  res := container.firstElement;
  name := res.localName;
  sd := Fcontext.getStructure('http://hl7.org/fhir/StructureDefinition/'+name);
  try
  if (sd = nil) then
    raise EFHIRException.create('Contained resource does not appear to be a FHIR resource (unknown name "'+res.localName+'")');
  if parent.Prop.Name = 'contained' then
    parent.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].Link, sd), fseContained)
  else
    parent.updateProperty(TFHIRMMProperty.create(Fcontext.Link, sd.Snapshot.ElementList[0].Link, sd), fseBundleEntry);
  parent.Type_ := name;
    parseChildren(res.localName, res, parent);
  finally
    sd.free;
end;
end;


procedure TFHIRMMXmlParser.reapComments(element : TMXmlElement; context : TFHIRMMElement);
var
  node : TMXmlElement;
begin
  node := element.previous;
  while (node <> nil) and (node.NodeType <> ntElement) do
  begin
    if (node.NodeType = ntComment) then
      context.getComments().insert(0, node.Text);
    node := node.previous;
  end;
  node := element.last;
  while (node <> nil) and (node.NodeType <> ntElement) do
  begin
    node := node.previous;
  end;
  while (node <> nil) do
  begin
    if (node.NodeType = ntComment) then
      context.getComments().add(node.Text);
    node := node.Next;
  end;
end;

function TFHIRMMXmlParser.start(node: TMXmlElement): TSourceLocation;
begin
  result := node.Start;
end;

function TFHIRMMXmlParser.isAttr(prop : TFHIRMMProperty) : boolean;
begin
  result := PropertyRepresentationXMLATTR in prop.Definition.Representation;
end;

function TFHIRMMXmlParser.isText(prop : TFHIRMMProperty) : boolean;
begin
  result := false;
end;

procedure TFHIRMMXmlParser.compose(e : TFHIRMMElement; stream : TStream; pretty : boolean; base : String);
var
  xml : TXmlBuilder;
begin
  xml := TFslXmlBuilder.Create;
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
  try
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
  finally
    sd.free;
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
  properties : TFslList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  processed : TFslStringSet;
  tr : TFHIRElementDefinitionType;
  ename, name : String;
begin
  //no-comments reapComments(obj, context);
  properties := getChildProperties(context.Prop, context.Name, '');
  processed := TFslStringSet.create;
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
          eName := prop.Name.substring(0, prop.Name.length-3) + capitalize(tr.Code);
          if (not isPrimitiveType(tr.Code) and obj.has(eName)) then
          begin
            parseChildComplex(path, obj, context, processed, prop, eName);
            break;
          end else if (isPrimitiveType(tr.Code) and (obj.has(eName) or obj.has('_'+eName))) then
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

procedure TFHIRMMJsonParser.parseChildComplex(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String);
var
  npath : String;
  e : TJsonNode;
  arr : TJsonArray;
  am : TJsonNode;
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

procedure TFHIRMMJsonParser.parseChildPrimitive(path : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String);
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

procedure TFHIRMMJsonParser.parseChildPrimitiveInstance(npath : String; obj: TJsonObject; context : TFHIRMMElement; processed : TFslStringSet; prop : TFHIRMMProperty; name : String; main, fork : TJsonNode);
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
  oStream : TFslVCLStream;
begin
  oStream := TFslVCLStream.Create;
  try
    oStream.Stream := stream;
    compose(e, oStream, pretty, base);
  finally
    oStream.free;
  end;
end;

procedure TFHIRMMJsonParser.compose(e : TFHIRMMElement; stream : TFslStream; pretty : boolean; base : String);
begin
  json := TJsonWriterDirect.Create;
  try
    json.Stream := stream.Link;
    json.Start;
    json.HasWhitespace := pretty;
    composeElement(e);
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRMMJsonParser.composeElement(e : TFHIRMMElement);
var
  done : TFslStringSet;
  child : TFHIRMMElement;
begin
  json.value('resourceType', e.type_);
  done := TFslStringSet.create;
  try
    {no-comments composeComments(e); }
    for child in e.Children do
      composeElement(e.Name, e, done, child);
  finally
    done.free;
  end;
end;

procedure TFHIRMMJsonParser.composeElement(path : String; e : TFHIRMMElement; done : TFslStringSet; child : TFHIRMMElement);
var
  list : TFHIRSelectionList;
begin
  if (child.special = fseBundleEntry) or not child.Prop.isList() then // for specials, ignore the cardinality of the stated type 
    composeElement(path, child)
  else if not (done.contains(child.Name)) then
  begin
    done.add(child.Name);
    list := TFHIRSelectionList.create;
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
  done : TFslStringSet;
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
        done := TFslStringSet.create;
        try
          for child in item.Children do
            composeElement(path+'.'+name+'[]', item, done, child);
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

procedure TFHIRMMJsonParser.composeElement(path : String; element : TFHIRMMElement);
var
  name : string;
  done : TFslStringSet;
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
    done := TFslStringSet.create;
    try
      for child in element.Children do
        composeElement(path+'.'+element.Name, element, done, child);
    finally
      done.free;
    end;
    json.finishObject;
  end;
end;


procedure TFHIRMMJsonParser.composeList(path: String; list: TFHIRSelectionList);
var
  ol : TFHIRObjectList;
begin
  ol := list.asValues;
  try
    composeList(path, ol);
  finally
    ol.Free;
  end;
end;

{ TFHIRMMResourceLoader }

function TFHIRMMResourceLoader.parse(r: TFHIRResource): TFHIRMMElement;
var
  name, path : String;
  sd : TFHIRStructureDefinition;
begin
  name := CODES_TFHIRResourceType[r.resourceType];
  path := name;

  sd := getDefinition(-1, -1, name);
  if (sd = nil) then
    raise EFHIRException.create('Unable to find definition for '+name);

  result := TFHIRMMElement.create(name, TFHIRMMProperty.create(FContext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  try
    result.Type_ := name;
    parseChildren(path, r, result);
    result.numberChildren();
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRMMResourceLoader.compose(e: TFHIRMMElement; stream: TStream; pretty: boolean; base: String);
begin
  raise EFHIRException.create('not implemented');
end;

function TFHIRMMResourceLoader.parse(r: TFHIRObject): TFHIRMMElement;
var
  name, path : String;
  sd : TFHIRStructureDefinition;
begin
  name := r.fhirType;
  path := name;

  sd := getDefinition(-1, -1, name);
  if (sd = nil) then
    raise EFHIRException.create('Unable to find definition for '+name);

  result := TFHIRMMElement.create(name, TFHIRMMProperty.create(FContext.link, sd.Snapshot.ElementList[0].Link, sd.Link));
  try
    result.Type_ := name;
    parseChildren(path, r, result);
    result.numberChildren();
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRMMResourceLoader.parseChildren(path: String; obj: TFHIRObject; context: TFHIRMMElement);
var
  properties : TFslList<TFHIRMMProperty>;
  prop : TFHIRMMProperty;
  name : String;
  tr : TFHIRElementDefinitionType;
  list : TFHIRSelectionList;
  o : TFHIRSelection;
  n : TFHIRMMElement;
begin
  properties := getChildProperties(context.Prop, context.Name, context.Type_);
  try
    for prop in properties do
    begin
      list := TFHIRSelectionList.create;
      try
        obj.ListChildrenByName(prop.name, list);
        for o in list do
        begin
          if (o <> nil) then
          begin
            if o.value is TFHIRObjectText then
              context.value := TFHIRObjectText(o.value).value
            else if o.value is TFhirXHtmlNode then
            begin
              n := TFHIRMMElement.create(prop.name, prop.Link);
              n.Xhtml := TFhirXHtmlNode(o.value).link;
              n.value := TFHIRXhtmlParser.compose(TFhirXHtmlNode(o.value));
              context.getChildren().add(n);
            end
            else if o.value is TFHIRObject then
            begin
              name := prop.name;
              if name.endsWith('[x]') then
                name := name.substring(0, name.length - 3)+capitalize(TFHIRObject(o.value).fhirType);
              n := TFHIRMMElement.create(name, prop.Link);
              context.getChildren().add(n);
              // is this a resource boundary?
              if prop.isResource then
                n.type_ := TFHIRObject(o.value).fhirType;
              parseChildren(path+'.'+name, o.value as TFHIRObject, n);
            end;
          end;
        end;
      finally
        list.free;
      end;
    end;
  finally
    properties.Free;
  end;
end;

{ TFHIRCustomResource }

constructor TFHIRCustomResource.Create(root: TFHIRMMElement);
begin
  inherited Create;
  FRoot := root;
end;

class function TFHIRCustomResource.CreateFromBase(context : TFHIRWorkerContext; base: TFHIRObject): TFHIRCustomResource;
var
  e : TFHIRMMElement;
  l : TFHIRMMResourceLoader;
begin
  l := TFHIRMMResourceLoader.create(context.link);
  try
    e := l.parse(base);
    try
      result := TFHIRCustomResource.create(e.link);
    finally
      e.free;
    end;
  finally
    l.free;
  end;
end;

destructor TFHIRCustomResource.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TFHIRCustomResource.SetRoot(const Value: TFHIRMMElement);
begin
  FRoot.Free;
  FRoot := Value;
end;

function TFHIRCustomResource.Link: TFHIRCustomResource;
begin
  result := TFHIRCustomResource(inherited Link);
end;

procedure TFHIRCustomResource.Assign(oSource: TFslObject);
begin
  raise EFHIRTodo.create('TFHIRCustomResource.Assign');
end;

function TFHIRCustomResource.Clone: TFHIRCustomResource;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.Clone:');
end;

function TFHIRCustomResource.equals(other : TObject): boolean;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.equalsDeep');
end;

function TFHIRCustomResource.FhirType: string;
begin
  result := FRoot.fhirType;
end;

procedure TFHIRCustomResource.GetChildrenByName(child_name: string; list: TFHIRSelectionList);
begin
  FRoot.GetChildrenByName(child_name, list);
end;

function TFHIRCustomResource.getId: string;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.getId');
end;

procedure TFHIRCustomResource.getProperty(name: String; checkValid: boolean; list: TFslList<TFHIRObject>);
begin
  raise EFHIRTodo.create('TFHIRCustomResource.getProperty');
end;

function TFHIRCustomResource.GetResourceType: TFhirResourceType;
begin
  result := frtCustom;
end;

function TFHIRCustomResource.isMetaDataBased: boolean;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.isMetaDataBased:');
end;

procedure TFHIRCustomResource.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  raise EFHIRTodo.create('TFHIRCustomResource.ListProperties');
end;

function TFHIRCustomResource.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.makeProperty');
end;

function TFHIRCustomResource.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRTodo.create('TFHIRCustomResource.setProperty');
end;

function TFHIRCustomResource.hasExtensions: boolean;
begin
  result := false;
end;

end.
