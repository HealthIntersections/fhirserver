unit FHIR.R2.ParserBase;

interface

uses
  SysUtils,
  FHIR.Support.Json, FHIR.Support.MXml, FHIR.Support.Xml, FHIR.Support.Turtle,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.R2.Types, FHIR.R2.Resources;

type
  TFHIRXmlParserBase2 = class (TFHIRXmlParserBase)
  protected
    function ParseResource(element : TMXmlElement; path : String) : TFhirResource; virtual;
    Function ParseResourceV(element : TMXmlElement; path : String) : TFhirResourceV; override;

    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;
    function ParseDataTypeV(element : TMXmlElement; name : String; type_ : TClass) : TFHIRObject; override;

    Function ParseInnerResource(element : TMXmlElement; path : String) : TFhirResource;
  end;

  TFHIRJsonParserBase2 = class (TFHIRJsonParserBase)
  protected
    function ParseResource(jsn : TJsonObject) : TFhirResource; virtual;
    Function ParseResourceV(jsn : TJsonObject) : TFhirResourceV; override;

    function ParseDataTypeV(jsn : TJsonObject; name : String; type_ : TClass) : TFHIRObject; override;
    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;

    procedure ParseInnerResource(jsn : TJsonObject; ctxt : TFHIRObjectList);  overload;
    function ParseInnerResource(jsn: TJsonObject) : TFhirResource; overload;
  end;

  TFHIRXmlComposerBase2 = class (TFHIRXmlComposerBase)
  protected
    procedure ComposeResourceV(xml : TXmlBuilder; resource : TFhirResourceV); override;
    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource); virtual;
  end;

  TFHIRJsonComposerBase2 = class (TFHIRJsonComposerBase)
  protected
    procedure ComposeResourceV(json : TJSONWriter; resource : TFhirResourceV); override;
    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource); virtual;
  end;

implementation

{ TFHIRXmlParserBase2 }

function TFHIRXmlParserBase2.ParseDataTypeV(element: TMXmlElement; name: String; type_: TClass): TFHIRObject;
begin
  result := ParseDataType(element, name, TFHIRTypeClass(type_));
end;

function TFHIRXmlParserBase2.ParseInnerResource(element: TMXmlElement; path: String): TFhirResource;
var
  child : TMXmlElement;
begin
  child := FirstChild(element);
  result := ParseResourceV(child, path) as TFhirResource;
  try
    child := NextSibling(child);
    if (child <> nil) then
      UnknownContent(child, path);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXmlParserBase2.ParseResource(element: TMXmlElement; path: String): TFhirResource;
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

function TFHIRXmlParserBase2.ParseResourceV(element: TMXmlElement; path: String): TFhirResourceV;
begin
  result := ParseResource(element, path);
end;

function TFHIRXmlParserBase2.ParseDataType(element: TMXmlElement; name: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;


{ TFHIRXmlComposerBase2 }


{ TFHIRXmlComposerBase2 }

procedure TFHIRXmlComposerBase2.ComposeResource(xml: TXmlBuilder; resource: TFhirResource);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRXmlComposerBase2.ComposeResourceV(xml: TXmlBuilder; resource: TFhirResourceV);
begin
  ComposeResource(xml, resource as TFhirResource);

end;

{ TFHIRJsonParserBase2 }

function TFHIRJsonParserBase2.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

function TFHIRJsonParserBase2.ParseResource(jsn: TJsonObject): TFhirResource;
begin
  raise exception.create('don''t use TFHIRJsonParserBase2 directly - use TFHIRXmlComposer');
end;

function TFHIRJsonParserBase2.ParseResourceV(jsn: TJsonObject): TFhirResourceV;
begin
  result := ParseResource(jsn);
end;

procedure TFHIRJsonParserBase2.ParseInnerResource(jsn: TJsonObject; ctxt: TFHIRObjectList);
begin
  ctxt.add(ParseResourceV(jsn));
end;

function TFHIRJsonParserBase2.ParseDataTypeV(jsn: TJsonObject; name: String; type_: TClass): TFHIRObject;
begin
  result := ParseDataType(jsn, name, TFHIRTypeClass(type_));
end;

function TFHIRJsonParserBase2.ParseInnerResource(jsn: TJsonObject) : TFhirResource;
begin
  result := ParseResourceV(jsn) as TFhirResource;
end;

{ TFHIRJsonComposerBase2 }

procedure TFHIRJsonComposerBase2.ComposeResource(json: TJSONWriter; resource: TFhirResource);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRJsonComposerBase2.ComposeResourceV(json: TJSONWriter; resource: TFhirResourceV);
begin
  ComposeResource(json, resource as TFhirResource);
end;

end.
