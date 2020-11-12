unit fhir5_parserBase;

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
  SysUtils,
  fsl_base, fsl_json, fsl_xml, fsl_turtle,
  fhir_objects,  fhir_parser, fhir5_types, fhir5_resources;

type
  TFHIRXmlParserBase5 = class (TFHIRXmlParserBase)
  protected
    function ParseResource(element : TMXmlElement; path : String) : TFhirResource; virtual;
    Function ParseResourceV(element : TMXmlElement; path : String) : TFhirResourceV; override;

    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; virtual;
    function ParseDataTypeV(element : TMXmlElement; rootName : String; type_ : TClass) : TFHIRObject; override;

    Function ParseInnerResource(element : TMXmlElement; path : String) : TFhirResource;
  end;

  TFHIRJsonParserBase5 = class (TFHIRJsonParserBase)
  protected
    function ParseResource(jsn : TJsonObject) : TFhirResource; virtual;
    Function ParseResourceV(jsn : TJsonObject) : TFhirResourceV; override;

    function ParseDataTypeV(jsn : TJsonObject; name : String; type_ : TClass) : TFHIRObject; override;
    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; virtual;

    procedure ParseInnerResource(jsn : TJsonObject; ctxt : TFHIRObjectList);  overload;
    function ParseInnerResource(jsn: TJsonObject) : TFhirResource; overload;
  end;

  TFHIRTurtleParserBase5 = class (TFHIRTurtleParserBase)
  protected
    function ParseResource(obj : TTurtleComplex) : TFhirResource; overload; virtual;
    Function ParseResourceV(obj : TTurtleComplex) : TFhirResourceV; override;

    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; virtual;

    function ParseInnerResource(obj : TTurtleObject) : TFHIRResource;
  end;

  TFHIRXmlComposerBase5 = class (TFHIRXmlComposerBase)
  protected
    procedure ComposeResourceV(xml : TXmlBuilder; resource : TFhirResourceV); override;
    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource); virtual;
  end;

  TFHIRJsonComposerBase5 = class (TFHIRJsonComposerBase)
  protected
    procedure ComposeResourceV(json : TJSONWriter; resource : TFhirResourceV); override;
    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource); virtual;
  end;

  TFHIRTurtleComposerBase5 = class (TFHIRTurtleComposerBase)
  protected
    procedure ComposeResourceV(parent :  TTurtleComplex; resource : TFhirResourceV); overload; override;
    procedure ComposeResource(parent :  TTurtleComplex; resource : TFhirResource); overload; virtual;
  end;

function hasSubsettedTag(list : TFHIRCodingList) : boolean;
function isSubsettedTag(c : TFHIRCoding) : boolean;

implementation

function hasSubsettedTag(list : TFHIRCodingList) : boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  for c in list do
    if isSubsettedTag(c) then
      exit(true);
end;

function isSubsettedTag(c : TFHIRCoding) : boolean;
begin
  result := (c.system = 'http://hl7.org/fhir/v3/ObservationValue') and (c.code = 'SUBSETTED');
end;


{ TFHIRXmlParserBase5 }

function TFHIRXmlParserBase5.ParseDataTypeV(element : TMXmlElement; rootName: String; type_: TClass): TFHIRObject;
begin
  result := ParseDataType(element, rootName, TFHIRDataTypeClass(type_));
end;

function TFHIRXmlParserBase5.ParseInnerResource(element: TMXmlElement; path: String): TFhirResource;
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

function TFHIRXmlParserBase5.ParseResource(element: TMXmlElement; path: String): TFhirResource;
begin
  raise EFHIRException.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

function TFHIRXmlParserBase5.ParseResourceV(element: TMXmlElement; path: String): TFhirResourceV;
begin
  result := ParseResource(element, path);
end;

function TFHIRXmlParserBase5.ParseDataType(element: TMXmlElement; name: String; type_: TFHIRDataTypeClass): TFHIRDataType;
begin
  raise EFHIRException.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;


{ TFHIRXmlComposerBase5 }


{ TFHIRXmlComposerBase5 }

procedure TFHIRXmlComposerBase5.ComposeResource(xml: TXmlBuilder; resource: TFhirResource);
begin
  raise EFHIRException.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRXmlComposerBase5.ComposeResourceV(xml: TXmlBuilder; resource: TFhirResourceV);
begin
  ComposeResource(xml, resource as TFhirResource);

end;

{ TFHIRJsonParserBase5 }

function TFHIRJsonParserBase5.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRDataTypeClass): TFHIRDataType;
begin
  raise EFHIRException.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

function TFHIRJsonParserBase5.ParseResource(jsn: TJsonObject): TFhirResource;
begin
  raise EFHIRException.create('don''t use TFHIRJsonParserBase5 directly - use TFHIRXmlComposer');
end;

function TFHIRJsonParserBase5.ParseResourceV(jsn: TJsonObject): TFhirResourceV;
begin
  result := ParseResource(jsn);
end;

procedure TFHIRJsonParserBase5.ParseInnerResource(jsn: TJsonObject; ctxt: TFHIRObjectList);
begin
  ctxt.add(ParseResourceV(jsn));
end;

function TFHIRJsonParserBase5.ParseDataTypeV(jsn: TJsonObject; name: String; type_: TClass): TFHIRObject;
begin
  result := ParseDataType(jsn, name, TFHIRDataTypeClass(type_));
end;

function TFHIRJsonParserBase5.ParseInnerResource(jsn: TJsonObject) : TFhirResource;
begin
  result := ParseResourceV(jsn) as TFhirResource;
end;

{ TFHIRJsonComposerBase5 }

procedure TFHIRJsonComposerBase5.ComposeResource(json: TJSONWriter; resource: TFhirResource);
begin
  raise EFHIRException.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRJsonComposerBase5.ComposeResourceV(json: TJSONWriter; resource: TFhirResourceV);
begin
  ComposeResource(json, resource as TFhirResource);
end;

{ TFHIRTurtleParserBase5 }

function TFHIRTurtleParserBase5.ParseDataType(obj: TTurtleComplex; name: String; type_: TFHIRDataTypeClass): TFHIRDataType;
begin
  raise EFHIRException.create('don''t use TFHIRTurtleParserBase5 directly - use TFHIRXmlParser');
end;

function TFHIRTurtleParserBase5.ParseInnerResource(obj: TTurtleObject): TFHIRResource;
var
  c : TTurtleComplex;
begin
  if obj = nil then
    result := nil
  else
  begin
    if obj is TTurtleComplex then
      c := obj as TTurtleComplex
    else if (obj is TTurtleURL) then
    begin
      c := FDoc.getObject(TTurtleURL(obj).uri);
      if c = nil then
        raise ERdfException.create('Unable to resolve internal resource reference in RDF - to '+TTurtleURL(obj).uri)
    end
    else
      raise ERdfException.create('Unable to process internal resource reference in RDF');
    result := ParseResourceV(c) as TFHIRResource;
  end;
end;

function TFHIRTurtleParserBase5.ParseResource(obj: TTurtleComplex): TFhirResource;
begin
  raise ERdfException.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

function TFHIRTurtleParserBase5.ParseResourceV(obj: TTurtleComplex): TFhirResourceV;
begin
  result := ParseResource(obj);
end;

{ TFHIRTurtleComposerBase5 }

procedure TFHIRTurtleComposerBase5.ComposeResource(parent: TTurtleComplex; resource: TFhirResource);
begin
  raise ERdfException.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRTurtleComposerBase5.ComposeResourceV(parent: TTurtleComplex; resource: TFhirResourceV);
begin
  ComposeResource(parent, resource as TFhirResource);
end;

end.
