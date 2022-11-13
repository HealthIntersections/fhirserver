unit fhir5_json;

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

{$i fhir.inc}
{$i fhir5.inc}

interface

{{mark}}

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_collections, fsl_json,
  fhir_parser, fhir_objects,
  fhir5_base, fhir5_constants, fhir5_enums, fhir5_types, fhir5_parserBase, fhir5_resources, fhir5_resources_base;

Type

  TFHIRJsonParser = class (TFHIRJsonParserBase5)
  private
    procedure ParseBaseProperties(jsn : TJsonObject; value : TFhirBase); overload;
    procedure ParseBaseProperties(jsn : TJsonObject; value : TFhirResource); overload;
  
  protected
{{abstract.types.intf.parser}}

    procedure ParseEnum(path :String; value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList; Const aNames, aSystems : Array Of String); overload;
    function ParseEnum(path : String; value : TJsonNode; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum; overload;
    procedure ParseDate(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseDate(value : TJsonNode; jsn : TJsonObject) : TFHIRDate; overload;
    procedure ParseDateTime(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseDateTime(value : TJsonNode; jsn : TJsonObject) : TFHIRDateTime; overload;
    procedure ParseString(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseString(value : TJsonNode; jsn : TJsonObject) : TFHIRString; overload;
    procedure ParseInteger(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseInteger(value : TJsonNode; jsn : TJsonObject) : TFHIRInteger; overload;
    procedure ParseUri(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseUri(value : TJsonNode; jsn : TJsonObject) : TFHIRUri; overload;
    procedure ParseInstant(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseInstant(value : TJsonNode; jsn : TJsonObject) : TFHIRInstant; overload;
    procedure ParseXhtml(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseXhtml(value : TJsonNode; jsn : TJsonObject) : TFHIRXhtml; overload;
    procedure ParseBoolean(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseBoolean(value : TJsonNode; jsn : TJsonObject) : TFHIRBoolean; overload;
    procedure ParseBase64Binary(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseBase64Binary(value : TJsonNode; jsn : TJsonObject) : TFHIRBase64Binary; overload;
    procedure ParseTime(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseTime(value : TJsonNode; jsn : TJsonObject) : TFHIRTime; overload;
    procedure ParseDecimal(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseDecimal(value : TJsonNode; jsn : TJsonObject) : TFHIRDecimal; overload;
    procedure ParseCode(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseCode(value : TJsonNode; jsn : TJsonObject) : TFHIRCode; overload;
    procedure ParseCanonical(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseCanonical(value : TJsonNode; jsn : TJsonObject) : TFHIRCanonical; overload;
    procedure ParseOid(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseOid(value : TJsonNode; jsn : TJsonObject) : TFHIROid; overload;
    procedure ParseUuid(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseUuid(value : TJsonNode; jsn : TJsonObject) : TFHIRUuid; overload;
    procedure ParseUrl(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseUrl(value : TJsonNode; jsn : TJsonObject) : TFHIRUrl; overload;
    procedure ParseMarkdown(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseMarkdown(value : TJsonNode; jsn : TJsonObject) : TFHIRMarkdown; overload;
    procedure ParseUnsignedInt(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseUnsignedInt(value : TJsonNode; jsn : TJsonObject) : TFHIRUnsignedInt; overload;
    procedure ParseId(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseId(value : TJsonNode; jsn : TJsonObject) : TFHIRId; overload;
    procedure ParsePositiveInt(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParsePositiveInt(value : TJsonNode; jsn : TJsonObject) : TFHIRPositiveInt; overload;
    procedure ParseInteger64(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList); overload;
    function ParseInteger64(value : TJsonNode; jsn : TJsonObject) : TFHIRInteger64; overload;

{{concrete.types.intf.parser}}

{{resources.intf.parser}}

    function ParseResource(jsn : TJsonObject) : TFhirResource; override;
    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; override;
  public
    function ParseFragment(jsn : TJsonObject; type_ : String) : TFHIRObject;  overload;
  end;

  TFHIRJsonComposer = class (TFHIRJsonComposerBase5)
  private
    Procedure ComposeBaseProperties(json : TJSONWriter; value : TFhirBase); overload;
    Procedure ComposeBaseProperties(json : TJSONWriter; value : TFhirResource); overload;

  protected
{{abstract.types.intf.composer}}

    Procedure ComposeEnumValue(json : TJSONWriter; name : String; value : TFhirEnum; Const aNames : Array Of String; inArray : boolean);
    Procedure ComposeEnumProps(json : TJSONWriter; name : String; value : TFhirEnum; Const aNames : Array Of String; inArray : boolean);
    Procedure ComposeDateValue(json : TJSONWriter; name : String; value : TFhirDate; inArray : boolean);
    Procedure ComposeDateProps(json : TJSONWriter; name : String; value : TFhirDate; inArray : boolean);
    Procedure ComposeDateTimeValue(json : TJSONWriter; name : String; value : TFhirDateTime; inArray : boolean);
    Procedure ComposeDateTimeProps(json : TJSONWriter; name : String; value : TFhirDateTime; inArray : boolean);
    Procedure ComposeStringValue(json : TJSONWriter; name : String; value : TFhirString; inArray : boolean);
    Procedure ComposeStringProps(json : TJSONWriter; name : String; value : TFhirString; inArray : boolean);
    Procedure ComposeIntegerValue(json : TJSONWriter; name : String; value : TFhirInteger; inArray : boolean);
    Procedure ComposeIntegerProps(json : TJSONWriter; name : String; value : TFhirInteger; inArray : boolean);
    Procedure ComposeUriValue(json : TJSONWriter; name : String; value : TFhirUri; inArray : boolean);
    Procedure ComposeUriProps(json : TJSONWriter; name : String; value : TFhirUri; inArray : boolean);
    Procedure ComposeInstantValue(json : TJSONWriter; name : String; value : TFhirInstant; inArray : boolean);
    Procedure ComposeInstantProps(json : TJSONWriter; name : String; value : TFhirInstant; inArray : boolean);
    Procedure ComposeXhtmlValue(json : TJSONWriter; name : String; value : TFhirXhtml; inArray : boolean);
    Procedure ComposeXhtmlProps(json : TJSONWriter; name : String; value : TFhirXhtml; inArray : boolean);
    Procedure ComposeBooleanValue(json : TJSONWriter; name : String; value : TFhirBoolean; inArray : boolean);
    Procedure ComposeBooleanProps(json : TJSONWriter; name : String; value : TFhirBoolean; inArray : boolean);
    Procedure ComposeBase64BinaryValue(json : TJSONWriter; name : String; value : TFhirBase64Binary; inArray : boolean);
    Procedure ComposeBase64BinaryProps(json : TJSONWriter; name : String; value : TFhirBase64Binary; inArray : boolean);
    Procedure ComposeTimeValue(json : TJSONWriter; name : String; value : TFhirTime; inArray : boolean);
    Procedure ComposeTimeProps(json : TJSONWriter; name : String; value : TFhirTime; inArray : boolean);
    Procedure ComposeDecimalValue(json : TJSONWriter; name : String; value : TFhirDecimal; inArray : boolean);
    Procedure ComposeDecimalProps(json : TJSONWriter; name : String; value : TFhirDecimal; inArray : boolean);
    Procedure ComposeCodeValue(json : TJSONWriter; name : String; value : TFhirCode; inArray : boolean);
    Procedure ComposeCodeProps(json : TJSONWriter; name : String; value : TFhirCode; inArray : boolean);
    Procedure ComposeCanonicalValue(json : TJSONWriter; name : String; value : TFhirCanonical; inArray : boolean);
    Procedure ComposeCanonicalProps(json : TJSONWriter; name : String; value : TFhirCanonical; inArray : boolean);
    Procedure ComposeOidValue(json : TJSONWriter; name : String; value : TFhirOid; inArray : boolean);
    Procedure ComposeOidProps(json : TJSONWriter; name : String; value : TFhirOid; inArray : boolean);
    Procedure ComposeUuidValue(json : TJSONWriter; name : String; value : TFhirUuid; inArray : boolean);
    Procedure ComposeUuidProps(json : TJSONWriter; name : String; value : TFhirUuid; inArray : boolean);
    Procedure ComposeUrlValue(json : TJSONWriter; name : String; value : TFhirUrl; inArray : boolean);
    Procedure ComposeUrlProps(json : TJSONWriter; name : String; value : TFhirUrl; inArray : boolean);
    Procedure ComposeMarkdownValue(json : TJSONWriter; name : String; value : TFhirMarkdown; inArray : boolean);
    Procedure ComposeMarkdownProps(json : TJSONWriter; name : String; value : TFhirMarkdown; inArray : boolean);
    Procedure ComposeUnsignedIntValue(json : TJSONWriter; name : String; value : TFhirUnsignedInt; inArray : boolean);
    Procedure ComposeUnsignedIntProps(json : TJSONWriter; name : String; value : TFhirUnsignedInt; inArray : boolean);
    Procedure ComposeIdValue(json : TJSONWriter; name : String; value : TFhirId; inArray : boolean);
    Procedure ComposeIdProps(json : TJSONWriter; name : String; value : TFhirId; inArray : boolean);
    Procedure ComposePositiveIntValue(json : TJSONWriter; name : String; value : TFhirPositiveInt; inArray : boolean);
    Procedure ComposePositiveIntProps(json : TJSONWriter; name : String; value : TFhirPositiveInt; inArray : boolean);
    Procedure ComposeInteger64Value(json : TJSONWriter; name : String; value : TFhirInteger64; inArray : boolean);
    Procedure ComposeInteger64Props(json : TJSONWriter; name : String; value : TFhirInteger64; inArray : boolean);

{{concrete.types.intf.composer}}

{{resources.intf.composer}}

  public
    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource); override;
    procedure ComposeBase(json : TJSONWriter; name : String; base : TFHIRObject); override;
  end;


implementation

{ TFHIRJsonParser }

procedure TFHIRJsonParser.ParseBaseProperties(jsn : TJsonObject; value : TFhirBase); 
begin
  parseComments(value, jsn);
  if KeepParseLocations then
  begin
    value.LocationData.ParseStart := jsn.LocationStart;
    value.LocationData.ParseFinish := jsn.LocationEnd;
  end;
end;

procedure TFHIRJsonParser.ParseBaseProperties(jsn : TJsonObject; value : TFhirResource);
begin
  parseComments(value, jsn);
  if KeepParseLocations then
  begin
    value.LocationData.ParseStart := jsn.LocationStart;
    value.LocationData.ParseFinish := jsn.LocationEnd;
  end;
end;

Procedure TFHIRJsonComposer.ComposeBaseProperties(json : TJSONWriter; value : TFhirBase); 
begin
  {no-comments composeComments(json, elem);}
end;

Procedure TFHIRJsonComposer.ComposeBaseProperties(json : TJSONWriter; value : TFhirResource); 
begin
  {no-comments composeComments(json, elem);}
end;

procedure TFHIRJsonParser.ParseElementProperties(jsn : TJsonObject; value : TFhirElement);
begin
  ParseBaseProperties(jsn, value); {jp2}
  if jsn.has('id') or jsn.has('_id') then
    value.idElement := parseString(jsn.node['id'], jsn.vObj['_id']); {L749}
  if jsn.has('extension') then
    iterateArray(jsn.vArr['extension'], value.extensionList, parseExtension);
end;

Procedure TFHIRJsonComposer.ComposeElementProperties(json : TJSONWriter; value : TFhirElement);
var
  i : integer {L426};
begin
  ComposeBaseProperties(json, value);  {L430}
  if (SummaryOption in [soFull, soSummary, soText, soData]) then
    ComposeStringValue(json, 'id', value.idElement, false); {L769}
  if (SummaryOption in [soFull, soSummary, soText, soData]) then
    ComposeStringProps(json, 'id', value.idElement, false); {L770}
  if (SummaryOption in [soFull, soData]) and (value.extensionList.Count > 0) then
  begin
    startArray(json, 'extension', value.extensionList, false);
    for i := 0 to value.extensionList.Count - 1 do
      ComposeExtension(json, '', value.extensionList[i]); {L682}
    finishArray(json, value.extensionList);
  end;
end;

procedure TFHIRJsonParser.ParseBackboneElementProperties(jsn : TJsonObject; value : TFhirBackboneElement);
begin
  ParseElementProperties(jsn, value); {jp2}
  if jsn.has('modifierExtension') then
    iterateArray(jsn.vArr['modifierExtension'], value.modifierExtensionList, parseExtension);
end;

Procedure TFHIRJsonComposer.ComposeBackboneElementProperties(json : TJSONWriter; value : TFhirBackboneElement);
var
  i : integer {L426};
begin
  ComposeElementProperties(json, value);  {L430}
  if (SummaryOption in [soFull, soSummary, soData]) and (value.modifierExtensionList.Count > 0) then
  begin
    startArray(json, 'modifierExtension', value.modifierExtensionList, false);
    for i := 0 to value.modifierExtensionList.Count - 1 do
      ComposeExtension(json, '', value.modifierExtensionList[i]); {L682}
    finishArray(json, value.modifierExtensionList);
  end;
end;

procedure TFHIRJsonParser.ParseDataTypeProperties(jsn : TJsonObject; value : TFhirDataType);
begin
  ParseElementProperties(jsn, value); {jp2}
end;

Procedure TFHIRJsonComposer.ComposeDataTypeProperties(json : TJSONWriter; value : TFhirDataType);
begin
  ComposeElementProperties(json, value);  {L430}
end;

procedure TFHIRJsonParser.ParseBackboneTypeProperties(jsn : TJsonObject; value : TFhirBackboneType);
begin
  ParseDataTypeProperties(jsn, value); {jp2}
  if jsn.has('modifierExtension') then
    iterateArray(jsn.vArr['modifierExtension'], value.modifierExtensionList, parseExtension);
end;

Procedure TFHIRJsonComposer.ComposeBackboneTypeProperties(json : TJSONWriter; value : TFhirBackboneType);
var
  i : integer {L426};
begin
  ComposeDataTypeProperties(json, value);  {L430}
  if (SummaryOption in [soFull, soSummary, soData]) and (value.modifierExtensionList.Count > 0) then
  begin
    startArray(json, 'modifierExtension', value.modifierExtensionList, false);
    for i := 0 to value.modifierExtensionList.Count - 1 do
      ComposeExtension(json, '', value.modifierExtensionList[i]); {L682}
    finishArray(json, value.modifierExtensionList);
  end;
end;

procedure TFHIRJsonParser.ParsePrimitiveTypeProperties(jsn : TJsonObject; value : TFhirPrimitiveType);
begin
  ParseDataTypeProperties(jsn, value); {jp2}
end;

Procedure TFHIRJsonComposer.ComposePrimitiveTypeProperties(json : TJSONWriter; value : TFhirPrimitiveType);
begin
  ComposeDataTypeProperties(json, value);  {L430}
end;

procedure TFHIRJsonParser.ParseEnum(path : String; value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList; Const aNames, aSystems : Array Of String);
begin
  ctxt.add(ParseEnum(path, value, jsn, aNames, aSystems));
end;

function TFHIRJsonParser.ParseEnum(path : String; value : TJsonNode; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(aNames, JsonToString(value));
  if (value <> nil) and (i < 0) then
    raise value.LocationStart.exception('unknown code: '+JsonToString(value)+' from a set of choices of '+StringArrayToCommaString(aNames)+' for "'+path+'"');
  result := TFHIREnum.create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    result.system := aSystems[i];
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeEnumValue(json : TJSONWriter; name : String; value : TFhirEnum; Const aNames : Array Of String; inArray : boolean);
begin
  if (value = nil) or (value.Value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeEnumProps(json : TJSONWriter; name : String; value : TFhirEnum; Const aNames : Array Of String; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments) }) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseDate(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseDate(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseDate(value : TJsonNode; jsn : TJsonObject) : TFHIRDate;
begin
  result := TFhirDate.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
     result.value := toTFslDateTime(JsonToString(value));
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDateValue(json : TJSONWriter; name : String; value : TFhirDate; inArray : boolean);
begin
  if (value = nil) or (value.value.null) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, asString(value.value));
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDateProps(json : TJSONWriter; name : String; value : TFhirDate; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseDateTime(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseDateTime(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseDateTime(value : TJsonNode; jsn : TJsonObject) : TFHIRDateTime;
begin
  result := TFhirDateTime.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
     result.value := toTFslDateTime(JsonToString(value));
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDateTimeValue(json : TJSONWriter; name : String; value : TFhirDateTime; inArray : boolean);
begin
  if (value = nil) or (value.value.null) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, asString(value.value));
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDateTimeProps(json : TJSONWriter; name : String; value : TFhirDateTime; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseString(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseString(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseString(value : TJsonNode; jsn : TJsonObject) : TFHIRString;
begin
  result := TFhirString.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeStringValue(json : TJSONWriter; name : String; value : TFhirString; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeStringProps(json : TJSONWriter; name : String; value : TFhirString; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseInteger(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseInteger(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseInteger(value : TJsonNode; jsn : TJsonObject) : TFHIRInteger;
begin
  result := TFhirInteger.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeIntegerValue(json : TJSONWriter; name : String; value : TFhirInteger; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    propNumber(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeIntegerProps(json : TJSONWriter; name : String; value : TFhirInteger; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseUri(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseUri(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseUri(value : TJsonNode; jsn : TJsonObject) : TFHIRUri;
begin
  result := TFhirUri.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUriValue(json : TJSONWriter; name : String; value : TFhirUri; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUriProps(json : TJSONWriter; name : String; value : TFhirUri; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseInstant(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseInstant(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseInstant(value : TJsonNode; jsn : TJsonObject) : TFHIRInstant;
begin
  result := TFhirInstant.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
     result.value := toTFslDateTime(JsonToString(value));
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeInstantValue(json : TJSONWriter; name : String; value : TFhirInstant; inArray : boolean);
begin
  if (value = nil) or (value.value.null) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, asString(value.value));
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeInstantProps(json : TJSONWriter; name : String; value : TFhirInstant; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseXhtml(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseXhtml(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseXhtml(value : TJsonNode; jsn : TJsonObject) : TFHIRXhtml;
begin
  result := TFhirXhtml.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeXhtmlValue(json : TJSONWriter; name : String; value : TFhirXhtml; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeXhtmlProps(json : TJSONWriter; name : String; value : TFhirXhtml; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseBoolean(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseBoolean(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseBoolean(value : TJsonNode; jsn : TJsonObject) : TFHIRBoolean;
begin
  result := TFhirBoolean.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := StringToBoolean(JsonToString(value));
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeBooleanValue(json : TJSONWriter; name : String; value : TFhirBoolean; inArray : boolean);
begin
  if (value = nil) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeBooleanProps(json : TJSONWriter; name : String; value : TFhirBoolean; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseBase64Binary(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseBase64Binary(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseBase64Binary(value : TJsonNode; jsn : TJsonObject) : TFHIRBase64Binary;
begin
  result := TFhirBase64Binary.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
     result.value := toTBytes(JsonToString(value));
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeBase64BinaryValue(json : TJSONWriter; name : String; value : TFhirBase64Binary; inArray : boolean);
begin
  if (value = nil) or (value.value = nil) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, asString(value.value));
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeBase64BinaryProps(json : TJSONWriter; name : String; value : TFhirBase64Binary; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseTime(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseTime(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseTime(value : TJsonNode; jsn : TJsonObject) : TFHIRTime;
begin
  result := TFhirTime.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeTimeValue(json : TJSONWriter; name : String; value : TFhirTime; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeTimeProps(json : TJSONWriter; name : String; value : TFhirTime; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseDecimal(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseDecimal(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseDecimal(value : TJsonNode; jsn : TJsonObject) : TFHIRDecimal;
begin
  result := TFhirDecimal.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDecimalValue(json : TJSONWriter; name : String; value : TFhirDecimal; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    propNumber(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeDecimalProps(json : TJSONWriter; name : String; value : TFhirDecimal; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseCode(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseCode(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseCode(value : TJsonNode; jsn : TJsonObject) : TFHIRCode;
begin
  result := TFhirCode.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeCodeValue(json : TJSONWriter; name : String; value : TFhirCode; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeCodeProps(json : TJSONWriter; name : String; value : TFhirCode; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseCanonical(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseCanonical(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseCanonical(value : TJsonNode; jsn : TJsonObject) : TFHIRCanonical;
begin
  result := TFhirCanonical.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeCanonicalValue(json : TJSONWriter; name : String; value : TFhirCanonical; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeCanonicalProps(json : TJSONWriter; name : String; value : TFhirCanonical; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseOid(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseOid(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseOid(value : TJsonNode; jsn : TJsonObject) : TFHIROid;
begin
  result := TFhirOid.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeOidValue(json : TJSONWriter; name : String; value : TFhirOid; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeOidProps(json : TJSONWriter; name : String; value : TFhirOid; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseUuid(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseUuid(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseUuid(value : TJsonNode; jsn : TJsonObject) : TFHIRUuid;
begin
  result := TFhirUuid.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUuidValue(json : TJSONWriter; name : String; value : TFhirUuid; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUuidProps(json : TJSONWriter; name : String; value : TFhirUuid; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseUrl(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseUrl(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseUrl(value : TJsonNode; jsn : TJsonObject) : TFHIRUrl;
begin
  result := TFhirUrl.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUrlValue(json : TJSONWriter; name : String; value : TFhirUrl; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUrlProps(json : TJSONWriter; name : String; value : TFhirUrl; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseMarkdown(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseMarkdown(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseMarkdown(value : TJsonNode; jsn : TJsonObject) : TFHIRMarkdown;
begin
  result := TFhirMarkdown.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeMarkdownValue(json : TJSONWriter; name : String; value : TFhirMarkdown; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeMarkdownProps(json : TJSONWriter; name : String; value : TFhirMarkdown; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseUnsignedInt(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseUnsignedInt(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseUnsignedInt(value : TJsonNode; jsn : TJsonObject) : TFHIRUnsignedInt;
begin
  result := TFhirUnsignedInt.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUnsignedIntValue(json : TJSONWriter; name : String; value : TFhirUnsignedInt; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    propNumber(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeUnsignedIntProps(json : TJSONWriter; name : String; value : TFhirUnsignedInt; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseId(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseId(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseId(value : TJsonNode; jsn : TJsonObject) : TFHIRId;
begin
  result := TFhirId.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeIdValue(json : TJSONWriter; name : String; value : TFhirId; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    prop(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeIdProps(json : TJSONWriter; name : String; value : TFhirId; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParsePositiveInt(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParsePositiveInt(value, jsn)) {1};
end;

function TFHIRJsonParser.ParsePositiveInt(value : TJsonNode; jsn : TJsonObject) : TFHIRPositiveInt;
begin
  result := TFhirPositiveInt.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposePositiveIntValue(json : TJSONWriter; name : String; value : TFhirPositiveInt; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    propNumber(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposePositiveIntProps(json : TJSONWriter; name : String; value : TFhirPositiveInt; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;

procedure TFHIRJsonParser.ParseInteger64(value : TJsonNode; jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseInteger64(value, jsn)) {1};
end;

function TFHIRJsonParser.ParseInteger64(value : TJsonNode; jsn : TJsonObject) : TFHIRInteger64;
begin
  result := TFhirInteger64.Create;
  try
    if (value <> nil) then
    begin
      if KeepParseLocations then
      begin
        result.LocationData.ParseStart2 := value.LocationStart;
        result.LocationData.ParseFinish2 := value.LocationEnd;
      end;
    end;
    result.value := JsonToString(value);
    if (jsn <> nil) then
      parseElementProperties(jsn, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRJsonComposer.ComposeInteger64Value(json : TJSONWriter; name : String; value : TFhirInteger64; inArray : boolean);
begin
  if (value = nil) or (value.value = '') then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart2 := json.SourceLocation;
    propNumber(json, name, value.value);
    if KeepLocationData then value.LocationData.ComposeFinish2 := json.SourceLocation;
  end;
end;

Procedure TFHIRJsonComposer.ComposeInteger64Props(json : TJSONWriter; name : String; value : TFhirInteger64; inArray : boolean);
begin
  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then
  begin
    if inArray then
    begin
      if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
      propNull(json, name);
      if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
    end;
    exit;
  end
  else
  begin
    if KeepLocationData then value.LocationData.ComposeStart := json.SourceLocation;
    if (inArray) then
      json.valueObject('')
    else
      json.valueObject('_'+name);
    ComposeElementProperties(json, value);
    json.finishObject;
    if KeepLocationData then value.LocationData.ComposeFinish := json.SourceLocation;
  end;
end;


{{concrete.types.impl}}
{{resources.impl}}

function TFHIRJsonParser.ParseResource(jsn : TJsonObject) : TFhirResource;
var
  s : String;
begin
  s := jsn['resourceType'];
  if (s = '') then
    raise jsn.locationStart.exception('The JSON Object has no resourceType property')
{{resources.parse}}
  else
    raise jsn.locationStart.exception('The element '+s+' is not a valid resource name');
end;

function TFHIRJsonParser.ParseFragment(jsn : TJsonObject; type_ : String) : TFHIRObject;
begin
  if (type_ = '') then
    raise EJsonException.create('no type provided')
{{fragments.parse}}
  else
    raise EJsonException.create('error: the element '+type_+' is not a valid fragment name');
end;

function TFHIRJsonParser.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;
begin
  if (type_ = nil) then
    raise EJsonException.create('no type provided')
{{types.parse}}
  else
    raise EJsonException.create('Unknown Type');
end;

procedure TFHIRJsonComposer.ComposeBase(json: TJSONWriter; name: String; base: TFHIRObject);
begin
  if (base is TFhirDate) then
    composeDateValue(json, name, TFhirDate(base), false)
  else if (base is TFhirDateTime) then
    composeDateTimeValue(json, name, TFhirDateTime(base), false)
  else if (base is TFhirString) then
    composeStringValue(json, name, TFhirString(base), false)
  else if (base is TFhirInteger) then
    composeIntegerValue(json, name, TFhirInteger(base), false)
  else if (base is TFhirUri) then
    composeUriValue(json, name, TFhirUri(base), false)
  else if (base is TFhirInstant) then
    composeInstantValue(json, name, TFhirInstant(base), false)
  else if (base is TFhirXhtml) then
    composeXhtmlValue(json, name, TFhirXhtml(base), false)
  else if (base is TFhirBoolean) then
    composeBooleanValue(json, name, TFhirBoolean(base), false)
  else if (base is TFhirBase64Binary) then
    composeBase64BinaryValue(json, name, TFhirBase64Binary(base), false)
  else if (base is TFhirTime) then
    composeTimeValue(json, name, TFhirTime(base), false)
  else if (base is TFhirDecimal) then
    composeDecimalValue(json, name, TFhirDecimal(base), false)
  else if (base is TFhirCode) then
    composeCodeValue(json, name, TFhirCode(base), false)
  else if (base is TFhirCanonical) then
    composeCanonicalValue(json, name, TFhirCanonical(base), false)
  else if (base is TFhirOid) then
    composeOidValue(json, name, TFhirOid(base), false)
  else if (base is TFhirUuid) then
    composeUuidValue(json, name, TFhirUuid(base), false)
  else if (base is TFhirUrl) then
    composeUrlValue(json, name, TFhirUrl(base), false)
  else if (base is TFhirMarkdown) then
    composeMarkdownValue(json, name, TFhirMarkdown(base), false)
  else if (base is TFhirUnsignedInt) then
    composeUnsignedIntValue(json, name, TFhirUnsignedInt(base), false)
  else if (base is TFhirId) then
    composeIdValue(json, name, TFhirId(base), false)
  else if (base is TFhirPositiveInt) then
    composePositiveIntValue(json, name, TFhirPositiveInt(base), false)
{{compose.all}}  
  else
    inherited ComposeBase(json, name, base);
end;

procedure TFHIRJsonComposer.ComposeResource(json : TJSONWriter; resource: TFhirResource);
begin
  if (resource = nil) Then
    Raise EJsonException.Create('error - resource is nil');
  json.value('resourceType', CODES_TFhirResourceType[resource.ResourceType]);
  Case resource.ResourceType of
{{compose.resource}}  
  else
    raise EJsonException.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');
  end;
end;

end.

