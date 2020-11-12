unit fhir5_xml;

{$I fhir5.inc}

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

interface

{{mark}}

uses
  SysUtils, Classes, 
  fsl_base, fsl_utilities, fsl_collections, fsl_xml, fsl_xml, 
  fhir_parser, fhir_objects, 
  fhir5_parserBase, fhir5_resources, fhir5_constants, fhir5_base, fhir5_enums, fhir5_types;

Type

  TFHIRXmlParser = class (TFHIRXmlParserBase5)
  protected
    Procedure ParseBaseAttributes(value : TFhirBase; path : string; element : TMXmlElement); overload;
    Function ParseBaseChild(value : TFhirBase; path : string; child : TMXmlElement) : boolean;  overload;
    Procedure ParseBaseAttributes(value : TFhirResource; path : string; element : TMXmlElement);  overload;
    Function ParseBaseChild(value : TFhirResource; path : string; child : TMXmlElement) : boolean;  overload;

{{types.abstract.parser.intf}}

    function ParseEnum(Const aNames, aSystems : Array Of String; element : TMXmlElement; path : String) : TFhirEnum;
    function ParseDate(element : TMXmlElement; path : string) : TFhirDate;
    function ParseDateTime(element : TMXmlElement; path : string) : TFhirDateTime;
    function ParseString(element : TMXmlElement; path : string) : TFhirString;
    function ParseInteger(element : TMXmlElement; path : string) : TFhirInteger;
    function ParseUri(element : TMXmlElement; path : string) : TFhirUri;
    function ParseInstant(element : TMXmlElement; path : string) : TFhirInstant;
    function ParseXhtml(element : TMXmlElement; path : string) : TFhirXhtml;
    function ParseBoolean(element : TMXmlElement; path : string) : TFhirBoolean;
    function ParseBase64Binary(element : TMXmlElement; path : string) : TFhirBase64Binary;
    function ParseTime(element : TMXmlElement; path : string) : TFhirTime;
    function ParseDecimal(element : TMXmlElement; path : string) : TFhirDecimal;
    function ParseCode(element : TMXmlElement; path : string) : TFhirCode;
    function ParseCanonical(element : TMXmlElement; path : string) : TFhirCanonical;
    function ParseOid(element : TMXmlElement; path : string) : TFhirOid;
    function ParseUuid(element : TMXmlElement; path : string) : TFhirUuid;
    function ParseUrl(element : TMXmlElement; path : string) : TFhirUrl;
    function ParseMarkdown(element : TMXmlElement; path : string) : TFhirMarkdown;
    function ParseUnsignedInt(element : TMXmlElement; path : string) : TFhirUnsignedInt;
    function ParseId(element : TMXmlElement; path : string) : TFhirId;
    function ParsePositiveInt(element : TMXmlElement; path : string) : TFhirPositiveInt;
    function ParseInteger64(element : TMXmlElement; path : string) : TFhirInteger64;

{{types.concrete.parser.intf}}

{{resources.abstract.parser.intf}}

{{resources.concrete.parser.intf}}

    function ParseResource(element : TMXmlElement; path : String) : TFhirResource; override;
    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; override;
  public
    function ParseFragment(element : TMXmlElement) : TFHIRObject; overload;
  end;

  TFHIRXmlComposer = class (TFHIRXmlComposerBase5)
  protected
    Procedure ComposeBaseAttributes(xml : TXmlBuilder; res : TFhirBase); overload;
    Procedure ComposeBaseChildren(xml : TXmlBuilder; value : TFhirBase); overload;
    Procedure ComposeBaseAttributes(xml : TXmlBuilder; res : TFhirResource); overload;
    Procedure ComposeBaseChildren(xml : TXmlBuilder; value : TFhirResource); overload;

{{types.abstract.composer.intf}}

    Procedure ComposeEnum(xml : TXmlBuilder; name : String; value : TFhirEnum; Const aNames : Array Of String);
    Procedure ComposeDate(xml : TXmlBuilder; name : String; value : TFhirDate);
    Procedure ComposeDateTime(xml : TXmlBuilder; name : String; value : TFhirDateTime);
    Procedure ComposeString(xml : TXmlBuilder; name : String; value : TFhirString);
    Procedure ComposeInteger(xml : TXmlBuilder; name : String; value : TFhirInteger);
    Procedure ComposeUri(xml : TXmlBuilder; name : String; value : TFhirUri);
    Procedure ComposeInstant(xml : TXmlBuilder; name : String; value : TFhirInstant);
    Procedure ComposeXhtml(xml : TXmlBuilder; name : String; value : TFhirXhtml);
    Procedure ComposeBoolean(xml : TXmlBuilder; name : String; value : TFhirBoolean);
    Procedure ComposeBase64Binary(xml : TXmlBuilder; name : String; value : TFhirBase64Binary);
    Procedure ComposeTime(xml : TXmlBuilder; name : String; value : TFhirTime);
    Procedure ComposeDecimal(xml : TXmlBuilder; name : String; value : TFhirDecimal);
    Procedure ComposeCode(xml : TXmlBuilder; name : String; value : TFhirCode);
    Procedure ComposeCanonical(xml : TXmlBuilder; name : String; value : TFhirCanonical);
    Procedure ComposeOid(xml : TXmlBuilder; name : String; value : TFhirOid);
    Procedure ComposeUuid(xml : TXmlBuilder; name : String; value : TFhirUuid);
    Procedure ComposeUrl(xml : TXmlBuilder; name : String; value : TFhirUrl);
    Procedure ComposeMarkdown(xml : TXmlBuilder; name : String; value : TFhirMarkdown);
    Procedure ComposeUnsignedInt(xml : TXmlBuilder; name : String; value : TFhirUnsignedInt);
    Procedure ComposeId(xml : TXmlBuilder; name : String; value : TFhirId);
    Procedure ComposePositiveInt(xml : TXmlBuilder; name : String; value : TFhirPositiveInt);
    Procedure ComposeInteger64(xml : TXmlBuilder; name : String; value : TFhirInteger64);

{{types.concrete.composer.intf}}

{{resources.abstract.composer.intf}}

{{resources.concrete.composer.intf}}

  public
    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource); override;
    procedure ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject); override;
  end;


implementation

{ TFHIRXmlParser / TFHIRXmlComposer }

Procedure TFHIRXmlParser.ParseBaseAttributes(value : TFhirBase; path : string; element : TMXmlElement); 
begin
  TakeCommentsStart(value);
  GetObjectLocation(value, element);
end;

Procedure TFHIRXmlParser.ParseBaseAttributes(value : TFhirResource; path : string; element : TMXmlElement);  
begin
  TakeCommentsStart(value);
  GetObjectLocation(value, element);
end;

Function TFHIRXmlParser.ParseBaseChild(value : TFhirBase; path : string; child : TMXmlElement) : boolean;  
begin
  result := false;
end;

Function TFHIRXmlParser.ParseBaseChild(value : TFhirResource; path : string; child : TMXmlElement) : boolean;  
begin
  result := false;
end;

Procedure TFHIRXmlComposer.ComposeBaseAttributes(xml : TXmlBuilder; res : TFhirBase); 
begin
  CommentsStart(xml, res);
end;

Procedure TFHIRXmlComposer.ComposeBaseAttributes(xml : TXmlBuilder; res : TFhirResource); 
begin
  CommentsStart(xml, res);
end;

Procedure TFHIRXmlComposer.ComposeBaseChildren(xml : TXmlBuilder; value : TFhirBase); 
begin
  // nothing
end;

Procedure TFHIRXmlComposer.ComposeBaseChildren(xml : TXmlBuilder; value : TFhirResource); 
begin
  // nothing
end;


{{types.abstract.impl}}

function TFHIRXmlParser.ParseEnum(Const aNames, aSystems : Array Of String; element : TMXmlElement; path : String) : TFhirEnum;
var
  child : TMXmlElement;
  i : integer;
begin
  result := TFhirEnum.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    i := StringArrayIndexOfSensitive(aNames, result.value);
    if i < 0 then
      raise EXmlException.create('unknown code: '+result.value+' from a set of choices of '+StringArrayToCommaString(aNames)+' for "'+path+'"');
    result.system := aSystems[i];
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeEnum(xml : TXmlBuilder; name : String; value : TFhirEnum; Const aNames : Array Of String);
begin
  if (value = nil) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseDate(element : TMXmlElement; path : string) : TFhirDate;
var
  child : TMXmlElement;
begin
  result := TFhirDate.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := toTFslDateTime(GetAttribute(element, 'value'));
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeDate(xml : TXmlBuilder; name : String; value : TFhirDate);
begin
  if (value = nil) or ((value.value.null) and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  if (value.value.notNull) then
    attribute(xml, 'value', asString(value.value));
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseDateTime(element : TMXmlElement; path : string) : TFhirDateTime;
var
  child : TMXmlElement;
begin
  result := TFhirDateTime.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := toTFslDateTime(GetAttribute(element, 'value'));
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeDateTime(xml : TXmlBuilder; name : String; value : TFhirDateTime);
begin
  if (value = nil) or ((value.value.null) and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  if (value.value.notNull) then
    attribute(xml, 'value', asString(value.value));
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseString(element : TMXmlElement; path : string) : TFhirString;
var
  child : TMXmlElement;
begin
  result := TFhirString.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeString(xml : TXmlBuilder; name : String; value : TFhirString);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseInteger(element : TMXmlElement; path : string) : TFhirInteger;
var
  child : TMXmlElement;
begin
  result := TFhirInteger.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeInteger(xml : TXmlBuilder; name : String; value : TFhirInteger);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseUri(element : TMXmlElement; path : string) : TFhirUri;
var
  child : TMXmlElement;
begin
  result := TFhirUri.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeUri(xml : TXmlBuilder; name : String; value : TFhirUri);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseInstant(element : TMXmlElement; path : string) : TFhirInstant;
var
  child : TMXmlElement;
begin
  result := TFhirInstant.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := toTFslDateTime(GetAttribute(element, 'value'));
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeInstant(xml : TXmlBuilder; name : String; value : TFhirInstant);
begin
  if (value = nil) or ((value.value.null) and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  if (value.value.notNull) then
    attribute(xml, 'value', asString(value.value));
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseXhtml(element : TMXmlElement; path : string) : TFhirXhtml;
var
  child : TMXmlElement;
begin
  result := TFhirXhtml.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeXhtml(xml : TXmlBuilder; name : String; value : TFhirXhtml);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseBoolean(element : TMXmlElement; path : string) : TFhirBoolean;
var
  child : TMXmlElement;
begin
  result := TFhirBoolean.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := StringToBoolean(GetAttribute(element, 'value'));
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeBoolean(xml : TXmlBuilder; name : String; value : TFhirBoolean);
begin
  if (value = nil) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', LCBooleanToString(value.value));
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseBase64Binary(element : TMXmlElement; path : string) : TFhirBase64Binary;
var
  child : TMXmlElement;
begin
  result := TFhirBase64Binary.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := toTBytes(GetAttribute(element, 'value'));
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeBase64Binary(xml : TXmlBuilder; name : String; value : TFhirBase64Binary);
begin
  if (value = nil) or ((value.value = nil) and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  if (value.value <> nil) then
    attribute(xml, 'value', asString(value.value));
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseTime(element : TMXmlElement; path : string) : TFhirTime;
var
  child : TMXmlElement;
begin
  result := TFhirTime.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeTime(xml : TXmlBuilder; name : String; value : TFhirTime);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseDecimal(element : TMXmlElement; path : string) : TFhirDecimal;
var
  child : TMXmlElement;
begin
  result := TFhirDecimal.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeDecimal(xml : TXmlBuilder; name : String; value : TFhirDecimal);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseCode(element : TMXmlElement; path : string) : TFhirCode;
var
  child : TMXmlElement;
begin
  result := TFhirCode.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeCode(xml : TXmlBuilder; name : String; value : TFhirCode);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseCanonical(element : TMXmlElement; path : string) : TFhirCanonical;
var
  child : TMXmlElement;
begin
  result := TFhirCanonical.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeCanonical(xml : TXmlBuilder; name : String; value : TFhirCanonical);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseOid(element : TMXmlElement; path : string) : TFhirOid;
var
  child : TMXmlElement;
begin
  result := TFhirOid.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeOid(xml : TXmlBuilder; name : String; value : TFhirOid);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseUuid(element : TMXmlElement; path : string) : TFhirUuid;
var
  child : TMXmlElement;
begin
  result := TFhirUuid.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeUuid(xml : TXmlBuilder; name : String; value : TFhirUuid);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseUrl(element : TMXmlElement; path : string) : TFhirUrl;
var
  child : TMXmlElement;
begin
  result := TFhirUrl.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeUrl(xml : TXmlBuilder; name : String; value : TFhirUrl);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseMarkdown(element : TMXmlElement; path : string) : TFhirMarkdown;
var
  child : TMXmlElement;
begin
  result := TFhirMarkdown.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeMarkdown(xml : TXmlBuilder; name : String; value : TFhirMarkdown);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseUnsignedInt(element : TMXmlElement; path : string) : TFhirUnsignedInt;
var
  child : TMXmlElement;
begin
  result := TFhirUnsignedInt.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeUnsignedInt(xml : TXmlBuilder; name : String; value : TFhirUnsignedInt);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseId(element : TMXmlElement; path : string) : TFhirId;
var
  child : TMXmlElement;
begin
  result := TFhirId.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeId(xml : TXmlBuilder; name : String; value : TFhirId);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParsePositiveInt(element : TMXmlElement; path : string) : TFhirPositiveInt;
var
  child : TMXmlElement;
begin
  result := TFhirPositiveInt.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
      child := NextSibling(child);
    end;
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposePositiveInt(xml : TXmlBuilder; name : String; value : TFhirPositiveInt);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

function TFHIRXmlParser.ParseInteger64(element : TMXmlElement; path : string) : TFhirInteger64;
var
  child : TMXmlElement;
begin
  result := TFhirInteger64.create;
  try
    ParseElementAttributes(result, path, element);
    result.value := GetAttribute(element, 'value');
    child := FirstChild(element);
    while (child <> nil) do
    begin
      if Not ParseElementChild(result, path, child) then
         UnknownContent(child, path);
    end;
      child := NextSibling(child);
    closeOutElement(result, element);

    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRXmlComposer.ComposeInteger64(xml : TXmlBuilder; name : String; value : TFhirInteger64);
begin
  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then
    exit;
  composeElementAttributes(xml, value);
  attribute(xml, 'value', value.value);
  xml.open(name);
  composeElementChildren(xml, value);
  closeOutElement(xml, value);
  xml.close(name);
end;

{{types.concrete.impl}}

{{resources.abstract.impl}}

{{resources.concrete.impl}}

function TFHIRXmlParser.ParseResource(element : TMXmlElement; path : String) : TFhirResource;
begin
  if (element = nil) Then
    Raise EXmlException.Create('error - element is nil')
{{parse.resource}}
  else
    raise EXmlException.create('Error: the element '+element.localName+' is not recognised as a valid resource name');
end;

procedure TFHIRXmlComposer.ComposeResource(xml : TXmlBuilder; resource: TFhirResource);
begin
  if (resource = nil) Then
    Raise EXmlException.Create('error - resource is nil');
  Case resource.ResourceType of
{{compose.resource}}
  else
    raise EXmlException.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');
  end;
end;

function TFHIRXmlParser.ParseFragment(element : TMXmlElement) : TFHIRObject;
begin
  if (element = nil) Then
    Raise EXmlException.Create('error - element is nil')
{{parse.fragment}}
  else
    raise EXmlException.create('error: the element '+element.Name+' is not a valid fragment name');
end;

function TFHIRXmlParser.ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRDataTypeClass) : TFhirDataType;
begin
  if (name <> '') and (name <> element.localName) then
    raise EXmlException.Create('Expected Name mismatch : expected "'+name+'"+, but found "'+element.localName+'"')
{{parse.type}}
  else
    raise EXmlException.create('Unknown Type');
end;

procedure TFHIRXmlComposer.ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject);
begin
  if (base = nil) Then
    Raise EXmlException.Create('error - base is nil')
{{compose.base}}
  else
    inherited ComposeBase(xml, name, base);
end;


end.

