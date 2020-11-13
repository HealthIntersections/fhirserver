unit fhir5_turtle;

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
  fsl_base, fsl_utilities, fsl_collections, fsl_turtle, 
  fhir_parser, fhir_objects, 
  fhir5_parserBase, fhir5_resources, fhir5_constants, fhir5_base, fhir5_enums, fhir5_types;

Type

  TFHIRTurtleParser = class (TFHIRTurtleParserBase5)
  protected
    procedure ParseBaseProperties(obj : TTurtleComplex; value : TFhirBase); overload;
    procedure ParseBaseProperties(obj : TTurtleComplex; value : TFhirResource); overload;
  
{{parse.types.abstract.intf}}

    function ParseEnum(obj : TTurtleComplex; Const aNames, aSystems : Array Of String) : TFHIREnum; overload;
    function ParseDate(obj : TTurtleComplex) : TFHIRDate; overload;
    function ParseDateTime(obj : TTurtleComplex) : TFHIRDateTime; overload;
    function ParseString(obj : TTurtleComplex) : TFHIRString; overload;
    function ParseInteger(obj : TTurtleComplex) : TFHIRInteger; overload;
    function ParseUri(obj : TTurtleComplex) : TFHIRUri; overload;
    function ParseInstant(obj : TTurtleComplex) : TFHIRInstant; overload;
    function ParseXhtml(obj : TTurtleComplex) : TFHIRXhtml; overload;
    function ParseBoolean(obj : TTurtleComplex) : TFHIRBoolean; overload;
    function ParseBase64Binary(obj : TTurtleComplex) : TFHIRBase64Binary; overload;
    function ParseTime(obj : TTurtleComplex) : TFHIRTime; overload;
    function ParseDecimal(obj : TTurtleComplex) : TFHIRDecimal; overload;
    function ParseCode(obj : TTurtleComplex) : TFHIRCode; overload;
    function ParseCanonical(obj : TTurtleComplex) : TFHIRCanonical; overload;
    function ParseOid(obj : TTurtleComplex) : TFHIROid; overload;
    function ParseUuid(obj : TTurtleComplex) : TFHIRUuid; overload;
    function ParseUrl(obj : TTurtleComplex) : TFHIRUrl; overload;
    function ParseMarkdown(obj : TTurtleComplex) : TFHIRMarkdown; overload;
    function ParseUnsignedInt(obj : TTurtleComplex) : TFHIRUnsignedInt; overload;
    function ParseId(obj : TTurtleComplex) : TFHIRId; overload;
    function ParsePositiveInt(obj : TTurtleComplex) : TFHIRPositiveInt; overload;
    function ParseInteger64(obj : TTurtleComplex) : TFHIRInteger64; overload;

{{parse.types.concrete.intf}}

{{parse.resources.abstract.intf}}

{{parse.resources.concrete.intf}}
    function ParseResource(obj : TTurtleComplex) : TFhirResource; override;
    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType; override;
  public
    function ParseFragment(obj : TTurtleComplex; type_ : String) : TFHIRObject;  overload;
  end;
  
  TFHIRTurtleComposer = class (TFHIRTurtleComposerBase5)
  protected
    procedure ComposeBase(parent :  TTurtleComplex; parentType, name : String; elem : TFhirBase; useType : boolean; index : integer); overload;
    procedure ComposeBase(parent :  TTurtleComplex; parentType, name : String; elem : TFhirResource; useType : boolean; index : integer); overload;
    
{{compose.types.abstract.intf}}
    
    Procedure ComposeEnum(parent :  TTurtleComplex; parentType, name : String; value : TFhirEnum; Const aNames, aSystems : Array Of String; useType : boolean; index : integer);
    Procedure ComposeDate(parent :  TTurtleComplex; parentType, name : String; value : TFhirDate; useType : boolean; index : integer);
    Procedure ComposeDateTime(parent :  TTurtleComplex; parentType, name : String; value : TFhirDateTime; useType : boolean; index : integer);
    Procedure ComposeString(parent :  TTurtleComplex; parentType, name : String; value : TFhirString; useType : boolean; index : integer);
    Procedure ComposeInteger(parent :  TTurtleComplex; parentType, name : String; value : TFhirInteger; useType : boolean; index : integer);
    Procedure ComposeUri(parent :  TTurtleComplex; parentType, name : String; value : TFhirUri; useType : boolean; index : integer);
    Procedure ComposeInstant(parent :  TTurtleComplex; parentType, name : String; value : TFhirInstant; useType : boolean; index : integer);
    Procedure ComposeXhtml(parent :  TTurtleComplex; parentType, name : String; value : TFhirXhtml; useType : boolean; index : integer);
    Procedure ComposeBoolean(parent :  TTurtleComplex; parentType, name : String; value : TFhirBoolean; useType : boolean; index : integer);
    Procedure ComposeBase64Binary(parent :  TTurtleComplex; parentType, name : String; value : TFhirBase64Binary; useType : boolean; index : integer);
    Procedure ComposeTime(parent :  TTurtleComplex; parentType, name : String; value : TFhirTime; useType : boolean; index : integer);
    Procedure ComposeDecimal(parent :  TTurtleComplex; parentType, name : String; value : TFhirDecimal; useType : boolean; index : integer);
    Procedure ComposeCode(parent :  TTurtleComplex; parentType, name : String; value : TFhirCode; useType : boolean; index : integer);
    Procedure ComposeCanonical(parent :  TTurtleComplex; parentType, name : String; value : TFhirCanonical; useType : boolean; index : integer);
    Procedure ComposeOid(parent :  TTurtleComplex; parentType, name : String; value : TFhirOid; useType : boolean; index : integer);
    Procedure ComposeUuid(parent :  TTurtleComplex; parentType, name : String; value : TFhirUuid; useType : boolean; index : integer);
    Procedure ComposeUrl(parent :  TTurtleComplex; parentType, name : String; value : TFhirUrl; useType : boolean; index : integer);
    Procedure ComposeMarkdown(parent :  TTurtleComplex; parentType, name : String; value : TFhirMarkdown; useType : boolean; index : integer);
    Procedure ComposeUnsignedInt(parent :  TTurtleComplex; parentType, name : String; value : TFhirUnsignedInt; useType : boolean; index : integer);
    Procedure ComposeId(parent :  TTurtleComplex; parentType, name : String; value : TFhirId; useType : boolean; index : integer);
    Procedure ComposePositiveInt(parent :  TTurtleComplex; parentType, name : String; value : TFhirPositiveInt; useType : boolean; index : integer);
    Procedure ComposeInteger64(parent :  TTurtleComplex; parentType, name : String; value : TFhirInteger64; useType : boolean; index : integer);

{{compose.types.concrete.intf}}

{{compose.resources.abstract.intf}}

{{compose.resources.concrete.intf}}

    procedure ComposeResource(parent :  TTurtleComplex; resource : TFhirResource); overload; override;
  end;


implementation

{ TFHIRTurtleParser / TFHIRTurtleComposer }

procedure TFHIRTurtleParser.ParseBaseProperties(obj : TTurtleComplex; value : TFhirBase); 
begin
  value.LocationStart := obj.Start;
  value.LocationEnd := obj.Stop;
end;

procedure TFHIRTurtleParser.ParseBaseProperties(obj : TTurtleComplex; value : TFhirResource); 
begin
  value.LocationStart := obj.Start;
  value.LocationEnd := obj.Stop;
end;

procedure TFHIRTurtleComposer.ComposeBase(parent :  TTurtleComplex; parentType, name : String; elem : TFhirBase; useType : boolean; index : integer); 
begin
  // nothing
end;

procedure TFHIRTurtleComposer.ComposeBase(parent :  TTurtleComplex; parentType, name : String; elem : TFhirResource; useType : boolean; index : integer); 
begin
  // nothing
end;


{{types.abstract.impl}}

function TFHIRTurtleParser.ParseEnum(obj : TTurtleComplex; Const aNames, aSystems : Array Of String) : TFHIREnum;
var
  i : integer;
  value : String;
begin
  if obj = nil then
    exit(nil);

  if (obj.has('http://hl7.org/fhir/value')) then
    value := obj.stringLiteral('http://hl7.org/fhir/value');
  i := StringArrayIndexOfSensitive(aNames, value);
  if (value <> '') and (i < 0) then
    raise ERdfException.create('unknown code: '+value+' from a set of choices of '+StringArrayToCommaString(aNames));
  result := TFHIREnum.create;
  try
    result.value := value;
    result.system := aSystems[i];
    parseElementProperties(obj, result);
    result.link;
  finally
    result.free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeEnum(parent :  TTurtleComplex; parentType, name : String; value : TFhirEnum; Const aNames, aSystems : Array Of String; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:code');
  this.addPredicate('fhir:value', ttlLiteral(value.value));
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseDate(obj : TTurtleComplex) : TFHIRDate;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirDate.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := toTFslDateTime(obj.stringLiteral('http://hl7.org/fhir/value'));
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeDate(parent :  TTurtleComplex; parentType, name : String; value : TFhirDate; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:date');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), dateXsdType(value.value));
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseDateTime(obj : TTurtleComplex) : TFHIRDateTime;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirDateTime.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := toTFslDateTime(obj.stringLiteral('http://hl7.org/fhir/value'));
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeDateTime(parent :  TTurtleComplex; parentType, name : String; value : TFhirDateTime; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:dateTime');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), dateXsdType(value.value));
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseString(obj : TTurtleComplex) : TFHIRString;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirString.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeString(parent :  TTurtleComplex; parentType, name : String; value : TFhirString; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:string');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:string');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseInteger(obj : TTurtleComplex) : TFHIRInteger;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirInteger.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeInteger(parent :  TTurtleComplex; parentType, name : String; value : TFhirInteger; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:integer');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:int');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseUri(obj : TTurtleComplex) : TFHIRUri;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirUri.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeUri(parent :  TTurtleComplex; parentType, name : String; value : TFhirUri; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:uri');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:anyURI');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseInstant(obj : TTurtleComplex) : TFHIRInstant;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirInstant.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := toTFslDateTime(obj.stringLiteral('http://hl7.org/fhir/value'));
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeInstant(parent :  TTurtleComplex; parentType, name : String; value : TFhirInstant; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:instant');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:dateTime');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseXhtml(obj : TTurtleComplex) : TFHIRXhtml;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirXhtml.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeXhtml(parent :  TTurtleComplex; parentType, name : String; value : TFhirXhtml; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:xhtml');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)));
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseBoolean(obj : TTurtleComplex) : TFHIRBoolean;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirBoolean.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
     result.value := StringToBoolean(obj.stringLiteral('http://hl7.org/fhir/value'));
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeBoolean(parent :  TTurtleComplex; parentType, name : String; value : TFhirBoolean; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:boolean');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:boolean');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseBase64Binary(obj : TTurtleComplex) : TFHIRBase64Binary;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirBase64Binary.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := toTBytes(obj.stringLiteral('http://hl7.org/fhir/value'));
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeBase64Binary(parent :  TTurtleComplex; parentType, name : String; value : TFhirBase64Binary; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:base64Binary');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:base64Binary');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseTime(obj : TTurtleComplex) : TFHIRTime;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirTime.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeTime(parent :  TTurtleComplex; parentType, name : String; value : TFhirTime; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:time');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:time');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseDecimal(obj : TTurtleComplex) : TFHIRDecimal;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirDecimal.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeDecimal(parent :  TTurtleComplex; parentType, name : String; value : TFhirDecimal; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:decimal');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:decimal');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseCode(obj : TTurtleComplex) : TFHIRCode;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirCode.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeCode(parent :  TTurtleComplex; parentType, name : String; value : TFhirCode; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:code');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:token');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseCanonical(obj : TTurtleComplex) : TFHIRCanonical;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirCanonical.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeCanonical(parent :  TTurtleComplex; parentType, name : String; value : TFhirCanonical; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:canonical');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:anyURI');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseOid(obj : TTurtleComplex) : TFHIROid;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirOid.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeOid(parent :  TTurtleComplex; parentType, name : String; value : TFhirOid; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:oid');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:anyURI');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseUuid(obj : TTurtleComplex) : TFHIRUuid;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirUuid.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeUuid(parent :  TTurtleComplex; parentType, name : String; value : TFhirUuid; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:uuid');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:anyURI');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseUrl(obj : TTurtleComplex) : TFHIRUrl;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirUrl.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeUrl(parent :  TTurtleComplex; parentType, name : String; value : TFhirUrl; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:url');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:anyURI');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseMarkdown(obj : TTurtleComplex) : TFHIRMarkdown;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirMarkdown.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeMarkdown(parent :  TTurtleComplex; parentType, name : String; value : TFhirMarkdown; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:markdown');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:string');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseUnsignedInt(obj : TTurtleComplex) : TFHIRUnsignedInt;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirUnsignedInt.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeUnsignedInt(parent :  TTurtleComplex; parentType, name : String; value : TFhirUnsignedInt; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:unsignedInt');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:nonNegativeInteger');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseId(obj : TTurtleComplex) : TFHIRId;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirId.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeId(parent :  TTurtleComplex; parentType, name : String; value : TFhirId; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:id');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:string');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParsePositiveInt(obj : TTurtleComplex) : TFHIRPositiveInt;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirPositiveInt.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposePositiveInt(parent :  TTurtleComplex; parentType, name : String; value : TFhirPositiveInt; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:positiveInt');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:positiveInteger');
  composeElement(this, parentType, name, value, false, index);
end;

function TFHIRTurtleParser.ParseInteger64(obj : TTurtleComplex) : TFHIRInteger64;
begin
  if (obj = nil) then
    exit(nil);
  result := TFhirInteger64.Create;
  try
    if (obj.has('http://hl7.org/fhir/value')) then
      result.value := obj.stringLiteral('http://hl7.org/fhir/value');
    parseElementProperties(obj, result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TFHIRTurtleComposer.ComposeInteger64(parent :  TTurtleComplex; parentType, name : String; value : TFhirInteger64; useType : boolean; index : integer);
var
  this : TTurtleComplex;
begin
  if (value = nil) then
    exit;
  this := parent.addPredicate('fhir:'+parentType+'.'+name);
  if (useType) then
    this.addPredicate('a', 'fhir:Integer64');
  this.addPredicate('fhir:value', ttlLiteral(asString(value.value)), 'xsd:Integer64eger');
  composeElement(this, parentType, name, value, false, index);
end;

{{types.concrete.impl}}

{{resources.abstract.impl}}

{{resources.concrete.impl}}

function TFHIRTurtleParser.ParseFragment(obj : TTurtleComplex; type_ : String) : TFHIRObject;
begin
  if (obj = nil) then
    Raise ERdfException.Create('error - object is nil')
{{parser.reg.fragment}}
  else
    raise ERdfException.create('error: the element '+type_+' is not a valid fragment name');
end;

function TFHIRTurtleParser.ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;
begin
  if (obj = nil) then
    Raise ERdfException.Create('error - object is nil')
{{parser.reg.type}}
  else
    raise ERdfException.create('Unknown Type');
end;

procedure TFHIRTurtleComposer.ComposeResource(parent : TTurtleComplex; resource : TFhirResource);
var
  this : TTurtleComplex;
begin
  if (resource = nil) Then
    Raise ERdfException.Create('error - resource is nil');
  this := parent;
  Case resource.ResourceType of
{{compose.reg.resource}}
  else
    raise ERdfException.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');
  end;
end;

function TFHIRTurtleParser.ParseResource(obj : TTurtleComplex) : TFhirResource;
var
  s : String;
begin
  s := rdfsType(obj);
  if (s = '') then
    Raise ERdfException.Create('error - object has no type')
{{parser.reg.resource}}
  else
    raise ERdfException.create('error: the element '+s+' is not a valid resource name');
end;

end.

