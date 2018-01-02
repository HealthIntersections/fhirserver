unit FHIRJavascriptReg;

{$I fhir.inc}

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

{$IFNDEF FHIR4}
This is the dstu4 version of the FHIR code
{$ENDIF}


interface

// FHIR v3.2.0 generated 2017-12-21T15:48:31+11:00

uses
  Javascript, FHIRJavascript;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element', 'id', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Element', 'extension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
end;


procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement', 'modifierExtension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
end;


procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource', 'id', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Resource', 'meta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Resource', 'implicitRules', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Resource', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
end;


procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource', 'text', 'Narrative', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DomainResource', 'contained', 'Resource', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'extension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'modifierExtension', 'Extension', getFHIRArrayProp, setFHIRArrayProp);
end;


procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'ParametersParameter', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ParametersParameter', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ParametersParameter', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter', 'valueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'resource', 'Resource', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'part', '@Parameters.parameter', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter', nil, 'ParametersParameter', FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;


procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters', nil, 'Parameters', FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;


procedure defineMetadataResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MetadataResource', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MetadataResource', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MetadataResource', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MetadataResource', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MetadataResource', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MetadataResource', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MetadataResource', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;


procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'Extension', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Extension', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'Extension', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Extension', 'valueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Extension', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension', nil, 'Extension', FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;


procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Narrative', 'div', 'xhtml', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative', nil, 'Narrative', FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;


procedure defineContributorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Contributor', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Contributor', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Contributor', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContributorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contributor', nil, 'Contributor', FHIRFactoryJs);
  defineContributorPropsJs(js, def);
end;


procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'data', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'Attachment', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'size', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'hash', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'Attachment', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'creation', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment', nil, 'Attachment', FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;


procedure defineDataRequirementCodeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementCodeFilter', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'valueSetUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'valueSetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDataRequirementCodeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementCodeFilter', nil, 'DataRequirementCodeFilter', FHIRFactoryJs);
  defineDataRequirementCodeFilterPropsJs(js, def);
end;


procedure defineDataRequirementDateFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementDateFilter', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDataRequirementDateFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementDateFilter', nil, 'DataRequirementDateFilter', FHIRFactoryJs);
  defineDataRequirementDateFilterPropsJs(js, def);
end;


procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DataRequirement', 'codeFilter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement', 'dateFilter', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement', nil, 'DataRequirement', FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;


procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Dosage', 'sequence', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Dosage', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Dosage', 'additionalInstruction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Dosage', 'patientInstruction', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Dosage', 'timing', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Dosage', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'site', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'doseRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'doseQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerPeriod', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerAdministration', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerLifetime', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'rateQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage', nil, 'Dosage', FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;


procedure defineMarketingStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'MarketingStatus', 'country', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'jurisdiction', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'dateRange', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'restoreDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineMarketingStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MarketingStatus', nil, 'MarketingStatus', FHIRFactoryJs);
  defineMarketingStatusPropsJs(js, def);
end;


procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Identifier', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Identifier', 'assigner', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier', nil, 'Identifier', FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;


procedure defineSubstanceAmountReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceAmountReferenceRange', 'lowLimit', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmountReferenceRange', 'highLimit', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceAmountReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmountReferenceRange', nil, 'SubstanceAmountReferenceRange', FHIRFactoryJs);
  defineSubstanceAmountReferenceRangePropsJs(js, def);
end;


procedure defineSubstanceAmountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceAmount', 'amountQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount', 'amountType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountText', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount', 'referenceRange', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceAmountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmount', nil, 'SubstanceAmount', FHIRFactoryJs);
  defineSubstanceAmountPropsJs(js, def);
end;


procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coding', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coding', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coding', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coding', 'userSelected', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding', nil, 'Coding', FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;


procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData', 'origin', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SampledData', 'period', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'lowerLimit', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'upperLimit', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'dimensions', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SampledData', 'data', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData', nil, 'SampledData', FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;


procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio', 'numerator', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Ratio', 'denominator', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio', nil, 'Ratio', FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;


procedure defineSubstanceMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceMoiety', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety', 'stereochemistry', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'opticalActivity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceMoiety', 'molecularFormula', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceMoiety', 'amount', 'SubstanceAmount', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceMoiety', nil, 'SubstanceMoiety', FHIRFactoryJs);
  defineSubstanceMoietyPropsJs(js, def);
end;


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference', 'reference', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Reference', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Reference', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference', nil, 'Reference', FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;


procedure defineTriggerDefinitionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinitionCondition', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinitionCondition', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTriggerDefinitionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinitionCondition', nil, 'TriggerDefinitionCondition', FHIRFactoryJs);
  defineTriggerDefinitionConditionPropsJs(js, def);
end;


procedure defineTriggerDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinition', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'timingReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'timingDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition', 'data', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'condition', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTriggerDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinition', nil, 'TriggerDefinition', FHIRFactoryJs);
  defineTriggerDefinitionPropsJs(js, def);
end;


procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period', 'start', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Period', 'end', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period', nil, 'Period', FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;


procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'Quantity', 'comparator', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'unit', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity', nil, 'Quantity', FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;


procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range', 'low', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Range', 'high', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range', nil, 'Range', FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;


procedure defineRelatedArtifactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'RelatedArtifact', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'citation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'document', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineRelatedArtifactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedArtifact', nil, 'RelatedArtifact', FHIRFactoryJs);
  defineRelatedArtifactPropsJs(js, def);
end;


procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation', 'authorReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Annotation', 'authorString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Annotation', 'time', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation', nil, 'Annotation', FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;


procedure defineProductShelfLifePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ProductShelfLife', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'period', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'specialPrecautionsForStorage', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductShelfLifeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductShelfLife', nil, 'ProductShelfLife', FHIRFactoryJs);
  defineProductShelfLifePropsJs(js, def);
end;


procedure defineContactDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactDetail', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContactDetail', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContactDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactDetail', nil, 'ContactDetail', FHIRFactoryJs);
  defineContactDetailPropsJs(js, def);
end;


procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineUsageContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UsageContext', nil, 'UsageContext', FHIRFactoryJs);
  defineUsageContextPropsJs(js, def);
end;


procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature', 'type', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Signature', 'when', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Signature', 'whoUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'whoReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'onBehalfOfUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'onBehalfOfReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'targetFormat', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'sigFormat', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Signature', 'blob', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature', nil, 'Signature', FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;


procedure defineProdCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ProdCharacteristic', 'height', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'width', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'depth', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'weight', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'nominalVolume', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'externalDiameter', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'shape', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProdCharacteristic', 'image', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProdCharacteristic', 'scoring', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProdCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProdCharacteristic', nil, 'ProdCharacteristic', FHIRFactoryJs);
  defineProdCharacteristicPropsJs(js, def);
end;


procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept', 'coding', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept', nil, 'CodeableConcept', FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;


procedure defineParameterDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ParameterDefinition', 'name', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ParameterDefinition', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineParameterDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParameterDefinition', nil, 'ParameterDefinition', FHIRFactoryJs);
  defineParameterDefinitionPropsJs(js, def);
end;


procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint', 'system', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'rank', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint', nil, 'ContactPoint', FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;


procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'family', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName', nil, 'HumanName', FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;


procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta', 'versionId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Meta', 'lastUpdated', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Meta', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Meta', 'security', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Meta', 'tag', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta', nil, 'Meta', FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;


procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'city', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'district', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'state', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'postalCode', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'country', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Address', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address', nil, 'Address', FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing', 'discriminator', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'ordered', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'rules', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing', nil, 'ElementDefinitionSlicing', FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingDiscriminatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingDiscriminatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicingDiscriminator', nil, 'ElementDefinitionSlicingDiscriminator', FHIRFactoryJs);
  defineElementDefinitionSlicingDiscriminatorPropsJs(js, def);
end;


procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase', 'min', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase', nil, 'ElementDefinitionBase', FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;


procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType', 'code', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType', 'profile', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType', 'targetProfile', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType', 'versioning', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType', nil, 'ElementDefinitionType', FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;


procedure defineElementDefinitionExamplePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionExample', 'label', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineElementDefinitionExampleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionExample', nil, 'ElementDefinitionExample', FHIRFactoryJs);
  defineElementDefinitionExamplePropsJs(js, def);
end;


procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint', 'key', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'requirements', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'human', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'xpath', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint', nil, 'ElementDefinitionConstraint', FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;


procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding', 'strength', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'valueSetUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'valueSetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding', nil, 'ElementDefinitionBinding', FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;


procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping', 'identity', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'map', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping', nil, 'ElementDefinitionMapping', FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;


procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinition', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'sliceName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'label', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'slicing', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'short', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'definition', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'comment', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'requirements', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'min', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'base', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'contentReference', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'type', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'meaningWhenMissing', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'orderMeaning', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'fixedId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedPositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'fixedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'patternBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'patternCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'patternId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'patternMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternPositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'patternString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'example', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'minValueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'minValuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'minValueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'minValueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'maxValuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'maxLength', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'constraint', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'mustSupport', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'isModifier', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'isSummary', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'binding', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'mapping', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition', nil, 'ElementDefinition', FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;


procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat', 'boundsDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'count', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'countMax', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'duration', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationMax', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationUnit', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'frequency', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'frequencyMax', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'period', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodMax', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodUnit', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'offset', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat', nil, 'TimingRepeat', FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;


procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Timing', 'repeat', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Timing', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing', nil, 'Timing', FHIRFactoryJs);
  defineTimingPropsJs(js, def);
end;


procedure defineCountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineCountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Count', nil, 'Count', FHIRFactoryJs);
  defineCountPropsJs(js, def);
end;


procedure defineMoneyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineMoneyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Money', nil, 'Money', FHIRFactoryJs);
  defineMoneyPropsJs(js, def);
end;


procedure defineAgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineAgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Age', nil, 'Age', FHIRFactoryJs);
  defineAgePropsJs(js, def);
end;


procedure defineDistancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineDistanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Distance', nil, 'Distance', FHIRFactoryJs);
  defineDistancePropsJs(js, def);
end;


procedure defineDurationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineDurationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Duration', nil, 'Duration', FHIRFactoryJs);
  defineDurationPropsJs(js, def);
end;


procedure defineAccountCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountCoverage', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AccountCoverage', 'priority', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineAccountCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountCoverage', nil, 'AccountCoverage', FHIRFactoryJs);
  defineAccountCoveragePropsJs(js, def);
end;


procedure defineAccountGuarantorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountGuarantor', 'party', 'Reference(Patient|RelatedPerson|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AccountGuarantor', 'onHold', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'AccountGuarantor', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineAccountGuarantorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountGuarantor', nil, 'AccountGuarantor', FHIRFactoryJs);
  defineAccountGuarantorPropsJs(js, def);
end;


procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Account', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Account', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Account', 'subject', 'Reference(Patient|Device|Practitioner|Location|HealthcareService|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'active', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'coverage', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Account', 'owner', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Account', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Account', 'guarantor', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Account', 'partOf', 'Reference(Account)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account', nil, 'Account', FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;


procedure defineActivityDefinitionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionParticipant', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineActivityDefinitionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionParticipant', nil, 'ActivityDefinitionParticipant', FHIRFactoryJs);
  defineActivityDefinitionParticipantPropsJs(js, def);
end;


procedure defineActivityDefinitionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'language', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineActivityDefinitionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionDynamicValue', nil, 'ActivityDefinitionDynamicValue', FHIRFactoryJs);
  defineActivityDefinitionDynamicValuePropsJs(js, def);
end;


procedure defineActivityDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ActivityDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'library', 'Reference(Library)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'doNotPerform', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'timingAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'productReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'productCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'dosage', 'Dosage', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'specimenRequirement', 'Reference(SpecimenDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'transform', 'Reference(StructureMap)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'dynamicValue', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineActivityDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinition', nil, 'ActivityDefinition', FHIRFactoryJs);
  defineActivityDefinitionPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntity', 'instance', 'Reference(Substance|Medication|MedicationAdministration|MedicationStatement|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity', 'causality', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAdverseEventSuspectEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntity', nil, 'AdverseEventSuspectEntity', FHIRFactoryJs);
  defineAdverseEventSuspectEntityPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityCausalityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'assessment', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'productRelatedness', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'author', 'Reference(Practitioner|PractitionerRole)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineAdverseEventSuspectEntityCausalityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntityCausality', nil, 'AdverseEventSuspectEntityCausality', FHIRFactoryJs);
  defineAdverseEventSuspectEntityCausalityPropsJs(js, def);
end;


procedure defineAdverseEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AdverseEvent', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'actuality', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'event', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'subject', 'Reference(Patient|Group|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent', 'resultingCondition', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'seriousness', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'severity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'recorder', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'eventParticipant', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent', 'suspectEntity', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'subjectMedicalHistory', 'Reference(Condition|Observation|AllergyIntolerance|FamilyMemberHistory|Immunization|Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'referenceDocument', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'study', 'Reference(ResearchStudy)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAdverseEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEvent', nil, 'AdverseEvent', FHIRFactoryJs);
  defineAdverseEventPropsJs(js, def);
end;


procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'substance', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'manifestation', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'onset', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'exposureRoute', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction', nil, 'AllergyIntoleranceReaction', FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;


procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance', 'clinicalStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'verificationStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'criticality', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'assertedDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'recorder', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'asserter', 'Reference(Patient|RelatedPerson|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'lastOccurrence', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance', 'reaction', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance', nil, 'AllergyIntolerance', FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;


procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant', 'required', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant', nil, 'AppointmentParticipant', FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;


procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'serviceCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'serviceType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'appointmentType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'indication', 'Reference(Condition|Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'priority', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'start', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'minutesDuration', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Appointment', 'slot', 'Reference(Slot)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'patientInstruction', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'incomingReferral', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'requestedPeriod', 'Period', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment', nil, 'Appointment', FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;


procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse', 'appointment', 'Reference(Appointment)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse', 'start', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse', 'participantType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse', 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse', 'participantStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse', nil, 'AppointmentResponse', FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;


procedure defineAuditEventAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEventAgent', 'reference', 'Reference(PractitionerRole|Practitioner|Organization|Device|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'userId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'altId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent', 'requestor', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventAgent', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'media', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'network', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'purposeOfUse', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgent', nil, 'AuditEventAgent', FHIRFactoryJs);
  defineAuditEventAgentPropsJs(js, def);
end;


procedure defineAuditEventAgentNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgentNetwork', 'address', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgentNetwork', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineAuditEventAgentNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgentNetwork', nil, 'AuditEventAgentNetwork', FHIRFactoryJs);
  defineAuditEventAgentNetworkPropsJs(js, def);
end;


procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource', 'site', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource', 'type', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource', nil, 'AuditEventSource', FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;


procedure defineAuditEventEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntity', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'role', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'lifecycle', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'securityLabel', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEntity', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity', 'query', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventEntity', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntity', nil, 'AuditEventEntity', FHIRFactoryJs);
  defineAuditEventEntityPropsJs(js, def);
end;


procedure defineAuditEventEntityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntityDetail', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineAuditEventEntityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntityDetail', nil, 'AuditEventEntityDetail', FHIRFactoryJs);
  defineAuditEventEntityDetailPropsJs(js, def);
end;


procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'subtype', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'action', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'recorded', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEvent', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'outcomeDesc', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'purposeOfEvent', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'agent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'source', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'entity', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent', nil, 'AuditEvent', FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;


procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Basic', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Basic', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Basic', 'created', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Basic', 'author', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic', nil, 'Basic', FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;


procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Binary', 'securityContext', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Binary', 'content', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary', nil, 'Binary', FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collector', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'source', 'Reference(Patient|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collectedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collectedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductCollection', nil, 'BiologicallyDerivedProductCollection', FHIRFactoryJs);
  defineBiologicallyDerivedProductCollectionPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'procedure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'additive', 'Reference(Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'timeDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'timePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductProcessing', nil, 'BiologicallyDerivedProductProcessing', FHIRFactoryJs);
  defineBiologicallyDerivedProductProcessingPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductManipulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'timeDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'timePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductManipulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductManipulation', nil, 'BiologicallyDerivedProductManipulation', FHIRFactoryJs);
  defineBiologicallyDerivedProductManipulationPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductStoragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'temperature', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'scale', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'duration', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductStorageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductStorage', nil, 'BiologicallyDerivedProductStorage', FHIRFactoryJs);
  defineBiologicallyDerivedProductStoragePropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'productCategory', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'productCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'request', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'quantity', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'parent', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'collection', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'processing', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'manipulation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'storage', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineBiologicallyDerivedProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProduct', nil, 'BiologicallyDerivedProduct', FHIRFactoryJs);
  defineBiologicallyDerivedProductPropsJs(js, def);
end;


procedure defineBodyStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodyStructure', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'BodyStructure', 'morphology', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure', 'location', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure', 'locationQualifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BodyStructure', 'image', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBodyStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodyStructure', nil, 'BodyStructure', FHIRFactoryJs);
  defineBodyStructurePropsJs(js, def);
end;


procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink', 'relation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleLink', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink', nil, 'BundleLink', FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;


procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry', 'link', '@Bundle.link', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry', 'fullUrl', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntry', 'resource', 'Resource', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'search', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'request', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'response', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry', nil, 'BundleEntry', FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;


procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch', 'score', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch', nil, 'BundleEntrySearch', FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;


procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest', 'method', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifNoneMatch', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifModifiedSince', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifMatch', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifNoneExist', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest', nil, 'BundleEntryRequest', FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;


procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse', 'status', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'location', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'etag', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'lastModified', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryResponse', 'outcome', 'Resource', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse', nil, 'BundleEntryResponse', FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;


procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Bundle', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Bundle', 'timestamp', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Bundle', 'total', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Bundle', 'link', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Bundle', 'entry', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Bundle', 'signature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle', nil, 'Bundle', FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;


procedure defineCapabilityStatementSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementSoftware', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware', 'releaseDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineCapabilityStatementSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementSoftware', nil, 'CapabilityStatementSoftware', FHIRFactoryJs);
  defineCapabilityStatementSoftwarePropsJs(js, def);
end;


procedure defineCapabilityStatementImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementImplementation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementImplementation', nil, 'CapabilityStatementImplementation', FHIRFactoryJs);
  defineCapabilityStatementImplementationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRest', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest', 'security', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRest', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'interaction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'searchParam', '@CapabilityStatement.rest.resource.searchParam', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'operation', '@CapabilityStatement.rest.resource.operation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRest', nil, 'CapabilityStatementRest', FHIRFactoryJs);
  defineCapabilityStatementRestPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'cors', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'service', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'certificate', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity', nil, 'CapabilityStatementRestSecurity', FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityCertificatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestSecurityCertificate', 'blob', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
end;

procedure defineCapabilityStatementRestSecurityCertificateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurityCertificate', nil, 'CapabilityStatementRestSecurityCertificate', FHIRFactoryJs);
  defineCapabilityStatementRestSecurityCertificatePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'supportedProfile', 'Reference(StructureDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'documentation', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'interaction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'versioning', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'readHistory', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'updateCreate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalCreate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalRead', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalUpdate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalDelete', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'searchParam', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'operation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResource', nil, 'CapabilityStatementRestResource', FHIRFactoryJs);
  defineCapabilityStatementRestResourcePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceInteraction', nil, 'CapabilityStatementRestResourceInteraction', FHIRFactoryJs);
  defineCapabilityStatementRestResourceInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'definition', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceSearchParam', nil, 'CapabilityStatementRestResourceSearchParam', FHIRFactoryJs);
  defineCapabilityStatementRestResourceSearchParamPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'definition', 'Reference(OperationDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'documentation', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceOperation', nil, 'CapabilityStatementRestResourceOperation', FHIRFactoryJs);
  defineCapabilityStatementRestResourceOperationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestInteraction', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestInteraction', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestInteraction', nil, 'CapabilityStatementRestInteraction', FHIRFactoryJs);
  defineCapabilityStatementRestInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessaging', 'endpoint', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'reliableCache', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'supportedMessage', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCapabilityStatementMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessaging', nil, 'CapabilityStatementMessaging', FHIRFactoryJs);
  defineCapabilityStatementMessagingPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint', 'protocol', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint', 'address', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEndpoint', nil, 'CapabilityStatementMessagingEndpoint', FHIRFactoryJs);
  defineCapabilityStatementMessagingEndpointPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingSupportedMessagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage', 'definition', 'Reference(MessageDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCapabilityStatementMessagingSupportedMessageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingSupportedMessage', nil, 'CapabilityStatementMessagingSupportedMessage', FHIRFactoryJs);
  defineCapabilityStatementMessagingSupportedMessagePropsJs(js, def);
end;


procedure defineCapabilityStatementDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementDocument', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCapabilityStatementDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementDocument', nil, 'CapabilityStatementDocument', FHIRFactoryJs);
  defineCapabilityStatementDocumentPropsJs(js, def);
end;


procedure defineCapabilityStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CapabilityStatement', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatement', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CapabilityStatement', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'software', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement', 'implementation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'acceptUnknown', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'rest', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'messaging', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'document', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCapabilityStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatement', nil, 'CapabilityStatement', FHIRFactoryJs);
  defineCapabilityStatementPropsJs(js, def);
end;


procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity', 'outcomeCodeableConcept', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'outcomeReference', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'progress', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'reference', 'Reference(Appointment|CommunicationRequest|DeviceRequest|MedicationRequest|NutritionOrder|Task|ServiceRequest|VisionPrescription|RequestGroup)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity', 'detail', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity', nil, 'CarePlanActivity', FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;


procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'instantiates', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'goal', 'Reference(Goal)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'statusReason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'prohibited', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|RelatedPerson|Patient|CareTeam|HealthcareService|Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'productCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'productReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'dailyAmount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail', nil, 'CarePlanActivityDetail', FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;


procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'basedOn', 'Reference(CarePlan)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'replaces', 'Reference(CarePlan)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'partOf', 'Reference(CarePlan)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'intent', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'author', 'Reference(Patient|Practitioner|PractitionerRole|Device|RelatedPerson|Organization|CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'careTeam', 'Reference(CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'addresses', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'supportingInfo', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'goal', 'Reference(Goal)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'activity', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan', nil, 'CarePlan', FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;


procedure defineCareTeamParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CareTeamParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'member', 'Reference(Practitioner|RelatedPerson|Patient|Organization|CareTeam)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'onBehalfOf', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCareTeamParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeamParticipant', nil, 'CareTeamParticipant', FHIRFactoryJs);
  defineCareTeamParticipantPropsJs(js, def);
end;


procedure defineCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CareTeam', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CareTeam', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CareTeam', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'reasonReference', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'managingOrganization', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeam', nil, 'CareTeam', FHIRFactoryJs);
  defineCareTeamPropsJs(js, def);
end;


procedure defineChargeItemParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemParticipant', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineChargeItemParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemParticipant', nil, 'ChargeItemParticipant', FHIRFactoryJs);
  defineChargeItemParticipantPropsJs(js, def);
end;


procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'partOf', 'Reference(ChargeItem)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'performingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'requestingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'bodysite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'factorOverride', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem', 'priceOverride', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'overrideReason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'enterer', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'enteredDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'service', 'Reference(DiagnosticReport|ImagingStudy|Immunization|MedicationAdministration|MedicationDispense|Observation|Procedure|SupplyDelivery)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'account', 'Reference(Account)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineChargeItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItem', nil, 'ChargeItem', FHIRFactoryJs);
  defineChargeItemPropsJs(js, def);
end;


procedure defineClaimRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimRelated', 'claim', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated', 'reference', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimRelated', nil, 'ClaimRelated', FHIRFactoryJs);
  defineClaimRelatedPropsJs(js, def);
end;


procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'resource', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee', nil, 'ClaimPayee', FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;


procedure defineClaimCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCareTeam', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCareTeam', 'provider', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam', 'responsible', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCareTeam', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam', 'qualification', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCareTeam', nil, 'ClaimCareTeam', FHIRFactoryJs);
  defineClaimCareTeamPropsJs(js, def);
end;


procedure defineClaimInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInformation', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInformation', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'timingDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimInformation', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimInformation', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInformation', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInformation', nil, 'ClaimInformation', FHIRFactoryJs);
  defineClaimInformationPropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimDiagnosis', 'packageCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis', nil, 'ClaimDiagnosis', FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;


procedure defineClaimProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimProcedure', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimProcedure', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimProcedure', nil, 'ClaimProcedure', FHIRFactoryJs);
  defineClaimProcedurePropsJs(js, def);
end;


procedure defineClaimInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInsurance', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInsurance', 'focal', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInsurance', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance', 'businessArrangement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimInsurance', 'claimResponse', 'Reference(ClaimResponse)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInsurance', nil, 'ClaimInsurance', FHIRFactoryJs);
  defineClaimInsurancePropsJs(js, def);
end;


procedure defineClaimAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimAccident', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimAccident', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident', 'locationAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident', 'locationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimAccident', nil, 'ClaimAccident', FHIRFactoryJs);
  defineClaimAccidentPropsJs(js, def);
end;


procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'servicedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem', 'servicedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'subSite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'encounter', 'Reference(Encounter)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem', nil, 'ClaimItem', FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;


procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'subDetail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail', nil, 'ClaimItemDetail', FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;


procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail', nil, 'ClaimItemDetailSubDetail', FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Claim', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'subType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Claim', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'billablePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Claim', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'fundsReserve', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'originalPrescription', 'Reference(MedicationRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'payee', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'referral', 'Reference(ServiceRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'facility', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'careTeam', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'information', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'diagnosis', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'procedure', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'insurance', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'accident', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'employmentImpacted', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'hospitalization', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'total', 'Money', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim', nil, 'Claim', FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;


procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem', 'itemSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem', nil, 'ClaimResponseItem', FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;


procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'amount', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication', nil, 'ClaimResponseItemAdjudication', FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail', 'detailSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'adjudication', '@ClaimResponse.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'subDetail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail', nil, 'ClaimResponseItemDetail', FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'subDetailSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'adjudication', '@ClaimResponse.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail', nil, 'ClaimResponseItemDetailSubDetail', FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'fee', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'adjudication', '@ClaimResponse.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem', nil, 'ClaimResponseAddItem', FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;


procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError', 'itemSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'detailSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'subDetailSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError', nil, 'ClaimResponseError', FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;


procedure defineClaimResponsePaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponsePayment', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'adjustment', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'adjustmentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponsePayment', 'amount', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponsePaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponsePayment', nil, 'ClaimResponsePayment', FHIRFactoryJs);
  defineClaimResponsePaymentPropsJs(js, def);
end;


procedure defineClaimResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseProcessNote', 'number', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'language', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseProcessNote', nil, 'ClaimResponseProcessNote', FHIRFactoryJs);
  defineClaimResponseProcessNotePropsJs(js, def);
end;


procedure defineClaimResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseInsurance', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'focal', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'businessArrangement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'claimResponse', 'Reference(ClaimResponse)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineClaimResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseInsurance', nil, 'ClaimResponseInsurance', FHIRFactoryJs);
  defineClaimResponseInsurancePropsJs(js, def);
end;


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'request', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'payeeType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'addItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'error', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'totalCost', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'unallocDeductable', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'totalBenefit', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'payment', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'reserved', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'processNote', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'communicationRequest', 'Reference(CommunicationRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'insurance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse', nil, 'ClaimResponse', FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;


procedure defineClinicalImpressionInvestigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigation', 'item', 'Reference(Observation|QuestionnaireResponse|FamilyMemberHistory|DiagnosticReport|RiskAssessment|ImagingStudy)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigation', nil, 'ClinicalImpressionInvestigation', FHIRFactoryJs);
  defineClinicalImpressionInvestigationPropsJs(js, def);
end;


procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding', 'itemCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding', 'itemReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding', 'basis', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding', nil, 'ClinicalImpressionFinding', FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;


procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'assessor', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'previous', 'Reference(ClinicalImpression)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'problem', 'Reference(Condition|AllergyIntolerance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'investigation', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'summary', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'finding', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisCodeableConcept', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisReference', 'Reference(RiskAssessment)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'action', 'Reference(ServiceRequest|Procedure|MedicationRequest|Appointment)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression', nil, 'ClinicalImpression', FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;


procedure defineCodeSystemFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemFilter', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCodeSystemFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemFilter', nil, 'CodeSystemFilter', FHIRFactoryJs);
  defineCodeSystemFilterPropsJs(js, def);
end;


procedure defineCodeSystemPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemProperty', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCodeSystemPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemProperty', nil, 'CodeSystemProperty', FHIRFactoryJs);
  defineCodeSystemPropertyPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConcept', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'definition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'designation', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept', 'property', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept', 'concept', '@CodeSystem.concept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConcept', nil, 'CodeSystemConcept', FHIRFactoryJs);
  defineCodeSystemConceptPropsJs(js, def);
end;


procedure defineCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'use', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptDesignation', nil, 'CodeSystemConceptDesignation', FHIRFactoryJs);
  defineCodeSystemConceptDesignationPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptProperty', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineCodeSystemConceptPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptProperty', nil, 'CodeSystemConceptProperty', FHIRFactoryJs);
  defineCodeSystemConceptPropertyPropsJs(js, def);
end;


procedure defineCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CodeSystem', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CodeSystem', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystem', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'caseSensitive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'valueSet', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'hierarchyMeaning', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'compositional', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'versionNeeded', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'content', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'supplements', 'Reference(CodeSystem)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CodeSystem', 'count', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'filter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'property', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'concept', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystem', nil, 'CodeSystem', FHIRFactoryJs);
  defineCodeSystemPropsJs(js, def);
end;


procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload', 'contentString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload', 'contentAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload', 'contentReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload', nil, 'CommunicationPayload', FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;


procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'basedOn', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'partOf', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'inResponseTo', 'Reference(Communication)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Communication', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Communication', 'medium', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson|Group|CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'topic', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'about', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'sent', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'received', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'sender', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'payload', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication', nil, 'Communication', FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;


procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload', nil, 'CommunicationRequestPayload', FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;


procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'basedOn', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'replaces', 'Reference(CommunicationRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'groupIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'medium', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'recipient', 'Reference(Device|Organization|Patient|Practitioner|RelatedPerson|Group|CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'about', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'payload', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'sender', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson|HealthcareService)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest', nil, 'CommunicationRequest', FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;


procedure defineCompartmentDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinitionResource', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinitionResource', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCompartmentDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinitionResource', nil, 'CompartmentDefinitionResource', FHIRFactoryJs);
  defineCompartmentDefinitionResourcePropsJs(js, def);
end;


procedure defineCompartmentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'search', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCompartmentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinition', nil, 'CompartmentDefinition', FHIRFactoryJs);
  defineCompartmentDefinitionPropsJs(js, def);
end;


procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester', 'time', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester', 'party', 'Reference(Patient|Practitioner|PractitionerRole|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester', nil, 'CompositionAttester', FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;


procedure defineCompositionRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionRelatesTo', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompositionRelatesTo', 'targetIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionRelatesTo', 'targetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCompositionRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionRelatesTo', nil, 'CompositionRelatesTo', FHIRFactoryJs);
  defineCompositionRelatesToPropsJs(js, def);
end;


procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent', 'detail', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent', nil, 'CompositionEvent', FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;


procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompositionSection', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'text', 'Narrative', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CompositionSection', 'orderedBy', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'entry', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection', 'emptyReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'section', '@Composition.section', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection', nil, 'CompositionSection', FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;


procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'class', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Composition', 'author', 'Reference(Practitioner|PractitionerRole|Device|Patient|RelatedPerson|Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'confidentiality', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Composition', 'attester', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'custodian', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'relatesTo', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'event', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'section', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition', nil, 'Composition', FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;


procedure defineConceptMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroup', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'sourceVersion', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'target', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'targetVersion', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'element', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroup', 'unmapped', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConceptMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroup', nil, 'ConceptMapGroup', FHIRFactoryJs);
  defineConceptMapGroupPropsJs(js, def);
end;


procedure defineConceptMapGroupElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElement', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElement', nil, 'ConceptMapGroupElement', FHIRFactoryJs);
  defineConceptMapGroupElementPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'equivalence', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'dependsOn', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'product', '@ConceptMap.group.element.target.dependsOn', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTarget', nil, 'ConceptMapGroupElementTarget', FHIRFactoryJs);
  defineConceptMapGroupElementTargetPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'property', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'code', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConceptMapGroupElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTargetDependsOn', nil, 'ConceptMapGroupElementTargetDependsOn', FHIRFactoryJs);
  defineConceptMapGroupElementTargetDependsOnPropsJs(js, def);
end;


procedure defineConceptMapGroupUnmappedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConceptMapGroupUnmappedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupUnmapped', nil, 'ConceptMapGroupUnmapped', FHIRFactoryJs);
  defineConceptMapGroupUnmappedPropsJs(js, def);
end;


procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'targetUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'targetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'group', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap', nil, 'ConceptMap', FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;


procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage', 'summary', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage', 'assessment', 'Reference(ClinicalImpression|DiagnosticReport|Observation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConditionStage', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage', nil, 'ConditionStage', FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;


procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConditionEvidence', 'detail', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence', nil, 'ConditionEvidence', FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;


procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'clinicalStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'verificationStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'severity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'onsetAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'abatementDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'abatementAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Condition', 'assertedDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'recorder', 'Reference(Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'asserter', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'stage', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'evidence', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition', nil, 'Condition', FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;


procedure defineConsentPolicyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentPolicy', 'authority', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConsentPolicy', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineConsentPolicyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentPolicy', nil, 'ConsentPolicy', FHIRFactoryJs);
  defineConsentPolicyPropsJs(js, def);
end;


procedure defineConsentVerificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentVerification', 'verified', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ConsentVerification', 'verifiedWith', 'Reference(Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConsentVerification', 'verificationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineConsentVerificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentVerification', nil, 'ConsentVerification', FHIRFactoryJs);
  defineConsentVerificationPropsJs(js, def);
end;


procedure defineConsentProvisionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvision', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConsentProvision', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision', 'actor', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'action', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'securityLabel', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'purpose', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'class', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'dataPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision', 'data', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'provision', '@Consent.provision', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineConsentProvisionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvision', nil, 'ConsentProvision', FHIRFactoryJs);
  defineConsentProvisionPropsJs(js, def);
end;


procedure defineConsentProvisionActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionActor', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvisionActor', 'reference', 'Reference(Device|Group|CareTeam|Organization|Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConsentProvisionActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionActor', nil, 'ConsentProvisionActor', FHIRFactoryJs);
  defineConsentProvisionActorPropsJs(js, def);
end;


procedure defineConsentProvisionDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionData', 'meaning', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ConsentProvisionData', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConsentProvisionDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionData', nil, 'ConsentProvisionData', FHIRFactoryJs);
  defineConsentProvisionDataPropsJs(js, def);
end;


procedure defineConsentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Consent', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Consent', 'scope', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Consent', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'dateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Consent', 'consentingParty', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'organization', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'sourceAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'sourceIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'sourceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'policy', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'policyRule', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'verification', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'provision', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineConsentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Consent', nil, 'Consent', FHIRFactoryJs);
  defineConsentPropsJs(js, def);
end;


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'issued', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm', 'applies', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'subType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'offer', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'asset', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'agent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'action', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'actionReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'group', '@Contract.term', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm', nil, 'ContractTerm', FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermOfferPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOffer', 'topic', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'decision', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContractTermOffer', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineContractTermOfferJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOffer', nil, 'ContractTermOffer', FHIRFactoryJs);
  defineContractTermOfferPropsJs(js, def);
end;


procedure defineContractTermAssetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAsset', 'class', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'dataPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'data', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'valuedItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'securityLabel', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractTermAssetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAsset', nil, 'ContractTermAsset', FHIRFactoryJs);
  defineContractTermAssetPropsJs(js, def);
end;


procedure defineContractTermAssetDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetData', 'meaning', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ContractTermAssetData', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractTermAssetDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetData', nil, 'ContractTermAssetData', FHIRFactoryJs);
  defineContractTermAssetDataPropsJs(js, def);
end;


procedure defineContractTermAssetValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'entityCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'entityReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'effectiveTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'points', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractTermAssetValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetValuedItem', nil, 'ContractTermAssetValuedItem', FHIRFactoryJs);
  defineContractTermAssetValuedItemPropsJs(js, def);
end;


procedure defineContractTermAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAgent', 'actor', 'Reference(Contract|Device|Group|Location|Organization|Patient|Practitioner|RelatedPerson|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAgent', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractTermAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAgent', nil, 'ContractTermAgent', FHIRFactoryJs);
  defineContractTermAgentPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'party', 'Reference(Organization|Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'signature', 'Signature', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner', nil, 'ContractSigner', FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;


procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly', 'contentAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly', 'contentReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly', nil, 'ContractFriendly', FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;


procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal', 'contentAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal', 'contentReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal', nil, 'ContractLegal', FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;


procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule', 'contentAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ContractRule', 'contentReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule', nil, 'ContractRule', FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;


procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Contract', 'contentDerivative', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'issued', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Contract', 'applies', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subject', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'authority', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'domain', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'term', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'signer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'friendly', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'legal', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'rule', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'legallyBindingAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'legallyBindingReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract', nil, 'Contract', FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;


procedure defineCoverageClassPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageClass', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CoverageClass', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageClass', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCoverageClassJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageClass', nil, 'CoverageClass', FHIRFactoryJs);
  defineCoverageClassPropsJs(js, def);
end;


procedure defineCoverageGroupingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageGrouping', 'group', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'groupDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subGroup', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subGroupDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'plan', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'planDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subPlan', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subPlanDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'class', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'classDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subClass', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'CoverageGrouping', 'subClassDisplay', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineCoverageGroupingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageGrouping', nil, 'CoverageGrouping', FHIRFactoryJs);
  defineCoverageGroupingPropsJs(js, def);
end;


procedure defineCoverageCopayPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCopay', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'CoverageCopay', 'value', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineCoverageCopayJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCopay', nil, 'CoverageCopay', FHIRFactoryJs);
  defineCoverageCopayPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'policyHolder', 'Reference(Patient|RelatedPerson|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriber', 'Reference(Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriberId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'beneficiary', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'dependent', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'payor', 'Reference(Organization|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'class', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'grouping', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'sequence', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'order', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Coverage', 'network', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'copay', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'contract', 'Reference(Contract)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage', nil, 'Coverage', FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;


procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation', 'action', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation', 'author', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation', nil, 'DetectedIssueMitigation', FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;


procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue', 'author', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'implicated', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue', 'detail', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'reference', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'mitigation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue', nil, 'DetectedIssue', FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;


procedure defineDeviceUdiPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdi', 'deviceIdentifier', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'jurisdiction', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'carrierHRF', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'carrierAIDC', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdi', 'issuer', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUdi', 'entryType', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineDeviceUdiJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdi', nil, 'DeviceUdi', FHIRFactoryJs);
  defineDeviceUdiPropsJs(js, def);
end;


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'udi', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufacturer', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufactureDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'expirationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'model', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'owner', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'contact', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Device', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Device', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Device', 'safety', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device', nil, 'Device', FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;


procedure defineDeviceComponentProductionSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'specType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'componentId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProductionSpecification', 'productionSpec', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineDeviceComponentProductionSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProductionSpecification', nil, 'DeviceComponentProductionSpecification', FHIRFactoryJs);
  defineDeviceComponentProductionSpecificationPropsJs(js, def);
end;


procedure defineDeviceComponentPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceComponentProperty', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponentProperty', 'valueQuantity', 'Quantity', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponentProperty', 'valueCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceComponentPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponentProperty', nil, 'DeviceComponentProperty', FHIRFactoryJs);
  defineDeviceComponentPropertyPropsJs(js, def);
end;


procedure defineDeviceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceComponent', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'lastSystemChange', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceComponent', 'source', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'parent', 'Reference(DeviceComponent)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'operationalStatus', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'parameterGroup', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'measurementPrinciple', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceComponent', 'productionSpecification', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceComponent', 'languageCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceComponent', 'property', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceComponent', nil, 'DeviceComponent', FHIRFactoryJs);
  defineDeviceComponentPropsJs(js, def);
end;


procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration', 'state', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration', 'time', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration', nil, 'DeviceMetricCalibration', FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;


procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceMetric', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'unit', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'source', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'parent', 'Reference(DeviceComponent)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'operationalStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'color', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'category', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'measurementPeriod', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'calibration', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric', nil, 'DeviceMetric', FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;


procedure defineDeviceRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceRequestParameter', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineDeviceRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequestParameter', nil, 'DeviceRequestParameter', FHIRFactoryJs);
  defineDeviceRequestParameterPropsJs(js, def);
end;


procedure defineDeviceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'basedOn', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'priorRequest', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'groupIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'intent', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'codeReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'codeCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'subject', 'Reference(Patient|Group|Location|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'requester', 'Reference(Device|Practitioner|PractitionerRole|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performerType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|HealthcareService|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'supportingInfo', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'relevantHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequest', nil, 'DeviceRequest', FHIRFactoryJs);
  defineDeviceRequestPropsJs(js, def);
end;


procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'basedOn', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DeviceUseStatement', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'derivedFrom', 'Reference(ServiceRequest|Procedure|Claim|Observation|QuestionnaireResponse|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'recordedOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'device', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference|Media)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement', nil, 'DeviceUseStatement', FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;


procedure defineDiagnosticReportMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportMedia', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportMedia', 'link', 'Reference(Media)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDiagnosticReportMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportMedia', nil, 'DiagnosticReportMedia', FHIRFactoryJs);
  defineDiagnosticReportMediaPropsJs(js, def);
end;


procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'basedOn', 'Reference(CarePlan|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'issued', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'resultsInterpreter', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'result', 'Reference(Observation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'imagingStudy', 'Reference(ImagingStudy)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'media', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'conclusion', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'codedDiagnosis', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'presentedForm', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport', nil, 'DiagnosticReport', FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;


procedure defineDocumentManifestAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestAgent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestAgent', 'who', 'Reference(Practitioner|PractitionerRole|Organization|Device|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentManifestAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestAgent', nil, 'DocumentManifestAgent', FHIRFactoryJs);
  defineDocumentManifestAgentPropsJs(js, def);
end;


procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated', 'ref', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated', nil, 'DocumentManifestRelated', FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;


procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest', 'masterIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'subject', 'Reference(Patient|Practitioner|Group|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest', 'agent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'recipient', 'Reference(Patient|Practitioner|PractitionerRole|RelatedPerson|Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'content', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest', nil, 'DocumentManifest', FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;


procedure defineDocumentReferenceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceAgent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceAgent', 'who', 'Reference(Practitioner|PractitionerRole|Organization|Device|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentReferenceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceAgent', nil, 'DocumentReferenceAgent', FHIRFactoryJs);
  defineDocumentReferenceAgentPropsJs(js, def);
end;


procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo', 'target', 'Reference(DocumentReference)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo', nil, 'DocumentReferenceRelatesTo', FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;


procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent', 'attachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent', 'format', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent', nil, 'DocumentReferenceContent', FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;


procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'event', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'facilityType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'practiceSetting', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'sourcePatientInfo', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext', nil, 'DocumentReferenceContext', FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;


procedure defineDocumentReferenceContextRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContextRelated', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContextRelated', 'ref', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentReferenceContextRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContextRelated', nil, 'DocumentReferenceContextRelated', FHIRFactoryJs);
  defineDocumentReferenceContextRelatedPropsJs(js, def);
end;


procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference', 'masterIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'docStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'class', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'subject', 'Reference(Patient|Practitioner|Group|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'date', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'agent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'authenticator', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'custodian', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'relatesTo', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'securityLabel', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'content', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'context', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference', nil, 'DocumentReference', FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;


procedure defineEligibilityRequestAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequestAuthorization', 'facility', 'Reference(Location|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEligibilityRequestAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequestAuthorization', nil, 'EligibilityRequestAuthorization', FHIRFactoryJs);
  defineEligibilityRequestAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'servicedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest', 'servicedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityRequest', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'facility', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'businessArrangement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityRequest', 'benefitCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'benefitSubCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityRequest', 'authorization', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityRequest', nil, 'EligibilityRequest', FHIRFactoryJs);
  defineEligibilityRequestPropsJs(js, def);
end;


procedure defineEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsurance', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance', 'contract', 'Reference(Contract)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsurance', 'benefitBalance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsurance', nil, 'EligibilityResponseInsurance', FHIRFactoryJs);
  defineEligibilityResponseInsurancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'subCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'excluded', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'network', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'unit', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'term', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalance', 'financial', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalance', nil, 'EligibilityResponseInsuranceBenefitBalance', FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalancePropsJs(js, def);
end;


procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'allowedMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'usedUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponseInsuranceBenefitBalanceFinancial', 'usedMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseInsuranceBenefitBalanceFinancial', nil, 'EligibilityResponseInsuranceBenefitBalanceFinancial', FHIRFactoryJs);
  defineEligibilityResponseInsuranceBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineEligibilityResponseAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'authorizationSequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'required', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponseAuthorization', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEligibilityResponseAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseAuthorization', nil, 'EligibilityResponseAuthorization', FHIRFactoryJs);
  defineEligibilityResponseAuthorizationPropsJs(js, def);
end;


procedure defineEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EligibilityResponseError', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponseError', nil, 'EligibilityResponseError', FHIRFactoryJs);
  defineEligibilityResponseErrorPropsJs(js, def);
end;


procedure defineEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EligibilityResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EligibilityResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'request', 'Reference(EligibilityRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'inforce', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'EligibilityResponse', 'insurance', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'preAuthRef', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EligibilityResponse', 'authorization', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EligibilityResponse', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EligibilityResponse', 'error', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EligibilityResponse', nil, 'EligibilityResponse', FHIRFactoryJs);
  defineEligibilityResponsePropsJs(js, def);
end;


procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory', nil, 'EncounterStatusHistory', FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;


procedure defineEncounterClassHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterClassHistory', 'class', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterClassHistory', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterClassHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterClassHistory', nil, 'EncounterClassHistory', FHIRFactoryJs);
  defineEncounterClassHistoryPropsJs(js, def);
end;


procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant', 'individual', 'Reference(Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant', nil, 'EncounterParticipant', FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;


procedure defineEncounterDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterDiagnosis', 'condition', 'Reference(Condition|Procedure)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis', 'rank', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineEncounterDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterDiagnosis', nil, 'EncounterDiagnosis', FHIRFactoryJs);
  defineEncounterDiagnosisPropsJs(js, def);
end;


procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization', 'preAdmissionIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'origin', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'admitSource', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'reAdmission', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dietPreference', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialCourtesy', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialArrangement', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'destination', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dischargeDisposition', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization', nil, 'EncounterHospitalization', FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;


procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation', nil, 'EncounterLocation', FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;


procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Encounter', 'statusHistory', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'class', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'classHistory', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'serviceType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'episodeOfCare', 'Reference(EpisodeOfCare)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'incomingReferral', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'appointment', 'Reference(Appointment)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'length', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'diagnosis', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'account', 'Reference(Account)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'hospitalization', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'location', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'serviceProvider', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'partOf', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter', nil, 'Encounter', FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;


procedure defineEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Endpoint', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Endpoint', 'connectionType', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Endpoint', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'contact', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'payloadType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'address', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Endpoint', nil, 'Endpoint', FHIRFactoryJs);
  defineEndpointPropsJs(js, def);
end;


procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'candidate', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest', nil, 'EnrollmentRequest', FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;


procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'request', 'Reference(EnrollmentRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse', nil, 'EnrollmentResponse', FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;


procedure defineEntryDefinitionRelatedEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EntryDefinitionRelatedEntry', 'relationtype', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinitionRelatedEntry', 'item', 'Reference(EntryDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEntryDefinitionRelatedEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinitionRelatedEntry', nil, 'EntryDefinitionRelatedEntry', FHIRFactoryJs);
  defineEntryDefinitionRelatedEntryPropsJs(js, def);
end;


procedure defineEntryDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EntryDefinition', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'purpose', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'referencedItem', 'Reference(Medication|Device|Organization|Practitioner|HealthcareService|ActivityDefinition|PlanDefinition|SpecimenDefinition|ObservationDefinition|Binary)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'additionalIdentifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'classification', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'validityPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EntryDefinition', 'lastUpdated', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EntryDefinition', 'additionalCharacteristic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'additionalClassification', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EntryDefinition', 'relatedEntry', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEntryDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EntryDefinition', nil, 'EntryDefinition', FHIRFactoryJs);
  defineEntryDefinitionPropsJs(js, def);
end;


procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory', nil, 'EpisodeOfCareStatusHistory', FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;


procedure defineEpisodeOfCareDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'condition', 'Reference(Condition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'rank', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineEpisodeOfCareDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareDiagnosis', nil, 'EpisodeOfCareDiagnosis', FHIRFactoryJs);
  defineEpisodeOfCareDiagnosisPropsJs(js, def);
end;


procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare', 'statusHistory', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'diagnosis', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'referralRequest', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'careManager', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'team', 'Reference(CareTeam)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'account', 'Reference(Account)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare', nil, 'EpisodeOfCare', FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;


procedure defineEventDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EventDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'EventDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'trigger', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineEventDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EventDefinition', nil, 'EventDefinition', FHIRFactoryJs);
  defineEventDefinitionPropsJs(js, def);
end;


procedure defineExampleScenarioActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioActor', 'actorId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineExampleScenarioActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioActor', nil, 'ExampleScenarioActor', FHIRFactoryJs);
  defineExampleScenarioActorPropsJs(js, def);
end;


procedure defineExampleScenarioInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstance', 'resourceId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'resourceType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'version', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'containedInstance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExampleScenarioInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstance', nil, 'ExampleScenarioInstance', FHIRFactoryJs);
  defineExampleScenarioInstancePropsJs(js, def);
end;


procedure defineExampleScenarioInstanceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceVersion', 'versionId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceVersion', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceVersion', nil, 'ExampleScenarioInstanceVersion', FHIRFactoryJs);
  defineExampleScenarioInstanceVersionPropsJs(js, def);
end;


procedure defineExampleScenarioInstanceContainedInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance', 'resourceId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance', 'versionId', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceContainedInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceContainedInstance', nil, 'ExampleScenarioInstanceContainedInstance', FHIRFactoryJs);
  defineExampleScenarioInstanceContainedInstancePropsJs(js, def);
end;


procedure defineExampleScenarioProcessPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcess', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'preConditions', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'postConditions', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'step', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcess', nil, 'ExampleScenarioProcess', FHIRFactoryJs);
  defineExampleScenarioProcessPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'process', '@ExampleScenario.process', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'pause', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'operation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'alternative', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStep', nil, 'ExampleScenarioProcessStep', FHIRFactoryJs);
  defineExampleScenarioProcessStepPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'number', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'initiator', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'receiver', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'initiatorActive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'receiverActive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'request', '@ExampleScenario.instance.containedInstance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'response', '@ExampleScenario.instance.containedInstance', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepOperation', nil, 'ExampleScenarioProcessStepOperation', FHIRFactoryJs);
  defineExampleScenarioProcessStepOperationPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'option', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternative', nil, 'ExampleScenarioProcessStepAlternative', FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativePropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativeOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternativeOption', 'step', '@ExampleScenario.process.step', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternativeOption', nil, 'ExampleScenarioProcessStepAlternativeOption', FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativeOptionPropsJs(js, def);
end;


procedure defineExampleScenarioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExampleScenario', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenario', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExampleScenario', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'actor', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'instance', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'process', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenario', 'workflow', 'Reference(ExampleScenario)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExampleScenarioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenario', nil, 'ExampleScenario', FHIRFactoryJs);
  defineExampleScenarioPropsJs(js, def);
end;


procedure defineExpansionProfileFixedVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileFixedVersion', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineExpansionProfileFixedVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileFixedVersion', nil, 'ExpansionProfileFixedVersion', FHIRFactoryJs);
  defineExpansionProfileFixedVersionPropsJs(js, def);
end;


procedure defineExpansionProfileExcludedSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileExcludedSystem', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileExcludedSystem', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineExpansionProfileExcludedSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileExcludedSystem', nil, 'ExpansionProfileExcludedSystem', FHIRFactoryJs);
  defineExpansionProfileExcludedSystemPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignation', 'include', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfileDesignation', 'exclude', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignation', nil, 'ExpansionProfileDesignation', FHIRFactoryJs);
  defineExpansionProfileDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationInclude', 'designation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationInclude', nil, 'ExpansionProfileDesignationInclude', FHIRFactoryJs);
  defineExpansionProfileDesignationIncludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationIncludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationIncludeDesignation', 'use', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationIncludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationIncludeDesignation', nil, 'ExpansionProfileDesignationIncludeDesignation', FHIRFactoryJs);
  defineExpansionProfileDesignationIncludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExclude', 'designation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExpansionProfileDesignationExcludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExclude', nil, 'ExpansionProfileDesignationExclude', FHIRFactoryJs);
  defineExpansionProfileDesignationExcludePropsJs(js, def);
end;


procedure defineExpansionProfileDesignationExcludeDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfileDesignationExcludeDesignation', 'use', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExpansionProfileDesignationExcludeDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfileDesignationExcludeDesignation', nil, 'ExpansionProfileDesignationExcludeDesignation', FHIRFactoryJs);
  defineExpansionProfileDesignationExcludeDesignationPropsJs(js, def);
end;


procedure defineExpansionProfilePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExpansionProfile', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExpansionProfile', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'fixedVersion', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExpansionProfile', 'excludedSystem', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'includeDesignations', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'designation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExpansionProfile', 'includeDefinition', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'activeOnly', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludeNested', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludeNotForUI', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'excludePostCoordinated', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExpansionProfile', 'displayLanguage', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExpansionProfile', 'limitedExpansion', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineExpansionProfileJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExpansionProfile', nil, 'ExpansionProfile', FHIRFactoryJs);
  defineExpansionProfilePropsJs(js, def);
end;


procedure defineExplanationOfBenefitRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'claim', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'reference', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitRelated', nil, 'ExplanationOfBenefitRelated', FHIRFactoryJs);
  defineExplanationOfBenefitRelatedPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'resource', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'party', 'Reference(Practitioner|Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee', nil, 'ExplanationOfBenefitPayee', FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'timingDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitInformation', 'reason', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInformation', nil, 'ExplanationOfBenefitInformation', FHIRFactoryJs);
  defineExplanationOfBenefitInformationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'provider', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'responsible', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'qualification', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitCareTeam', nil, 'ExplanationOfBenefitCareTeam', FHIRFactoryJs);
  defineExplanationOfBenefitCareTeamPropsJs(js, def);
end;


procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'packageCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitDiagnosis', nil, 'ExplanationOfBenefitDiagnosis', FHIRFactoryJs);
  defineExplanationOfBenefitDiagnosisPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcedure', nil, 'ExplanationOfBenefitProcedure', FHIRFactoryJs);
  defineExplanationOfBenefitProcedurePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInsurance', 'coverage', 'Reference(Coverage)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInsurance', nil, 'ExplanationOfBenefitInsurance', FHIRFactoryJs);
  defineExplanationOfBenefitInsurancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'locationAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'locationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAccident', nil, 'ExplanationOfBenefitAccident', FHIRFactoryJs);
  defineExplanationOfBenefitAccidentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'servicedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'servicedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'subSite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'encounter', 'Reference(Encounter)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'adjudication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItem', nil, 'ExplanationOfBenefitItem', FHIRFactoryJs);
  defineExplanationOfBenefitItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'amount', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'value', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineExplanationOfBenefitItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemAdjudication', nil, 'ExplanationOfBenefitItemAdjudication', FHIRFactoryJs);
  defineExplanationOfBenefitItemAdjudicationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'subDetail', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetail', nil, 'ExplanationOfBenefitItemDetail', FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'revenue', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'programCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'unitPrice', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'factor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'net', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'udi', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetailSubDetail', nil, 'ExplanationOfBenefitItemDetailSubDetail', FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'service', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'modifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'fee', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'adjudication', '@ExplanationOfBenefit.item.adjudication', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem', nil, 'ExplanationOfBenefitAddItem', FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'adjustment', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'adjustmentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'amount', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayment', nil, 'ExplanationOfBenefitPayment', FHIRFactoryJs);
  defineExplanationOfBenefitPaymentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'number', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'language', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcessNote', nil, 'ExplanationOfBenefitProcessNote', FHIRFactoryJs);
  defineExplanationOfBenefitProcessNotePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'subCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'excluded', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'network', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'unit', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'term', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'financial', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalance', nil, 'ExplanationOfBenefitBenefitBalance', FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'usedUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'usedMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalanceFinancial', nil, 'ExplanationOfBenefitBenefitBalanceFinancial', FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'subType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'billablePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'referral', 'Reference(ServiceRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'facility', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claim', 'Reference(Claim)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claimResponse', 'Reference(ClaimResponse)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'related', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'prescription', 'Reference(MedicationRequest|VisionPrescription)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'originalPrescription', 'Reference(MedicationRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payee', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'information', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'careTeam', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'diagnosis', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'procedure', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'precedence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurance', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'accident', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'employmentImpacted', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'hospitalization', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'addItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'totalCost', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'unallocDeductable', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'totalBenefit', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payment', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'processNote', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'benefitBalance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit', nil, 'ExplanationOfBenefit', FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition', nil, 'FamilyMemberHistoryCondition', FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'dataAbsentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'estimatedAge', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'reasonReference', 'Reference(Condition|Observation|AllergyIntolerance|QuestionnaireResponse|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'condition', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory', nil, 'FamilyMemberHistory', FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;


procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Flag', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Flag', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Flag', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'subject', 'Reference(Patient|Location|Group|Organization|Practitioner|PlanDefinition|Medication|Procedure)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'author', 'Reference(Device|Organization|Patient|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag', nil, 'Flag', FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;


procedure defineGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalTarget', 'measure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'dueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'GoalTarget', 'dueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalTarget', nil, 'GoalTarget', FHIRFactoryJs);
  defineGoalTargetPropsJs(js, def);
end;


procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Goal', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'description', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'subject', 'Reference(Patient|Group|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'startDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'startCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'target', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'statusDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'statusReason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Goal', 'expressedBy', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'addresses', 'Reference(Condition|Observation|MedicationStatement|NutritionOrder|ServiceRequest|RiskAssessment)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'outcomeCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'outcomeReference', 'Reference(Observation)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal', nil, 'Goal', FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLink', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'sliceName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'GraphDefinitionLink', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLink', nil, 'GraphDefinitionLink', FHIRFactoryJs);
  defineGraphDefinitionLinkPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'params', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'profile', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'compartment', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'link', '@GraphDefinition.link', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTarget', nil, 'GraphDefinitionLinkTarget', FHIRFactoryJs);
  defineGraphDefinitionLinkTargetPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetCompartmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'rule', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineGraphDefinitionLinkTargetCompartmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTargetCompartment', nil, 'GraphDefinitionLinkTargetCompartment', FHIRFactoryJs);
  defineGraphDefinitionLinkTargetCompartmentPropsJs(js, def);
end;


procedure defineGraphDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'GraphDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'GraphDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'GraphDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'start', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'profile', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'link', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGraphDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinition', nil, 'GraphDefinition', FHIRFactoryJs);
  defineGraphDefinitionPropsJs(js, def);
end;


procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'exclude', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic', nil, 'GroupCharacteristic', FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;


procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember', 'entity', 'Reference(Patient|Practitioner|Device|Medication|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupMember', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GroupMember', 'inactive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember', nil, 'GroupMember', FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;


procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Group', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Group', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Group', 'actual', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Group', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Group', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Group', 'quantity', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Group', 'characteristic', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Group', 'member', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group', nil, 'Group', FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;


procedure defineGuidanceResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'GuidanceResponse', 'requestId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'module', 'Reference(ServiceDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse', 'performer', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'evaluationMessage', 'Reference(OperationOutcome)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'outputParameters', 'Reference(Parameters)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'result', 'Reference(CarePlan|RequestGroup)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'dataRequirement', 'DataRequirement', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse', nil, 'GuidanceResponse', FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
end;


procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'allDay', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'availableStartTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'availableEndTime', 'time', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime', nil, 'HealthcareServiceAvailableTime', FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;


procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable', 'during', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable', nil, 'HealthcareServiceNotAvailable', FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;


procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService', 'providedBy', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'location', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'extraDetails', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'photo', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'coverageArea', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'serviceProvisionCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'eligibility', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'eligibilityNote', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'characteristic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'referralMethod', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'appointmentRequired', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService', 'availableTime', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'notAvailable', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'availabilityExceptions', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService', nil, 'HealthcareService', FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;


procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'number', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'modality', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'numberOfInstances', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'availability', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'bodySite', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'laterality', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'started', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'instance', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries', nil, 'ImagingStudySeries', FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;


procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'number', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'sopClass', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance', nil, 'ImagingStudySeriesInstance', FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;


procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy', 'uid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'accession', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'availability', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'modalityList', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'subject', 'Reference(Patient|Device|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'started', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy', 'basedOn', 'Reference(CarePlan|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'referrer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'interpreter', 'Reference(Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfSeries', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfInstances', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'procedureReference', 'Reference(Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'procedureCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'series', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy', nil, 'ImagingStudy', FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;


procedure defineImmunizationPractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPractitioner', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPractitioner', 'actor', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImmunizationPractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPractitioner', nil, 'ImmunizationPractitioner', FHIRFactoryJs);
  defineImmunizationPractitionerPropsJs(js, def);
end;


procedure defineImmunizationEducationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationEducation', 'documentType', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation', 'reference', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation', 'publicationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEducation', 'presentationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineImmunizationEducationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEducation', nil, 'ImmunizationEducation', FHIRFactoryJs);
  defineImmunizationEducationPropsJs(js, def);
end;


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'vaccineCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'primarySource', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'reportOrigin', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'manufacturer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'expirationDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'site', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'doseQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'practitioner', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'education', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'programEligibility', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'fundingSource', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization', nil, 'Immunization', FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;


procedure defineImmunizationEvaluationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationEvaluation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'authority', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'targetDisease', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'immunizationEvent', 'Reference(Immunization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatusReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'series', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseNumber', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'seriesDoses', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineImmunizationEvaluationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEvaluation', nil, 'ImmunizationEvaluation', FHIRFactoryJs);
  defineImmunizationEvaluationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'vaccineCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'targetDisease', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'contraindicatedVaccineCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'forecastStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'forecastReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'dateCriterion', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'series', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'doseNumber', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'seriesDoses', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingImmunization', 'Reference(Immunization|ImmunizationEvaluation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingPatientInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation', nil, 'ImmunizationRecommendationRecommendation', FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion', 'value', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion', nil, 'ImmunizationRecommendationRecommendationDateCriterion', FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;


procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'authority', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'recommendation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation', nil, 'ImmunizationRecommendation', FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;


procedure defineImplementationGuideDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependency', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependency', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImplementationGuideDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependency', nil, 'ImplementationGuideDependency', FHIRFactoryJs);
  defineImplementationGuideDependencyPropsJs(js, def);
end;


procedure defineImplementationGuidePackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackage', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackage', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuidePackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackage', nil, 'ImplementationGuidePackage', FHIRFactoryJs);
  defineImplementationGuidePackagePropsJs(js, def);
end;


procedure defineImplementationGuidePackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'example', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'acronym', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'sourceUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'sourceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuidePackageResource', 'exampleFor', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuidePackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePackageResource', nil, 'ImplementationGuidePackageResource', FHIRFactoryJs);
  defineImplementationGuidePackageResourcePropsJs(js, def);
end;


procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal', nil, 'ImplementationGuideGlobal', FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;


procedure defineImplementationGuidePagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuidePage', 'source', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'format', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuidePage', 'page', '@ImplementationGuide.page', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuidePageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuidePage', nil, 'ImplementationGuidePage', FHIRFactoryJs);
  defineImplementationGuidePagePropsJs(js, def);
end;


procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'dependency', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'package', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'global', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'page', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide', nil, 'ImplementationGuide', FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;


procedure defineImplementationGuideInputDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputDependency', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputDependency', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImplementationGuideInputDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputDependency', nil, 'ImplementationGuideInputDependency', FHIRFactoryJs);
  defineImplementationGuideInputDependencyPropsJs(js, def);
end;


procedure defineImplementationGuideInputPackagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackage', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuideInputPackageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPackage', nil, 'ImplementationGuideInputPackage', FHIRFactoryJs);
  defineImplementationGuideInputPackagePropsJs(js, def);
end;


procedure defineImplementationGuideInputPackageResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'exampleBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideInputPackageResource', 'exampleReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideInputPackageResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPackageResource', nil, 'ImplementationGuideInputPackageResource', FHIRFactoryJs);
  defineImplementationGuideInputPackageResourcePropsJs(js, def);
end;


procedure defineImplementationGuideInputGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputGlobal', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputGlobal', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideInputGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputGlobal', nil, 'ImplementationGuideInputGlobal', FHIRFactoryJs);
  defineImplementationGuideInputGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideInputPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInputPage', 'sourceUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'sourceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'format', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInputPage', 'page', '@ImplementationGuideInput.page', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuideInputPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInputPage', nil, 'ImplementationGuideInputPage', FHIRFactoryJs);
  defineImplementationGuideInputPagePropsJs(js, def);
end;


procedure defineImplementationGuideInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideInput', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideInput', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuideInput', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideInput', 'dependency', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'package', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'global', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideInput', 'page', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideInput', nil, 'ImplementationGuideInput', FHIRFactoryJs);
  defineImplementationGuideInputPropsJs(js, def);
end;


procedure defineImplementationGuideOutputDependencyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputDependency', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputDependency', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImplementationGuideOutputDependencyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputDependency', nil, 'ImplementationGuideOutputDependency', FHIRFactoryJs);
  defineImplementationGuideOutputDependencyPropsJs(js, def);
end;


procedure defineImplementationGuideOutputResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'reference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'exampleBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'exampleReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideOutputResource', 'relativePath', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImplementationGuideOutputResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputResource', nil, 'ImplementationGuideOutputResource', FHIRFactoryJs);
  defineImplementationGuideOutputResourcePropsJs(js, def);
end;


procedure defineImplementationGuideOutputGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputGlobal', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputGlobal', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineImplementationGuideOutputGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputGlobal', nil, 'ImplementationGuideOutputGlobal', FHIRFactoryJs);
  defineImplementationGuideOutputGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideOutputPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutputPage', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutputPage', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineImplementationGuideOutputPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutputPage', nil, 'ImplementationGuideOutputPage', FHIRFactoryJs);
  defineImplementationGuideOutputPagePropsJs(js, def);
end;


procedure defineImplementationGuideOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideOutput', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'dependency', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'resource', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'global', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'rendering', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideOutput', 'page', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineImplementationGuideOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideOutput', nil, 'ImplementationGuideOutput', FHIRFactoryJs);
  defineImplementationGuideOutputPropsJs(js, def);
end;


procedure defineInvoiceParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'InvoiceParticipant', 'actor', 'Reference(Practitioner|Organization|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineInvoiceParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceParticipant', nil, 'InvoiceParticipant', FHIRFactoryJs);
  defineInvoiceParticipantPropsJs(js, def);
end;


procedure defineInvoiceLineItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItem', 'sequence', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'InvoiceLineItem', 'chargeItem', 'Reference(ChargeItem)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem', 'priceComponent', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineInvoiceLineItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItem', nil, 'InvoiceLineItem', FHIRFactoryJs);
  defineInvoiceLineItemPropsJs(js, def);
end;


procedure defineInvoiceLineItemPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'factor', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'amount', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineInvoiceLineItemPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItemPriceComponent', nil, 'InvoiceLineItemPriceComponent', FHIRFactoryJs);
  defineInvoiceLineItemPriceComponentPropsJs(js, def);
end;


procedure defineInvoicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Invoice', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'cancelledReason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'recipient', 'Reference(Organization|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Invoice', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'issuer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'account', 'Reference(Account)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'lineItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'totalPriceComponent', '@Invoice.lineItem.priceComponent', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'totalNet', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'totalGross', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'paymentTerms', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineInvoiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Invoice', nil, 'Invoice', FHIRFactoryJs);
  defineInvoicePropsJs(js, def);
end;


procedure defineItemInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ItemInstance', 'count', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ItemInstance', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ItemInstance', 'manufactureDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance', 'expiryDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ItemInstance', 'currentSWVersion', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'serialNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'carrierAIDC', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ItemInstance', 'carrierHRF', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineItemInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ItemInstance', nil, 'ItemInstance', FHIRFactoryJs);
  defineItemInstancePropsJs(js, def);
end;


procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Library', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Library', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Library', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Library', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'parameter', 'ParameterDefinition', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'dataRequirement', 'DataRequirement', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Library', 'content', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineLibraryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Library', nil, 'Library', FHIRFactoryJs);
  defineLibraryPropsJs(js, def);
end;


procedure defineLinkageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LinkageItem', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'LinkageItem', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineLinkageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LinkageItem', nil, 'LinkageItem', FHIRFactoryJs);
  defineLinkageItemPropsJs(js, def);
end;


procedure defineLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Linkage', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Linkage', 'author', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Linkage', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Linkage', nil, 'Linkage', FHIRFactoryJs);
  defineLinkagePropsJs(js, def);
end;


procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry', 'flag', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ListEntry', 'deleted', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry', 'item', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry', nil, 'ListEntry', FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;


procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'List', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'List', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'List', 'source', 'Reference(Practitioner|PractitionerRole|Patient|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'orderedBy', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'List', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'List', 'entry', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'List', 'emptyReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List', nil, 'List', FHIRFactoryJs);
  defineListPropsJs(js, def);
end;


procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition', 'longitude', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition', 'latitude', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition', 'altitude', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition', nil, 'LocationPosition', FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;


procedure defineLocationHoursOfOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationHoursOfOperation', 'allDay', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'LocationHoursOfOperation', 'openingTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'LocationHoursOfOperation', 'closingTime', 'time', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineLocationHoursOfOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationHoursOfOperation', nil, 'LocationHoursOfOperation', FHIRFactoryJs);
  defineLocationHoursOfOperationPropsJs(js, def);
end;


procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Location', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Location', 'operationalStatus', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Location', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Location', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Location', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Location', 'address', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'physicalType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'position', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'partOf', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Location', 'hoursOfOperation', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Location', 'availabilityExceptions', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Location', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location', nil, 'Location', FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;


procedure defineMeasureGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroup', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroup', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup', 'population', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MeasureGroup', 'stratifier', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMeasureGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroup', nil, 'MeasureGroup', FHIRFactoryJs);
  defineMeasureGroupPropsJs(js, def);
end;


procedure defineMeasureGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupPopulation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation', 'criteria', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMeasureGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupPopulation', nil, 'MeasureGroupPopulation', FHIRFactoryJs);
  defineMeasureGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifier', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'criteria', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier', nil, 'MeasureGroupStratifier', FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData', 'usage', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData', 'criteria', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMeasureSupplementalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureSupplementalData', nil, 'MeasureSupplementalData', FHIRFactoryJs);
  defineMeasureSupplementalDataPropsJs(js, def);
end;


procedure defineMeasurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Measure', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Measure', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'subject', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'library', 'Reference(Library)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'disclaimer', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'scoring', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'compositeScoring', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'riskAdjustment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rateAggregation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rationale', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'clinicalRecommendationStatement', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'improvementNotation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'guidance', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'set', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Measure', 'group', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'supplementalData', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMeasureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Measure', nil, 'Measure', FHIRFactoryJs);
  defineMeasurePropsJs(js, def);
end;


procedure defineMeasureReportGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroup', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup', 'population', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroup', 'measureScore', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup', 'stratifier', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMeasureReportGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroup', nil, 'MeasureReportGroup', FHIRFactoryJs);
  defineMeasureReportGroupPropsJs(js, def);
end;


procedure defineMeasureReportGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'count', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'subjects', 'Reference(List)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMeasureReportGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupPopulation', nil, 'MeasureReportGroupPopulation', FHIRFactoryJs);
  defineMeasureReportGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifier', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifier', 'stratum', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMeasureReportGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifier', nil, 'MeasureReportGroupStratifier', FHIRFactoryJs);
  defineMeasureReportGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'population', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'measureScore', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratum', nil, 'MeasureReportGroupStratifierStratum', FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'count', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'subjects', 'Reference(List)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumPopulation', nil, 'MeasureReportGroupStratifierStratumPopulation', FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPopulationPropsJs(js, def);
end;


procedure defineMeasureReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MeasureReport', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'measure', 'Reference(Measure)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'subject', 'Reference(Patient|Practitioner|Location|Device|RelatedPerson|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport', 'reportingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'group', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport', 'evaluatedResources', 'Reference(Bundle)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMeasureReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReport', nil, 'MeasureReport', FHIRFactoryJs);
  defineMeasureReportPropsJs(js, def);
end;


procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'basedOn', 'Reference(ServiceRequest|CarePlan)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'partOf', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Media', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'modality', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'view', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'subject', 'Reference(Patient|Practitioner|Group|Device|Specimen|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'createdDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'createdPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'issued', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'operator', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Media', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'deviceName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Media', 'device', 'Reference(Device|DeviceMetric|DeviceComponent)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'height', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'width', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'frames', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'duration', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'Media', 'content', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Media', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media', nil, 'Media', FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;


procedure defineMedicationIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationIngredient', 'itemCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient', 'itemReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient', 'isActive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationIngredient', 'amount', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationIngredient', nil, 'MedicationIngredient', FHIRFactoryJs);
  defineMedicationIngredientPropsJs(js, def);
end;


procedure defineMedicationBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationBatch', 'lotNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationBatch', 'expirationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineMedicationBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationBatch', nil, 'MedicationBatch', FHIRFactoryJs);
  defineMedicationBatchPropsJs(js, def);
end;


procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Medication', 'manufacturer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'ingredient', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Medication', 'batch', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication', nil, 'Medication', FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;


procedure defineMedicationAdministrationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationPerformer', 'function', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationPerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationAdministrationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationPerformer', nil, 'MedicationAdministrationPerformer', FHIRFactoryJs);
  defineMedicationAdministrationPerformerPropsJs(js, def);
end;


procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'site', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'route', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'dose', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage', nil, 'MedicationAdministrationDosage', FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;


procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'partOf', 'Reference(MedicationAdministration|Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'performer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'statusReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'request', 'Reference(MedicationRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'device', 'Reference(Device)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'dosage', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'eventHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration', nil, 'MedicationAdministration', FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;


procedure defineMedicationDispensePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispensePerformer', 'function', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispensePerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationDispensePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispensePerformer', nil, 'MedicationDispensePerformer', FHIRFactoryJs);
  defineMedicationDispensePerformerPropsJs(js, def);
end;


procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'wasSubstituted', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'responsibleParty', 'Reference(Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution', nil, 'MedicationDispenseSubstitution', FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;


procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'partOf', 'Reference(Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'performer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'authorizingPrescription', 'Reference(MedicationRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'daysSupply', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'whenPrepared', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'whenHandedOver', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'destination', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'receiver', 'Reference(Patient|Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'dosageInstruction', 'Dosage', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'substitution', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'detectedIssue', 'Reference(DetectedIssue)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'eventHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense', nil, 'MedicationDispense', FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'validityPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'numberOfRepeatsAllowed', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'expectedSupplyDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'performer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequest', nil, 'MedicationRequestDispenseRequest', FHIRFactoryJs);
  defineMedicationRequestDispenseRequestPropsJs(js, def);
end;


procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution', 'allowed', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequestSubstitution', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicationRequestSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestSubstitution', nil, 'MedicationRequestSubstitution', FHIRFactoryJs);
  defineMedicationRequestSubstitutionPropsJs(js, def);
end;


procedure defineMedicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'intent', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'supportingInformation', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson|CareTeam)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performerType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'recorder', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'reasonReference', 'Reference(Condition|Observation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'basedOn', 'Reference(CarePlan|MedicationRequest|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'groupIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'dosageInstruction', 'Dosage', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'dispenseRequest', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'substitution', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'priorPrescription', 'Reference(MedicationRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'detectedIssue', 'Reference(DetectedIssue)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'eventHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequest', nil, 'MedicationRequest', FHIRFactoryJs);
  defineMedicationRequestPropsJs(js, def);
end;


procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'basedOn', 'Reference(MedicationRequest|CarePlan|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'partOf', 'Reference(MedicationAdministration|MedicationDispense|MedicationStatement|Procedure|Observation)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement', 'statusReason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'dateAsserted', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'informationSource', 'Reference(Patient|Practitioner|RelatedPerson|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'derivedFrom', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'dosage', 'Dosage', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement', nil, 'MedicationStatement', FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;


procedure defineMedicinalProductNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductName', 'fullName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductName', 'namePart', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductName', 'countryLanguage', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductName', nil, 'MedicinalProductName', FHIRFactoryJs);
  defineMedicinalProductNamePropsJs(js, def);
end;


procedure defineMedicinalProductNameNamePartPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameNamePart', 'part', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductNameNamePart', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductNameNamePartJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameNamePart', nil, 'MedicinalProductNameNamePart', FHIRFactoryJs);
  defineMedicinalProductNameNamePartPropsJs(js, def);
end;


procedure defineMedicinalProductNameCountryLanguagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'country', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'jurisdiction', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'language', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductNameCountryLanguageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameCountryLanguage', nil, 'MedicinalProductNameCountryLanguage', FHIRFactoryJs);
  defineMedicinalProductNameCountryLanguagePropsJs(js, def);
end;


procedure defineMedicinalProductManufacturingBusinessOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'operationType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'authorisationReferenceNumber', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'effectiveDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'confidentialityIndicator', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'manufacturer', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'regulator', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductManufacturingBusinessOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufacturingBusinessOperation', nil, 'MedicinalProductManufacturingBusinessOperation', FHIRFactoryJs);
  defineMedicinalProductManufacturingBusinessOperationPropsJs(js, def);
end;


procedure defineMedicinalProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProduct', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'combinedPharmaceuticalDoseForm', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'additionalMonitoringIndicator', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'paediatricUseIndicator', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'orphanDesignationStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'productClassification', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'marketingAuthorization', 'Reference(MedicinalProductAuthorization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'packagedMedicinalProduct', 'Reference(MedicinalProductPackaged)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'pharmaceuticalProduct', 'Reference(MedicinalProductPharmaceutical)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'clinicalParticulars', 'Reference(MedicinalProductClinicals)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'attachedDocument', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'masterFile', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'name', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'crossReference', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'manufacturingBusinessOperation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProduct', nil, 'MedicinalProduct', FHIRFactoryJs);
  defineMedicinalProductPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'country', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'jurisdiction', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'number', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'legalStatusOfSupply', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationJurisdictionalAuthorization', nil, 'MedicinalProductAuthorizationJurisdictionalAuthorization', FHIRFactoryJs);
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'number', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'date', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'application', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedure', nil, 'MedicinalProductAuthorizationProcedure', FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedurePropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'number', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedureApplication', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineMedicinalProductAuthorizationProcedureApplicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedureApplication', nil, 'MedicinalProductAuthorizationProcedureApplication', FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedureApplicationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorization', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'country', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'legalStatusOfSupply', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'statusDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'restoreDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'validityPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dataExclusivityPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dateOfFirstAuthorization', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'internationalBirthDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'jurisdictionalAuthorization', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'holder', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'regulator', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'procedure', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'marketingStatus', 'MarketingStatus', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorization', nil, 'MedicinalProductAuthorization', FHIRFactoryJs);
  defineMedicinalProductAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'symptomConditionEffect', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'classification', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'frequencyOfOccurrence', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffects', 'population', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffects', nil, 'MedicinalProductClinicalsUndesirableEffects', FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'ageRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'ageCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'gender', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'race', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsUndesirableEffectsPopulation', 'physiologicalCondition', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsUndesirableEffectsPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsUndesirableEffectsPopulation', nil, 'MedicinalProductClinicalsUndesirableEffectsPopulation', FHIRFactoryJs);
  defineMedicinalProductClinicalsUndesirableEffectsPopulationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'diseaseSymptomProcedure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'diseaseStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'comorbidity', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'intendedEffect', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'duration', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'undesirableEffects', '@MedicinalProductClinicals.undesirableEffects', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'otherTherapy', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndication', 'population', '@MedicinalProductClinicals.undesirableEffects.population', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndication', nil, 'MedicinalProductClinicalsTherapeuticIndication', FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'therapyRelationshipType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'medicationCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', 'medicationReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', nil, 'MedicinalProductClinicalsTherapeuticIndicationOtherTherapy', FHIRFactoryJs);
  defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsContraindicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'disease', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'diseaseStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'comorbidity', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'therapeuticIndication', '@MedicinalProductClinicals.therapeuticIndication', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'otherTherapy', '@MedicinalProductClinicals.therapeuticIndication.otherTherapy', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsContraindication', 'population', '@MedicinalProductClinicals.undesirableEffects.population', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsContraindicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsContraindication', nil, 'MedicinalProductClinicalsContraindication', FHIRFactoryJs);
  defineMedicinalProductClinicalsContraindicationPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsInteractionsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'interactant', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'effect', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'incidence', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductClinicalsInteractions', 'management', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductClinicalsInteractionsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicalsInteractions', nil, 'MedicinalProductClinicalsInteractions', FHIRFactoryJs);
  defineMedicinalProductClinicalsInteractionsPropsJs(js, def);
end;


procedure defineMedicinalProductClinicalsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductClinicals', 'undesirableEffects', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'therapeuticIndication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'contraindication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductClinicals', 'interactions', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductClinicalsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductClinicals', nil, 'MedicinalProductClinicals', FHIRFactoryJs);
  defineMedicinalProductClinicalsPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'substance', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'alternate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductDeviceSpecMaterial', 'allergenicIndicator', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineMedicinalProductDeviceSpecMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpecMaterial', nil, 'MedicinalProductDeviceSpecMaterial', FHIRFactoryJs);
  defineMedicinalProductDeviceSpecMaterialPropsJs(js, def);
end;


procedure defineMedicinalProductDeviceSpecPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'tradeName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'listingNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'modelNumber', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'sterilityIndicator', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'sterilisationRequirement', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'usage', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'nomenclature', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'shelfLife', 'ProductShelfLife', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'physicalCharacteristics', 'ProdCharacteristic', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'otherCharacteristics', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'batchIdentifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'manufacturer', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductDeviceSpec', 'material', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductDeviceSpecJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductDeviceSpec', nil, 'MedicinalProductDeviceSpec', FHIRFactoryJs);
  defineMedicinalProductDeviceSpecPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'group', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'confidentiality', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'strength', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstance', nil, 'MedicinalProductIngredientSpecifiedSubstance', FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'presentation', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'concentration', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'measurementPoint', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'country', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'referenceStrength', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrength', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrength', FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'substance', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSubstance', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSubstance', 'strength', '@MedicinalProductIngredient.specifiedSubstance.strength', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSubstance', nil, 'MedicinalProductIngredientSubstance', FHIRFactoryJs);
  defineMedicinalProductIngredientSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredient', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'allergenicIndicator', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'manufacturer', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'specifiedSubstance', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'substance', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredient', nil, 'MedicinalProductIngredient', FHIRFactoryJs);
  defineMedicinalProductIngredientPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedBatchIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier', 'outerPackaging', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier', 'immediatePackaging', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedBatchIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedBatchIdentifier', nil, 'MedicinalProductPackagedBatchIdentifier', FHIRFactoryJs);
  defineMedicinalProductPackagedBatchIdentifierPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'material', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'alternateMaterial', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturer', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'device', 'Reference(MedicinalProductDeviceSpec)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturedItem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'otherCharacteristics', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'packageItem', '@MedicinalProductPackaged.packageItem', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'physicalCharacteristics', 'ProdCharacteristic', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'shelfLifeStorage', 'ProductShelfLife', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedPackageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItem', nil, 'MedicinalProductPackagedPackageItem', FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'manufacturedDoseForm', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'unitOfPresentation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'xManufacturer', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'ingredient', 'Reference(MedicinalProductIngredient)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItemManufacturedItem', 'physicalCharacteristics', 'ProdCharacteristic', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedPackageItemManufacturedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItemManufacturedItem', nil, 'MedicinalProductPackagedPackageItemManufacturedItem', FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemManufacturedItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackaged', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'marketingStatus', 'MarketingStatus', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'batchIdentifier', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'packageItem', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackaged', nil, 'MedicinalProductPackaged', FHIRFactoryJs);
  defineMedicinalProductPackagedPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMedicinalProductPharmaceuticalCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalCharacteristics', nil, 'MedicinalProductPharmaceuticalCharacteristics', FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'administrableDoseForm', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'unitOfPresentation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'routeOfAdministration', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'ingredient', 'Reference(MedicinalProductIngredient)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'characteristics', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceutical', nil, 'MedicinalProductPharmaceutical', FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalPropsJs(js, def);
end;


procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'profile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'min', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMessageDefinitionFocusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionFocus', nil, 'MessageDefinitionFocus', FHIRFactoryJs);
  defineMessageDefinitionFocusPropsJs(js, def);
end;


procedure defineMessageDefinitionAllowedResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionAllowedResponse', 'message', 'Reference(MessageDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinitionAllowedResponse', 'situation', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMessageDefinitionAllowedResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionAllowedResponse', nil, 'MessageDefinitionAllowedResponse', FHIRFactoryJs);
  defineMessageDefinitionAllowedResponsePropsJs(js, def);
end;


procedure defineMessageDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'MessageDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'MessageDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'base', 'Reference(MessageDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition', 'parent', 'Reference(ActivityDefinition|PlanDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'replaces', 'Reference(MessageDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'event', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'category', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'focus', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'responseRequired', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition', 'allowedResponse', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineMessageDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinition', nil, 'MessageDefinition', FHIRFactoryJs);
  defineMessageDefinitionPropsJs(js, def);
end;


procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'target', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination', 'endpoint', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'receiver', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination', nil, 'MessageHeaderDestination', FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;


procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'software', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'contact', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource', 'endpoint', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource', nil, 'MessageHeaderSource', FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;


procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse', 'identifier', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse', 'details', 'Reference(OperationOutcome)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse', nil, 'MessageHeaderResponse', FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;


procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader', 'event', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'destination', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'sender', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'enterer', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'author', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'source', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'responsible', 'Reference(Practitioner|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'reason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'response', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'focus', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'definition', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader', nil, 'MessageHeader', FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;


procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'preferred', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId', nil, 'NamingSystemUniqueId', FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;


procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'responsible', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'uniqueId', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem', nil, 'NamingSystem', FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'schedule', 'Timing', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'nutrient', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'texture', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'fluidConsistencyType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'instruction', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet', nil, 'NutritionOrderOralDiet', FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient', 'modifier', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient', 'amount', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient', nil, 'NutritionOrderOralDietNutrient', FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture', 'modifier', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture', 'foodType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture', nil, 'NutritionOrderOralDietTexture', FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;


procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'productName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'schedule', 'Timing', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'instruction', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement', nil, 'NutritionOrderSupplement', FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'baseFormulaType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'baseFormulaProductName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'additiveType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'additiveProductName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'caloricDensity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'routeofAdministration', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'administration', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'maxVolumeToDeliver', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'administrationInstruction', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula', nil, 'NutritionOrderEnteralFormula', FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'schedule', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'rateQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'rateRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration', nil, 'NutritionOrderEnteralFormulaAdministration', FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;


procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'dateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder', 'orderer', 'Reference(Practitioner|PractitionerRole)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'allergyIntolerance', 'Reference(AllergyIntolerance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'foodPreferenceModifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'excludeFoodModifier', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'oralDiet', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'supplement', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'enteralFormula', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder', nil, 'NutritionOrder', FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;


procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange', 'low', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'high', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'appliesTo', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ObservationReferenceRange', 'age', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange', nil, 'ObservationReferenceRange', FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;


procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ObservationComponent', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ObservationComponent', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'dataAbsentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'interpretation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'referenceRange', '@Observation.referenceRange', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent', nil, 'ObservationComponent', FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;


procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'basedOn', 'Reference(CarePlan|DeviceRequest|ImmunizationRecommendation|MedicationRequest|NutritionOrder|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'partOf', 'Reference(MedicationAdministration|MedicationDispense|MedicationStatement|Procedure|Immunization|ImagingStudy)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'subject', 'Reference(Patient|Group|Device|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'issued', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Observation', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Observation', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'dataAbsentReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'interpretation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Observation', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'specimen', 'Reference(Specimen)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'device', 'Reference(Device|DeviceComponent|DeviceMetric)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'referenceRange', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'hasMember', 'Reference(Observation|QuestionnaireResponse|Sequence)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'derivedFrom', 'Reference(DocumentReference|ImagingStudy|Media|QuestionnaireResponse|Observation|Sequence)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'component', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation', nil, 'Observation', FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;


procedure defineObservationDefinitionQuantitativeDetailsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'customaryUnit', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'unit', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'conversionFactor', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'decimalPrecision', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineObservationDefinitionQuantitativeDetailsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQuantitativeDetails', nil, 'ObservationDefinitionQuantitativeDetails', FHIRFactoryJs);
  defineObservationDefinitionQuantitativeDetailsPropsJs(js, def);
end;


procedure defineObservationDefinitionQualifiedIntervalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'range', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'appliesTo', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'age', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'gestationalAge', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'condition', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineObservationDefinitionQualifiedIntervalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQualifiedInterval', nil, 'ObservationDefinitionQualifiedInterval', FHIRFactoryJs);
  defineObservationDefinitionQualifiedIntervalPropsJs(js, def);
end;


procedure defineObservationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ObservationDefinition', 'category', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'code', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'permittedDataType', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'multipleResultsAllowed', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ObservationDefinition', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'preferredReportName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'quantitativeDetails', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'qualifiedInterval', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'validCodedValueSet', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'normalCodedValueSet', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'abnormalCodedValueSet', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'criticalCodedValueSet', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineObservationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinition', nil, 'ObservationDefinition', FHIRFactoryJs);
  defineObservationDefinitionPropsJs(js, def);
end;


procedure defineOccupationalDataEmploymentStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataEmploymentStatus', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataEmploymentStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataEmploymentStatus', nil, 'OccupationalDataEmploymentStatus', FHIRFactoryJs);
  defineOccupationalDataEmploymentStatusPropsJs(js, def);
end;


procedure defineOccupationalDataRetirementStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataRetirementStatus', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OccupationalDataRetirementStatus', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataRetirementStatus', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataRetirementStatus', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataRetirementStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataRetirementStatus', nil, 'OccupationalDataRetirementStatus', FHIRFactoryJs);
  defineOccupationalDataRetirementStatusPropsJs(js, def);
end;


procedure defineOccupationalDataCombatZoneHazardousDutyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataCombatZoneHazardousDuty', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataCombatZoneHazardousDuty', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataCombatZoneHazardousDuty', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataCombatZoneHazardousDutyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataCombatZoneHazardousDuty', nil, 'OccupationalDataCombatZoneHazardousDuty', FHIRFactoryJs);
  defineOccupationalDataCombatZoneHazardousDutyPropsJs(js, def);
end;


procedure defineOccupationalDataUsualOccupationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'duration', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupation', 'industry', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataUsualOccupationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataUsualOccupation', nil, 'OccupationalDataUsualOccupation', FHIRFactoryJs);
  defineOccupationalDataUsualOccupationPropsJs(js, def);
end;


procedure defineOccupationalDataUsualOccupationDurationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataUsualOccupationDuration', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupationDuration', 'value', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataUsualOccupationDurationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataUsualOccupationDuration', nil, 'OccupationalDataUsualOccupationDuration', FHIRFactoryJs);
  defineOccupationalDataUsualOccupationDurationPropsJs(js, def);
end;


procedure defineOccupationalDataUsualOccupationIndustryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataUsualOccupationIndustry', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataUsualOccupationIndustry', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataUsualOccupationIndustryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataUsualOccupationIndustry', nil, 'OccupationalDataUsualOccupationIndustry', FHIRFactoryJs);
  defineOccupationalDataUsualOccupationIndustryPropsJs(js, def);
end;


procedure defineOccupationalDataPastOrPresentOccupationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'effectiveDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalDataPastOrPresentOccupation', 'value', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataPastOrPresentOccupationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalDataPastOrPresentOccupation', nil, 'OccupationalDataPastOrPresentOccupation', FHIRFactoryJs);
  defineOccupationalDataPastOrPresentOccupationPropsJs(js, def);
end;


procedure defineOccupationalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OccupationalData', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OccupationalData', 'subject', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OccupationalData', 'author', 'Reference(Practitioner|PractitionerRole|Patient|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData', 'employmentStatus', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'retirementStatus', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'combatZoneHazardousDuty', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OccupationalData', 'usualOccupation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OccupationalData', 'pastOrPresentOccupation', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOccupationalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OccupationalData', nil, 'OccupationalData', FHIRFactoryJs);
  defineOccupationalDataPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter', 'name', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'use', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'searchType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'binding', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'part', '@OperationDefinition.parameter', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter', nil, 'OperationDefinitionParameter', FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'strength', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'valueSetUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'valueSetReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding', nil, 'OperationDefinitionParameterBinding', FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;


procedure defineOperationDefinitionOverloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionOverload', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineOperationDefinitionOverloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionOverload', nil, 'OperationDefinitionOverload', FHIRFactoryJs);
  defineOperationDefinitionOverloadPropsJs(js, def);
end;


procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'affectsState', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'comment', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'base', 'Reference(OperationDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'system', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'type', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'instance', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'inputProfile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'outputProfile', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinition', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'overload', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition', nil, 'OperationDefinition', FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;


procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue', 'severity', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'details', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'diagnostics', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue', nil, 'OperationOutcomeIssue', FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;


procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome', 'issue', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome', nil, 'OperationOutcome', FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;


procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact', 'purpose', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact', 'name', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact', 'address', 'Address', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact', nil, 'OrganizationContact', FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;


procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Organization', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Organization', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'partOf', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Organization', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization', nil, 'Organization', FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;


procedure defineOrganizationRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'allDay', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'availableStartTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleAvailableTime', 'availableEndTime', 'time', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineOrganizationRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleAvailableTime', nil, 'OrganizationRoleAvailableTime', FHIRFactoryJs);
  defineOrganizationRoleAvailableTimePropsJs(js, def);
end;


procedure defineOrganizationRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationRoleNotAvailable', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OrganizationRoleNotAvailable', 'during', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineOrganizationRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRoleNotAvailable', nil, 'OrganizationRoleNotAvailable', FHIRFactoryJs);
  defineOrganizationRoleNotAvailablePropsJs(js, def);
end;


procedure defineOrganizationRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrganizationRole', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationRole', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'participatingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'OrganizationRole', 'network', 'Reference(Organization)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'location', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'healthcareService', 'Reference(HealthcareService)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'availableTime', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'notAvailable', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'OrganizationRole', 'availabilityExceptions', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'OrganizationRole', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineOrganizationRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationRole', nil, 'OrganizationRole', FHIRFactoryJs);
  defineOrganizationRolePropsJs(js, def);
end;


procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact', 'relationship', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PatientContact', 'name', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PatientContact', 'address', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PatientContact', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact', nil, 'PatientContact', FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;


procedure definePatientAnimalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientAnimal', 'species', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal', 'breed', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientAnimal', 'genderStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePatientAnimalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientAnimal', nil, 'PatientAnimal', FHIRFactoryJs);
  definePatientAnimalPropsJs(js, def);
end;


procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication', 'language', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication', 'preferred', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication', nil, 'PatientCommunication', FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;


procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink', 'other', 'Reference(Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PatientLink', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink', nil, 'PatientLink', FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;


procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'name', 'HumanName', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Patient', 'birthDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Patient', 'deceasedBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'deceasedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Patient', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'maritalStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'multipleBirthBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'multipleBirthInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Patient', 'photo', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'animal', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'communication', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'generalPractitioner', 'Reference(Organization|Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'link', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient', nil, 'Patient', FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;


procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentNotice', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'response', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'statusDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'paymentStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice', nil, 'PaymentNotice', FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;


procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'response', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'submitter', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'payee', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'amount', 'Money', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail', nil, 'PaymentReconciliationDetail', FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;


procedure definePaymentReconciliationProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationProcessNote', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliationProcessNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePaymentReconciliationProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationProcessNote', nil, 'PaymentReconciliationProcessNote', FHIRFactoryJs);
  definePaymentReconciliationProcessNotePropsJs(js, def);
end;


procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'request', 'Reference(ProcessRequest)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'detail', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'total', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'processNote', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation', nil, 'PaymentReconciliation', FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;


procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink', 'target', 'Reference(Patient|Practitioner|RelatedPerson|Person)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PersonLink', 'assurance', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink', nil, 'PersonLink', FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;


procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Person', 'name', 'HumanName', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Person', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Person', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Person', 'birthDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Person', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Person', 'photo', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Person', 'managingOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Person', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Person', 'link', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person', nil, 'Person', FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;


procedure definePlanDefinitionGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoal', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'description', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'priority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'start', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'addresses', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'documentation', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePlanDefinitionGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoal', nil, 'PlanDefinitionGoal', FHIRFactoryJs);
  definePlanDefinitionGoalPropsJs(js, def);
end;


procedure definePlanDefinitionGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'measure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'due', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePlanDefinitionGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoalTarget', nil, 'PlanDefinitionGoalTarget', FHIRFactoryJs);
  definePlanDefinitionGoalTargetPropsJs(js, def);
end;


procedure definePlanDefinitionActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionAction', 'prefix', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'textEquivalent', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'documentation', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'triggerDefinition', 'TriggerDefinition', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'condition', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'input', 'DataRequirement', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'output', 'DataRequirement', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'relatedAction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'groupingBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'selectionBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'requiredBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'precheckBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'cardinalityBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'definition', 'Reference(ActivityDefinition|PlanDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'transform', 'Reference(StructureMap)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'dynamicValue', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'action', '@PlanDefinition.action', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePlanDefinitionActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionAction', nil, 'PlanDefinitionAction', FHIRFactoryJs);
  definePlanDefinitionActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'language', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePlanDefinitionActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionCondition', nil, 'PlanDefinitionActionCondition', FHIRFactoryJs);
  definePlanDefinitionActionConditionPropsJs(js, def);
end;


procedure definePlanDefinitionActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'actionId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'relationship', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'offsetDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'offsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePlanDefinitionActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionRelatedAction', nil, 'PlanDefinitionActionRelatedAction', FHIRFactoryJs);
  definePlanDefinitionActionRelatedActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionParticipant', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionParticipant', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePlanDefinitionActionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionParticipant', nil, 'PlanDefinitionActionParticipant', FHIRFactoryJs);
  definePlanDefinitionActionParticipantPropsJs(js, def);
end;


procedure definePlanDefinitionActionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'language', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePlanDefinitionActionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionDynamicValue', nil, 'PlanDefinitionActionDynamicValue', FHIRFactoryJs);
  definePlanDefinitionActionDynamicValuePropsJs(js, def);
end;


procedure definePlanDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'PlanDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'library', 'Reference(Library)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'goal', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePlanDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinition', nil, 'PlanDefinition', FHIRFactoryJs);
  definePlanDefinitionPropsJs(js, def);
end;


procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification', 'issuer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification', nil, 'PractitionerQualification', FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;


procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner', 'name', 'HumanName', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Practitioner', 'birthDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner', 'photo', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'qualification', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'communication', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner', nil, 'Practitioner', FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;


procedure definePractitionerRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'allDay', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'availableStartTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'availableEndTime', 'time', getFHIRStringProp, setFHIRStringProp);
end;

procedure definePractitionerRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleAvailableTime', nil, 'PractitionerRoleAvailableTime', FHIRFactoryJs);
  definePractitionerRoleAvailableTimePropsJs(js, def);
end;


procedure definePractitionerRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleNotAvailable', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleNotAvailable', 'during', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure definePractitionerRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleNotAvailable', nil, 'PractitionerRoleNotAvailable', FHIRFactoryJs);
  definePractitionerRoleNotAvailablePropsJs(js, def);
end;


procedure definePractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PractitionerRole', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRole', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'practitioner', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'location', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'healthcareService', 'Reference(HealthcareService)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'availableTime', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'notAvailable', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'availabilityExceptions', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'PractitionerRole', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure definePractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRole', nil, 'PractitionerRole', FHIRFactoryJs);
  definePractitionerRolePropsJs(js, def);
end;


procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'actor', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'onBehalfOf', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer', nil, 'ProcedurePerformer', FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;


procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice', 'action', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice', 'manipulated', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice', nil, 'ProcedureFocalDevice', FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;


procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'basedOn', 'Reference(CarePlan|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'partOf', 'Reference(Procedure|Observation|MedicationAdministration)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure', 'performedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'performedAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'reasonReference', 'Reference(Condition|Observation|Procedure|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'report', 'Reference(DiagnosticReport)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complication', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complicationDetail', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'followUp', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'focalDevice', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedReference', 'Reference(Device|Medication|Substance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure', nil, 'Procedure', FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProcessRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessRequestItem', 'sequenceLinkId', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineProcessRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequestItem', nil, 'ProcessRequestItem', FHIRFactoryJs);
  defineProcessRequestItemPropsJs(js, def);
end;


procedure defineProcessRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'action', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'target', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessRequest', 'provider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'response', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessRequest', 'nullify', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ProcessRequest', 'reference', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessRequest', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessRequest', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProcessRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessRequest', nil, 'ProcessRequest', FHIRFactoryJs);
  defineProcessRequestPropsJs(js, def);
end;


procedure defineProcessResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcessResponseProcessNote', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessResponseProcessNote', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineProcessResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponseProcessNote', nil, 'ProcessResponseProcessNote', FHIRFactoryJs);
  defineProcessResponseProcessNotePropsJs(js, def);
end;


procedure defineProcessResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProcessResponse', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'created', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ProcessResponse', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'request', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'outcome', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'disposition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProcessResponse', 'requestProvider', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'requestOrganization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'form', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProcessResponse', 'processNote', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'error', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProcessResponse', 'communicationRequest', 'Reference(CommunicationRequest)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProcessResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcessResponse', nil, 'ProcessResponse', FHIRFactoryJs);
  defineProcessResponsePropsJs(js, def);
end;


procedure defineProductPlanContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanContact', 'purpose', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact', 'name', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanContact', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanContact', 'address', 'Address', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProductPlanContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanContact', nil, 'ProductPlanContact', FHIRFactoryJs);
  defineProductPlanContactPropsJs(js, def);
end;


procedure defineProductPlanCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverage', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverage', 'benefit', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverage', nil, 'ProductPlanCoverage', FHIRFactoryJs);
  defineProductPlanCoveragePropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefit', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefit', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanCoverageBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefit', nil, 'ProductPlanCoverageBenefit', FHIRFactoryJs);
  defineProductPlanCoverageBenefitPropsJs(js, def);
end;


procedure defineProductPlanCoverageBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanCoverageBenefitItem', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanCoverageBenefitItem', 'benefitValue', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProductPlanCoverageBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanCoverageBenefitItem', nil, 'ProductPlanCoverageBenefitItem', FHIRFactoryJs);
  defineProductPlanCoverageBenefitItemPropsJs(js, def);
end;


procedure defineProductPlanPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlan', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlan', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProductPlanPlan', 'premium', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlan', 'category', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlan', nil, 'ProductPlanPlan', FHIRFactoryJs);
  defineProductPlanPlanPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategory', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategory', 'benefit', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanPlanCategoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategory', nil, 'ProductPlanPlanCategory', FHIRFactoryJs);
  defineProductPlanPlanCategoryPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefit', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefit', 'cost', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanPlanCategoryBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategoryBenefit', nil, 'ProductPlanPlanCategoryBenefit', FHIRFactoryJs);
  defineProductPlanPlanCategoryBenefitPropsJs(js, def);
end;


procedure defineProductPlanPlanCategoryBenefitCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'applicability', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlanPlanCategoryBenefitCost', 'value', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProductPlanPlanCategoryBenefitCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlanPlanCategoryBenefitCost', nil, 'ProductPlanPlanCategoryBenefitCost', FHIRFactoryJs);
  defineProductPlanPlanCategoryBenefitCostPropsJs(js, def);
end;


procedure defineProductPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ProductPlan', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProductPlan', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProductPlan', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'ownedBy', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'administeredBy', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'coverageArea', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProductPlan', 'contact', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'coverage', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'plan', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProductPlan', 'endpoint', 'Reference(Endpoint)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProductPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductPlan', nil, 'ProductPlan', FHIRFactoryJs);
  defineProductPlanPropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'role', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent', 'whoIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'whoReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'onBehalfOfIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'onBehalfOfReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent', nil, 'ProvenanceAgent', FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;


procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity', 'role', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity', 'whatIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity', 'whatReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity', 'agent', '@Provenance.agent', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity', nil, 'ProvenanceEntity', FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;


procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance', 'target', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'occurredPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'occurredDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance', 'recorded', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance', 'location', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'reason', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'activity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'agent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'entity', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'signature', 'Signature', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance', nil, 'Provenance', FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;


procedure defineQuestionnaireItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItem', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'definition', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'prefix', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'enableWhen', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'required', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'repeats', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'readOnly', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'maxLength', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem', 'options', 'Reference(ValueSet)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'option', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'initialReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItem', 'item', '@Questionnaire.item', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItem', nil, 'QuestionnaireItem', FHIRFactoryJs);
  defineQuestionnaireItemPropsJs(js, def);
end;


procedure defineQuestionnaireItemEnableWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'question', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'hasAnswer', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineQuestionnaireItemEnableWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemEnableWhen', nil, 'QuestionnaireItemEnableWhen', FHIRFactoryJs);
  defineQuestionnaireItemEnableWhenPropsJs(js, def);
end;


procedure defineQuestionnaireItemOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemOption', 'initialSelected', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineQuestionnaireItemOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemOption', nil, 'QuestionnaireItemOption', FHIRFactoryJs);
  defineQuestionnaireItemOptionPropsJs(js, def);
end;


procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Questionnaire', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Questionnaire', 'code', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire', nil, 'Questionnaire', FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItem', 'linkId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'definition', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'text', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'answer', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'item', '@QuestionnaireResponse.item', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItem', nil, 'QuestionnaireResponseItem', FHIRFactoryJs);
  defineQuestionnaireResponseItemPropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'item', '@QuestionnaireResponse.item', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItemAnswer', nil, 'QuestionnaireResponseItemAnswer', FHIRFactoryJs);
  defineQuestionnaireResponseItemAnswerPropsJs(js, def);
end;


procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'basedOn', 'Reference(CarePlan|ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'partOf', 'Reference(Observation|Procedure)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'questionnaire', 'Reference(Questionnaire)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse', 'subject', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'authored', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse', 'author', 'Reference(Device|Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'source', 'Reference(Patient|Practitioner|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'item', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse', nil, 'QuestionnaireResponse', FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;


procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'RelatedPerson', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson', 'relationship', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'name', 'HumanName', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'telecom', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'gender', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson', 'birthDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson', 'address', 'Address', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'photo', 'Attachment', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson', nil, 'RelatedPerson', FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;


procedure defineRequestGroupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupAction', 'prefix', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'textEquivalent', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'code', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'documentation', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'condition', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'relatedAction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'timingDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroupAction', 'timingAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'participant', 'Reference(Patient|Person|Practitioner|RelatedPerson)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'groupingBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'selectionBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'requiredBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'precheckBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'cardinalityBehavior', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'action', '@RequestGroup.action', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineRequestGroupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupAction', nil, 'RequestGroupAction', FHIRFactoryJs);
  defineRequestGroupActionPropsJs(js, def);
end;


procedure defineRequestGroupActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionCondition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'language', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineRequestGroupActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionCondition', nil, 'RequestGroupActionCondition', FHIRFactoryJs);
  defineRequestGroupActionConditionPropsJs(js, def);
end;


procedure defineRequestGroupActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'actionId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'relationship', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'offsetDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'offsetRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineRequestGroupActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionRelatedAction', nil, 'RequestGroupActionRelatedAction', FHIRFactoryJs);
  defineRequestGroupActionRelatedActionPropsJs(js, def);
end;


procedure defineRequestGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RequestGroup', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'definition', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'basedOn', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'replaces', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'groupIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'intent', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup', 'author', 'Reference(Device|Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'reasonReference', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineRequestGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroup', nil, 'RequestGroup', FHIRFactoryJs);
  defineRequestGroupPropsJs(js, def);
end;


procedure defineResearchStudyArmPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyArm', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyArm', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudyArm', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineResearchStudyArmJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyArm', nil, 'ResearchStudyArm', FHIRFactoryJs);
  defineResearchStudyArmPropsJs(js, def);
end;


procedure defineResearchStudyObjectivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyObjective', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyObjective', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineResearchStudyObjectiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyObjective', nil, 'ResearchStudyObjective', FHIRFactoryJs);
  defineResearchStudyObjectivePropsJs(js, def);
end;


procedure defineResearchStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchStudy', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'protocol', 'Reference(PlanDefinition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'partOf', 'Reference(ResearchStudy)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'primaryPurposeType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'phase', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'focus', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'condition', 'Reference(Condition)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'keyword', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'location', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'enrollment', 'Reference(Group)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'sponsor', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'principalInvestigator', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'site', 'Reference(Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'reasonStopped', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'arm', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'objective', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineResearchStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudy', nil, 'ResearchStudy', FHIRFactoryJs);
  defineResearchStudyPropsJs(js, def);
end;


procedure defineResearchSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchSubject', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ResearchSubject', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'study', 'Reference(ResearchStudy)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'individual', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'assignedArm', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'actualArm', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'consent', 'Reference(Consent)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineResearchSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchSubject', nil, 'ResearchSubject', FHIRFactoryJs);
  defineResearchSubjectPropsJs(js, def);
end;


procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction', 'outcome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'qualitativeRisk', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'relativeRisk', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'whenPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'whenRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'rationale', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction', nil, 'RiskAssessmentPrediction', FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;


procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'basedOn', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'parent', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'subject', 'Reference(Patient|Group)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'condition', 'Reference(Condition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'performer', 'Reference(Practitioner|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'basis', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'prediction', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'mitigation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment', nil, 'RiskAssessment', FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;


procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'active', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Schedule', 'serviceCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Schedule', 'serviceType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'actor', 'Reference(Patient|Practitioner|PractitionerRole|RelatedPerson|Device|HealthcareService|Location)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'planningHorizon', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Schedule', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule', nil, 'Schedule', FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;


procedure defineSearchParameterComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterComponent', 'definition', 'Reference(SearchParameter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SearchParameterComponent', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSearchParameterComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterComponent', nil, 'SearchParameterComponent', FHIRFactoryJs);
  defineSearchParameterComponentPropsJs(js, def);
end;


procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'derivedFrom', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpath', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpathUsage', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'component', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter', nil, 'SearchParameter', FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceReferenceSeq', 'chromosome', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'genomeBuild', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqId', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqPointer', 'Reference(Sequence)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'referenceSeqString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'strand', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'windowStart', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceReferenceSeq', 'windowEnd', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceReferenceSeq', nil, 'SequenceReferenceSeq', FHIRFactoryJs);
  defineSequenceReferenceSeqPropsJs(js, def);
end;


procedure defineSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceVariant', 'start', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant', 'end', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceVariant', 'observedAllele', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'referenceAllele', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'cigar', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceVariant', 'variantPointer', 'Reference(Observation)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceVariant', nil, 'SequenceVariant', FHIRFactoryJs);
  defineSequenceVariantPropsJs(js, def);
end;


procedure defineSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceQuality', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceQuality', 'standardSequence', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'start', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality', 'end', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceQuality', 'score', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceQuality', 'truthTP', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'queryTP', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'truthFN', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'queryFP', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'gtFP', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'precision', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'recall', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'fScore', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceQuality', 'roc', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceQuality', nil, 'SequenceQuality', FHIRFactoryJs);
  defineSequenceQualityPropsJs(js, def);
end;


procedure defineSequenceQualityRocPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
end;

procedure defineSequenceQualityRocJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceQualityRoc', nil, 'SequenceQualityRoc', FHIRFactoryJs);
  defineSequenceQualityRocPropsJs(js, def);
end;


procedure defineSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceRepository', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'datasetId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'variantsetId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceRepository', 'readsetId', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceRepository', nil, 'SequenceRepository', FHIRFactoryJs);
  defineSequenceRepositoryPropsJs(js, def);
end;


procedure defineSequenceStructureVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariant', 'precision', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SequenceStructureVariant', 'reportedaCGHRatio', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'SequenceStructureVariant', 'length', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariant', 'outer', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SequenceStructureVariant', 'inner', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSequenceStructureVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariant', nil, 'SequenceStructureVariant', FHIRFactoryJs);
  defineSequenceStructureVariantPropsJs(js, def);
end;


procedure defineSequenceStructureVariantOuterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantOuter', 'start', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantOuter', 'end', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantOuterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantOuter', nil, 'SequenceStructureVariantOuter', FHIRFactoryJs);
  defineSequenceStructureVariantOuterPropsJs(js, def);
end;


procedure defineSequenceStructureVariantInnerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SequenceStructureVariantInner', 'start', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SequenceStructureVariantInner', 'end', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
end;

procedure defineSequenceStructureVariantInnerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SequenceStructureVariantInner', nil, 'SequenceStructureVariantInner', FHIRFactoryJs);
  defineSequenceStructureVariantInnerPropsJs(js, def);
end;


procedure defineSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Sequence', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Sequence', 'coordinateSystem', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Sequence', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'specimen', 'Reference(Specimen)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'device', 'Reference(Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'performer', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'referenceSeq', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Sequence', 'variant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'observedSeq', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Sequence', 'quality', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'readCoverage', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'Sequence', 'repository', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'pointer', 'Reference(Sequence)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Sequence', 'structureVariant', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Sequence', nil, 'Sequence', FHIRFactoryJs);
  defineSequencePropsJs(js, def);
end;


procedure defineServiceDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ServiceDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'usage', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'approvalDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'lastReviewDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceDefinition', 'effectivePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'topic', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'contributor', 'Contributor', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceDefinition', 'relatedArtifact', 'RelatedArtifact', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'trigger', 'TriggerDefinition', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'dataRequirement', 'DataRequirement', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceDefinition', 'operationDefinition', 'Reference(OperationDefinition)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineServiceDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceDefinition', nil, 'ServiceDefinition', FHIRFactoryJs);
  defineServiceDefinitionPropsJs(js, def);
end;


procedure defineServiceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceRequest', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'basedOn', 'Reference(CarePlan|ServiceRequest|MedicationRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'replaces', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'requisition', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'intent', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'doNotPerform', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'orderDetail', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'subject', 'Reference(Patient|Group|Location|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performerType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performer', 'Reference(Practitioner|PractitionerRole|Organization|Patient|Device|RelatedPerson|HealthcareService|CareTeam)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'insurance', 'Reference(Coverage|ClaimResponse)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'supportingInfo', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'specimen', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'bodySite', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'patientInstruction', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'relevantHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineServiceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceRequest', nil, 'ServiceRequest', FHIRFactoryJs);
  defineServiceRequestPropsJs(js, def);
end;


procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'serviceCategory', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'serviceType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'specialty', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'appointmentType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'schedule', 'Reference(Schedule)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Slot', 'start', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Slot', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Slot', 'overbooked', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'Slot', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot', nil, 'Slot', FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;


procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection', 'collector', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'collectedDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection', 'collectedPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'bodySite', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection', nil, 'SpecimenCollection', FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;


procedure defineSpecimenProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenProcessing', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenProcessing', 'procedure', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenProcessing', 'additive', 'Reference(Substance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SpecimenProcessing', 'timeDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenProcessing', 'timePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSpecimenProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenProcessing', nil, 'SpecimenProcessing', FHIRFactoryJs);
  defineSpecimenProcessingPropsJs(js, def);
end;


procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'capacity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'specimenQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'additiveCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'additiveReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer', nil, 'SpecimenContainer', FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;


procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'accessionIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Specimen', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'subject', 'Reference(Patient|Group|Device|Substance)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'receivedTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen', 'parent', 'Reference(Specimen)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'request', 'Reference(ServiceRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'collection', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'processing', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'container', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen', nil, 'Specimen', FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'isDerived', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'preference', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerMaterial', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerCap', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerDescription', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerCapacity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerMinimumVolume', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerAdditive', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'containerPreparation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'requirement', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'retentionTime', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'rejectionCriterion', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLab', 'handling', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLab', nil, 'SpecimenDefinitionSpecimenToLab', FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabPropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive', 'additiveCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabContainerAdditive', 'additiveReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabContainerAdditiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabContainerAdditive', nil, 'SpecimenDefinitionSpecimenToLabContainerAdditive', FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabContainerAdditivePropsJs(js, def);
end;


procedure defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'conditionSet', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'tempRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'maxDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'lightExposure', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionSpecimenToLabHandling', 'instruction', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSpecimenDefinitionSpecimenToLabHandlingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionSpecimenToLabHandling', nil, 'SpecimenDefinitionSpecimenToLabHandling', FHIRFactoryJs);
  defineSpecimenDefinitionSpecimenToLabHandlingPropsJs(js, def);
end;


procedure defineSpecimenDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinition', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'typeCollected', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'patientPreparation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition', 'timeAspect', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition', 'collection', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition', 'specimenToLab', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinition', nil, 'SpecimenDefinition', FHIRFactoryJs);
  defineSpecimenDefinitionPropsJs(js, def);
end;


procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping', 'identity', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping', nil, 'StructureDefinitionMapping', FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;


procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot', 'element', 'ElementDefinition', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot', nil, 'StructureDefinitionSnapshot', FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;


procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential', 'element', 'ElementDefinition', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential', nil, 'StructureDefinitionDifferential', FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;


procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'keyword', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'fhirVersion', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'mapping', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'kind', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'abstract', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'contextType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'baseDefinition', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'derivation', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'snapshot', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition', 'differential', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition', nil, 'StructureDefinition', FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;


procedure defineStructureMapStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapStructure', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'alias', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureMapStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapStructure', nil, 'StructureMapStructure', FHIRFactoryJs);
  defineStructureMapStructurePropsJs(js, def);
end;


procedure defineStructureMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroup', 'name', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'extends', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'typeMode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'input', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroup', 'rule', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroup', nil, 'StructureMapGroup', FHIRFactoryJs);
  defineStructureMapGroupPropsJs(js, def);
end;


procedure defineStructureMapGroupInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupInput', 'name', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'mode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureMapGroupInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupInput', nil, 'StructureMapGroupInput', FHIRFactoryJs);
  defineStructureMapGroupInputPropsJs(js, def);
end;


procedure defineStructureMapGroupRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRule', 'name', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRule', 'source', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'rule', '@StructureMap.group.rule', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'dependent', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'documentation', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRule', nil, 'StructureMapGroupRule', FHIRFactoryJs);
  defineStructureMapGroupRulePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'context', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'min', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'max', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'element', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'listMode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'variable', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'condition', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'check', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleSource', nil, 'StructureMapGroupRuleSource', FHIRFactoryJs);
  defineStructureMapGroupRuleSourcePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'context', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'contextType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'element', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'variable', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'listRuleId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'transform', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureMapGroupRuleTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTarget', nil, 'StructureMapGroupRuleTarget', FHIRFactoryJs);
  defineStructureMapGroupRuleTargetPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
end;

procedure defineStructureMapGroupRuleTargetParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTargetParameter', nil, 'StructureMapGroupRuleTargetParameter', FHIRFactoryJs);
  defineStructureMapGroupRuleTargetParameterPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleDependentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleDependent', 'name', 'id', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleDependentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleDependent', nil, 'StructureMapGroupRuleDependent', FHIRFactoryJs);
  defineStructureMapGroupRuleDependentPropsJs(js, def);
end;


procedure defineStructureMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureMap', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'StructureMap', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMap', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'structure', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'group', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineStructureMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMap', nil, 'StructureMap', FHIRFactoryJs);
  defineStructureMapPropsJs(js, def);
end;


procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'endpoint', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'payload', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel', nil, 'SubscriptionChannel', FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;


procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'contact', 'ContactPoint', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Subscription', 'end', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription', 'reason', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'criteria', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'error', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'channel', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Subscription', 'tag', 'Coding', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription', nil, 'Subscription', FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;


procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance', 'expiry', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance', nil, 'SubstanceInstance', FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;


procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient', 'quantity', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient', 'substanceCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient', 'substanceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient', nil, 'SubstanceIngredient', FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;


procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Substance', 'category', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Substance', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Substance', 'instance', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'ingredient', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance', nil, 'Substance', FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSet', 'ratioType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSet', 'startingMaterial', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstancePolymerMonomerSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSet', nil, 'SubstancePolymerMonomerSet', FHIRFactoryJs);
  defineSubstancePolymerMonomerSetPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'material', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'isDefining', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'amount', 'SubstanceAmount', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstancePolymerMonomerSetStartingMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSetStartingMaterial', nil, 'SubstancePolymerMonomerSetStartingMaterial', FHIRFactoryJs);
  defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeat', 'numberOfUnits', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'averageMolecularFormula', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'repeatUnitAmountType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'repeatUnit', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeat', nil, 'SubstancePolymerRepeat', FHIRFactoryJs);
  defineSubstancePolymerRepeatPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'orientationOfPolymerisation', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'repeatUnit', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'amount', 'SubstanceAmount', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'degreeOfPolymerisation', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'structuralRepresentation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnit', nil, 'SubstancePolymerRepeatRepeatUnit', FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', 'degree', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', 'amount', 'SubstanceAmount', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', nil, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'representation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'attachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitStructuralRepresentation', nil, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstancePolymerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstancePolymer', 'class', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer', 'geometry', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer', 'copolymerConnectivity', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer', 'monomerSet', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer', 'repeat', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstancePolymerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymer', nil, 'SubstancePolymer', FHIRFactoryJs);
  defineSubstancePolymerPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGenePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'geneSequenceOrigin', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'gene', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'source', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGene', nil, 'SubstanceReferenceInformationGene', FHIRFactoryJs);
  defineSubstanceReferenceInformationGenePropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGeneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'element', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'source', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGeneElement', nil, 'SubstanceReferenceInformationGeneElement', FHIRFactoryJs);
  defineSubstanceReferenceInformationGeneElementPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'domain', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'classification', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'subtype', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'source', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationClassification', nil, 'SubstanceReferenceInformationClassification', FHIRFactoryJs);
  defineSubstanceReferenceInformationClassificationPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationRelationshipPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'substanceReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'substanceCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'relationship', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'interaction', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'isDefining', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'amountText', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationRelationship', 'source', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationRelationshipJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationRelationship', nil, 'SubstanceReferenceInformationRelationship', FHIRFactoryJs);
  defineSubstanceReferenceInformationRelationshipPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'target', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'interaction', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organism', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organismType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'source', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceReferenceInformationTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationTarget', nil, 'SubstanceReferenceInformationTarget', FHIRFactoryJs);
  defineSubstanceReferenceInformationTargetPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformation', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'gene', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'geneElement', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'classification', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'relationship', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'target', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformation', nil, 'SubstanceReferenceInformation', FHIRFactoryJs);
  defineSubstanceReferenceInformationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'role', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'stereochemistry', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'opticalActivity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'molecularFormula', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'amount', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSubstanceSpecificationMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationMoiety', nil, 'SubstanceSpecificationMoiety', FHIRFactoryJs);
  defineSubstanceSpecificationMoietyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'name', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'parameters', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'substanceId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'substanceName', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'amount', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSubstanceSpecificationPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationProperty', nil, 'SubstanceSpecificationProperty', FHIRFactoryJs);
  defineSubstanceSpecificationPropertyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'stereochemistry', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'opticalActivity', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularFormula', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularFormulaByMoiety', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'isotope', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'referenceSource', 'Reference(DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'structuralRepresentation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructure', nil, 'SubstanceSpecificationStructure', FHIRFactoryJs);
  defineSubstanceSpecificationStructurePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideId', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideName', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'substitutionType', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'nuclideHalfLife', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'amount', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'molecularWeight', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotope', nil, 'SubstanceSpecificationStructureIsotope', FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'amount', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotopeMolecularWeight', nil, 'SubstanceSpecificationStructureIsotopeMolecularWeight', FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'representation', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureStructuralRepresentation', 'attachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureStructuralRepresentation', nil, 'SubstanceSpecificationStructureStructuralRepresentation', FHIRFactoryJs);
  defineSubstanceSpecificationStructureStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'statusDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceCode', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineSubstanceSpecificationSubstanceCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceCode', nil, 'SubstanceSpecificationSubstanceCode', FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceCodePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'language', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'domain', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceName', 'officialName', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationSubstanceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceName', nil, 'SubstanceSpecificationSubstanceName', FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'authority', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'status', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationSubstanceNameOfficialName', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineSubstanceSpecificationSubstanceNameOfficialNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationSubstanceNameOfficialName', nil, 'SubstanceSpecificationSubstanceNameOfficialName', FHIRFactoryJs);
  defineSubstanceSpecificationSubstanceNameOfficialNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecification', 'comment', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification', 'stoichiometric', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecification', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'moiety', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'property', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'referenceInformation', 'Reference(SubstanceReferenceInformation)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'structure', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'substanceCode', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'substanceName', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'polymer', 'Reference(SubstancePolymer)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecification', nil, 'SubstanceSpecification', FHIRFactoryJs);
  defineSubstanceSpecificationPropsJs(js, def);
end;


procedure defineSupplyDeliverySuppliedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'itemCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'itemReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSupplyDeliverySuppliedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDeliverySuppliedItem', nil, 'SupplyDeliverySuppliedItem', FHIRFactoryJs);
  defineSupplyDeliverySuppliedItemPropsJs(js, def);
end;


procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'basedOn', 'Reference(SupplyRequest)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'partOf', 'Reference(SupplyDelivery|Contract)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'suppliedItem', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'supplier', 'Reference(Practitioner|PractitionerRole|Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'destination', 'Reference(Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'receiver', 'Reference(Practitioner)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery', nil, 'SupplyDelivery', FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;


procedure defineSupplyRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestParameter', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineSupplyRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestParameter', nil, 'SupplyRequestParameter', FHIRFactoryJs);
  defineSupplyRequestParameterPropsJs(js, def);
end;


procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest', 'category', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest', 'itemCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'itemReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'quantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'occurrenceDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest', 'occurrencePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'occurrenceTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest', 'requester', 'Reference(Practitioner|PractitionerRole|Organization|Patient|RelatedPerson|Device)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'supplier', 'Reference(Organization|HealthcareService)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonCode', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonReference', 'Reference(Condition|Observation|DiagnosticReport|DocumentReference)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'deliverFrom', 'Reference(Organization|Location)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'deliverTo', 'Reference(Organization|Location|Patient)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest', nil, 'SupplyRequest', FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;


procedure defineTaskRestrictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRestriction', 'repetitions', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TaskRestriction', 'period', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskRestriction', 'recipient', 'Reference(Patient|Practitioner|RelatedPerson|Group|Organization)', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTaskRestrictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRestriction', nil, 'TaskRestriction', FHIRFactoryJs);
  defineTaskRestrictionPropsJs(js, def);
end;


procedure defineTaskInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskInput', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'TaskInput', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TaskInput', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TaskInput', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput', 'valueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTaskInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskInput', nil, 'TaskInput', FHIRFactoryJs);
  defineTaskInputPropsJs(js, def);
end;


procedure defineTaskOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskOutput', 'type', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueBase64Binary', 'base64Binary', getFHIRBinaryProp, setFHIRBinaryProp);
  js.registerElement(def, 'TaskOutput', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TaskOutput', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueDateTime', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TaskOutput', 'valueId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueInstant', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput', 'valueMarkdown', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueOid', 'oid', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valuePositiveInt', 'positiveInt', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueTime', 'time', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUnsignedInt', 'unsignedInt', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueAddress', 'Address', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAge', 'Age', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAnnotation', 'Annotation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAttachment', 'Attachment', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCoding', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContactPoint', 'ContactPoint', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCount', 'Count', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDistance', 'Distance', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDuration', 'Duration', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueHumanName', 'HumanName', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueMoney', 'Money', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valuePeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRange', 'Range', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRatio', 'Ratio', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueSampledData', 'SampledData', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueSignature', 'Signature', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueTiming', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDosage', 'Dosage', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContactDetail', 'ContactDetail', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContributor', 'Contributor', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDataRequirement', 'DataRequirement', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueParameterDefinition', 'ParameterDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRelatedArtifact', 'RelatedArtifact', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueTriggerDefinition', 'TriggerDefinition', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueUsageContext', 'UsageContext', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueMeta', 'Meta', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTaskOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskOutput', nil, 'TaskOutput', FHIRFactoryJs);
  defineTaskOutputPropsJs(js, def);
end;


procedure defineTaskPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Task', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'instantiatesUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Task', 'instantiatesReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'basedOn', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'groupIdentifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'partOf', 'Reference(Task)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Task', 'statusReason', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'businessStatus', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'intent', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Task', 'priority', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Task', 'code', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'Task', 'focus', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'for', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'context', 'Reference(Encounter|EpisodeOfCare)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'executionPeriod', 'Period', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'authoredOn', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'lastModified', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'requester', 'Reference(Device|Organization|Patient|Practitioner|PractitionerRole|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'performerType', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'owner', 'Reference(Practitioner|PractitionerRole|Organization|CareTeam|HealthcareService|Patient|Device|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonCode', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonReference', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'relevantHistory', 'Reference(Provenance)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'restriction', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'Task', 'input', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'Task', 'output', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTaskJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Task', nil, 'Task', FHIRFactoryJs);
  defineTaskPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'version', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystem', nil, 'TerminologyCapabilitiesCodeSystem', FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'code', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'isDefault', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'compositional', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'filter', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersion', nil, 'TerminologyCapabilitiesCodeSystemVersion', FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersionFilter', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersionFilter', nil, 'TerminologyCapabilitiesCodeSystemVersionFilter', FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'hierarchical', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'paging', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'incomplete', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'definition', 'Reference(StructureDefinition)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'profile', 'Reference(ExpansionProfile)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'textFilter', 'markdown', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansion', nil, 'TerminologyCapabilitiesExpansion', FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesValidateCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesValidateCode', 'translations', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesValidateCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesValidateCode', nil, 'TerminologyCapabilitiesValidateCode', FHIRFactoryJs);
  defineTerminologyCapabilitiesValidateCodePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesTranslationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesTranslation', 'needsMap', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesTranslationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesTranslation', nil, 'TerminologyCapabilitiesTranslation', FHIRFactoryJs);
  defineTerminologyCapabilitiesTranslationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesClosurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesClosure', 'translation', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesClosureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesClosure', nil, 'TerminologyCapabilitiesClosure', FHIRFactoryJs);
  defineTerminologyCapabilitiesClosurePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilities', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TerminologyCapabilities', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'lockedDate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities', 'codeSystem', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'expansion', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'codeSearch', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'validateCode', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'translation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'closure', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTerminologyCapabilitiesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilities', nil, 'TerminologyCapabilities', FHIRFactoryJs);
  defineTerminologyCapabilitiesPropsJs(js, def);
end;


procedure defineTestReportParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportParticipant', 'type', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant', 'uri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestReportParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportParticipant', nil, 'TestReportParticipant', FHIRFactoryJs);
  defineTestReportParticipantPropsJs(js, def);
end;


procedure defineTestReportSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetup', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestReportSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetup', nil, 'TestReportSetup', FHIRFactoryJs);
  defineTestReportSetupPropsJs(js, def);
end;


procedure defineTestReportSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupAction', 'operation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestReportSetupAction', 'assert', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestReportSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupAction', nil, 'TestReportSetupAction', FHIRFactoryJs);
  defineTestReportSetupActionPropsJs(js, def);
end;


procedure defineTestReportSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionOperation', 'result', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation', 'message', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation', 'detail', 'uri', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestReportSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionOperation', nil, 'TestReportSetupActionOperation', FHIRFactoryJs);
  defineTestReportSetupActionOperationPropsJs(js, def);
end;


procedure defineTestReportSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionAssert', 'result', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert', 'message', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert', 'detail', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestReportSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionAssert', nil, 'TestReportSetupActionAssert', FHIRFactoryJs);
  defineTestReportSetupActionAssertPropsJs(js, def);
end;


procedure defineTestReportTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTest', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportTest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReportTest', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestReportTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTest', nil, 'TestReportTest', FHIRFactoryJs);
  defineTestReportTestPropsJs(js, def);
end;


procedure defineTestReportTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTestAction', 'operation', '@TestReport.setup.action.operation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestReportTestAction', 'assert', '@TestReport.setup.action.assert', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestReportTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTestAction', nil, 'TestReportTestAction', FHIRFactoryJs);
  defineTestReportTestActionPropsJs(js, def);
end;


procedure defineTestReportTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardown', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestReportTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardown', nil, 'TestReportTeardown', FHIRFactoryJs);
  defineTestReportTeardownPropsJs(js, def);
end;


procedure defineTestReportTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardownAction', 'operation', '@TestReport.setup.action.operation', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestReportTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardownAction', nil, 'TestReportTeardownAction', FHIRFactoryJs);
  defineTestReportTeardownActionPropsJs(js, def);
end;


procedure defineTestReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestReport', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'testScript', 'Reference(TestScript)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'result', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'score', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'TestReport', 'tester', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'issued', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TestReport', 'participant', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestReport', 'setup', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'test', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestReport', 'teardown', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReport', nil, 'TestReport', FHIRFactoryJs);
  defineTestReportPropsJs(js, def);
end;


procedure defineTestScriptOriginPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptOrigin', 'index', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptOrigin', 'profile', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptOriginJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptOrigin', nil, 'TestScriptOrigin', FHIRFactoryJs);
  defineTestScriptOriginPropsJs(js, def);
end;


procedure defineTestScriptDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptDestination', 'index', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptDestination', 'profile', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptDestination', nil, 'TestScriptDestination', FHIRFactoryJs);
  defineTestScriptDestinationPropsJs(js, def);
end;


procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata', 'link', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata', 'capability', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata', nil, 'TestScriptMetadata', FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;


procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink', nil, 'TestScriptMetadataLink', FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;


procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability', 'required', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'validated', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'destination', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'capabilities', 'Reference(CapabilityStatement)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability', nil, 'TestScriptMetadataCapability', FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;


procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture', 'autocreate', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture', 'autodelete', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture', nil, 'TestScriptFixture', FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;


procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'defaultValue', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'headerField', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'hint', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'sourceId', 'id', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable', nil, 'TestScriptVariable', FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;


procedure defineTestScriptRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRule', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRule', 'param', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRule', nil, 'TestScriptRule', FHIRFactoryJs);
  defineTestScriptRulePropsJs(js, def);
end;


procedure defineTestScriptRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptRuleParam', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleParam', nil, 'TestScriptRuleParam', FHIRFactoryJs);
  defineTestScriptRuleParamPropsJs(js, def);
end;


procedure defineTestScriptRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRuleset', 'resource', 'Reference(Any)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptRuleset', 'rule', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRuleset', nil, 'TestScriptRuleset', FHIRFactoryJs);
  defineTestScriptRulesetPropsJs(js, def);
end;


procedure defineTestScriptRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRule', 'ruleId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRule', 'param', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRule', nil, 'TestScriptRulesetRule', FHIRFactoryJs);
  defineTestScriptRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptRulesetRuleParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptRulesetRuleParam', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptRulesetRuleParam', nil, 'TestScriptRulesetRuleParam', FHIRFactoryJs);
  defineTestScriptRulesetRuleParamPropsJs(js, def);
end;


procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup', nil, 'TestScriptSetup', FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;


procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction', 'operation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction', 'assert', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction', nil, 'TestScriptSetupAction', FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'type', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'resource', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'label', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'accept', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'destination', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'encodeRequestUrl', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'origin', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'params', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'requestHeader', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'requestId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'responseId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'sourceId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'targetId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'url', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation', nil, 'TestScriptSetupActionOperation', FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader', 'field', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader', nil, 'TestScriptSetupActionOperationRequestHeader', FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'label', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'direction', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourceId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourceExpression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourcePath', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'contentType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'expression', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'headerField', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'minimumId', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'navigationLinks', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'operator', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'path', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'requestMethod', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'requestURL', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'resource', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'response', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'responseCode', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'rule', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'ruleset', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'sourceId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'validateProfileId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'warningOnly', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert', nil, 'TestScriptSetupActionAssert', FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRule', 'ruleId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRule', 'param', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRule', nil, 'TestScriptSetupActionAssertRule', FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleParam', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleParam', nil, 'TestScriptSetupActionAssertRuleParam', FHIRFactoryJs);
  defineTestScriptSetupActionAssertRuleParamPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset', 'rulesetId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRuleset', 'rule', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRuleset', nil, 'TestScriptSetupActionAssertRuleset', FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule', 'ruleId', 'id', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRule', 'param', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRule', nil, 'TestScriptSetupActionAssertRulesetRule', FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRulePropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssertRulesetRuleParam', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineTestScriptSetupActionAssertRulesetRuleParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssertRulesetRuleParam', nil, 'TestScriptSetupActionAssertRulesetRuleParam', FHIRFactoryJs);
  defineTestScriptSetupActionAssertRulesetRuleParamPropsJs(js, def);
end;


procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'description', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest', nil, 'TestScriptTest', FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;


procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction', 'operation', '@TestScript.setup.action.operation', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction', 'assert', '@TestScript.setup.action.assert', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction', nil, 'TestScriptTestAction', FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;


procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown', 'action', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown', nil, 'TestScriptTeardown', FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;


procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction', 'operation', '@TestScript.setup.action.operation', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction', nil, 'TestScriptTeardownAction', FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;


procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'TestScript', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'origin', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'destination', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'metadata', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'fixture', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'profile', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'variable', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'rule', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'ruleset', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'setup', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'test', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'teardown', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript', nil, 'TestScript', FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;


procedure defineUserSessionStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionStatus', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'UserSessionStatus', 'source', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineUserSessionStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionStatus', nil, 'UserSessionStatus', FHIRFactoryJs);
  defineUserSessionStatusPropsJs(js, def);
end;


procedure defineUserSessionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'UserSessionContext', 'type', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'UserSessionContext', 'valueCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UserSessionContext', 'valueQuantity', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineUserSessionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSessionContext', nil, 'UserSessionContext', FHIRFactoryJs);
  defineUserSessionContextPropsJs(js, def);
end;


procedure defineUserSessionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'UserSession', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'user', 'Reference(Device|Practitioner|Patient|RelatedPerson)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'status', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'workstation', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'UserSession', 'focus', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'UserSession', 'created', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession', 'expires', 'instant', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'UserSession', 'context', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineUserSessionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UserSession', nil, 'UserSession', FHIRFactoryJs);
  defineUserSessionPropsJs(js, def);
end;


procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose', 'lockedDate', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetCompose', 'inactive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCompose', 'include', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose', 'exclude', '@ValueSet.compose.include', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose', nil, 'ValueSetCompose', FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;


procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'concept', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'filter', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude', nil, 'ValueSetComposeInclude', FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'designation', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept', nil, 'ValueSetComposeIncludeConcept', FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'language', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'use', 'Coding', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'value', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConceptDesignation', nil, 'ValueSetComposeIncludeConceptDesignation', FHIRFactoryJs);
  defineValueSetComposeIncludeConceptDesignationPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'property', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'op', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'value', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter', nil, 'ValueSetComposeIncludeFilter', FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;


procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion', 'identifier', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion', 'timestamp', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion', 'total', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion', 'offset', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion', 'parameter', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion', 'contains', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion', nil, 'ValueSetExpansion', FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;


procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueString', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueBoolean', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueInteger', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueDecimal', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueUri', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueCode', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter', nil, 'ValueSetExpansionParameter', FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;


procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains', 'system', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'abstract', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'inactive', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'code', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'display', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'designation', '@ValueSet.compose.include.concept.designation', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'contains', '@ValueSet.expansion.contains', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains', nil, 'ValueSetExpansionContains', FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;


procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet', 'url', 'uri', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'version', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'name', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'title', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'experimental', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'date', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet', 'publisher', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'contact', 'ContactDetail', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'description', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'useContext', 'UsageContext', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'jurisdiction', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'immutable', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'purpose', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'copyright', 'markdown', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'extensible', 'boolean', getFHIRBooleanProp, setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'compose', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'ValueSet', 'expansion', '', getFHIRObjectProp, setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet', nil, 'ValueSet', FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;


procedure defineVerificationResultPrimarySourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultPrimarySource', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'type', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationProcess', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationStatus', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'canPushUpdates', 'code', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineVerificationResultPrimarySourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultPrimarySource', nil, 'VerificationResultPrimarySource', FHIRFactoryJs);
  defineVerificationResultPrimarySourcePropsJs(js, def);
end;


procedure defineVerificationResultAttestationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultAttestation', 'source', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'method', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'date', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultAttestation', 'sourceIdentityCertificate', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation', 'proxyIdentityCertificate', 'string', getFHIRStringProp, setFHIRStringProp);
end;

procedure defineVerificationResultAttestationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultAttestation', nil, 'VerificationResultAttestation', FHIRFactoryJs);
  defineVerificationResultAttestationPropsJs(js, def);
end;


procedure defineVerificationResultValidatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultValidator', 'identifier', 'Identifier', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator', 'organization', 'Reference(Organization)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator', 'identityCertificate', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator', 'dateValidated', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
end;

procedure defineVerificationResultValidatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultValidator', nil, 'VerificationResultValidator', FHIRFactoryJs);
  defineVerificationResultValidatorPropsJs(js, def);
end;


procedure defineVerificationResultPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VerificationResult', 'target', 'Reference(Any)', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'need', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'statusDate', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'validationType', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'validationProcess', 'CodeableConcept', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'frequency', 'Timing', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'lastPerformed', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'nextScheduled', 'date', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'failureAction', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'primarySource', '', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'attestation', '', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'validator', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineVerificationResultJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResult', nil, 'VerificationResult', FHIRFactoryJs);
  defineVerificationResultPropsJs(js, def);
end;


procedure defineVisionPrescriptionDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionDispense', 'product', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'eye', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'sphere', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'cylinder', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'axis', 'integer', getFHIRIntegerProp, setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'prism', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'base', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'add', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'power', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'backCurve', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'diameter', 'decimal', getFHIRDecimalProp, setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'duration', 'Quantity', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'color', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'brand', 'string', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionDispense', 'note', 'Annotation', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineVisionPrescriptionDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionDispense', nil, 'VisionPrescriptionDispense', FHIRFactoryJs);
  defineVisionPrescriptionDispensePropsJs(js, def);
end;


procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription', 'identifier', 'Identifier', getFHIRArrayProp, setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription', 'status', 'code', getFHIRStringProp, setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription', 'patient', 'Reference(Patient)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'encounter', 'Reference(Encounter)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'dateWritten', 'dateTime', getFHIRDateTimeProp, setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription', 'prescriber', 'Reference(Practitioner)', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'reasonCodeableConcept', 'CodeableConcept', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'reasonReference', 'Reference', getFHIRObjectProp, setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'dispense', '', getFHIRArrayProp, setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription', nil, 'VisionPrescription', FHIRFactoryJs);
  defineVisionPrescriptionPropsJs(js, def);
end;


procedure registerFHIRTypes(js : TFHIRJavascript);

begin

  defineParametersParameterJs(js); 
  defineParametersJs(js); 
  defineExtensionJs(js); 
  defineNarrativeJs(js); 
  defineContributorJs(js); 
  defineAttachmentJs(js); 
  defineDataRequirementCodeFilterJs(js); 
  defineDataRequirementDateFilterJs(js); 
  defineDataRequirementJs(js); 
  defineDosageJs(js); 
  defineMarketingStatusJs(js); 
  defineIdentifierJs(js); 
  defineSubstanceAmountReferenceRangeJs(js); 
  defineSubstanceAmountJs(js); 
  defineCodingJs(js); 
  defineSampledDataJs(js); 
  defineRatioJs(js); 
  defineSubstanceMoietyJs(js); 
  defineReferenceJs(js); 
  defineTriggerDefinitionConditionJs(js); 
  defineTriggerDefinitionJs(js); 
  definePeriodJs(js); 
  defineQuantityJs(js); 
  defineRangeJs(js); 
  defineRelatedArtifactJs(js); 
  defineAnnotationJs(js); 
  defineProductShelfLifeJs(js); 
  defineContactDetailJs(js); 
  defineUsageContextJs(js); 
  defineSignatureJs(js); 
  defineProdCharacteristicJs(js); 
  defineCodeableConceptJs(js); 
  defineParameterDefinitionJs(js); 
  defineContactPointJs(js); 
  defineHumanNameJs(js); 
  defineMetaJs(js); 
  defineAddressJs(js); 
  defineElementDefinitionSlicingJs(js); 
  defineElementDefinitionSlicingDiscriminatorJs(js); 
  defineElementDefinitionBaseJs(js); 
  defineElementDefinitionTypeJs(js); 
  defineElementDefinitionExampleJs(js); 
  defineElementDefinitionConstraintJs(js); 
  defineElementDefinitionBindingJs(js); 
  defineElementDefinitionMappingJs(js); 
  defineElementDefinitionJs(js); 
  defineTimingRepeatJs(js); 
  defineTimingJs(js); 
  defineCountJs(js); 
  defineMoneyJs(js); 
  defineAgeJs(js); 
  defineDistanceJs(js); 
  defineDurationJs(js); 
  defineAccountCoverageJs(js); 
  defineAccountGuarantorJs(js); 
  defineAccountJs(js); 
  defineActivityDefinitionParticipantJs(js); 
  defineActivityDefinitionDynamicValueJs(js); 
  defineActivityDefinitionJs(js); 
  defineAdverseEventSuspectEntityJs(js); 
  defineAdverseEventSuspectEntityCausalityJs(js); 
  defineAdverseEventJs(js); 
  defineAllergyIntoleranceReactionJs(js); 
  defineAllergyIntoleranceJs(js); 
  defineAppointmentParticipantJs(js); 
  defineAppointmentJs(js); 
  defineAppointmentResponseJs(js); 
  defineAuditEventAgentJs(js); 
  defineAuditEventAgentNetworkJs(js); 
  defineAuditEventSourceJs(js); 
  defineAuditEventEntityJs(js); 
  defineAuditEventEntityDetailJs(js); 
  defineAuditEventJs(js); 
  defineBasicJs(js); 
  defineBinaryJs(js); 
  defineBiologicallyDerivedProductCollectionJs(js); 
  defineBiologicallyDerivedProductProcessingJs(js); 
  defineBiologicallyDerivedProductManipulationJs(js); 
  defineBiologicallyDerivedProductStorageJs(js); 
  defineBiologicallyDerivedProductJs(js); 
  defineBodyStructureJs(js); 
  defineBundleLinkJs(js); 
  defineBundleEntryJs(js); 
  defineBundleEntrySearchJs(js); 
  defineBundleEntryRequestJs(js); 
  defineBundleEntryResponseJs(js); 
  defineBundleJs(js); 
  defineCapabilityStatementSoftwareJs(js); 
  defineCapabilityStatementImplementationJs(js); 
  defineCapabilityStatementRestJs(js); 
  defineCapabilityStatementRestSecurityJs(js); 
  defineCapabilityStatementRestSecurityCertificateJs(js); 
  defineCapabilityStatementRestResourceJs(js); 
  defineCapabilityStatementRestResourceInteractionJs(js); 
  defineCapabilityStatementRestResourceSearchParamJs(js); 
  defineCapabilityStatementRestResourceOperationJs(js); 
  defineCapabilityStatementRestInteractionJs(js); 
  defineCapabilityStatementMessagingJs(js); 
  defineCapabilityStatementMessagingEndpointJs(js); 
  defineCapabilityStatementMessagingSupportedMessageJs(js); 
  defineCapabilityStatementDocumentJs(js); 
  defineCapabilityStatementJs(js); 
  defineCarePlanActivityJs(js); 
  defineCarePlanActivityDetailJs(js); 
  defineCarePlanJs(js); 
  defineCareTeamParticipantJs(js); 
  defineCareTeamJs(js); 
  defineChargeItemParticipantJs(js); 
  defineChargeItemJs(js); 
  defineClaimRelatedJs(js); 
  defineClaimPayeeJs(js); 
  defineClaimCareTeamJs(js); 
  defineClaimInformationJs(js); 
  defineClaimDiagnosisJs(js); 
  defineClaimProcedureJs(js); 
  defineClaimInsuranceJs(js); 
  defineClaimAccidentJs(js); 
  defineClaimItemJs(js); 
  defineClaimItemDetailJs(js); 
  defineClaimItemDetailSubDetailJs(js); 
  defineClaimJs(js); 
  defineClaimResponseItemJs(js); 
  defineClaimResponseItemAdjudicationJs(js); 
  defineClaimResponseItemDetailJs(js); 
  defineClaimResponseItemDetailSubDetailJs(js); 
  defineClaimResponseAddItemJs(js); 
  defineClaimResponseErrorJs(js); 
  defineClaimResponsePaymentJs(js); 
  defineClaimResponseProcessNoteJs(js); 
  defineClaimResponseInsuranceJs(js); 
  defineClaimResponseJs(js); 
  defineClinicalImpressionInvestigationJs(js); 
  defineClinicalImpressionFindingJs(js); 
  defineClinicalImpressionJs(js); 
  defineCodeSystemFilterJs(js); 
  defineCodeSystemPropertyJs(js); 
  defineCodeSystemConceptJs(js); 
  defineCodeSystemConceptDesignationJs(js); 
  defineCodeSystemConceptPropertyJs(js); 
  defineCodeSystemJs(js); 
  defineCommunicationPayloadJs(js); 
  defineCommunicationJs(js); 
  defineCommunicationRequestPayloadJs(js); 
  defineCommunicationRequestJs(js); 
  defineCompartmentDefinitionResourceJs(js); 
  defineCompartmentDefinitionJs(js); 
  defineCompositionAttesterJs(js); 
  defineCompositionRelatesToJs(js); 
  defineCompositionEventJs(js); 
  defineCompositionSectionJs(js); 
  defineCompositionJs(js); 
  defineConceptMapGroupJs(js); 
  defineConceptMapGroupElementJs(js); 
  defineConceptMapGroupElementTargetJs(js); 
  defineConceptMapGroupElementTargetDependsOnJs(js); 
  defineConceptMapGroupUnmappedJs(js); 
  defineConceptMapJs(js); 
  defineConditionStageJs(js); 
  defineConditionEvidenceJs(js); 
  defineConditionJs(js); 
  defineConsentPolicyJs(js); 
  defineConsentVerificationJs(js); 
  defineConsentProvisionJs(js); 
  defineConsentProvisionActorJs(js); 
  defineConsentProvisionDataJs(js); 
  defineConsentJs(js); 
  defineContractTermJs(js); 
  defineContractTermOfferJs(js); 
  defineContractTermAssetJs(js); 
  defineContractTermAssetDataJs(js); 
  defineContractTermAssetValuedItemJs(js); 
  defineContractTermAgentJs(js); 
  defineContractSignerJs(js); 
  defineContractFriendlyJs(js); 
  defineContractLegalJs(js); 
  defineContractRuleJs(js); 
  defineContractJs(js); 
  defineCoverageClassJs(js); 
  defineCoverageGroupingJs(js); 
  defineCoverageCopayJs(js); 
  defineCoverageJs(js); 
  defineDetectedIssueMitigationJs(js); 
  defineDetectedIssueJs(js); 
  defineDeviceUdiJs(js); 
  defineDeviceJs(js); 
  defineDeviceComponentProductionSpecificationJs(js); 
  defineDeviceComponentPropertyJs(js); 
  defineDeviceComponentJs(js); 
  defineDeviceMetricCalibrationJs(js); 
  defineDeviceMetricJs(js); 
  defineDeviceRequestParameterJs(js); 
  defineDeviceRequestJs(js); 
  defineDeviceUseStatementJs(js); 
  defineDiagnosticReportMediaJs(js); 
  defineDiagnosticReportJs(js); 
  defineDocumentManifestAgentJs(js); 
  defineDocumentManifestRelatedJs(js); 
  defineDocumentManifestJs(js); 
  defineDocumentReferenceAgentJs(js); 
  defineDocumentReferenceRelatesToJs(js); 
  defineDocumentReferenceContentJs(js); 
  defineDocumentReferenceContextJs(js); 
  defineDocumentReferenceContextRelatedJs(js); 
  defineDocumentReferenceJs(js); 
  defineEligibilityRequestAuthorizationJs(js); 
  defineEligibilityRequestJs(js); 
  defineEligibilityResponseInsuranceJs(js); 
  defineEligibilityResponseInsuranceBenefitBalanceJs(js); 
  defineEligibilityResponseInsuranceBenefitBalanceFinancialJs(js); 
  defineEligibilityResponseAuthorizationJs(js); 
  defineEligibilityResponseErrorJs(js); 
  defineEligibilityResponseJs(js); 
  defineEncounterStatusHistoryJs(js); 
  defineEncounterClassHistoryJs(js); 
  defineEncounterParticipantJs(js); 
  defineEncounterDiagnosisJs(js); 
  defineEncounterHospitalizationJs(js); 
  defineEncounterLocationJs(js); 
  defineEncounterJs(js); 
  defineEndpointJs(js); 
  defineEnrollmentRequestJs(js); 
  defineEnrollmentResponseJs(js); 
  defineEntryDefinitionRelatedEntryJs(js); 
  defineEntryDefinitionJs(js); 
  defineEpisodeOfCareStatusHistoryJs(js); 
  defineEpisodeOfCareDiagnosisJs(js); 
  defineEpisodeOfCareJs(js); 
  defineEventDefinitionJs(js); 
  defineExampleScenarioActorJs(js); 
  defineExampleScenarioInstanceJs(js); 
  defineExampleScenarioInstanceVersionJs(js); 
  defineExampleScenarioInstanceContainedInstanceJs(js); 
  defineExampleScenarioProcessJs(js); 
  defineExampleScenarioProcessStepJs(js); 
  defineExampleScenarioProcessStepOperationJs(js); 
  defineExampleScenarioProcessStepAlternativeJs(js); 
  defineExampleScenarioProcessStepAlternativeOptionJs(js); 
  defineExampleScenarioJs(js); 
  defineExpansionProfileFixedVersionJs(js); 
  defineExpansionProfileExcludedSystemJs(js); 
  defineExpansionProfileDesignationJs(js); 
  defineExpansionProfileDesignationIncludeJs(js); 
  defineExpansionProfileDesignationIncludeDesignationJs(js); 
  defineExpansionProfileDesignationExcludeJs(js); 
  defineExpansionProfileDesignationExcludeDesignationJs(js); 
  defineExpansionProfileJs(js); 
  defineExplanationOfBenefitRelatedJs(js); 
  defineExplanationOfBenefitPayeeJs(js); 
  defineExplanationOfBenefitInformationJs(js); 
  defineExplanationOfBenefitCareTeamJs(js); 
  defineExplanationOfBenefitDiagnosisJs(js); 
  defineExplanationOfBenefitProcedureJs(js); 
  defineExplanationOfBenefitInsuranceJs(js); 
  defineExplanationOfBenefitAccidentJs(js); 
  defineExplanationOfBenefitItemJs(js); 
  defineExplanationOfBenefitItemAdjudicationJs(js); 
  defineExplanationOfBenefitItemDetailJs(js); 
  defineExplanationOfBenefitItemDetailSubDetailJs(js); 
  defineExplanationOfBenefitAddItemJs(js); 
  defineExplanationOfBenefitPaymentJs(js); 
  defineExplanationOfBenefitProcessNoteJs(js); 
  defineExplanationOfBenefitBenefitBalanceJs(js); 
  defineExplanationOfBenefitBenefitBalanceFinancialJs(js); 
  defineExplanationOfBenefitJs(js); 
  defineFamilyMemberHistoryConditionJs(js); 
  defineFamilyMemberHistoryJs(js); 
  defineFlagJs(js); 
  defineGoalTargetJs(js); 
  defineGoalJs(js); 
  defineGraphDefinitionLinkJs(js); 
  defineGraphDefinitionLinkTargetJs(js); 
  defineGraphDefinitionLinkTargetCompartmentJs(js); 
  defineGraphDefinitionJs(js); 
  defineGroupCharacteristicJs(js); 
  defineGroupMemberJs(js); 
  defineGroupJs(js); 
  defineGuidanceResponseJs(js); 
  defineHealthcareServiceAvailableTimeJs(js); 
  defineHealthcareServiceNotAvailableJs(js); 
  defineHealthcareServiceJs(js); 
  defineImagingStudySeriesJs(js); 
  defineImagingStudySeriesInstanceJs(js); 
  defineImagingStudyJs(js); 
  defineImmunizationPractitionerJs(js); 
  defineImmunizationEducationJs(js); 
  defineImmunizationJs(js); 
  defineImmunizationEvaluationJs(js); 
  defineImmunizationRecommendationRecommendationJs(js); 
  defineImmunizationRecommendationRecommendationDateCriterionJs(js); 
  defineImmunizationRecommendationJs(js); 
  defineImplementationGuideDependencyJs(js); 
  defineImplementationGuidePackageJs(js); 
  defineImplementationGuidePackageResourceJs(js); 
  defineImplementationGuideGlobalJs(js); 
  defineImplementationGuidePageJs(js); 
  defineImplementationGuideJs(js); 
  defineImplementationGuideInputDependencyJs(js); 
  defineImplementationGuideInputPackageJs(js); 
  defineImplementationGuideInputPackageResourceJs(js); 
  defineImplementationGuideInputGlobalJs(js); 
  defineImplementationGuideInputPageJs(js); 
  defineImplementationGuideInputJs(js); 
  defineImplementationGuideOutputDependencyJs(js); 
  defineImplementationGuideOutputResourceJs(js); 
  defineImplementationGuideOutputGlobalJs(js); 
  defineImplementationGuideOutputPageJs(js); 
  defineImplementationGuideOutputJs(js); 
  defineInvoiceParticipantJs(js); 
  defineInvoiceLineItemJs(js); 
  defineInvoiceLineItemPriceComponentJs(js); 
  defineInvoiceJs(js); 
  defineItemInstanceJs(js); 
  defineLibraryJs(js); 
  defineLinkageItemJs(js); 
  defineLinkageJs(js); 
  defineListEntryJs(js); 
  defineListJs(js); 
  defineLocationPositionJs(js); 
  defineLocationHoursOfOperationJs(js); 
  defineLocationJs(js); 
  defineMeasureGroupJs(js); 
  defineMeasureGroupPopulationJs(js); 
  defineMeasureGroupStratifierJs(js); 
  defineMeasureSupplementalDataJs(js); 
  defineMeasureJs(js); 
  defineMeasureReportGroupJs(js); 
  defineMeasureReportGroupPopulationJs(js); 
  defineMeasureReportGroupStratifierJs(js); 
  defineMeasureReportGroupStratifierStratumJs(js); 
  defineMeasureReportGroupStratifierStratumPopulationJs(js); 
  defineMeasureReportJs(js); 
  defineMediaJs(js); 
  defineMedicationIngredientJs(js); 
  defineMedicationBatchJs(js); 
  defineMedicationJs(js); 
  defineMedicationAdministrationPerformerJs(js); 
  defineMedicationAdministrationDosageJs(js); 
  defineMedicationAdministrationJs(js); 
  defineMedicationDispensePerformerJs(js); 
  defineMedicationDispenseSubstitutionJs(js); 
  defineMedicationDispenseJs(js); 
  defineMedicationRequestDispenseRequestJs(js); 
  defineMedicationRequestSubstitutionJs(js); 
  defineMedicationRequestJs(js); 
  defineMedicationStatementJs(js); 
  defineMedicinalProductNameJs(js); 
  defineMedicinalProductNameNamePartJs(js); 
  defineMedicinalProductNameCountryLanguageJs(js); 
  defineMedicinalProductManufacturingBusinessOperationJs(js); 
  defineMedicinalProductJs(js); 
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js); 
  defineMedicinalProductAuthorizationProcedureJs(js); 
  defineMedicinalProductAuthorizationProcedureApplicationJs(js); 
  defineMedicinalProductAuthorizationJs(js); 
  defineMedicinalProductClinicalsUndesirableEffectsJs(js); 
  defineMedicinalProductClinicalsUndesirableEffectsPopulationJs(js); 
  defineMedicinalProductClinicalsTherapeuticIndicationJs(js); 
  defineMedicinalProductClinicalsTherapeuticIndicationOtherTherapyJs(js); 
  defineMedicinalProductClinicalsContraindicationJs(js); 
  defineMedicinalProductClinicalsInteractionsJs(js); 
  defineMedicinalProductClinicalsJs(js); 
  defineMedicinalProductDeviceSpecMaterialJs(js); 
  defineMedicinalProductDeviceSpecJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js); 
  defineMedicinalProductIngredientSubstanceJs(js); 
  defineMedicinalProductIngredientJs(js); 
  defineMedicinalProductPackagedBatchIdentifierJs(js); 
  defineMedicinalProductPackagedPackageItemJs(js); 
  defineMedicinalProductPackagedPackageItemManufacturedItemJs(js); 
  defineMedicinalProductPackagedJs(js); 
  defineMedicinalProductPharmaceuticalCharacteristicsJs(js); 
  defineMedicinalProductPharmaceuticalJs(js); 
  defineMessageDefinitionFocusJs(js); 
  defineMessageDefinitionAllowedResponseJs(js); 
  defineMessageDefinitionJs(js); 
  defineMessageHeaderDestinationJs(js); 
  defineMessageHeaderSourceJs(js); 
  defineMessageHeaderResponseJs(js); 
  defineMessageHeaderJs(js); 
  defineNamingSystemUniqueIdJs(js); 
  defineNamingSystemJs(js); 
  defineNutritionOrderOralDietJs(js); 
  defineNutritionOrderOralDietNutrientJs(js); 
  defineNutritionOrderOralDietTextureJs(js); 
  defineNutritionOrderSupplementJs(js); 
  defineNutritionOrderEnteralFormulaJs(js); 
  defineNutritionOrderEnteralFormulaAdministrationJs(js); 
  defineNutritionOrderJs(js); 
  defineObservationReferenceRangeJs(js); 
  defineObservationComponentJs(js); 
  defineObservationJs(js); 
  defineObservationDefinitionQuantitativeDetailsJs(js); 
  defineObservationDefinitionQualifiedIntervalJs(js); 
  defineObservationDefinitionJs(js); 
  defineOccupationalDataEmploymentStatusJs(js); 
  defineOccupationalDataRetirementStatusJs(js); 
  defineOccupationalDataCombatZoneHazardousDutyJs(js); 
  defineOccupationalDataUsualOccupationJs(js); 
  defineOccupationalDataUsualOccupationDurationJs(js); 
  defineOccupationalDataUsualOccupationIndustryJs(js); 
  defineOccupationalDataPastOrPresentOccupationJs(js); 
  defineOccupationalDataJs(js); 
  defineOperationDefinitionParameterJs(js); 
  defineOperationDefinitionParameterBindingJs(js); 
  defineOperationDefinitionOverloadJs(js); 
  defineOperationDefinitionJs(js); 
  defineOperationOutcomeIssueJs(js); 
  defineOperationOutcomeJs(js); 
  defineOrganizationContactJs(js); 
  defineOrganizationJs(js); 
  defineOrganizationRoleAvailableTimeJs(js); 
  defineOrganizationRoleNotAvailableJs(js); 
  defineOrganizationRoleJs(js); 
  definePatientContactJs(js); 
  definePatientAnimalJs(js); 
  definePatientCommunicationJs(js); 
  definePatientLinkJs(js); 
  definePatientJs(js); 
  definePaymentNoticeJs(js); 
  definePaymentReconciliationDetailJs(js); 
  definePaymentReconciliationProcessNoteJs(js); 
  definePaymentReconciliationJs(js); 
  definePersonLinkJs(js); 
  definePersonJs(js); 
  definePlanDefinitionGoalJs(js); 
  definePlanDefinitionGoalTargetJs(js); 
  definePlanDefinitionActionJs(js); 
  definePlanDefinitionActionConditionJs(js); 
  definePlanDefinitionActionRelatedActionJs(js); 
  definePlanDefinitionActionParticipantJs(js); 
  definePlanDefinitionActionDynamicValueJs(js); 
  definePlanDefinitionJs(js); 
  definePractitionerQualificationJs(js); 
  definePractitionerJs(js); 
  definePractitionerRoleAvailableTimeJs(js); 
  definePractitionerRoleNotAvailableJs(js); 
  definePractitionerRoleJs(js); 
  defineProcedurePerformerJs(js); 
  defineProcedureFocalDeviceJs(js); 
  defineProcedureJs(js); 
  defineProcessRequestItemJs(js); 
  defineProcessRequestJs(js); 
  defineProcessResponseProcessNoteJs(js); 
  defineProcessResponseJs(js); 
  defineProductPlanContactJs(js); 
  defineProductPlanCoverageJs(js); 
  defineProductPlanCoverageBenefitJs(js); 
  defineProductPlanCoverageBenefitItemJs(js); 
  defineProductPlanPlanJs(js); 
  defineProductPlanPlanCategoryJs(js); 
  defineProductPlanPlanCategoryBenefitJs(js); 
  defineProductPlanPlanCategoryBenefitCostJs(js); 
  defineProductPlanJs(js); 
  defineProvenanceAgentJs(js); 
  defineProvenanceEntityJs(js); 
  defineProvenanceJs(js); 
  defineQuestionnaireItemJs(js); 
  defineQuestionnaireItemEnableWhenJs(js); 
  defineQuestionnaireItemOptionJs(js); 
  defineQuestionnaireJs(js); 
  defineQuestionnaireResponseItemJs(js); 
  defineQuestionnaireResponseItemAnswerJs(js); 
  defineQuestionnaireResponseJs(js); 
  defineRelatedPersonJs(js); 
  defineRequestGroupActionJs(js); 
  defineRequestGroupActionConditionJs(js); 
  defineRequestGroupActionRelatedActionJs(js); 
  defineRequestGroupJs(js); 
  defineResearchStudyArmJs(js); 
  defineResearchStudyObjectiveJs(js); 
  defineResearchStudyJs(js); 
  defineResearchSubjectJs(js); 
  defineRiskAssessmentPredictionJs(js); 
  defineRiskAssessmentJs(js); 
  defineScheduleJs(js); 
  defineSearchParameterComponentJs(js); 
  defineSearchParameterJs(js); 
  defineSequenceReferenceSeqJs(js); 
  defineSequenceVariantJs(js); 
  defineSequenceQualityJs(js); 
  defineSequenceQualityRocJs(js); 
  defineSequenceRepositoryJs(js); 
  defineSequenceStructureVariantJs(js); 
  defineSequenceStructureVariantOuterJs(js); 
  defineSequenceStructureVariantInnerJs(js); 
  defineSequenceJs(js); 
  defineServiceDefinitionJs(js); 
  defineServiceRequestJs(js); 
  defineSlotJs(js); 
  defineSpecimenCollectionJs(js); 
  defineSpecimenProcessingJs(js); 
  defineSpecimenContainerJs(js); 
  defineSpecimenJs(js); 
  defineSpecimenDefinitionSpecimenToLabJs(js); 
  defineSpecimenDefinitionSpecimenToLabContainerAdditiveJs(js); 
  defineSpecimenDefinitionSpecimenToLabHandlingJs(js); 
  defineSpecimenDefinitionJs(js); 
  defineStructureDefinitionMappingJs(js); 
  defineStructureDefinitionSnapshotJs(js); 
  defineStructureDefinitionDifferentialJs(js); 
  defineStructureDefinitionJs(js); 
  defineStructureMapStructureJs(js); 
  defineStructureMapGroupJs(js); 
  defineStructureMapGroupInputJs(js); 
  defineStructureMapGroupRuleJs(js); 
  defineStructureMapGroupRuleSourceJs(js); 
  defineStructureMapGroupRuleTargetJs(js); 
  defineStructureMapGroupRuleTargetParameterJs(js); 
  defineStructureMapGroupRuleDependentJs(js); 
  defineStructureMapJs(js); 
  defineSubscriptionChannelJs(js); 
  defineSubscriptionJs(js); 
  defineSubstanceInstanceJs(js); 
  defineSubstanceIngredientJs(js); 
  defineSubstanceJs(js); 
  defineSubstancePolymerMonomerSetJs(js); 
  defineSubstancePolymerMonomerSetStartingMaterialJs(js); 
  defineSubstancePolymerRepeatJs(js); 
  defineSubstancePolymerRepeatRepeatUnitJs(js); 
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js); 
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js); 
  defineSubstancePolymerJs(js); 
  defineSubstanceReferenceInformationGeneJs(js); 
  defineSubstanceReferenceInformationGeneElementJs(js); 
  defineSubstanceReferenceInformationClassificationJs(js); 
  defineSubstanceReferenceInformationRelationshipJs(js); 
  defineSubstanceReferenceInformationTargetJs(js); 
  defineSubstanceReferenceInformationJs(js); 
  defineSubstanceSpecificationMoietyJs(js); 
  defineSubstanceSpecificationPropertyJs(js); 
  defineSubstanceSpecificationStructureJs(js); 
  defineSubstanceSpecificationStructureIsotopeJs(js); 
  defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js); 
  defineSubstanceSpecificationStructureStructuralRepresentationJs(js); 
  defineSubstanceSpecificationSubstanceCodeJs(js); 
  defineSubstanceSpecificationSubstanceNameJs(js); 
  defineSubstanceSpecificationSubstanceNameOfficialNameJs(js); 
  defineSubstanceSpecificationJs(js); 
  defineSupplyDeliverySuppliedItemJs(js); 
  defineSupplyDeliveryJs(js); 
  defineSupplyRequestParameterJs(js); 
  defineSupplyRequestJs(js); 
  defineTaskRestrictionJs(js); 
  defineTaskInputJs(js); 
  defineTaskOutputJs(js); 
  defineTaskJs(js); 
  defineTerminologyCapabilitiesCodeSystemJs(js); 
  defineTerminologyCapabilitiesCodeSystemVersionJs(js); 
  defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js); 
  defineTerminologyCapabilitiesExpansionJs(js); 
  defineTerminologyCapabilitiesValidateCodeJs(js); 
  defineTerminologyCapabilitiesTranslationJs(js); 
  defineTerminologyCapabilitiesClosureJs(js); 
  defineTerminologyCapabilitiesJs(js); 
  defineTestReportParticipantJs(js); 
  defineTestReportSetupJs(js); 
  defineTestReportSetupActionJs(js); 
  defineTestReportSetupActionOperationJs(js); 
  defineTestReportSetupActionAssertJs(js); 
  defineTestReportTestJs(js); 
  defineTestReportTestActionJs(js); 
  defineTestReportTeardownJs(js); 
  defineTestReportTeardownActionJs(js); 
  defineTestReportJs(js); 
  defineTestScriptOriginJs(js); 
  defineTestScriptDestinationJs(js); 
  defineTestScriptMetadataJs(js); 
  defineTestScriptMetadataLinkJs(js); 
  defineTestScriptMetadataCapabilityJs(js); 
  defineTestScriptFixtureJs(js); 
  defineTestScriptVariableJs(js); 
  defineTestScriptRuleJs(js); 
  defineTestScriptRuleParamJs(js); 
  defineTestScriptRulesetJs(js); 
  defineTestScriptRulesetRuleJs(js); 
  defineTestScriptRulesetRuleParamJs(js); 
  defineTestScriptSetupJs(js); 
  defineTestScriptSetupActionJs(js); 
  defineTestScriptSetupActionOperationJs(js); 
  defineTestScriptSetupActionOperationRequestHeaderJs(js); 
  defineTestScriptSetupActionAssertJs(js); 
  defineTestScriptSetupActionAssertRuleJs(js); 
  defineTestScriptSetupActionAssertRuleParamJs(js); 
  defineTestScriptSetupActionAssertRulesetJs(js); 
  defineTestScriptSetupActionAssertRulesetRuleJs(js); 
  defineTestScriptSetupActionAssertRulesetRuleParamJs(js); 
  defineTestScriptTestJs(js); 
  defineTestScriptTestActionJs(js); 
  defineTestScriptTeardownJs(js); 
  defineTestScriptTeardownActionJs(js); 
  defineTestScriptJs(js); 
  defineUserSessionStatusJs(js); 
  defineUserSessionContextJs(js); 
  defineUserSessionJs(js); 
  defineValueSetComposeJs(js); 
  defineValueSetComposeIncludeJs(js); 
  defineValueSetComposeIncludeConceptJs(js); 
  defineValueSetComposeIncludeConceptDesignationJs(js); 
  defineValueSetComposeIncludeFilterJs(js); 
  defineValueSetExpansionJs(js); 
  defineValueSetExpansionParameterJs(js); 
  defineValueSetExpansionContainsJs(js); 
  defineValueSetJs(js); 
  defineVerificationResultPrimarySourceJs(js); 
  defineVerificationResultAttestationJs(js); 
  defineVerificationResultValidatorJs(js); 
  defineVerificationResultJs(js); 
  defineVisionPrescriptionDispenseJs(js); 
  defineVisionPrescriptionJs(js); 

end;

end.

