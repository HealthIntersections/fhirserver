unit FHIR.R4.Javascript;

{$I fhir.r4.inc}

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

// FHIR v4.0.0 generated 2018-12-13T15:13:20+11:00

uses
  FHIR.Javascript, FHIR.Javascript.Base;

procedure registerFHIRTypes(js : TFHIRJavascript);

implementation

procedure defineElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Element', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Element', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneElement', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineBackboneTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'BackboneType', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  js.registerElement(def, 'Resource', 'id', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource', 'meta', 'Meta', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Resource', 'implicitRules', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Resource', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;


procedure defineDomainResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'DomainResource', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DomainResource', 'contained', 'Resource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'extension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DomainResource', 'modifierExtension', 'Extension', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;


procedure defineParametersParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ParametersParameter', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ParametersParameter', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ParametersParameter', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ParametersParameter', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ParametersParameter', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParametersParameter', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParametersParameter', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ParametersParameter', 'part', '@Parameters.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParametersParameter', nil, 'ParametersParameter', js.FHIRFactoryJs);
  defineParametersParameterPropsJs(js, def);
end;


procedure defineParametersPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Parameters', 'parameter', 'ParametersParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineParametersJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Parameters', nil, 'Parameters', js.FHIRFactoryJs);
  defineParametersPropsJs(js, def);
end;


procedure defineMetadataResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
end;


procedure defineExtensionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Extension', 'url', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Extension', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Extension', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Extension', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Extension', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Extension', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Extension', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Extension', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExtensionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Extension', nil, 'Extension', js.FHIRFactoryJs);
  defineExtensionPropsJs(js, def);
end;


procedure defineNarrativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Narrative', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Narrative', 'div', 'xhtml', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNarrativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Narrative', nil, 'Narrative', js.FHIRFactoryJs);
  defineNarrativePropsJs(js, def);
end;


procedure defineContributorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Contributor', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contributor', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContributorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contributor', nil, 'Contributor', js.FHIRFactoryJs);
  defineContributorPropsJs(js, def);
end;


procedure defineAttachmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Attachment', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'size', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'hash', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'Attachment', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Attachment', 'creation', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineAttachmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Attachment', nil, 'Attachment', js.FHIRFactoryJs);
  defineAttachmentPropsJs(js, def);
end;


procedure defineDataRequirementCodeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementCodeFilter', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'searchParam', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementCodeFilter', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementCodeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementCodeFilter', nil, 'DataRequirementCodeFilter', js.FHIRFactoryJs);
  defineDataRequirementCodeFilterPropsJs(js, def);
end;


procedure defineDataRequirementDateFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementDateFilter', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'searchParam', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirementDateFilter', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDataRequirementDateFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementDateFilter', nil, 'DataRequirementDateFilter', js.FHIRFactoryJs);
  defineDataRequirementDateFilterPropsJs(js, def);
end;


procedure defineDataRequirementSortPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirementSort', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirementSort', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDataRequirementSortJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirementSort', nil, 'DataRequirementSort', js.FHIRFactoryJs);
  defineDataRequirementSortPropsJs(js, def);
end;


procedure defineDataRequirementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DataRequirement', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DataRequirement', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DataRequirement', 'codeFilter', 'DataRequirementCodeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement', 'dateFilter', 'DataRequirementDateFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DataRequirement', 'limit', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'DataRequirement', 'sort', 'DataRequirementSort', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDataRequirementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DataRequirement', nil, 'DataRequirement', js.FHIRFactoryJs);
  defineDataRequirementPropsJs(js, def);
end;


procedure defineDosageDoseAndRatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'DosageDoseAndRate', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate', 'doseRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate', 'rateRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DosageDoseAndRate', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageDoseAndRateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DosageDoseAndRate', nil, 'DosageDoseAndRate', js.FHIRFactoryJs);
  defineDosageDoseAndRatePropsJs(js, def);
end;


procedure defineDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Dosage', 'sequence', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Dosage', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage', 'additionalInstruction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Dosage', 'timing', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Dosage', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'doseAndRate', 'DosageDoseAndRate', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Dosage', 'maxDosePerPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerAdministration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Dosage', 'maxDosePerLifetime', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Dosage', nil, 'Dosage', js.FHIRFactoryJs);
  defineDosagePropsJs(js, def);
end;


procedure defineMoneyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Money', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Money', 'currency', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMoneyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Money', nil, 'Money', js.FHIRFactoryJs);
  defineMoneyPropsJs(js, def);
end;


procedure defineMarketingStatusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'MarketingStatus', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'dateRange', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MarketingStatus', 'restoreDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMarketingStatusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MarketingStatus', nil, 'MarketingStatus', js.FHIRFactoryJs);
  defineMarketingStatusPropsJs(js, def);
end;


procedure defineIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Identifier', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Identifier', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Identifier', 'assigner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Identifier', nil, 'Identifier', js.FHIRFactoryJs);
  defineIdentifierPropsJs(js, def);
end;


procedure defineSubstanceAmountReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceAmountReferenceRange', 'lowLimit', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmountReferenceRange', 'highLimit', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmountReferenceRange', nil, 'SubstanceAmountReferenceRange', js.FHIRFactoryJs);
  defineSubstanceAmountReferenceRangePropsJs(js, def);
end;


procedure defineSubstanceAmountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'SubstanceAmount', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceAmount', 'amountText', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceAmount', 'referenceRange', 'SubstanceAmountReferenceRange', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceAmountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceAmount', nil, 'SubstanceAmount', js.FHIRFactoryJs);
  defineSubstanceAmountPropsJs(js, def);
end;


procedure defineCodingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Coding', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coding', 'userSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCodingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coding', nil, 'Coding', js.FHIRFactoryJs);
  defineCodingPropsJs(js, def);
end;


procedure defineSampledDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'SampledData', 'origin', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SampledData', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'lowerLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'upperLimit', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'SampledData', 'dimensions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SampledData', 'data', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSampledDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SampledData', nil, 'SampledData', js.FHIRFactoryJs);
  defineSampledDataPropsJs(js, def);
end;


procedure definePopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Population', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population', 'ageCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population', 'gender', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population', 'race', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Population', 'physiologicalCondition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Population', nil, 'Population', js.FHIRFactoryJs);
  definePopulationPropsJs(js, def);
end;


procedure defineRatioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Ratio', 'numerator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Ratio', 'denominator', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRatioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Ratio', nil, 'Ratio', js.FHIRFactoryJs);
  defineRatioPropsJs(js, def);
end;


procedure defineReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Reference', 'reference', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference', 'type', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Reference', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Reference', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Reference', nil, 'Reference', js.FHIRFactoryJs);
  defineReferencePropsJs(js, def);
end;


procedure defineTriggerDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TriggerDefinition', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TriggerDefinition', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'timingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TriggerDefinition', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TriggerDefinition', 'data', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TriggerDefinition', 'condition', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTriggerDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TriggerDefinition', nil, 'TriggerDefinition', js.FHIRFactoryJs);
  defineTriggerDefinitionPropsJs(js, def);
end;


procedure definePeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Period', 'start', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Period', 'end', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure definePeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Period', nil, 'Period', js.FHIRFactoryJs);
  definePeriodPropsJs(js, def);
end;


procedure defineQuantityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Quantity', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Quantity', 'comparator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'unit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Quantity', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineQuantityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Quantity', nil, 'Quantity', js.FHIRFactoryJs);
  defineQuantityPropsJs(js, def);
end;


procedure defineRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Range', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Range', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Range', nil, 'Range', js.FHIRFactoryJs);
  defineRangePropsJs(js, def);
end;


procedure defineRelatedArtifactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'RelatedArtifact', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'citation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedArtifact', 'document', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedArtifact', 'resource', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRelatedArtifactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedArtifact', nil, 'RelatedArtifact', js.FHIRFactoryJs);
  defineRelatedArtifactPropsJs(js, def);
end;


procedure defineAnnotationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Annotation', 'authorReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Annotation', 'authorString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Annotation', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Annotation', 'text', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAnnotationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Annotation', nil, 'Annotation', js.FHIRFactoryJs);
  defineAnnotationPropsJs(js, def);
end;


procedure defineProductShelfLifePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProductShelfLife', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'period', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProductShelfLife', 'specialPrecautionsForStorage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProductShelfLifeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProductShelfLife', nil, 'ProductShelfLife', js.FHIRFactoryJs);
  defineProductShelfLifePropsJs(js, def);
end;


procedure defineContactDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactDetail', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactDetail', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContactDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactDetail', nil, 'ContactDetail', js.FHIRFactoryJs);
  defineContactDetailPropsJs(js, def);
end;


procedure defineExpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Expression', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Expression', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Expression', nil, 'Expression', js.FHIRFactoryJs);
  defineExpressionPropsJs(js, def);
end;


procedure defineUsageContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'UsageContext', 'code', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'UsageContext', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineUsageContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('UsageContext', nil, 'UsageContext', js.FHIRFactoryJs);
  defineUsageContextPropsJs(js, def);
end;


procedure defineSignaturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Signature', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Signature', 'when', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Signature', 'who', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'onBehalfOf', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Signature', 'targetFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'sigFormat', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Signature', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineSignatureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Signature', nil, 'Signature', js.FHIRFactoryJs);
  defineSignaturePropsJs(js, def);
end;


procedure defineProdCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ProdCharacteristic', 'height', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'width', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'depth', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'weight', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'nominalVolume', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'externalDiameter', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProdCharacteristic', 'shape', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProdCharacteristic', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProdCharacteristic', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProdCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProdCharacteristic', nil, 'ProdCharacteristic', js.FHIRFactoryJs);
  defineProdCharacteristicPropsJs(js, def);
end;


procedure defineCodeableConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'CodeableConcept', 'coding', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeableConcept', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeableConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeableConcept', nil, 'CodeableConcept', js.FHIRFactoryJs);
  defineCodeableConceptPropsJs(js, def);
end;


procedure defineParameterDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ParameterDefinition', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ParameterDefinition', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ParameterDefinition', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineParameterDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ParameterDefinition', nil, 'ParameterDefinition', js.FHIRFactoryJs);
  defineParameterDefinitionPropsJs(js, def);
end;


procedure defineContactPointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ContactPoint', 'system', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContactPoint', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContactPoint', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContactPointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContactPoint', nil, 'ContactPoint', js.FHIRFactoryJs);
  defineContactPointPropsJs(js, def);
end;


procedure defineHumanNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'HumanName', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'family', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HumanName', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHumanNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HumanName', nil, 'HumanName', js.FHIRFactoryJs);
  defineHumanNamePropsJs(js, def);
end;


procedure defineMetaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Meta', 'versionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta', 'lastUpdated', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Meta', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Meta', 'security', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Meta', 'tag', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMetaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Meta', nil, 'Meta', js.FHIRFactoryJs);
  defineMetaPropsJs(js, def);
end;


procedure defineAddressPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'Address', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'city', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'district', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'state', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'postalCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'country', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Address', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAddressJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Address', nil, 'Address', js.FHIRFactoryJs);
  defineAddressPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicing', 'discriminator', 'ElementDefinitionSlicingDiscriminator', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'ordered', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionSlicing', 'rules', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicing', nil, 'ElementDefinitionSlicing', js.FHIRFactoryJs);
  defineElementDefinitionSlicingPropsJs(js, def);
end;


procedure defineElementDefinitionSlicingDiscriminatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionSlicingDiscriminator', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionSlicingDiscriminatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionSlicingDiscriminator', nil, 'ElementDefinitionSlicingDiscriminator', js.FHIRFactoryJs);
  defineElementDefinitionSlicingDiscriminatorPropsJs(js, def);
end;


procedure defineElementDefinitionBasePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBase', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBase', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBaseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBase', nil, 'ElementDefinitionBase', js.FHIRFactoryJs);
  defineElementDefinitionBasePropsJs(js, def);
end;


procedure defineElementDefinitionTypePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionType', 'code', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionType', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionTypeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionType', nil, 'ElementDefinitionType', js.FHIRFactoryJs);
  defineElementDefinitionTypePropsJs(js, def);
end;


procedure defineElementDefinitionExamplePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionExample', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinitionExample', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineElementDefinitionExampleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionExample', nil, 'ElementDefinitionExample', js.FHIRFactoryJs);
  defineElementDefinitionExamplePropsJs(js, def);
end;


procedure defineElementDefinitionConstraintPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionConstraint', 'key', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'requirements', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'human', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionConstraint', 'source', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionConstraintJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionConstraint', nil, 'ElementDefinitionConstraint', js.FHIRFactoryJs);
  defineElementDefinitionConstraintPropsJs(js, def);
end;


procedure defineElementDefinitionBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionBinding', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionBinding', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionBinding', nil, 'ElementDefinitionBinding', js.FHIRFactoryJs);
  defineElementDefinitionBindingPropsJs(js, def);
end;


procedure defineElementDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'ElementDefinitionMapping', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'map', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinitionMapping', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineElementDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinitionMapping', nil, 'ElementDefinitionMapping', js.FHIRFactoryJs);
  defineElementDefinitionMappingPropsJs(js, def);
end;


procedure defineElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'ElementDefinition', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'sliceIsConstraining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'slicing', 'ElementDefinitionSlicing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'short', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'definition', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'requirements', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'base', 'ElementDefinitionBase', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'contentReference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'type', 'ElementDefinitionType', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'meaningWhenMissing', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'orderMeaning', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'fixedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'fixedId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'fixedInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'fixedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'fixedDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'ElementDefinition', 'patternBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'patternCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'patternId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'patternInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'patternMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'patternString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'patternAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'patternDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'example', 'ElementDefinitionExample', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'minValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'minValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'minValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'minValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'minValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'minValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'maxValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'maxValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ElementDefinition', 'constraint', 'ElementDefinitionConstraint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ElementDefinition', 'mustSupport', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'isModifier', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'isModifierReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ElementDefinition', 'isSummary', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ElementDefinition', 'binding', 'ElementDefinitionBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ElementDefinition', 'mapping', 'ElementDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ElementDefinition', nil, 'ElementDefinition', js.FHIRFactoryJs);
  defineElementDefinitionPropsJs(js, def);
end;


procedure defineTimingRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineElementPropsJs(js, def);
  js.registerElement(def, 'TimingRepeat', 'boundsDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'boundsPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TimingRepeat', 'count', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'countMax', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'durationUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'frequency', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'frequencyMax', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TimingRepeat', 'period', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodMax', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TimingRepeat', 'periodUnit', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TimingRepeat', 'offset', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTimingRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TimingRepeat', nil, 'TimingRepeat', js.FHIRFactoryJs);
  defineTimingRepeatPropsJs(js, def);
end;


procedure defineTimingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneTypePropsJs(js, def);
  js.registerElement(def, 'Timing', 'repeat', 'TimingRepeat', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Timing', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTimingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Timing', nil, 'Timing', js.FHIRFactoryJs);
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
  def := js.defineClass('Count', nil, 'Count', js.FHIRFactoryJs);
  defineCountPropsJs(js, def);
end;


procedure defineAgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineQuantityPropsJs(js, def);
end;

procedure defineAgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Age', nil, 'Age', js.FHIRFactoryJs);
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
  def := js.defineClass('Distance', nil, 'Distance', js.FHIRFactoryJs);
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
  def := js.defineClass('Duration', nil, 'Duration', js.FHIRFactoryJs);
  defineDurationPropsJs(js, def);
end;


procedure defineAccountCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountCoverage', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountCoverage', 'priority', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineAccountCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountCoverage', nil, 'AccountCoverage', js.FHIRFactoryJs);
  defineAccountCoveragePropsJs(js, def);
end;


procedure defineAccountGuarantorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AccountGuarantor', 'party', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AccountGuarantor', 'onHold', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AccountGuarantor', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountGuarantorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AccountGuarantor', nil, 'AccountGuarantor', js.FHIRFactoryJs);
  defineAccountGuarantorPropsJs(js, def);
end;


procedure defineAccountPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Account', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account', 'subject', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account', 'servicePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account', 'coverage', 'AccountCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Account', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Account', 'guarantor', 'AccountGuarantor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Account', 'partOf', 'Reference(Account)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAccountJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Account', nil, 'Account', js.FHIRFactoryJs);
  defineAccountPropsJs(js, def);
end;


procedure defineActivityDefinitionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionParticipant', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionParticipant', nil, 'ActivityDefinitionParticipant', js.FHIRFactoryJs);
  defineActivityDefinitionParticipantPropsJs(js, def);
end;


procedure defineActivityDefinitionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinitionDynamicValue', 'expression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineActivityDefinitionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinitionDynamicValue', nil, 'ActivityDefinitionDynamicValue', js.FHIRFactoryJs);
  defineActivityDefinitionDynamicValuePropsJs(js, def);
end;


procedure defineActivityDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ActivityDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ActivityDefinition', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ActivityDefinition', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'participant', 'ActivityDefinitionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ActivityDefinition', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'specimenRequirement', 'Reference(SpecimenDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'observationRequirement', 'Reference(ObservationDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'observationResultRequirement', 'Reference(ObservationDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ActivityDefinition', 'transform', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ActivityDefinition', 'dynamicValue', 'ActivityDefinitionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineActivityDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ActivityDefinition', nil, 'ActivityDefinition', js.FHIRFactoryJs);
  defineActivityDefinitionPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntity', 'instance', 'Reference(Immunization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntity', 'causality', 'AdverseEventSuspectEntityCausality', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventSuspectEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntity', nil, 'AdverseEventSuspectEntity', js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityPropsJs(js, def);
end;


procedure defineAdverseEventSuspectEntityCausalityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'assessment', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'productRelatedness', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEventSuspectEntityCausality', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAdverseEventSuspectEntityCausalityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEventSuspectEntityCausality', nil, 'AdverseEventSuspectEntityCausality', js.FHIRFactoryJs);
  defineAdverseEventSuspectEntityCausalityPropsJs(js, def);
end;


procedure defineAdverseEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AdverseEvent', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'actuality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AdverseEvent', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'event', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent', 'detected', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent', 'recordedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AdverseEvent', 'resultingCondition', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'seriousness', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'recorder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AdverseEvent', 'contributor', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'suspectEntity', 'AdverseEventSuspectEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'subjectMedicalHistory', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'referenceDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AdverseEvent', 'study', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAdverseEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AdverseEvent', nil, 'AdverseEvent', js.FHIRFactoryJs);
  defineAdverseEventPropsJs(js, def);
end;


procedure defineAllergyIntoleranceReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'manifestation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'onset', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'exposureRoute', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntoleranceReaction', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntoleranceReaction', nil, 'AllergyIntoleranceReaction', js.FHIRFactoryJs);
  defineAllergyIntoleranceReactionPropsJs(js, def);
end;


procedure defineAllergyIntolerancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AllergyIntolerance', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance', 'clinicalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'verificationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'criticality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AllergyIntolerance', 'recordedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'asserter', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AllergyIntolerance', 'lastOccurrence', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AllergyIntolerance', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AllergyIntolerance', 'reaction', 'AllergyIntoleranceReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAllergyIntoleranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AllergyIntolerance', nil, 'AllergyIntolerance', js.FHIRFactoryJs);
  defineAllergyIntolerancePropsJs(js, def);
end;


procedure defineAppointmentParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AppointmentParticipant', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentParticipant', 'actor', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentParticipant', 'required', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentParticipant', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineAppointmentParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentParticipant', nil, 'AppointmentParticipant', js.FHIRFactoryJs);
  defineAppointmentParticipantPropsJs(js, def);
end;


procedure defineAppointmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Appointment', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'cancelationReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Appointment', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'priority', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'minutesDuration', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Appointment', 'slot', 'Reference(Slot)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Appointment', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Appointment', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'participant', 'AppointmentParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Appointment', 'requestedPeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAppointmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Appointment', nil, 'Appointment', js.FHIRFactoryJs);
  defineAppointmentPropsJs(js, def);
end;


procedure defineAppointmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AppointmentResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse', 'appointment', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AppointmentResponse', 'participantType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AppointmentResponse', 'actor', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AppointmentResponse', 'participantStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AppointmentResponse', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAppointmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AppointmentResponse', nil, 'AppointmentResponse', js.FHIRFactoryJs);
  defineAppointmentResponsePropsJs(js, def);
end;


procedure defineAuditEventAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventAgent', 'who', 'Reference(PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'altId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgent', 'requestor', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'AuditEventAgent', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'media', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'network', 'AuditEventAgentNetwork', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventAgent', 'purposeOfUse', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgent', nil, 'AuditEventAgent', js.FHIRFactoryJs);
  defineAuditEventAgentPropsJs(js, def);
end;


procedure defineAuditEventAgentNetworkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventAgentNetwork', 'address', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventAgentNetwork', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineAuditEventAgentNetworkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventAgentNetwork', nil, 'AuditEventAgentNetwork', js.FHIRFactoryJs);
  defineAuditEventAgentNetworkPropsJs(js, def);
end;


procedure defineAuditEventSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventSource', 'site', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventSource', 'observer', 'Reference(PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventSource', 'type', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventSource', nil, 'AuditEventSource', js.FHIRFactoryJs);
  defineAuditEventSourcePropsJs(js, def);
end;


procedure defineAuditEventEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntity', 'what', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'role', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'lifecycle', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEventEntity', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEventEntity', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntity', 'query', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'AuditEventEntity', 'detail', 'AuditEventEntityDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntity', nil, 'AuditEventEntity', js.FHIRFactoryJs);
  defineAuditEventEntityPropsJs(js, def);
end;


procedure defineAuditEventEntityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'AuditEventEntityDetail', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEventEntityDetail', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineAuditEventEntityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEventEntityDetail', nil, 'AuditEventEntityDetail', js.FHIRFactoryJs);
  defineAuditEventEntityDetailPropsJs(js, def);
end;


procedure defineAuditEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'AuditEvent', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'subtype', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'action', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'AuditEvent', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'outcomeDesc', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'AuditEvent', 'purposeOfEvent', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'agent', 'AuditEventAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'AuditEvent', 'source', 'AuditEventSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'AuditEvent', 'entity', 'AuditEventEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineAuditEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('AuditEvent', nil, 'AuditEvent', js.FHIRFactoryJs);
  defineAuditEventPropsJs(js, def);
end;


procedure defineBasicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Basic', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Basic', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Basic', 'created', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Basic', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBasicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Basic', nil, 'Basic', js.FHIRFactoryJs);
  defineBasicPropsJs(js, def);
end;


procedure defineBinaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Binary', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Binary', 'securityContext', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Binary', 'data', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineBinaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Binary', nil, 'Binary', js.FHIRFactoryJs);
  defineBinaryPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductCollection', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductCollection', nil, 'BiologicallyDerivedProductCollection', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductCollectionPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'additive', 'Reference(Substance)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductProcessing', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductProcessing', nil, 'BiologicallyDerivedProductProcessing', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductProcessingPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductManipulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BiologicallyDerivedProductManipulation', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductManipulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductManipulation', nil, 'BiologicallyDerivedProductManipulation', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductManipulationPropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductStoragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'temperature', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'scale', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProductStorage', 'duration', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBiologicallyDerivedProductStorageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProductStorage', nil, 'BiologicallyDerivedProductStorage', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductStoragePropsJs(js, def);
end;


procedure defineBiologicallyDerivedProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'productCategory', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'productCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'request', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'quantity', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'parent', 'Reference(BiologicallyDerivedProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'collection', 'BiologicallyDerivedProductCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'processing', 'BiologicallyDerivedProductProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'manipulation', 'BiologicallyDerivedProductManipulation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BiologicallyDerivedProduct', 'storage', 'BiologicallyDerivedProductStorage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineBiologicallyDerivedProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BiologicallyDerivedProduct', nil, 'BiologicallyDerivedProduct', js.FHIRFactoryJs);
  defineBiologicallyDerivedProductPropsJs(js, def);
end;


procedure defineBodyStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'BodyStructure', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'BodyStructure', 'morphology', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure', 'location', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BodyStructure', 'locationQualifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BodyStructure', 'image', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BodyStructure', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBodyStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BodyStructure', nil, 'BodyStructure', js.FHIRFactoryJs);
  defineBodyStructurePropsJs(js, def);
end;


procedure defineBundleLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleLink', 'relation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleLink', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleLink', nil, 'BundleLink', js.FHIRFactoryJs);
  defineBundleLinkPropsJs(js, def);
end;


procedure defineBundleEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntry', 'link', '@Bundle.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'BundleEntry', 'fullUrl', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntry', 'resource', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'search', 'BundleEntrySearch', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'request', 'BundleEntryRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'BundleEntry', 'response', 'BundleEntryResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntry', nil, 'BundleEntry', js.FHIRFactoryJs);
  defineBundleEntryPropsJs(js, def);
end;


procedure defineBundleEntrySearchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntrySearch', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntrySearch', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineBundleEntrySearchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntrySearch', nil, 'BundleEntrySearch', js.FHIRFactoryJs);
  defineBundleEntrySearchPropsJs(js, def);
end;


procedure defineBundleEntryRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryRequest', 'method', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifNoneMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifModifiedSince', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifMatch', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryRequest', 'ifNoneExist', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineBundleEntryRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryRequest', nil, 'BundleEntryRequest', js.FHIRFactoryJs);
  defineBundleEntryRequestPropsJs(js, def);
end;


procedure defineBundleEntryResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'BundleEntryResponse', 'status', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'location', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'etag', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'BundleEntryResponse', 'lastModified', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'BundleEntryResponse', 'outcome', 'Resource', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleEntryResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('BundleEntryResponse', nil, 'BundleEntryResponse', js.FHIRFactoryJs);
  defineBundleEntryResponsePropsJs(js, def);
end;


procedure defineBundlePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineResourcePropsJs(js, def);
  js.registerElement(def, 'Bundle', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Bundle', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle', 'timestamp', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Bundle', 'total', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Bundle', 'link', 'BundleLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle', 'entry', 'BundleEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Bundle', 'signature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineBundleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Bundle', nil, 'Bundle', js.FHIRFactoryJs);
  defineBundlePropsJs(js, def);
end;


procedure defineCapabilityStatementSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementSoftware', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementSoftware', 'releaseDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineCapabilityStatementSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementSoftware', nil, 'CapabilityStatementSoftware', js.FHIRFactoryJs);
  defineCapabilityStatementSoftwarePropsJs(js, def);
end;


procedure defineCapabilityStatementImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementImplementation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementImplementation', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCapabilityStatementImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementImplementation', nil, 'CapabilityStatementImplementation', js.FHIRFactoryJs);
  defineCapabilityStatementImplementationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRest', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRest', 'security', 'CapabilityStatementRestSecurity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementRest', 'resource', 'CapabilityStatementRestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'interaction', 'CapabilityStatementRestInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'searchParam', '@CapabilityStatement.rest.resource.searchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRest', 'operation', '@CapabilityStatement.rest.resource.operation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRest', nil, 'CapabilityStatementRest', js.FHIRFactoryJs);
  defineCapabilityStatementRestPropsJs(js, def);
end;


procedure defineCapabilityStatementRestSecurityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'cors', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'service', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestSecurity', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestSecurityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestSecurity', nil, 'CapabilityStatementRestSecurity', js.FHIRFactoryJs);
  defineCapabilityStatementRestSecurityPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResource', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'interaction', 'CapabilityStatementRestResourceInteraction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'versioning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'readHistory', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'updateCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalCreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalRead', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalUpdate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'conditionalDelete', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'searchParam', 'CapabilityStatementRestResourceSearchParam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementRestResource', 'operation', 'CapabilityStatementRestResourceOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementRestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResource', nil, 'CapabilityStatementRestResource', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourcePropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceInteraction', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceInteraction', nil, 'CapabilityStatementRestResourceInteraction', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceSearchParamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceSearchParam', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceSearchParamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceSearchParam', nil, 'CapabilityStatementRestResourceSearchParam', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceSearchParamPropsJs(js, def);
end;


procedure defineCapabilityStatementRestResourceOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestResourceOperation', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestResourceOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestResourceOperation', nil, 'CapabilityStatementRestResourceOperation', js.FHIRFactoryJs);
  defineCapabilityStatementRestResourceOperationPropsJs(js, def);
end;


procedure defineCapabilityStatementRestInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementRestInteraction', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementRestInteraction', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementRestInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementRestInteraction', nil, 'CapabilityStatementRestInteraction', js.FHIRFactoryJs);
  defineCapabilityStatementRestInteractionPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessaging', 'endpoint', 'CapabilityStatementMessagingEndpoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'reliableCache', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessaging', 'supportedMessage', 'CapabilityStatementMessagingSupportedMessage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementMessagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessaging', nil, 'CapabilityStatementMessaging', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint', 'protocol', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatementMessagingEndpoint', 'address', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingEndpoint', nil, 'CapabilityStatementMessagingEndpoint', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingEndpointPropsJs(js, def);
end;


procedure defineCapabilityStatementMessagingSupportedMessagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementMessagingSupportedMessage', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementMessagingSupportedMessageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementMessagingSupportedMessage', nil, 'CapabilityStatementMessagingSupportedMessage', js.FHIRFactoryJs);
  defineCapabilityStatementMessagingSupportedMessagePropsJs(js, def);
end;


procedure defineCapabilityStatementDocumentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CapabilityStatementDocument', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument', 'documentation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatementDocument', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCapabilityStatementDocumentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatementDocument', nil, 'CapabilityStatementDocument', js.FHIRFactoryJs);
  defineCapabilityStatementDocumentPropsJs(js, def);
end;


procedure defineCapabilityStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CapabilityStatement', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CapabilityStatement', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CapabilityStatement', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'software', 'CapabilityStatementSoftware', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement', 'implementation', 'CapabilityStatementImplementation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CapabilityStatement', 'fhirVersion', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CapabilityStatement', 'rest', 'CapabilityStatementRest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'messaging', 'CapabilityStatementMessaging', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CapabilityStatement', 'document', 'CapabilityStatementDocument', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCapabilityStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CapabilityStatement', nil, 'CapabilityStatement', js.FHIRFactoryJs);
  defineCapabilityStatementPropsJs(js, def);
end;


procedure defineCarePlanActivityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivity', 'outcomeCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'outcomeReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'progress', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivity', 'reference', 'Reference(Appointment)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivity', 'detail', 'CarePlanActivityDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCarePlanActivityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivity', nil, 'CarePlanActivity', js.FHIRFactoryJs);
  defineCarePlanActivityPropsJs(js, def);
end;


procedure defineCarePlanActivityDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CarePlanActivityDetail', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'scheduledString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'dailyAmount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlanActivityDetail', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCarePlanActivityDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlanActivityDetail', nil, 'CarePlanActivityDetail', js.FHIRFactoryJs);
  defineCarePlanActivityDetailPropsJs(js, def);
end;


procedure defineCarePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CarePlan', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'replaces', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'partOf', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CarePlan', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CarePlan', 'author', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CarePlan', 'contributor', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'careTeam', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'goal', 'Reference(Goal)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'activity', 'CarePlanActivity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CarePlan', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCarePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CarePlan', nil, 'CarePlan', js.FHIRFactoryJs);
  defineCarePlanPropsJs(js, def);
end;


procedure defineCareTeamParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CareTeamParticipant', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeamParticipant', 'member', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeamParticipant', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCareTeamParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeamParticipant', nil, 'CareTeamParticipant', js.FHIRFactoryJs);
  defineCareTeamParticipantPropsJs(js, def);
end;


procedure defineCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CareTeam', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CareTeam', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CareTeam', 'participant', 'CareTeamParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'managingOrganization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CareTeam', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CareTeam', nil, 'CareTeam', js.FHIRFactoryJs);
  defineCareTeamPropsJs(js, def);
end;


procedure defineCatalogEntryRelatedEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CatalogEntryRelatedEntry', 'relationtype', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CatalogEntryRelatedEntry', 'item', 'Reference(CatalogEntry)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCatalogEntryRelatedEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CatalogEntryRelatedEntry', nil, 'CatalogEntryRelatedEntry', js.FHIRFactoryJs);
  defineCatalogEntryRelatedEntryPropsJs(js, def);
end;


procedure defineCatalogEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CatalogEntry', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry', 'orderable', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CatalogEntry', 'referencedItem', 'Reference(Medication)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry', 'additionalIdentifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry', 'classification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CatalogEntry', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CatalogEntry', 'validTo', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CatalogEntry', 'lastUpdated', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CatalogEntry', 'additionalCharacteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry', 'additionalClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CatalogEntry', 'relatedEntry', 'CatalogEntryRelatedEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCatalogEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CatalogEntry', nil, 'CatalogEntry', js.FHIRFactoryJs);
  defineCatalogEntryPropsJs(js, def);
end;


procedure defineChargeItemPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemPerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemPerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemPerformer', nil, 'ChargeItemPerformer', js.FHIRFactoryJs);
  defineChargeItemPerformerPropsJs(js, def);
end;


procedure defineChargeItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItem', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'partOf', 'Reference(ChargeItem)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'performer', 'ChargeItemPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'performingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'requestingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'costCenter', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'bodysite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'factorOverride', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItem', 'priceOverride', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'overrideReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItem', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'enteredDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItem', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'service', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'productReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'productCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItem', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItem', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItem', nil, 'ChargeItem', js.FHIRFactoryJs);
  defineChargeItemPropsJs(js, def);
end;


procedure defineChargeItemDefinitionApplicabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionApplicability', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability', 'language', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionApplicability', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineChargeItemDefinitionApplicabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionApplicability', nil, 'ChargeItemDefinitionApplicability', js.FHIRFactoryJs);
  defineChargeItemDefinitionApplicabilityPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup', 'applicability', '@ChargeItemDefinition.applicability', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroup', 'priceComponent', 'ChargeItemDefinitionPropertyGroupPriceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionPropertyGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroup', nil, 'ChargeItemDefinitionPropertyGroup', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ChargeItemDefinitionPropertyGroupPriceComponent', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineChargeItemDefinitionPropertyGroupPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinitionPropertyGroupPriceComponent', nil, 'ChargeItemDefinitionPropertyGroupPriceComponent', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropertyGroupPriceComponentPropsJs(js, def);
end;


procedure defineChargeItemDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ChargeItemDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ChargeItemDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ChargeItemDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ChargeItemDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ChargeItemDefinition', 'instance', 'Reference(Medication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'applicability', 'ChargeItemDefinitionApplicability', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ChargeItemDefinition', 'propertyGroup', 'ChargeItemDefinitionPropertyGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineChargeItemDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ChargeItemDefinition', nil, 'ChargeItemDefinition', js.FHIRFactoryJs);
  defineChargeItemDefinitionPropsJs(js, def);
end;


procedure defineClaimRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimRelated', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimRelated', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimRelated', nil, 'ClaimRelated', js.FHIRFactoryJs);
  defineClaimRelatedPropsJs(js, def);
end;


procedure defineClaimPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimPayee', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimPayee', 'party', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimPayee', nil, 'ClaimPayee', js.FHIRFactoryJs);
  defineClaimPayeePropsJs(js, def);
end;


procedure defineClaimCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimCareTeam', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimCareTeam', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimCareTeam', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimCareTeam', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimCareTeam', nil, 'ClaimCareTeam', js.FHIRFactoryJs);
  defineClaimCareTeamPropsJs(js, def);
end;


procedure defineClaimSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimSupportingInfo', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimSupportingInfo', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimSupportingInfo', nil, 'ClaimSupportingInfo', js.FHIRFactoryJs);
  defineClaimSupportingInfoPropsJs(js, def);
end;


procedure defineClaimDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimDiagnosis', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimDiagnosis', 'onAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimDiagnosis', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimDiagnosis', nil, 'ClaimDiagnosis', js.FHIRFactoryJs);
  defineClaimDiagnosisPropsJs(js, def);
end;


procedure defineClaimProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimProcedure', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimProcedure', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimProcedure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimProcedure', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimProcedure', nil, 'ClaimProcedure', js.FHIRFactoryJs);
  defineClaimProcedurePropsJs(js, def);
end;


procedure defineClaimInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimInsurance', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimInsurance', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimInsurance', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimInsurance', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimInsurance', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimInsurance', nil, 'ClaimInsurance', js.FHIRFactoryJs);
  defineClaimInsurancePropsJs(js, def);
end;


procedure defineClaimAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimAccident', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimAccident', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimAccident', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimAccident', nil, 'ClaimAccident', js.FHIRFactoryJs);
  defineClaimAccidentPropsJs(js, def);
end;


procedure defineClaimItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItem', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItem', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimItem', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItem', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItem', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItem', 'detail', 'ClaimItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItem', nil, 'ClaimItem', js.FHIRFactoryJs);
  defineClaimItemPropsJs(js, def);
end;


procedure defineClaimItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetail', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetail', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetail', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetail', 'subDetail', 'ClaimItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetail', nil, 'ClaimItemDetail', js.FHIRFactoryJs);
  defineClaimItemDetailPropsJs(js, def);
end;


procedure defineClaimItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimItemDetailSubDetail', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimItemDetailSubDetail', nil, 'ClaimItemDetailSubDetail', js.FHIRFactoryJs);
  defineClaimItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Claim', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Claim', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Claim', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'related', 'ClaimRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'prescription', 'Reference(DeviceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'originalPrescription', 'Reference(DeviceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'payee', 'ClaimPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'careTeam', 'ClaimCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'supportingInfo', 'ClaimSupportingInfo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'diagnosis', 'ClaimDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'procedure', 'ClaimProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'insurance', 'ClaimInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'accident', 'ClaimAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Claim', 'item', 'ClaimItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Claim', 'total', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Claim', nil, 'Claim', js.FHIRFactoryJs);
  defineClaimPropsJs(js, def);
end;


procedure defineClaimResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItem', 'itemSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItem', 'adjudication', 'ClaimResponseItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItem', 'detail', 'ClaimResponseItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItem', nil, 'ClaimResponseItem', js.FHIRFactoryJs);
  defineClaimResponseItemPropsJs(js, def);
end;


procedure defineClaimResponseItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseItemAdjudication', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineClaimResponseItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemAdjudication', nil, 'ClaimResponseItemAdjudication', js.FHIRFactoryJs);
  defineClaimResponseItemAdjudicationPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetail', 'detailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseItemDetail', 'subDetail', 'ClaimResponseItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetail', nil, 'ClaimResponseItemDetail', js.FHIRFactoryJs);
  defineClaimResponseItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'subDetailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseItemDetailSubDetail', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseItemDetailSubDetail', nil, 'ClaimResponseItemDetailSubDetail', js.FHIRFactoryJs);
  defineClaimResponseItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItem', 'provider', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItem', 'detail', 'ClaimResponseAddItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItem', nil, 'ClaimResponseAddItem', js.FHIRFactoryJs);
  defineClaimResponseAddItemPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetail', 'subDetail', 'ClaimResponseAddItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetail', nil, 'ClaimResponseAddItemDetail', js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailPropsJs(js, def);
end;


procedure defineClaimResponseAddItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseAddItemDetailSubDetail', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseAddItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseAddItemDetailSubDetail', nil, 'ClaimResponseAddItemDetailSubDetail', js.FHIRFactoryJs);
  defineClaimResponseAddItemDetailSubDetailPropsJs(js, def);
end;


procedure defineClaimResponseTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseTotal', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseTotal', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseTotal', nil, 'ClaimResponseTotal', js.FHIRFactoryJs);
  defineClaimResponseTotalPropsJs(js, def);
end;


procedure defineClaimResponsePaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponsePayment', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponsePayment', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponsePayment', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponsePaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponsePayment', nil, 'ClaimResponsePayment', js.FHIRFactoryJs);
  defineClaimResponsePaymentPropsJs(js, def);
end;


procedure defineClaimResponseProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseProcessNote', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseProcessNote', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseProcessNote', nil, 'ClaimResponseProcessNote', js.FHIRFactoryJs);
  defineClaimResponseProcessNotePropsJs(js, def);
end;


procedure defineClaimResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseInsurance', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponseInsurance', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseInsurance', nil, 'ClaimResponseInsurance', js.FHIRFactoryJs);
  defineClaimResponseInsurancePropsJs(js, def);
end;


procedure defineClaimResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClaimResponseError', 'itemSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'detailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'subDetailSequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ClaimResponseError', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineClaimResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponseError', nil, 'ClaimResponseError', js.FHIRFactoryJs);
  defineClaimResponseErrorPropsJs(js, def);
end;


procedure defineClaimResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClaimResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClaimResponse', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'requestor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'request', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'preAuthRef', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClaimResponse', 'preAuthPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'payeeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'item', 'ClaimResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'addItem', 'ClaimResponseAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'adjudication', '@ClaimResponse.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'total', 'ClaimResponseTotal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'payment', 'ClaimResponsePayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'formCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'form', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClaimResponse', 'processNote', 'ClaimResponseProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'communicationRequest', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'insurance', 'ClaimResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClaimResponse', 'error', 'ClaimResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClaimResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClaimResponse', nil, 'ClaimResponse', js.FHIRFactoryJs);
  defineClaimResponsePropsJs(js, def);
end;


procedure defineClinicalImpressionInvestigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionInvestigation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionInvestigation', 'item', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionInvestigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionInvestigation', nil, 'ClinicalImpressionInvestigation', js.FHIRFactoryJs);
  defineClinicalImpressionInvestigationPropsJs(js, def);
end;


procedure defineClinicalImpressionFindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ClinicalImpressionFinding', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding', 'itemReference', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpressionFinding', 'basis', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineClinicalImpressionFindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpressionFinding', nil, 'ClinicalImpressionFinding', js.FHIRFactoryJs);
  defineClinicalImpressionFindingPropsJs(js, def);
end;


procedure defineClinicalImpressionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ClinicalImpression', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ClinicalImpression', 'assessor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'previous', 'Reference(ClinicalImpression)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ClinicalImpression', 'problem', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'investigation', 'ClinicalImpressionInvestigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'summary', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ClinicalImpression', 'finding', 'ClinicalImpressionFinding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisCodeableConcept', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'prognosisReference', 'Reference(RiskAssessment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ClinicalImpression', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineClinicalImpressionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ClinicalImpression', nil, 'ClinicalImpression', js.FHIRFactoryJs);
  defineClinicalImpressionPropsJs(js, def);
end;


procedure defineCodeSystemFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemFilter', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemFilter', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemFilter', nil, 'CodeSystemFilter', js.FHIRFactoryJs);
  defineCodeSystemFilterPropsJs(js, def);
end;


procedure defineCodeSystemPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemProperty', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemProperty', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemProperty', nil, 'CodeSystemProperty', js.FHIRFactoryJs);
  defineCodeSystemPropertyPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConcept', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'definition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConcept', 'designation', 'CodeSystemConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept', 'property', 'CodeSystemConceptProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystemConcept', 'concept', '@CodeSystem.concept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConcept', nil, 'CodeSystemConcept', js.FHIRFactoryJs);
  defineCodeSystemConceptPropsJs(js, def);
end;


procedure defineCodeSystemConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptDesignation', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCodeSystemConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptDesignation', nil, 'CodeSystemConceptDesignation', js.FHIRFactoryJs);
  defineCodeSystemConceptDesignationPropsJs(js, def);
end;


procedure defineCodeSystemConceptPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CodeSystemConceptProperty', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystemConceptProperty', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineCodeSystemConceptPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystemConceptProperty', nil, 'CodeSystemConceptProperty', js.FHIRFactoryJs);
  defineCodeSystemConceptPropertyPropsJs(js, def);
end;


procedure defineCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CodeSystem', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CodeSystem', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'caseSensitive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'hierarchyMeaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'versionNeeded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CodeSystem', 'content', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'supplements', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'count', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CodeSystem', 'filter', 'CodeSystemFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'property', 'CodeSystemProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CodeSystem', 'concept', 'CodeSystemConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CodeSystem', nil, 'CodeSystem', js.FHIRFactoryJs);
  defineCodeSystemPropsJs(js, def);
end;


procedure defineCommunicationPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationPayload', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationPayload', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationPayload', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationPayload', nil, 'CommunicationPayload', js.FHIRFactoryJs);
  defineCommunicationPayloadPropsJs(js, def);
end;


procedure defineCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Communication', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'inResponseTo', 'Reference(Communication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Communication', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'topic', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'sent', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'received', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Communication', 'recipient', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'sender', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Communication', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'payload', 'CommunicationPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Communication', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Communication', nil, 'Communication', js.FHIRFactoryJs);
  defineCommunicationPropsJs(js, def);
end;


procedure defineCommunicationRequestPayloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequestPayload', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCommunicationRequestPayloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequestPayload', nil, 'CommunicationRequestPayload', js.FHIRFactoryJs);
  defineCommunicationRequestPayloadPropsJs(js, def);
end;


procedure defineCommunicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CommunicationRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'replaces', 'Reference(CommunicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CommunicationRequest', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CommunicationRequest', 'medium', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'about', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'payload', 'CommunicationRequestPayload', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CommunicationRequest', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'recipient', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'sender', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CommunicationRequest', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCommunicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CommunicationRequest', nil, 'CommunicationRequest', js.FHIRFactoryJs);
  defineCommunicationRequestPropsJs(js, def);
end;


procedure defineCompartmentDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinitionResource', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinitionResource', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCompartmentDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinitionResource', nil, 'CompartmentDefinitionResource', js.FHIRFactoryJs);
  defineCompartmentDefinitionResourcePropsJs(js, def);
end;


procedure defineCompartmentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'CompartmentDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompartmentDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompartmentDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompartmentDefinition', 'search', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CompartmentDefinition', 'resource', 'CompartmentDefinitionResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompartmentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompartmentDefinition', nil, 'CompartmentDefinition', js.FHIRFactoryJs);
  defineCompartmentDefinitionPropsJs(js, def);
end;


procedure defineCompositionAttesterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionAttester', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionAttester', 'time', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CompositionAttester', 'party', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionAttesterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionAttester', nil, 'CompositionAttester', js.FHIRFactoryJs);
  defineCompositionAttesterPropsJs(js, def);
end;


procedure defineCompositionRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionRelatesTo', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionRelatesTo', 'targetIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionRelatesTo', 'targetReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCompositionRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionRelatesTo', nil, 'CompositionRelatesTo', js.FHIRFactoryJs);
  defineCompositionRelatesToPropsJs(js, def);
end;


procedure defineCompositionEventPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionEvent', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionEvent', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionEvent', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionEventJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionEvent', nil, 'CompositionEvent', js.FHIRFactoryJs);
  defineCompositionEventPropsJs(js, def);
end;


procedure defineCompositionSectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CompositionSection', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection', 'focus', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'text', 'Narrative', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CompositionSection', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'entry', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CompositionSection', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CompositionSection', 'section', '@Composition.section', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionSectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CompositionSection', nil, 'CompositionSection', js.FHIRFactoryJs);
  defineCompositionSectionPropsJs(js, def);
end;


procedure defineCompositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Composition', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Composition', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition', 'confidentiality', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Composition', 'attester', 'CompositionAttester', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Composition', 'relatesTo', 'CompositionRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'event', 'CompositionEvent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Composition', 'section', 'CompositionSection', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCompositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Composition', nil, 'Composition', js.FHIRFactoryJs);
  defineCompositionPropsJs(js, def);
end;


procedure defineConceptMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroup', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'sourceVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'target', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'targetVersion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroup', 'element', 'ConceptMapGroupElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroup', 'unmapped', 'ConceptMapGroupUnmapped', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConceptMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroup', nil, 'ConceptMapGroup', js.FHIRFactoryJs);
  defineConceptMapGroupPropsJs(js, def);
end;


procedure defineConceptMapGroupElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElement', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElement', 'target', 'ConceptMapGroupElementTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElement', nil, 'ConceptMapGroupElement', js.FHIRFactoryJs);
  defineConceptMapGroupElementPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'equivalence', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'dependsOn', 'ConceptMapGroupElementTargetDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMapGroupElementTarget', 'product', '@ConceptMap.group.element.target.dependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapGroupElementTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTarget', nil, 'ConceptMapGroupElementTarget', js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetPropsJs(js, def);
end;


procedure defineConceptMapGroupElementTargetDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'property', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'system', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupElementTargetDependsOn', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupElementTargetDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupElementTargetDependsOn', nil, 'ConceptMapGroupElementTargetDependsOn', js.FHIRFactoryJs);
  defineConceptMapGroupElementTargetDependsOnPropsJs(js, def);
end;


procedure defineConceptMapGroupUnmappedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMapGroupUnmapped', 'url', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConceptMapGroupUnmappedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMapGroupUnmapped', nil, 'ConceptMapGroupUnmapped', js.FHIRFactoryJs);
  defineConceptMapGroupUnmappedPropsJs(js, def);
end;


procedure defineConceptMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ConceptMap', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConceptMap', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConceptMap', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ConceptMap', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConceptMap', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'sourceCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'targetUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'targetCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConceptMap', 'group', 'ConceptMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConceptMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConceptMap', nil, 'ConceptMap', js.FHIRFactoryJs);
  defineConceptMapPropsJs(js, def);
end;


procedure defineConditionStagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionStage', 'summary', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConditionStage', 'assessment', 'Reference(ClinicalImpression)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionStage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConditionStageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionStage', nil, 'ConditionStage', js.FHIRFactoryJs);
  defineConditionStagePropsJs(js, def);
end;


procedure defineConditionEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConditionEvidence', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConditionEvidence', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConditionEvidence', nil, 'ConditionEvidence', js.FHIRFactoryJs);
  defineConditionEvidencePropsJs(js, def);
end;


procedure defineConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Condition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'clinicalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'verificationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'severity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition', 'abatementDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'abatementAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'abatementString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Condition', 'recordedDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Condition', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'asserter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Condition', 'stage', 'ConditionStage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'evidence', 'ConditionEvidence', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Condition', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Condition', nil, 'Condition', js.FHIRFactoryJs);
  defineConditionPropsJs(js, def);
end;


procedure defineConsentPolicyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentPolicy', 'authority', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentPolicy', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineConsentPolicyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentPolicy', nil, 'ConsentPolicy', js.FHIRFactoryJs);
  defineConsentPolicyPropsJs(js, def);
end;


procedure defineConsentVerificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentVerification', 'verified', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ConsentVerification', 'verifiedWith', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentVerification', 'verificationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineConsentVerificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentVerification', nil, 'ConsentVerification', js.FHIRFactoryJs);
  defineConsentVerificationPropsJs(js, def);
end;


procedure defineConsentProvisionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvision', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvision', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision', 'actor', 'ConsentProvisionActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'action', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'securityLabel', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'purpose', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'class', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'dataPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvision', 'data', 'ConsentProvisionData', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ConsentProvision', 'provision', '@Consent.provision', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineConsentProvisionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvision', nil, 'ConsentProvision', js.FHIRFactoryJs);
  defineConsentProvisionPropsJs(js, def);
end;


procedure defineConsentProvisionActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionActor', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ConsentProvisionActor', 'reference', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionActor', nil, 'ConsentProvisionActor', js.FHIRFactoryJs);
  defineConsentProvisionActorPropsJs(js, def);
end;


procedure defineConsentProvisionDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ConsentProvisionData', 'meaning', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ConsentProvisionData', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentProvisionDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ConsentProvisionData', nil, 'ConsentProvisionData', js.FHIRFactoryJs);
  defineConsentProvisionDataPropsJs(js, def);
end;


procedure defineConsentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Consent', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Consent', 'scope', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Consent', 'performer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'organization', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'sourceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'sourceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'policy', 'ConsentPolicy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'policyRule', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Consent', 'verification', 'ConsentVerification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Consent', 'provision', 'ConsentProvision', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineConsentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Consent', nil, 'Consent', js.FHIRFactoryJs);
  defineConsentPropsJs(js, def);
end;


procedure defineContractContentDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractContentDefinition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition', 'publisher', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractContentDefinition', 'publicationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractContentDefinition', 'publicationStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractContentDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractContentDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractContentDefinition', nil, 'ContractContentDefinition', js.FHIRFactoryJs);
  defineContractContentDefinitionPropsJs(js, def);
end;


procedure defineContractTermPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTerm', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTerm', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'topicCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'topicReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTerm', 'securityLabel', 'ContractTermSecurityLabel', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'offer', 'ContractTermOffer', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTerm', 'asset', 'ContractTermAsset', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'action', 'ContractTermAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTerm', 'group', '@Contract.term', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTerm', nil, 'ContractTerm', js.FHIRFactoryJs);
  defineContractTermPropsJs(js, def);
end;


procedure defineContractTermSecurityLabelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermSecurityLabel', 'classification', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermSecurityLabel', 'category', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermSecurityLabel', 'control', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermSecurityLabelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermSecurityLabel', nil, 'ContractTermSecurityLabel', js.FHIRFactoryJs);
  defineContractTermSecurityLabelPropsJs(js, def);
end;


procedure defineContractTermOfferPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOffer', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer', 'party', 'ContractTermOfferParty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer', 'topic', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'decision', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOffer', 'decisionMode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer', 'answer', 'ContractTermOfferAnswer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOffer', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermOfferJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOffer', nil, 'ContractTermOffer', js.FHIRFactoryJs);
  defineContractTermOfferPropsJs(js, def);
end;


procedure defineContractTermOfferPartyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOfferParty', 'reference', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermOfferParty', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermOfferPartyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOfferParty', nil, 'ContractTermOfferParty', js.FHIRFactoryJs);
  defineContractTermOfferPartyPropsJs(js, def);
end;


procedure defineContractTermOfferAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermOfferAnswer', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermOfferAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermOfferAnswer', nil, 'ContractTermOfferAnswer', js.FHIRFactoryJs);
  defineContractTermOfferAnswerPropsJs(js, def);
end;


procedure defineContractTermAssetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAsset', 'scope', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'typeReference', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'subtype', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'relationship', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAsset', 'context', 'ContractTermAssetContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAsset', 'periodType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'period', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'usePeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAsset', 'answer', '@Contract.term.offer.answer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAsset', 'valuedItem', 'ContractTermAssetValuedItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermAssetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAsset', nil, 'ContractTermAsset', js.FHIRFactoryJs);
  defineContractTermAssetPropsJs(js, def);
end;


procedure defineContractTermAssetContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetContext', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetContext', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAssetContext', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineContractTermAssetContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetContext', nil, 'ContractTermAssetContext', js.FHIRFactoryJs);
  defineContractTermAssetContextPropsJs(js, def);
end;


procedure defineContractTermAssetValuedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'entityCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'entityReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'effectiveTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'points', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'payment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'paymentDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'responsible', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAssetValuedItem', 'recipient', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermAssetValuedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAssetValuedItem', nil, 'ContractTermAssetValuedItem', js.FHIRFactoryJs);
  defineContractTermAssetValuedItemPropsJs(js, def);
end;


procedure defineContractTermActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermAction', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ContractTermAction', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'subject', 'ContractTermActionSubject', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction', 'intent', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ContractTermAction', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'requester', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction', 'performerType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction', 'performerRole', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'performer', 'Reference(RelatedPerson)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractTermAction', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermAction', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractTermActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermAction', nil, 'ContractTermAction', js.FHIRFactoryJs);
  defineContractTermActionPropsJs(js, def);
end;


procedure defineContractTermActionSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractTermActionSubject', 'reference', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ContractTermActionSubject', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractTermActionSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractTermActionSubject', nil, 'ContractTermActionSubject', js.FHIRFactoryJs);
  defineContractTermActionSubjectPropsJs(js, def);
end;


procedure defineContractSignerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractSigner', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'party', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractSigner', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineContractSignerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractSigner', nil, 'ContractSigner', js.FHIRFactoryJs);
  defineContractSignerPropsJs(js, def);
end;


procedure defineContractFriendlyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractFriendly', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractFriendly', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractFriendlyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractFriendly', nil, 'ContractFriendly', js.FHIRFactoryJs);
  defineContractFriendlyPropsJs(js, def);
end;


procedure defineContractLegalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractLegal', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractLegal', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractLegalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractLegal', nil, 'ContractLegal', js.FHIRFactoryJs);
  defineContractLegalPropsJs(js, def);
end;


procedure defineContractRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ContractRule', 'contentAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ContractRule', 'contentReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ContractRule', nil, 'ContractRule', js.FHIRFactoryJs);
  defineContractRulePropsJs(js, def);
end;


procedure defineContractPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Contract', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'legalState', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'instantiatesCanonical', 'Reference(Contract)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'instantiatesUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'contentDerivative', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Contract', 'applies', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'expirationType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subject', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'authority', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'domain', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'site', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Contract', 'author', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'scope', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'topicCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'topicReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'subType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'contentDefinition', 'ContractContentDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'term', 'ContractTerm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'signer', 'ContractSigner', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'friendly', 'ContractFriendly', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'legal', 'ContractLegal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'rule', 'ContractRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Contract', 'legallyBindingAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Contract', 'legallyBindingReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineContractJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Contract', nil, 'Contract', js.FHIRFactoryJs);
  defineContractPropsJs(js, def);
end;


procedure defineCoverageClassPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageClass', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageClass', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageClass', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageClassJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageClass', nil, 'CoverageClass', js.FHIRFactoryJs);
  defineCoverageClassPropsJs(js, def);
end;


procedure defineCoverageCostToBeneficiaryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCostToBeneficiary', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiary', 'exception', 'CoverageCostToBeneficiaryException', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageCostToBeneficiaryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCostToBeneficiary', nil, 'CoverageCostToBeneficiary', js.FHIRFactoryJs);
  defineCoverageCostToBeneficiaryPropsJs(js, def);
end;


procedure defineCoverageCostToBeneficiaryExceptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageCostToBeneficiaryException', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageCostToBeneficiaryException', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageCostToBeneficiaryExceptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageCostToBeneficiaryException', nil, 'CoverageCostToBeneficiaryException', js.FHIRFactoryJs);
  defineCoverageCostToBeneficiaryExceptionPropsJs(js, def);
end;


procedure defineCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Coverage', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'policyHolder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriber', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'subscriberId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'beneficiary', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'dependent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Coverage', 'payor', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'class', 'CoverageClass', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'order', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Coverage', 'network', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Coverage', 'costToBeneficiary', 'CoverageCostToBeneficiary', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Coverage', 'subrogation', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Coverage', 'contract', 'Reference(Contract)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Coverage', nil, 'Coverage', js.FHIRFactoryJs);
  defineCoveragePropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo', 'information', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestSupportingInfo', 'appliesToAll', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineCoverageEligibilityRequestSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestSupportingInfo', nil, 'CoverageEligibilityRequestSupportingInfo', js.FHIRFactoryJs);
  defineCoverageEligibilityRequestSupportingInfoPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestInsurance', 'businessArrangement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageEligibilityRequestInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestInsurance', nil, 'CoverageEligibilityRequestInsurance', js.FHIRFactoryJs);
  defineCoverageEligibilityRequestInsurancePropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'diagnosis', 'CoverageEligibilityRequestItemDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequestItem', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityRequestItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestItem', nil, 'CoverageEligibilityRequestItem', js.FHIRFactoryJs);
  defineCoverageEligibilityRequestItemPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestItemDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequestItemDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequestItemDiagnosis', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityRequestItemDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequestItemDiagnosis', nil, 'CoverageEligibilityRequestItemDiagnosis', js.FHIRFactoryJs);
  defineCoverageEligibilityRequestItemDiagnosisPropsJs(js, def);
end;


procedure defineCoverageEligibilityRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'supportingInfo', 'CoverageEligibilityRequestSupportingInfo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'insurance', 'CoverageEligibilityRequestInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityRequest', 'item', 'CoverageEligibilityRequestItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityRequest', nil, 'CoverageEligibilityRequest', js.FHIRFactoryJs);
  defineCoverageEligibilityRequestPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance', 'inforce', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance', 'benefitPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsurance', 'item', 'CoverageEligibilityResponseInsuranceItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityResponseInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsurance', nil, 'CoverageEligibilityResponseInsurance', js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsurancePropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsuranceItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'benefit', 'CoverageEligibilityResponseInsuranceItemBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'authorizationRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'authorizationSupporting', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItem', 'authorizationUrl', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineCoverageEligibilityResponseInsuranceItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsuranceItem', nil, 'CoverageEligibilityResponseInsuranceItem', js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsuranceItemPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseInsuranceItemBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'usedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponseInsuranceItemBenefit', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityResponseInsuranceItemBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseInsuranceItemBenefit', nil, 'CoverageEligibilityResponseInsuranceItemBenefit', js.FHIRFactoryJs);
  defineCoverageEligibilityResponseInsuranceItemBenefitPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponseErrorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponseError', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineCoverageEligibilityResponseErrorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponseError', nil, 'CoverageEligibilityResponseError', js.FHIRFactoryJs);
  defineCoverageEligibilityResponseErrorPropsJs(js, def);
end;


procedure defineCoverageEligibilityResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'CoverageEligibilityResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'requestor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'request', 'Reference(CoverageEligibilityRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'insurance', 'CoverageEligibilityResponseInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'preAuthRef', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'CoverageEligibilityResponse', 'error', 'CoverageEligibilityResponseError', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineCoverageEligibilityResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('CoverageEligibilityResponse', nil, 'CoverageEligibilityResponse', js.FHIRFactoryJs);
  defineCoverageEligibilityResponsePropsJs(js, def);
end;


procedure defineDetectedIssueEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueEvidence', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssueEvidence', 'detail', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueEvidence', nil, 'DetectedIssueEvidence', js.FHIRFactoryJs);
  defineDetectedIssueEvidencePropsJs(js, def);
end;


procedure defineDetectedIssueMitigationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DetectedIssueMitigation', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssueMitigation', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssueMitigation', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDetectedIssueMitigationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssueMitigation', nil, 'DetectedIssueMitigation', js.FHIRFactoryJs);
  defineDetectedIssueMitigationPropsJs(js, def);
end;


procedure defineDetectedIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DetectedIssue', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'identifiedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DetectedIssue', 'identifiedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DetectedIssue', 'implicated', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue', 'evidence', 'DetectedIssueEvidence', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DetectedIssue', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DetectedIssue', 'mitigation', 'DetectedIssueMitigation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDetectedIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DetectedIssue', nil, 'DetectedIssue', js.FHIRFactoryJs);
  defineDetectedIssuePropsJs(js, def);
end;


procedure defineDeviceUdiCarrierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceUdiCarrier', 'deviceIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier', 'issuer', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier', 'jurisdiction', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier', 'carrierAIDC', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'DeviceUdiCarrier', 'carrierHRF', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUdiCarrier', 'entryType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceUdiCarrierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUdiCarrier', nil, 'DeviceUdiCarrier', js.FHIRFactoryJs);
  defineDeviceUdiCarrierPropsJs(js, def);
end;


procedure defineDeviceDeviceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDeviceName', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDeviceName', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDeviceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDeviceName', nil, 'DeviceDeviceName', js.FHIRFactoryJs);
  defineDeviceDeviceNamePropsJs(js, def);
end;


procedure defineDeviceSpecializationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceSpecialization', 'systemType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceSpecialization', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceSpecializationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceSpecialization', nil, 'DeviceSpecialization', js.FHIRFactoryJs);
  defineDeviceSpecializationPropsJs(js, def);
end;


procedure defineDeviceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceVersion', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceVersion', 'component', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceVersion', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceVersion', nil, 'DeviceVersion', js.FHIRFactoryJs);
  defineDeviceVersionPropsJs(js, def);
end;


procedure defineDevicePropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceProperty', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceProperty', 'valueQuantity', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceProperty', 'valueCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDevicePropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceProperty', nil, 'DeviceProperty', js.FHIRFactoryJs);
  defineDevicePropertyPropsJs(js, def);
end;


procedure defineDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Device', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'definition', 'Reference(DeviceDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'udiCarrier', 'DeviceUdiCarrier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'distinctIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufacturer', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'manufactureDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Device', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'serialNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'deviceName', 'DeviceDeviceName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'modelNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'partNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'specialization', 'DeviceSpecialization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'version', 'DeviceVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'property', 'DeviceProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Device', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Device', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'safety', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Device', 'parent', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Device', nil, 'Device', js.FHIRFactoryJs);
  defineDevicePropsJs(js, def);
end;


procedure defineDeviceDefinitionUdiDeviceIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier', 'deviceIdentifier', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier', 'issuer', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionUdiDeviceIdentifier', 'jurisdiction', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionUdiDeviceIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionUdiDeviceIdentifier', nil, 'DeviceDefinitionUdiDeviceIdentifier', js.FHIRFactoryJs);
  defineDeviceDefinitionUdiDeviceIdentifierPropsJs(js, def);
end;


procedure defineDeviceDefinitionDeviceNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionDeviceName', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionDeviceName', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionDeviceNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionDeviceName', nil, 'DeviceDefinitionDeviceName', js.FHIRFactoryJs);
  defineDeviceDefinitionDeviceNamePropsJs(js, def);
end;


procedure defineDeviceDefinitionSpecializationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionSpecialization', 'systemType', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinitionSpecialization', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineDeviceDefinitionSpecializationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionSpecialization', nil, 'DeviceDefinitionSpecialization', js.FHIRFactoryJs);
  defineDeviceDefinitionSpecializationPropsJs(js, def);
end;


procedure defineDeviceDefinitionCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionCapability', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionCapability', 'description', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionCapability', nil, 'DeviceDefinitionCapability', js.FHIRFactoryJs);
  defineDeviceDefinitionCapabilityPropsJs(js, def);
end;


procedure defineDeviceDefinitionPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionProperty', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionProperty', 'valueQuantity', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinitionProperty', 'valueCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionProperty', nil, 'DeviceDefinitionProperty', js.FHIRFactoryJs);
  defineDeviceDefinitionPropertyPropsJs(js, def);
end;


procedure defineDeviceDefinitionMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceDefinitionMaterial', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinitionMaterial', 'alternate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'DeviceDefinitionMaterial', 'allergenicIndicator', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineDeviceDefinitionMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinitionMaterial', nil, 'DeviceDefinitionMaterial', js.FHIRFactoryJs);
  defineDeviceDefinitionMaterialPropsJs(js, def);
end;


procedure defineDeviceDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'udiDeviceIdentifier', 'DeviceDefinitionUdiDeviceIdentifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'manufacturerString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition', 'manufacturerReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'deviceName', 'DeviceDefinitionDeviceName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'modelNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'specialization', 'DeviceDefinitionSpecialization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'safety', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'shelfLifeStorage', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'languageCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'capability', 'DeviceDefinitionCapability', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'property', 'DeviceDefinitionProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'owner', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition', 'onlineInformation', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceDefinition', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceDefinition', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'parentDevice', 'Reference(DeviceDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceDefinition', 'material', 'DeviceDefinitionMaterial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceDefinition', nil, 'DeviceDefinition', js.FHIRFactoryJs);
  defineDeviceDefinitionPropsJs(js, def);
end;


procedure defineDeviceMetricCalibrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceMetricCalibration', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration', 'state', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetricCalibration', 'time', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineDeviceMetricCalibrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetricCalibration', nil, 'DeviceMetricCalibration', js.FHIRFactoryJs);
  defineDeviceMetricCalibrationPropsJs(js, def);
end;


procedure defineDeviceMetricPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceMetric', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceMetric', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'source', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'parent', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'operationalStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'color', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceMetric', 'measurementPeriod', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceMetric', 'calibration', 'DeviceMetricCalibration', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceMetricJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceMetric', nil, 'DeviceMetric', js.FHIRFactoryJs);
  defineDeviceMetricPropsJs(js, def);
end;


procedure defineDeviceRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DeviceRequestParameter', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequestParameter', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineDeviceRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequestParameter', nil, 'DeviceRequestParameter', js.FHIRFactoryJs);
  defineDeviceRequestParameterPropsJs(js, def);
end;


procedure defineDeviceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'priorRequest', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceRequest', 'codeReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'codeCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'parameter', 'DeviceRequestParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceRequest', 'requester', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceRequest', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceRequest', nil, 'DeviceRequest', js.FHIRFactoryJs);
  defineDeviceRequestPropsJs(js, def);
end;


procedure defineDeviceUseStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DeviceUseStatement', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DeviceUseStatement', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'derivedFrom', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'recordedOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DeviceUseStatement', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DeviceUseStatement', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DeviceUseStatement', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDeviceUseStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DeviceUseStatement', nil, 'DeviceUseStatement', js.FHIRFactoryJs);
  defineDeviceUseStatementPropsJs(js, def);
end;


procedure defineDiagnosticReportMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DiagnosticReportMedia', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReportMedia', 'link', 'Reference(Media)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDiagnosticReportMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReportMedia', nil, 'DiagnosticReportMedia', js.FHIRFactoryJs);
  defineDiagnosticReportMediaPropsJs(js, def);
end;


procedure defineDiagnosticReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DiagnosticReport', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DiagnosticReport', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DiagnosticReport', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'resultsInterpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'result', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'imagingStudy', 'Reference(ImagingStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'media', 'DiagnosticReportMedia', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'conclusion', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DiagnosticReport', 'conclusionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DiagnosticReport', 'presentedForm', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDiagnosticReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DiagnosticReport', nil, 'DiagnosticReport', js.FHIRFactoryJs);
  defineDiagnosticReportPropsJs(js, def);
end;


procedure defineDocumentManifestRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentManifestRelated', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifestRelated', 'ref', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentManifestRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifestRelated', nil, 'DocumentManifestRelated', js.FHIRFactoryJs);
  defineDocumentManifestRelatedPropsJs(js, def);
end;


procedure defineDocumentManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentManifest', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentManifest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentManifest', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'recipient', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'source', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentManifest', 'content', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentManifest', 'related', 'DocumentManifestRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentManifest', nil, 'DocumentManifest', js.FHIRFactoryJs);
  defineDocumentManifestPropsJs(js, def);
end;


procedure defineDocumentReferenceRelatesToPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceRelatesTo', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReferenceRelatesTo', 'target', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceRelatesToJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceRelatesTo', nil, 'DocumentReferenceRelatesTo', js.FHIRFactoryJs);
  defineDocumentReferenceRelatesToPropsJs(js, def);
end;


procedure defineDocumentReferenceContentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContent', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContent', 'format', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceContentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContent', nil, 'DocumentReferenceContent', js.FHIRFactoryJs);
  defineDocumentReferenceContentPropsJs(js, def);
end;


procedure defineDocumentReferenceContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'DocumentReferenceContext', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext', 'event', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReferenceContext', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'facilityType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'practiceSetting', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'sourcePatientInfo', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReferenceContext', 'related', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineDocumentReferenceContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReferenceContext', nil, 'DocumentReferenceContext', js.FHIRFactoryJs);
  defineDocumentReferenceContextPropsJs(js, def);
end;


procedure defineDocumentReferencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'DocumentReference', 'masterIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'docStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'date', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'DocumentReference', 'author', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'authenticator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'custodian', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'DocumentReference', 'relatesTo', 'DocumentReferenceRelatesTo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'DocumentReference', 'securityLabel', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'content', 'DocumentReferenceContent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'DocumentReference', 'context', 'DocumentReferenceContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineDocumentReferenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('DocumentReference', nil, 'DocumentReference', js.FHIRFactoryJs);
  defineDocumentReferencePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisSampleSizePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize', 'numberOfStudies', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'EffectEvidenceSynthesisSampleSize', 'numberOfParticipants', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEffectEvidenceSynthesisSampleSizeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisSampleSize', nil, 'EffectEvidenceSynthesisSampleSize', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisSampleSizePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisResultsByExposurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure', 'exposureState', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure', 'variantState', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisResultsByExposure', 'riskEvidenceSynthesis', 'Reference(RiskEvidenceSynthesis)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEffectEvidenceSynthesisResultsByExposureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisResultsByExposure', nil, 'EffectEvidenceSynthesisResultsByExposure', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisResultsByExposurePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisEffectEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'variantState', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'unitOfMeasure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimate', 'precisionEstimate', 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisEffectEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisEffectEstimate', nil, 'EffectEvidenceSynthesisEffectEstimate', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisEffectEstimatePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', 'level', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', 'from', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', 'to', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', nil, 'EffectEvidenceSynthesisEffectEstimatePrecisionEstimate', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimatePropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisCertaintyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty', 'rating', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertainty', 'certaintySubcomponent', 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisCertaintyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisCertainty', nil, 'EffectEvidenceSynthesisCertainty', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisCertaintyPropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent', 'rating', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesisCertaintyCertaintySubcomponent', nil, 'EffectEvidenceSynthesisCertaintyCertaintySubcomponent', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js, def);
end;


procedure defineEffectEvidenceSynthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'synthesisType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'studyType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'population', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'exposure', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'exposureAlternative', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'outcome', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'sampleSize', 'EffectEvidenceSynthesisSampleSize', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'resultsByExposure', 'EffectEvidenceSynthesisResultsByExposure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'effectEstimate', 'EffectEvidenceSynthesisEffectEstimate', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EffectEvidenceSynthesis', 'certainty', 'EffectEvidenceSynthesisCertainty', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEffectEvidenceSynthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EffectEvidenceSynthesis', nil, 'EffectEvidenceSynthesis', js.FHIRFactoryJs);
  defineEffectEvidenceSynthesisPropsJs(js, def);
end;


procedure defineEncounterStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterStatusHistory', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterStatusHistory', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterStatusHistory', nil, 'EncounterStatusHistory', js.FHIRFactoryJs);
  defineEncounterStatusHistoryPropsJs(js, def);
end;


procedure defineEncounterClassHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterClassHistory', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterClassHistory', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterClassHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterClassHistory', nil, 'EncounterClassHistory', js.FHIRFactoryJs);
  defineEncounterClassHistoryPropsJs(js, def);
end;


procedure defineEncounterParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterParticipant', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterParticipant', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterParticipant', 'individual', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterParticipant', nil, 'EncounterParticipant', js.FHIRFactoryJs);
  defineEncounterParticipantPropsJs(js, def);
end;


procedure defineEncounterDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterDiagnosis', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis', 'use', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterDiagnosis', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEncounterDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterDiagnosis', nil, 'EncounterDiagnosis', js.FHIRFactoryJs);
  defineEncounterDiagnosisPropsJs(js, def);
end;


procedure defineEncounterHospitalizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterHospitalization', 'preAdmissionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'origin', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'admitSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'reAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dietPreference', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialCourtesy', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'specialArrangement', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EncounterHospitalization', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterHospitalization', 'dischargeDisposition', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterHospitalizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterHospitalization', nil, 'EncounterHospitalization', js.FHIRFactoryJs);
  defineEncounterHospitalizationPropsJs(js, def);
end;


procedure defineEncounterLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EncounterLocation', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EncounterLocation', 'physicalType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EncounterLocation', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EncounterLocation', nil, 'EncounterLocation', js.FHIRFactoryJs);
  defineEncounterLocationPropsJs(js, def);
end;


procedure defineEncounterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Encounter', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Encounter', 'statusHistory', 'EncounterStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'class', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'classHistory', 'EncounterClassHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'serviceType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'episodeOfCare', 'Reference(EpisodeOfCare)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'participant', 'EncounterParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'appointment', 'Reference(Appointment)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'length', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'diagnosis', 'EncounterDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'hospitalization', 'EncounterHospitalization', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'location', 'EncounterLocation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Encounter', 'serviceProvider', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Encounter', 'partOf', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEncounterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Encounter', nil, 'Encounter', js.FHIRFactoryJs);
  defineEncounterPropsJs(js, def);
end;


procedure defineEndpointPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Endpoint', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint', 'connectionType', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Endpoint', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Endpoint', 'payloadType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Endpoint', 'address', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEndpointJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Endpoint', nil, 'Endpoint', js.FHIRFactoryJs);
  defineEndpointPropsJs(js, def);
end;


procedure defineEnrollmentRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentRequest', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentRequest', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'candidate', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentRequest', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentRequest', nil, 'EnrollmentRequest', js.FHIRFactoryJs);
  defineEnrollmentRequestPropsJs(js, def);
end;


procedure defineEnrollmentResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EnrollmentResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EnrollmentResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'request', 'Reference(EnrollmentRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EnrollmentResponse', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EnrollmentResponse', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EnrollmentResponse', 'requestProvider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEnrollmentResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EnrollmentResponse', nil, 'EnrollmentResponse', js.FHIRFactoryJs);
  defineEnrollmentResponsePropsJs(js, def);
end;


procedure defineEpisodeOfCareStatusHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareStatusHistory', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCareStatusHistory', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineEpisodeOfCareStatusHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareStatusHistory', nil, 'EpisodeOfCareStatusHistory', js.FHIRFactoryJs);
  defineEpisodeOfCareStatusHistoryPropsJs(js, def);
end;


procedure defineEpisodeOfCareDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCareDiagnosis', 'rank', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineEpisodeOfCareDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCareDiagnosis', nil, 'EpisodeOfCareDiagnosis', js.FHIRFactoryJs);
  defineEpisodeOfCareDiagnosisPropsJs(js, def);
end;


procedure defineEpisodeOfCarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'EpisodeOfCare', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EpisodeOfCare', 'statusHistory', 'EpisodeOfCareStatusHistory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'diagnosis', 'EpisodeOfCareDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'referralRequest', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'careManager', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EpisodeOfCare', 'team', 'Reference(CareTeam)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EpisodeOfCare', 'account', 'Reference(Account)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEpisodeOfCareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EpisodeOfCare', nil, 'EpisodeOfCare', js.FHIRFactoryJs);
  defineEpisodeOfCarePropsJs(js, def);
end;


procedure defineEventDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EventDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EventDefinition', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EventDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EventDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EventDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EventDefinition', 'trigger', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEventDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EventDefinition', nil, 'EventDefinition', js.FHIRFactoryJs);
  defineEventDefinitionPropsJs(js, def);
end;


procedure defineEvidencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Evidence', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'shortTitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Evidence', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Evidence', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Evidence', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'exposureBackground', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Evidence', 'exposureVariant', 'Reference(EvidenceVariable)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Evidence', 'outcome', 'Reference(EvidenceVariable)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEvidenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Evidence', nil, 'Evidence', js.FHIRFactoryJs);
  defineEvidencePropsJs(js, def);
end;


procedure defineEvidenceVariableCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'definitionTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'usageContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'participantEffectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'participantEffectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'participantEffectiveDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'participantEffectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'timeFromStart', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariableCharacteristic', 'groupMeasure', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineEvidenceVariableCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EvidenceVariableCharacteristic', nil, 'EvidenceVariableCharacteristic', js.FHIRFactoryJs);
  defineEvidenceVariableCharacteristicPropsJs(js, def);
end;


procedure defineEvidenceVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'EvidenceVariable', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'shortTitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'EvidenceVariable', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'EvidenceVariable', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'EvidenceVariable', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'EvidenceVariable', 'characteristic', 'EvidenceVariableCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineEvidenceVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('EvidenceVariable', nil, 'EvidenceVariable', js.FHIRFactoryJs);
  defineEvidenceVariablePropsJs(js, def);
end;


procedure defineExampleScenarioActorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioActor', 'actorId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioActor', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioActorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioActor', nil, 'ExampleScenarioActor', js.FHIRFactoryJs);
  defineExampleScenarioActorPropsJs(js, def);
end;


procedure defineExampleScenarioInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstance', 'resourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'resourceType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'version', 'ExampleScenarioInstanceVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioInstance', 'containedInstance', 'ExampleScenarioInstanceContainedInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstance', nil, 'ExampleScenarioInstance', js.FHIRFactoryJs);
  defineExampleScenarioInstancePropsJs(js, def);
end;


procedure defineExampleScenarioInstanceVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceVersion', 'versionId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceVersion', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceVersion', nil, 'ExampleScenarioInstanceVersion', js.FHIRFactoryJs);
  defineExampleScenarioInstanceVersionPropsJs(js, def);
end;


procedure defineExampleScenarioInstanceContainedInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance', 'resourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioInstanceContainedInstance', 'versionId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineExampleScenarioInstanceContainedInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioInstanceContainedInstance', nil, 'ExampleScenarioInstanceContainedInstance', js.FHIRFactoryJs);
  defineExampleScenarioInstanceContainedInstancePropsJs(js, def);
end;


procedure defineExampleScenarioProcessPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcess', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'preConditions', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'postConditions', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcess', 'step', 'ExampleScenarioProcessStep', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcess', nil, 'ExampleScenarioProcess', js.FHIRFactoryJs);
  defineExampleScenarioProcessPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'process', '@ExampleScenario.process', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'pause', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'operation', 'ExampleScenarioProcessStepOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStep', 'alternative', 'ExampleScenarioProcessStepAlternative', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStep', nil, 'ExampleScenarioProcessStep', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'number', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'initiator', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'receiver', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'initiatorActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'receiverActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'request', '@ExampleScenario.instance.containedInstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExampleScenarioProcessStepOperation', 'response', '@ExampleScenario.instance.containedInstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExampleScenarioProcessStepOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepOperation', nil, 'ExampleScenarioProcessStepOperation', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepOperationPropsJs(js, def);
end;


procedure defineExampleScenarioProcessStepAlternativePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenarioProcessStepAlternative', 'step', '@ExampleScenario.process.step', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioProcessStepAlternativeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenarioProcessStepAlternative', nil, 'ExampleScenarioProcessStepAlternative', js.FHIRFactoryJs);
  defineExampleScenarioProcessStepAlternativePropsJs(js, def);
end;


procedure defineExampleScenarioPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ExampleScenario', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExampleScenario', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExampleScenario', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExampleScenario', 'actor', 'ExampleScenarioActor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'instance', 'ExampleScenarioInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExampleScenario', 'process', 'ExampleScenarioProcess', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExampleScenarioJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExampleScenario', nil, 'ExampleScenario', js.FHIRFactoryJs);
  defineExampleScenarioPropsJs(js, def);
end;


procedure defineExplanationOfBenefitRelatedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitRelated', 'reference', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitRelatedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitRelated', nil, 'ExplanationOfBenefitRelated', js.FHIRFactoryJs);
  defineExplanationOfBenefitRelatedPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPayeePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayee', 'party', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPayeeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayee', nil, 'ExplanationOfBenefitPayee', js.FHIRFactoryJs);
  defineExplanationOfBenefitPayeePropsJs(js, def);
end;


procedure defineExplanationOfBenefitCareTeamPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'responsible', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitCareTeam', 'qualification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitCareTeamJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitCareTeam', nil, 'ExplanationOfBenefitCareTeam', js.FHIRFactoryJs);
  defineExplanationOfBenefitCareTeamPropsJs(js, def);
end;


procedure defineExplanationOfBenefitSupportingInfoPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'timingDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitSupportingInfo', 'reason', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitSupportingInfoJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitSupportingInfo', nil, 'ExplanationOfBenefitSupportingInfo', js.FHIRFactoryJs);
  defineExplanationOfBenefitSupportingInfoPropsJs(js, def);
end;


procedure defineExplanationOfBenefitDiagnosisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'diagnosisReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'onAdmission', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitDiagnosis', 'packageCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitDiagnosisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitDiagnosis', nil, 'ExplanationOfBenefitDiagnosis', js.FHIRFactoryJs);
  defineExplanationOfBenefitDiagnosisPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'procedureReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitProcedure', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcedure', nil, 'ExplanationOfBenefitProcedure', js.FHIRFactoryJs);
  defineExplanationOfBenefitProcedurePropsJs(js, def);
end;


procedure defineExplanationOfBenefitInsurancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitInsurance', 'focal', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitInsurance', 'coverage', 'Reference(Coverage)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitInsuranceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitInsurance', nil, 'ExplanationOfBenefitInsurance', js.FHIRFactoryJs);
  defineExplanationOfBenefitInsurancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitAccidentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAccident', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitAccidentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAccident', nil, 'ExplanationOfBenefitAccident', js.FHIRFactoryJs);
  defineExplanationOfBenefitAccidentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'encounter', 'Reference(Encounter)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'adjudication', 'ExplanationOfBenefitItemAdjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItem', 'detail', 'ExplanationOfBenefitItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItem', nil, 'ExplanationOfBenefitItem', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemAdjudicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemAdjudication', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineExplanationOfBenefitItemAdjudicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemAdjudication', nil, 'ExplanationOfBenefitItemAdjudication', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemAdjudicationPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetail', 'subDetail', 'ExplanationOfBenefitItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetail', nil, 'ExplanationOfBenefitItemDetail', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'revenue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'udi', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitItemDetailSubDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitItemDetailSubDetail', nil, 'ExplanationOfBenefitItemDetailSubDetail', js.FHIRFactoryJs);
  defineExplanationOfBenefitItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'provider', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'programCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'servicedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'servicedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'locationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'locationAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'locationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'subSite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItem', 'detail', 'ExplanationOfBenefitAddItemDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItem', nil, 'ExplanationOfBenefitAddItem', js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetail', 'subDetail', 'ExplanationOfBenefitAddItemDetailSubDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItemDetail', nil, 'ExplanationOfBenefitAddItemDetail', js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitAddItemDetailSubDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'productOrService', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'modifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'unitPrice', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'net', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitAddItemDetailSubDetail', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitAddItemDetailSubDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitAddItemDetailSubDetail', nil, 'ExplanationOfBenefitAddItemDetailSubDetail', js.FHIRFactoryJs);
  defineExplanationOfBenefitAddItemDetailSubDetailPropsJs(js, def);
end;


procedure defineExplanationOfBenefitTotalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitTotal', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitTotal', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitTotalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitTotal', nil, 'ExplanationOfBenefitTotal', js.FHIRFactoryJs);
  defineExplanationOfBenefitTotalPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPaymentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'adjustment', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'adjustmentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitPayment', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitPaymentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitPayment', nil, 'ExplanationOfBenefitPayment', js.FHIRFactoryJs);
  defineExplanationOfBenefitPaymentPropsJs(js, def);
end;


procedure defineExplanationOfBenefitProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'number', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitProcessNote', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitProcessNote', nil, 'ExplanationOfBenefitProcessNote', js.FHIRFactoryJs);
  defineExplanationOfBenefitProcessNotePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'excluded', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'network', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'term', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalance', 'financial', 'ExplanationOfBenefitBenefitBalanceFinancial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalance', nil, 'ExplanationOfBenefitBenefitBalance', js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalancePropsJs(js, def);
end;


procedure defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'allowedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'usedUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefitBenefitBalanceFinancial', 'usedMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineExplanationOfBenefitBenefitBalanceFinancialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefitBenefitBalanceFinancial', nil, 'ExplanationOfBenefitBenefitBalanceFinancial', js.FHIRFactoryJs);
  defineExplanationOfBenefitBenefitBalanceFinancialPropsJs(js, def);
end;


procedure defineExplanationOfBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ExplanationOfBenefit', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'subType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'billablePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'fundsReserveRequested', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'fundsReserve', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'related', 'ExplanationOfBenefitRelated', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'prescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'originalPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payee', 'ExplanationOfBenefitPayee', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'referral', 'Reference(ServiceRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'facility', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claim', 'Reference(Claim)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'claimResponse', 'Reference(ClaimResponse)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'preAuthRefPeriod', 'Period', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'careTeam', 'ExplanationOfBenefitCareTeam', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'supportingInfo', 'ExplanationOfBenefitSupportingInfo', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'diagnosis', 'ExplanationOfBenefitDiagnosis', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'procedure', 'ExplanationOfBenefitProcedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'precedence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'insurance', 'ExplanationOfBenefitInsurance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'accident', 'ExplanationOfBenefitAccident', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'item', 'ExplanationOfBenefitItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'addItem', 'ExplanationOfBenefitAddItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'adjudication', '@ExplanationOfBenefit.item.adjudication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'total', 'ExplanationOfBenefitTotal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'payment', 'ExplanationOfBenefitPayment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'formCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'form', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'processNote', 'ExplanationOfBenefitProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'benefitPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ExplanationOfBenefit', 'benefitBalance', 'ExplanationOfBenefitBenefitBalance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineExplanationOfBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ExplanationOfBenefit', nil, 'ExplanationOfBenefit', js.FHIRFactoryJs);
  defineExplanationOfBenefitPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'contributedToDeath', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'onsetString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistoryCondition', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistoryCondition', nil, 'FamilyMemberHistoryCondition', js.FHIRFactoryJs);
  defineFamilyMemberHistoryConditionPropsJs(js, def);
end;


procedure defineFamilyMemberHistoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'FamilyMemberHistory', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'sex', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'bornString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'ageString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'estimatedAge', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'FamilyMemberHistory', 'deceasedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'FamilyMemberHistory', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'FamilyMemberHistory', 'condition', 'FamilyMemberHistoryCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineFamilyMemberHistoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('FamilyMemberHistory', nil, 'FamilyMemberHistory', js.FHIRFactoryJs);
  defineFamilyMemberHistoryPropsJs(js, def);
end;


procedure defineFlagPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Flag', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Flag', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Flag', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Flag', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineFlagJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Flag', nil, 'Flag', js.FHIRFactoryJs);
  defineFlagPropsJs(js, def);
end;


procedure defineGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GoalTarget', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'detailString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GoalTarget', 'detailBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GoalTarget', 'detailInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GoalTarget', 'detailRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GoalTarget', 'dueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GoalTarget', 'dueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GoalTarget', nil, 'GoalTarget', js.FHIRFactoryJs);
  defineGoalTargetPropsJs(js, def);
end;


procedure defineGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Goal', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'lifecycleStatus', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal', 'achievementStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'startDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'startCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'target', 'GoalTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'statusDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Goal', 'statusReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Goal', 'expressedBy', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Goal', 'addresses', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'outcomeCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Goal', 'outcomeReference', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Goal', nil, 'Goal', js.FHIRFactoryJs);
  defineGoalPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLink', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'sliceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'GraphDefinitionLink', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLink', 'target', 'GraphDefinitionLinkTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLink', nil, 'GraphDefinitionLink', js.FHIRFactoryJs);
  defineGraphDefinitionLinkPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'compartment', 'GraphDefinitionLinkTargetCompartment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinitionLinkTarget', 'link', '@GraphDefinition.link', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionLinkTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTarget', nil, 'GraphDefinitionLinkTarget', js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetPropsJs(js, def);
end;


procedure defineGraphDefinitionLinkTargetCompartmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'rule', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinitionLinkTargetCompartment', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineGraphDefinitionLinkTargetCompartmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinitionLinkTargetCompartment', nil, 'GraphDefinitionLinkTargetCompartment', js.FHIRFactoryJs);
  defineGraphDefinitionLinkTargetCompartmentPropsJs(js, def);
end;


procedure defineGraphDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'GraphDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GraphDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GraphDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GraphDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'start', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GraphDefinition', 'link', 'GraphDefinitionLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGraphDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GraphDefinition', nil, 'GraphDefinition', js.FHIRFactoryJs);
  defineGraphDefinitionPropsJs(js, def);
end;


procedure defineGroupCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupCharacteristic', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupCharacteristic', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'GroupCharacteristic', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineGroupCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupCharacteristic', nil, 'GroupCharacteristic', js.FHIRFactoryJs);
  defineGroupCharacteristicPropsJs(js, def);
end;


procedure defineGroupMemberPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'GroupMember', 'entity', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GroupMember', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineGroupMemberJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GroupMember', nil, 'GroupMember', js.FHIRFactoryJs);
  defineGroupMemberPropsJs(js, def);
end;


procedure defineGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Group', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group', 'actual', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Group', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group', 'quantity', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Group', 'managingEntity', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Group', 'characteristic', 'GroupCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Group', 'member', 'GroupMember', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Group', nil, 'Group', js.FHIRFactoryJs);
  defineGroupPropsJs(js, def);
end;


procedure defineGuidanceResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'GuidanceResponse', 'requestIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'moduleUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'moduleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'moduleCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'GuidanceResponse', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'GuidanceResponse', 'performer', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'evaluationMessage', 'Reference(OperationOutcome)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'GuidanceResponse', 'outputParameters', 'Reference(Parameters)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'result', 'Reference(CarePlan)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'GuidanceResponse', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineGuidanceResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('GuidanceResponse', nil, 'GuidanceResponse', js.FHIRFactoryJs);
  defineGuidanceResponsePropsJs(js, def);
end;


procedure defineHealthcareServiceEligibilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceEligibility', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareServiceEligibility', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceEligibilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceEligibility', nil, 'HealthcareServiceEligibility', js.FHIRFactoryJs);
  defineHealthcareServiceEligibilityPropsJs(js, def);
end;


procedure defineHealthcareServiceAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceAvailableTime', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineHealthcareServiceAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceAvailableTime', nil, 'HealthcareServiceAvailableTime', js.FHIRFactoryJs);
  defineHealthcareServiceAvailableTimePropsJs(js, def);
end;


procedure defineHealthcareServiceNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'HealthcareServiceNotAvailable', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareServiceNotAvailable', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineHealthcareServiceNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareServiceNotAvailable', nil, 'HealthcareServiceNotAvailable', js.FHIRFactoryJs);
  defineHealthcareServiceNotAvailablePropsJs(js, def);
end;


procedure defineHealthcareServicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'HealthcareService', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService', 'providedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'extraDetails', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'HealthcareService', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'serviceProvisionCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'eligibility', 'HealthcareServiceEligibility', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'program', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'characteristic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'communication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'referralMethod', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'appointmentRequired', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'HealthcareService', 'availableTime', 'HealthcareServiceAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'notAvailable', 'HealthcareServiceNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'HealthcareService', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'HealthcareService', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineHealthcareServiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('HealthcareService', nil, 'HealthcareService', js.FHIRFactoryJs);
  defineHealthcareServicePropsJs(js, def);
end;


procedure defineImagingStudySeriesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeries', 'uid', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'modality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeries', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'bodySite', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'laterality', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeries', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudySeries', 'performer', 'ImagingStudySeriesPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudySeries', 'instance', 'ImagingStudySeriesInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudySeriesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeries', nil, 'ImagingStudySeries', js.FHIRFactoryJs);
  defineImagingStudySeriesPropsJs(js, def);
end;


procedure defineImagingStudySeriesPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesPerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesPerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImagingStudySeriesPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesPerformer', nil, 'ImagingStudySeriesPerformer', js.FHIRFactoryJs);
  defineImagingStudySeriesPerformerPropsJs(js, def);
end;


procedure defineImagingStudySeriesInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'uid', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'sopClass', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'number', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudySeriesInstance', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImagingStudySeriesInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudySeriesInstance', nil, 'ImagingStudySeriesInstance', js.FHIRFactoryJs);
  defineImagingStudySeriesInstancePropsJs(js, def);
end;


procedure defineImagingStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImagingStudy', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'modality', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'started', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImagingStudy', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'referrer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'interpreter', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfSeries', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'numberOfInstances', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'procedureReference', 'Reference(Procedure)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'procedureCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImagingStudy', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImagingStudy', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImagingStudy', 'series', 'ImagingStudySeries', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImagingStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImagingStudy', nil, 'ImagingStudy', js.FHIRFactoryJs);
  defineImagingStudyPropsJs(js, def);
end;


procedure defineImmunizationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationPerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationPerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImmunizationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationPerformer', nil, 'ImmunizationPerformer', js.FHIRFactoryJs);
  defineImmunizationPerformerPropsJs(js, def);
end;


procedure defineImmunizationEducationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationEducation', 'documentType', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation', 'reference', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEducation', 'publicationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEducation', 'presentationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationEducationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEducation', nil, 'ImmunizationEducation', js.FHIRFactoryJs);
  defineImmunizationEducationPropsJs(js, def);
end;


procedure defineImmunizationReactionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationReaction', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationReaction', 'detail', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationReaction', 'reported', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineImmunizationReactionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationReaction', nil, 'ImmunizationReaction', js.FHIRFactoryJs);
  defineImmunizationReactionPropsJs(js, def);
end;


procedure defineImmunizationProtocolAppliedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'targetDisease', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'doseNumberPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'doseNumberString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'seriesDosesPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationProtocolApplied', 'seriesDosesString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationProtocolAppliedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationProtocolApplied', nil, 'ImmunizationProtocolApplied', js.FHIRFactoryJs);
  defineImmunizationProtocolAppliedPropsJs(js, def);
end;


procedure defineImmunizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Immunization', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'vaccineCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'occurrenceString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'recorded', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'primarySource', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'reportOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Immunization', 'expirationDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Immunization', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'doseQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'performer', 'ImmunizationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'isSubpotent', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Immunization', 'subpotentReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'education', 'ImmunizationEducation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'programEligibility', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'fundingSource', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Immunization', 'reaction', 'ImmunizationReaction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Immunization', 'protocolApplied', 'ImmunizationProtocolApplied', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Immunization', nil, 'Immunization', js.FHIRFactoryJs);
  defineImmunizationPropsJs(js, def);
end;


procedure defineImmunizationEvaluationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationEvaluation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'targetDisease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'immunizationEvent', 'Reference(Immunization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseStatusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseNumberPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'doseNumberString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'seriesDosesPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationEvaluation', 'seriesDosesString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImmunizationEvaluationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationEvaluation', nil, 'ImmunizationEvaluation', js.FHIRFactoryJs);
  defineImmunizationEvaluationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'vaccineCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'targetDisease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'contraindicatedVaccineCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'forecastStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'forecastReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'dateCriterion', 'ImmunizationRecommendationRecommendationDateCriterion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'series', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'doseNumberPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'doseNumberString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'seriesDosesPositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'seriesDosesString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingImmunization', 'Reference(Immunization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendation', 'supportingPatientInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendation', nil, 'ImmunizationRecommendationRecommendation', js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationPropsJs(js, def);
end;


procedure defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendationRecommendationDateCriterion', 'value', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineImmunizationRecommendationRecommendationDateCriterionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendationRecommendationDateCriterion', nil, 'ImmunizationRecommendationRecommendationDateCriterion', js.FHIRFactoryJs);
  defineImmunizationRecommendationRecommendationDateCriterionPropsJs(js, def);
end;


procedure defineImmunizationRecommendationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ImmunizationRecommendation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'authority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImmunizationRecommendation', 'recommendation', 'ImmunizationRecommendationRecommendation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImmunizationRecommendationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImmunizationRecommendation', nil, 'ImmunizationRecommendation', js.FHIRFactoryJs);
  defineImmunizationRecommendationPropsJs(js, def);
end;


procedure defineImplementationGuideDependsOnPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDependsOn', 'uri', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn', 'packageId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDependsOn', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDependsOnJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDependsOn', nil, 'ImplementationGuideDependsOn', js.FHIRFactoryJs);
  defineImplementationGuideDependsOnPropsJs(js, def);
end;


procedure defineImplementationGuideGlobalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideGlobal', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideGlobal', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideGlobalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideGlobal', nil, 'ImplementationGuideGlobal', js.FHIRFactoryJs);
  defineImplementationGuideGlobalPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinition', 'grouping', 'ImplementationGuideDefinitionGrouping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition', 'resource', 'ImplementationGuideDefinitionResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition', 'page', 'ImplementationGuideDefinitionPage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinition', 'parameter', 'ImplementationGuideDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideDefinition', 'template', 'ImplementationGuideDefinitionTemplate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinition', nil, 'ImplementationGuideDefinition', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionGroupingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionGrouping', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionGrouping', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionGroupingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionGrouping', nil, 'ImplementationGuideDefinitionGrouping', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionGroupingPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'exampleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionResource', 'groupingId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionResource', nil, 'ImplementationGuideDefinitionResource', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionResourcePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionPage', 'nameUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage', 'nameReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage', 'generation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionPage', 'page', '@ImplementationGuide.definition.page', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideDefinitionPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionPage', nil, 'ImplementationGuideDefinitionPage', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionPagePropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionParameter', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionParameter', nil, 'ImplementationGuideDefinitionParameter', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionParameterPropsJs(js, def);
end;


procedure defineImplementationGuideDefinitionTemplatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate', 'source', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideDefinitionTemplate', 'scope', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideDefinitionTemplateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideDefinitionTemplate', nil, 'ImplementationGuideDefinitionTemplate', js.FHIRFactoryJs);
  defineImplementationGuideDefinitionTemplatePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifest', 'rendering', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifest', 'resource', 'ImplementationGuideManifestResource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuideManifest', 'page', 'ImplementationGuideManifestPage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineImplementationGuideManifestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifest', nil, 'ImplementationGuideManifest', js.FHIRFactoryJs);
  defineImplementationGuideManifestPropsJs(js, def);
end;


procedure defineImplementationGuideManifestResourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestResource', 'reference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuideManifestResource', 'exampleBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuideManifestResource', 'exampleCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestResource', 'relativePath', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestResourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestResource', nil, 'ImplementationGuideManifestResource', js.FHIRFactoryJs);
  defineImplementationGuideManifestResourcePropsJs(js, def);
end;


procedure defineImplementationGuideManifestPagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ImplementationGuideManifestPage', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuideManifestPage', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineImplementationGuideManifestPageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuideManifestPage', nil, 'ImplementationGuideManifestPage', js.FHIRFactoryJs);
  defineImplementationGuideManifestPagePropsJs(js, def);
end;


procedure defineImplementationGuidePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ImplementationGuide', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ImplementationGuide', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ImplementationGuide', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'packageId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'license', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ImplementationGuide', 'dependsOn', 'ImplementationGuideDependsOn', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'global', 'ImplementationGuideGlobal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ImplementationGuide', 'definition', 'ImplementationGuideDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ImplementationGuide', 'manifest', 'ImplementationGuideManifest', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineImplementationGuideJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ImplementationGuide', nil, 'ImplementationGuide', js.FHIRFactoryJs);
  defineImplementationGuidePropsJs(js, def);
end;


procedure defineInsurancePlanContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanContact', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanContact', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanContact', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanContact', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanContact', nil, 'InsurancePlanContact', js.FHIRFactoryJs);
  defineInsurancePlanContactPropsJs(js, def);
end;


procedure defineInsurancePlanCoveragePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverage', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanCoverage', 'benefit', 'InsurancePlanCoverageBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanCoverageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverage', nil, 'InsurancePlanCoverage', js.FHIRFactoryJs);
  defineInsurancePlanCoveragePropsJs(js, def);
end;


procedure defineInsurancePlanCoverageBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverageBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefit', 'requirement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefit', 'limit', 'InsurancePlanCoverageBenefitLimit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanCoverageBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverageBenefit', nil, 'InsurancePlanCoverageBenefit', js.FHIRFactoryJs);
  defineInsurancePlanCoverageBenefitPropsJs(js, def);
end;


procedure defineInsurancePlanCoverageBenefitLimitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanCoverageBenefitLimit', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanCoverageBenefitLimit', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanCoverageBenefitLimitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanCoverageBenefitLimit', nil, 'InsurancePlanCoverageBenefitLimit', js.FHIRFactoryJs);
  defineInsurancePlanCoverageBenefitLimitPropsJs(js, def);
end;


procedure defineInsurancePlanPlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlan', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlan', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan', 'generalCost', 'InsurancePlanPlanGeneralCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlan', 'specificCost', 'InsurancePlanPlanSpecificCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlan', nil, 'InsurancePlanPlan', js.FHIRFactoryJs);
  defineInsurancePlanPlanPropsJs(js, def);
end;


procedure defineInsurancePlanPlanGeneralCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost', 'groupSize', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost', 'cost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanGeneralCost', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineInsurancePlanPlanGeneralCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanGeneralCost', nil, 'InsurancePlanPlanGeneralCost', js.FHIRFactoryJs);
  defineInsurancePlanPlanGeneralCostPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCost', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCost', 'benefit', 'InsurancePlanPlanSpecificCostBenefit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanSpecificCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCost', nil, 'InsurancePlanPlanSpecificCost', js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostBenefitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefit', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefit', 'cost', 'InsurancePlanPlanSpecificCostBenefitCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanPlanSpecificCostBenefitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCostBenefit', nil, 'InsurancePlanPlanSpecificCostBenefit', js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostBenefitPropsJs(js, def);
end;


procedure defineInsurancePlanPlanSpecificCostBenefitCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost', 'applicability', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost', 'qualifiers', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlanPlanSpecificCostBenefitCost', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInsurancePlanPlanSpecificCostBenefitCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlanPlanSpecificCostBenefitCost', nil, 'InsurancePlanPlanSpecificCostBenefitCost', js.FHIRFactoryJs);
  defineInsurancePlanPlanSpecificCostBenefitCostPropsJs(js, def);
end;


procedure defineInsurancePlanPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'InsurancePlan', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlan', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InsurancePlan', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan', 'ownedBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan', 'administeredBy', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InsurancePlan', 'coverageArea', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'contact', 'InsurancePlanContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'coverage', 'InsurancePlanCoverage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'InsurancePlan', 'plan', 'InsurancePlanPlan', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInsurancePlanJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InsurancePlan', nil, 'InsurancePlan', js.FHIRFactoryJs);
  defineInsurancePlanPropsJs(js, def);
end;


procedure defineInvoiceParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceParticipant', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceParticipant', nil, 'InvoiceParticipant', js.FHIRFactoryJs);
  defineInvoiceParticipantPropsJs(js, def);
end;


procedure defineInvoiceLineItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItem', 'sequence', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'InvoiceLineItem', 'chargeItemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem', 'chargeItemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItem', 'priceComponent', 'InvoiceLineItemPriceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceLineItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItem', nil, 'InvoiceLineItem', js.FHIRFactoryJs);
  defineInvoiceLineItemPropsJs(js, def);
end;


procedure defineInvoiceLineItemPriceComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'factor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'InvoiceLineItemPriceComponent', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineInvoiceLineItemPriceComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('InvoiceLineItemPriceComponent', nil, 'InvoiceLineItemPriceComponent', js.FHIRFactoryJs);
  defineInvoiceLineItemPriceComponentPropsJs(js, def);
end;


procedure defineInvoicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Invoice', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'cancelledReason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'recipient', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Invoice', 'participant', 'InvoiceParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'account', 'Reference(Account)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'lineItem', 'InvoiceLineItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'totalPriceComponent', '@Invoice.lineItem.priceComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Invoice', 'totalNet', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'totalGross', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Invoice', 'paymentTerms', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Invoice', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineInvoiceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Invoice', nil, 'Invoice', js.FHIRFactoryJs);
  defineInvoicePropsJs(js, def);
end;


procedure defineLibraryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Library', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Library', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Library', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Library', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Library', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'parameter', 'ParameterDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'dataRequirement', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Library', 'content', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLibraryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Library', nil, 'Library', js.FHIRFactoryJs);
  defineLibraryPropsJs(js, def);
end;


procedure defineLinkageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LinkageItem', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LinkageItem', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineLinkageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LinkageItem', nil, 'LinkageItem', js.FHIRFactoryJs);
  defineLinkageItemPropsJs(js, def);
end;


procedure defineLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Linkage', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Linkage', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Linkage', 'item', 'LinkageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Linkage', nil, 'Linkage', js.FHIRFactoryJs);
  defineLinkagePropsJs(js, def);
end;


procedure defineListEntryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ListEntry', 'flag', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ListEntry', 'deleted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ListEntry', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ListEntry', 'item', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListEntryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ListEntry', nil, 'ListEntry', js.FHIRFactoryJs);
  defineListEntryPropsJs(js, def);
end;


procedure defineListPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'List', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'List', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'List', 'source', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'orderedBy', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'List', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List', 'entry', 'ListEntry', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'List', 'emptyReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineListJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('List', nil, 'List', js.FHIRFactoryJs);
  defineListPropsJs(js, def);
end;


procedure defineLocationPositionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationPosition', 'longitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition', 'latitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'LocationPosition', 'altitude', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineLocationPositionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationPosition', nil, 'LocationPosition', js.FHIRFactoryJs);
  defineLocationPositionPropsJs(js, def);
end;


procedure defineLocationHoursOfOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'LocationHoursOfOperation', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'LocationHoursOfOperation', 'openingTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'LocationHoursOfOperation', 'closingTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineLocationHoursOfOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('LocationHoursOfOperation', nil, 'LocationHoursOfOperation', js.FHIRFactoryJs);
  defineLocationHoursOfOperationPropsJs(js, def);
end;


procedure defineLocationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Location', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location', 'operationalStatus', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'physicalType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'position', 'LocationPosition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'partOf', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Location', 'hoursOfOperation', 'LocationHoursOfOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Location', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Location', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineLocationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Location', nil, 'Location', js.FHIRFactoryJs);
  defineLocationPropsJs(js, def);
end;


procedure defineMeasureGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroup', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroup', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroup', 'population', 'MeasureGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureGroup', 'stratifier', 'MeasureGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroup', nil, 'MeasureGroup', js.FHIRFactoryJs);
  defineMeasureGroupPropsJs(js, def);
end;


procedure defineMeasureGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupPopulation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupPopulation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupPopulation', 'criteria', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupPopulation', nil, 'MeasureGroupPopulation', js.FHIRFactoryJs);
  defineMeasureGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifier', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'criteria', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifier', 'component', 'MeasureGroupStratifierComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifier', nil, 'MeasureGroupStratifier', js.FHIRFactoryJs);
  defineMeasureGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureGroupStratifierComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureGroupStratifierComponent', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureGroupStratifierComponent', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureGroupStratifierComponent', 'criteria', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureGroupStratifierComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureGroupStratifierComponent', nil, 'MeasureGroupStratifierComponent', js.FHIRFactoryJs);
  defineMeasureGroupStratifierComponentPropsJs(js, def);
end;


procedure defineMeasureSupplementalDataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureSupplementalData', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureSupplementalData', 'usage', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureSupplementalData', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureSupplementalData', 'criteria', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureSupplementalDataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureSupplementalData', nil, 'MeasureSupplementalData', js.FHIRFactoryJs);
  defineMeasureSupplementalDataPropsJs(js, def);
end;


procedure defineMeasurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Measure', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Measure', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Measure', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'disclaimer', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'scoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'compositeScoring', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'riskAdjustment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rateAggregation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'rationale', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'clinicalRecommendationStatement', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'improvementNotation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Measure', 'guidance', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Measure', 'group', 'MeasureGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Measure', 'supplementalData', 'MeasureSupplementalData', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Measure', nil, 'Measure', js.FHIRFactoryJs);
  defineMeasurePropsJs(js, def);
end;


procedure defineMeasureReportGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroup', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup', 'population', 'MeasureReportGroupPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroup', 'measureScore', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroup', 'stratifier', 'MeasureReportGroupStratifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroup', nil, 'MeasureReportGroup', js.FHIRFactoryJs);
  defineMeasureReportGroupPropsJs(js, def);
end;


procedure defineMeasureReportGroupPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupPopulation', 'subjectResults', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupPopulation', nil, 'MeasureReportGroupPopulation', js.FHIRFactoryJs);
  defineMeasureReportGroupPopulationPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifier', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifier', 'stratum', 'MeasureReportGroupStratifierStratum', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportGroupStratifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifier', nil, 'MeasureReportGroupStratifier', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'value', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'component', 'MeasureReportGroupStratifierStratumComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'population', 'MeasureReportGroupStratifierStratumPopulation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratum', 'measureScore', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratum', nil, 'MeasureReportGroupStratifierStratum', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumComponent', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumComponent', 'value', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumComponent', nil, 'MeasureReportGroupStratifierStratumComponent', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumComponentPropsJs(js, def);
end;


procedure defineMeasureReportGroupStratifierStratumPopulationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'count', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MeasureReportGroupStratifierStratumPopulation', 'subjectResults', 'Reference(List)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMeasureReportGroupStratifierStratumPopulationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReportGroupStratifierStratumPopulation', nil, 'MeasureReportGroupStratifierStratumPopulation', js.FHIRFactoryJs);
  defineMeasureReportGroupStratifierStratumPopulationPropsJs(js, def);
end;


procedure defineMeasureReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MeasureReport', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'measure', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MeasureReport', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MeasureReport', 'reporter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'improvementNotation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MeasureReport', 'group', 'MeasureReportGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MeasureReport', 'evaluatedResource', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMeasureReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MeasureReport', nil, 'MeasureReport', js.FHIRFactoryJs);
  defineMeasureReportPropsJs(js, def);
end;


procedure defineMediaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Media', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'basedOn', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'partOf', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'modality', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'view', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'createdDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'createdPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Media', 'operator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Media', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'deviceName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Media', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'height', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'width', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'frames', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Media', 'duration', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'Media', 'content', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Media', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMediaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Media', nil, 'Media', js.FHIRFactoryJs);
  defineMediaPropsJs(js, def);
end;


procedure defineMedicationIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationIngredient', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationIngredient', 'isActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationIngredient', 'strength', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationIngredient', nil, 'MedicationIngredient', js.FHIRFactoryJs);
  defineMedicationIngredientPropsJs(js, def);
end;


procedure defineMedicationBatchPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationBatch', 'lotNumber', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationBatch', 'expirationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineMedicationBatchJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationBatch', nil, 'MedicationBatch', js.FHIRFactoryJs);
  defineMedicationBatchPropsJs(js, def);
end;


procedure defineMedicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Medication', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Medication', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'form', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'amount', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Medication', 'ingredient', 'MedicationIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Medication', 'batch', 'MedicationBatch', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Medication', nil, 'Medication', js.FHIRFactoryJs);
  defineMedicationPropsJs(js, def);
end;


procedure defineMedicationAdministrationPerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationPerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationPerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationPerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationPerformer', nil, 'MedicationAdministrationPerformer', js.FHIRFactoryJs);
  defineMedicationAdministrationPerformerPropsJs(js, def);
end;


procedure defineMedicationAdministrationDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationAdministrationDosage', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'site', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'route', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'dose', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministrationDosage', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationAdministrationDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministrationDosage', nil, 'MedicationAdministrationDosage', js.FHIRFactoryJs);
  defineMedicationAdministrationDosagePropsJs(js, def);
end;


procedure defineMedicationAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationAdministration', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationAdministration', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationAdministration', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'performer', 'MedicationAdministrationPerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'request', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'device', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationAdministration', 'dosage', 'MedicationAdministrationDosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationAdministration', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationAdministration', nil, 'MedicationAdministration', js.FHIRFactoryJs);
  defineMedicationAdministrationPropsJs(js, def);
end;


procedure defineMedicationDispensePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispensePerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispensePerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationDispensePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispensePerformer', nil, 'MedicationDispensePerformer', js.FHIRFactoryJs);
  defineMedicationDispensePerformerPropsJs(js, def);
end;


procedure defineMedicationDispenseSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'wasSubstituted', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispenseSubstitution', 'responsibleParty', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispenseSubstitution', nil, 'MedicationDispenseSubstitution', js.FHIRFactoryJs);
  defineMedicationDispenseSubstitutionPropsJs(js, def);
end;


procedure defineMedicationDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationDispense', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'partOf', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'statusReasonReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'performer', 'MedicationDispensePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'authorizingPrescription', 'Reference(MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'daysSupply', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'whenPrepared', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'whenHandedOver', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationDispense', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'receiver', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'substitution', 'MedicationDispenseSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationDispense', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationDispense', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationDispense', nil, 'MedicationDispense', js.FHIRFactoryJs);
  defineMedicationDispensePropsJs(js, def);
end;


procedure defineMedicationKnowledgeRelatedMedicationKnowledgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRelatedMedicationKnowledge', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRelatedMedicationKnowledge', 'reference', 'Reference(MedicationKnowledge)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeRelatedMedicationKnowledgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRelatedMedicationKnowledge', nil, 'MedicationKnowledgeRelatedMedicationKnowledge', js.FHIRFactoryJs);
  defineMedicationKnowledgeRelatedMedicationKnowledgePropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonographPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonograph', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonograph', 'source', 'Reference(DocumentReference)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeMonographJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonograph', nil, 'MedicationKnowledgeMonograph', js.FHIRFactoryJs);
  defineMedicationKnowledgeMonographPropsJs(js, def);
end;


procedure defineMedicationKnowledgeIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeIngredient', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient', 'isActive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationKnowledgeIngredient', 'strength', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeIngredient', nil, 'MedicationKnowledgeIngredient', js.FHIRFactoryJs);
  defineMedicationKnowledgeIngredientPropsJs(js, def);
end;


procedure defineMedicationKnowledgeCostPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeCost', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeCost', 'source', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeCost', 'cost', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeCostJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeCost', nil, 'MedicationKnowledgeCost', js.FHIRFactoryJs);
  defineMedicationKnowledgeCostPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMonitoringProgramPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMonitoringProgram', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicationKnowledgeMonitoringProgramJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMonitoringProgram', nil, 'MedicationKnowledgeMonitoringProgram', js.FHIRFactoryJs);
  defineMedicationKnowledgeMonitoringProgramPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines', 'dosage', 'MedicationKnowledgeAdministrationGuidelinesDosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines', 'indicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines', 'indicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelines', 'patientCharacteristics', 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelines', nil, 'MedicationKnowledgeAdministrationGuidelines', js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesDosagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesDosage', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesDosage', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesDosageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelinesDosage', nil, 'MedicationKnowledgeAdministrationGuidelinesDosage', js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesDosagePropsJs(js, def);
end;


procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', 'characteristicCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', 'characteristicQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', nil, 'MedicationKnowledgeAdministrationGuidelinesPatientCharacteristics', js.FHIRFactoryJs);
  defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsPropsJs(js, def);
end;


procedure defineMedicationKnowledgeMedicineClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeMedicineClassification', 'classification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeMedicineClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeMedicineClassification', nil, 'MedicationKnowledgeMedicineClassification', js.FHIRFactoryJs);
  defineMedicationKnowledgeMedicineClassificationPropsJs(js, def);
end;


procedure defineMedicationKnowledgePackagingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgePackaging', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgePackaging', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgePackagingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgePackaging', nil, 'MedicationKnowledgePackaging', js.FHIRFactoryJs);
  defineMedicationKnowledgePackagingPropsJs(js, def);
end;


procedure defineMedicationKnowledgeDrugCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeDrugCharacteristic', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
end;

procedure defineMedicationKnowledgeDrugCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeDrugCharacteristic', nil, 'MedicationKnowledgeDrugCharacteristic', js.FHIRFactoryJs);
  defineMedicationKnowledgeDrugCharacteristicPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatory', 'regulatoryAuthority', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory', 'substitution', 'MedicationKnowledgeRegulatorySubstitution', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory', 'schedule', 'MedicationKnowledgeRegulatorySchedule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatory', 'maxDispense', 'MedicationKnowledgeRegulatoryMaxDispense', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatory', nil, 'MedicationKnowledgeRegulatory', js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatoryPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatorySubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySubstitution', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySubstitution', 'allowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineMedicationKnowledgeRegulatorySubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatorySubstitution', nil, 'MedicationKnowledgeRegulatorySubstitution', js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatorySubstitutionPropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatorySchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatorySchedule', 'schedule', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatorySchedule', nil, 'MedicationKnowledgeRegulatorySchedule', js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatorySchedulePropsJs(js, def);
end;


procedure defineMedicationKnowledgeRegulatoryMaxDispensePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeRegulatoryMaxDispense', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledgeRegulatoryMaxDispense', 'period', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeRegulatoryMaxDispenseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeRegulatoryMaxDispense', nil, 'MedicationKnowledgeRegulatoryMaxDispense', js.FHIRFactoryJs);
  defineMedicationKnowledgeRegulatoryMaxDispensePropsJs(js, def);
end;


procedure defineMedicationKnowledgeKineticsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledgeKinetics', 'areaUnderCurve', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeKinetics', 'lethalDose50', 'Quantity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledgeKinetics', 'halfLifePeriod', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationKnowledgeKineticsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledgeKinetics', nil, 'MedicationKnowledgeKinetics', js.FHIRFactoryJs);
  defineMedicationKnowledgeKineticsPropsJs(js, def);
end;


procedure defineMedicationKnowledgePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationKnowledge', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge', 'manufacturer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge', 'doseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge', 'relatedMedicationKnowledge', 'MedicationKnowledgeRelatedMedicationKnowledge', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'associatedMedication', 'Reference(Medication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'productType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'monograph', 'MedicationKnowledgeMonograph', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'ingredient', 'MedicationKnowledgeIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'preparationInstruction', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationKnowledge', 'intendedRoute', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'cost', 'MedicationKnowledgeCost', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'monitoringProgram', 'MedicationKnowledgeMonitoringProgram', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'administrationGuidelines', 'MedicationKnowledgeAdministrationGuidelines', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'medicineClassification', 'MedicationKnowledgeMedicineClassification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'packaging', 'MedicationKnowledgePackaging', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationKnowledge', 'drugCharacteristic', 'MedicationKnowledgeDrugCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'contraindication', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'regulatory', 'MedicationKnowledgeRegulatory', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationKnowledge', 'kinetics', 'MedicationKnowledgeKinetics', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationKnowledgeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationKnowledge', nil, 'MedicationKnowledge', js.FHIRFactoryJs);
  defineMedicationKnowledgePropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'initialFill', 'MedicationRequestDispenseRequestInitialFill', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'dispenseInterval', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'numberOfRepeatsAllowed', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'expectedSupplyDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequest', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequest', nil, 'MedicationRequestDispenseRequest', js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestPropsJs(js, def);
end;


procedure defineMedicationRequestDispenseRequestInitialFillPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestDispenseRequestInitialFill', 'duration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestDispenseRequestInitialFillJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestDispenseRequestInitialFill', nil, 'MedicationRequestDispenseRequestInitialFill', js.FHIRFactoryJs);
  defineMedicationRequestDispenseRequestInitialFillPropsJs(js, def);
end;


procedure defineMedicationRequestSubstitutionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicationRequestSubstitution', 'allowedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequestSubstitution', 'allowedCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequestSubstitution', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicationRequestSubstitutionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequestSubstitution', nil, 'MedicationRequestSubstitution', js.FHIRFactoryJs);
  defineMedicationRequestSubstitutionPropsJs(js, def);
end;


procedure defineMedicationRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationRequest', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequest', 'reportedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicationRequest', 'reportedReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'supportingInformation', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationRequest', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'recorder', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'courseOfTherapyType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'dosageInstruction', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'dispenseRequest', 'MedicationRequestDispenseRequest', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'substitution', 'MedicationRequestSubstitution', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'priorPrescription', 'Reference(MedicationRequest)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationRequest', 'detectedIssue', 'Reference(DetectedIssue)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationRequest', 'eventHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationRequest', nil, 'MedicationRequest', js.FHIRFactoryJs);
  defineMedicationRequestPropsJs(js, def);
end;


procedure defineMedicationStatementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicationStatement', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'basedOn', 'Reference(MedicationRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicationStatement', 'statusReason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'context', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'dateAsserted', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicationStatement', 'informationSource', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicationStatement', 'derivedFrom', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicationStatement', 'dosage', 'Dosage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicationStatementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicationStatement', nil, 'MedicationStatement', js.FHIRFactoryJs);
  defineMedicationStatementPropsJs(js, def);
end;


procedure defineMedicinalProductNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductName', 'productName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductName', 'namePart', 'MedicinalProductNameNamePart', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductName', 'countryLanguage', 'MedicinalProductNameCountryLanguage', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductName', nil, 'MedicinalProductName', js.FHIRFactoryJs);
  defineMedicinalProductNamePropsJs(js, def);
end;


procedure defineMedicinalProductNameNamePartPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameNamePart', 'part', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductNameNamePart', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameNamePartJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameNamePart', nil, 'MedicinalProductNameNamePart', js.FHIRFactoryJs);
  defineMedicinalProductNameNamePartPropsJs(js, def);
end;


procedure defineMedicinalProductNameCountryLanguagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'jurisdiction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductNameCountryLanguage', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductNameCountryLanguageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductNameCountryLanguage', nil, 'MedicinalProductNameCountryLanguage', js.FHIRFactoryJs);
  defineMedicinalProductNameCountryLanguagePropsJs(js, def);
end;


procedure defineMedicinalProductManufacturingBusinessOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'operationType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'authorisationReferenceNumber', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'effectiveDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'confidentialityIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufacturingBusinessOperation', 'regulator', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductManufacturingBusinessOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufacturingBusinessOperation', nil, 'MedicinalProductManufacturingBusinessOperation', js.FHIRFactoryJs);
  defineMedicinalProductManufacturingBusinessOperationPropsJs(js, def);
end;


procedure defineMedicinalProductSpecialDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'intendedUse', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'indicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'indicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductSpecialDesignation', 'species', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductSpecialDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductSpecialDesignation', nil, 'MedicinalProductSpecialDesignation', js.FHIRFactoryJs);
  defineMedicinalProductSpecialDesignationPropsJs(js, def);
end;


procedure defineMedicinalProductPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProduct', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'domain', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'combinedPharmaceuticalDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'additionalMonitoringIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'paediatricUseIndicator', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProduct', 'productClassification', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'pharmaceuticalProduct', 'Reference(MedicinalProductPharmaceutical)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'packagedMedicinalProduct', 'Reference(MedicinalProductPackaged)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'attachedDocument', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'masterFile', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'contact', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'clinicalTrial', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'name', 'MedicinalProductName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'crossReference', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'manufacturingBusinessOperation', 'MedicinalProductManufacturingBusinessOperation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProduct', 'specialDesignation', 'MedicinalProductSpecialDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProduct', nil, 'MedicinalProduct', js.FHIRFactoryJs);
  defineMedicinalProductPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'country', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationJurisdictionalAuthorization', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationJurisdictionalAuthorization', nil, 'MedicinalProductAuthorizationJurisdictionalAuthorization', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'datePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'dateDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorizationProcedure', 'application', '@MedicinalProductAuthorization.procedure', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductAuthorizationProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorizationProcedure', nil, 'MedicinalProductAuthorizationProcedure', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationProcedurePropsJs(js, def);
end;


procedure defineMedicinalProductAuthorizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductAuthorization', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'subject', 'Reference(MedicinalProduct)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'restoreDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'validityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dataExclusivityPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'dateOfFirstAuthorization', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'internationalBirthDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'legalBasis', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'jurisdictionalAuthorization', 'MedicinalProductAuthorizationJurisdictionalAuthorization', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'holder', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'regulator', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductAuthorization', 'procedure', 'MedicinalProductAuthorizationProcedure', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductAuthorizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductAuthorization', nil, 'MedicinalProductAuthorization', js.FHIRFactoryJs);
  defineMedicinalProductAuthorizationPropsJs(js, def);
end;


procedure defineMedicinalProductContraindicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy', 'therapyRelationshipType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindicationOtherTherapy', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductContraindicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductContraindicationOtherTherapy', nil, 'MedicinalProductContraindicationOtherTherapy', js.FHIRFactoryJs);
  defineMedicinalProductContraindicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductContraindicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductContraindication', 'subject', 'Reference(MedicinalProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'disease', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'therapeuticIndication', 'Reference(MedicinalProductIndication)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'otherTherapy', 'MedicinalProductContraindicationOtherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductContraindication', 'population', 'Population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductContraindicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductContraindication', nil, 'MedicinalProductContraindication', js.FHIRFactoryJs);
  defineMedicinalProductContraindicationPropsJs(js, def);
end;


procedure defineMedicinalProductIndicationOtherTherapyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy', 'therapyRelationshipType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy', 'medicationCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndicationOtherTherapy', 'medicationReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIndicationOtherTherapyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIndicationOtherTherapy', nil, 'MedicinalProductIndicationOtherTherapy', js.FHIRFactoryJs);
  defineMedicinalProductIndicationOtherTherapyPropsJs(js, def);
end;


procedure defineMedicinalProductIndicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIndication', 'subject', 'Reference(MedicinalProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication', 'diseaseSymptomProcedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication', 'diseaseStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication', 'comorbidity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication', 'intendedEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIndication', 'otherTherapy', 'MedicinalProductIndicationOtherTherapy', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication', 'undesirableEffect', 'Reference(MedicinalProductUndesirableEffect)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIndication', 'population', 'Population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIndicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIndication', nil, 'MedicinalProductIndication', js.FHIRFactoryJs);
  defineMedicinalProductIndicationPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'group', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'confidentiality', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstance', 'strength', 'MedicinalProductIngredientSpecifiedSubstanceStrength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstance', nil, 'MedicinalProductIngredientSpecifiedSubstance', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'presentation', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'presentationLowLimit', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'concentration', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'concentrationLowLimit', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'measurementPoint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrength', 'referenceStrength', 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrength', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrength', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'substance', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'strength', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'strengthLowLimit', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'measurementPoint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', 'country', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', nil, 'MedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrength', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthPropsJs(js, def);
end;


procedure defineMedicinalProductIngredientSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredientSubstance', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredientSubstance', 'strength', '@MedicinalProductIngredient.specifiedSubstance.strength', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductIngredientSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredientSubstance', nil, 'MedicinalProductIngredientSubstance', js.FHIRFactoryJs);
  defineMedicinalProductIngredientSubstancePropsJs(js, def);
end;


procedure defineMedicinalProductIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductIngredient', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'allergenicIndicator', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'specifiedSubstance', 'MedicinalProductIngredientSpecifiedSubstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductIngredient', 'substance', 'MedicinalProductIngredientSubstance', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductIngredient', nil, 'MedicinalProductIngredient', js.FHIRFactoryJs);
  defineMedicinalProductIngredientPropsJs(js, def);
end;


procedure defineMedicinalProductInteractionInteractantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductInteractionInteractant', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteractionInteractant', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductInteractionInteractantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductInteractionInteractant', nil, 'MedicinalProductInteractionInteractant', js.FHIRFactoryJs);
  defineMedicinalProductInteractionInteractantPropsJs(js, def);
end;


procedure defineMedicinalProductInteractionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductInteraction', 'subject', 'Reference(MedicinalProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'interactant', 'MedicinalProductInteractionInteractant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'effect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'incidence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductInteraction', 'management', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductInteractionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductInteraction', nil, 'MedicinalProductInteraction', js.FHIRFactoryJs);
  defineMedicinalProductInteractionPropsJs(js, def);
end;


procedure defineMedicinalProductManufacturedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductManufactured', 'manufacturedDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductManufactured', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductManufacturedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductManufactured', nil, 'MedicinalProductManufactured', js.FHIRFactoryJs);
  defineMedicinalProductManufacturedPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedBatchIdentifierPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier', 'outerPackaging', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedBatchIdentifier', 'immediatePackaging', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPackagedBatchIdentifierJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedBatchIdentifier', nil, 'MedicinalProductPackagedBatchIdentifier', js.FHIRFactoryJs);
  defineMedicinalProductPackagedBatchIdentifierPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPackageItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'material', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'alternateMaterial', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'device', 'Reference(DeviceDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturedItem', 'Reference(MedicinalProductManufactured)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'packageItem', '@MedicinalProductPackaged.packageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'physicalCharacteristics', 'ProdCharacteristic', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'otherCharacteristics', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'shelfLifeStorage', 'ProductShelfLife', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackagedPackageItem', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedPackageItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackagedPackageItem', nil, 'MedicinalProductPackagedPackageItem', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPackageItemPropsJs(js, def);
end;


procedure defineMedicinalProductPackagedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPackaged', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'subject', 'Reference(MedicinalProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'legalStatusOfSupply', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'marketingStatus', 'MarketingStatus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'marketingAuthorization', 'Reference(MedicinalProductAuthorization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'manufacturer', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'batchIdentifier', 'MedicinalProductPackagedBatchIdentifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPackaged', 'packageItem', 'MedicinalProductPackagedPackageItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPackagedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPackaged', nil, 'MedicinalProductPackaged', js.FHIRFactoryJs);
  defineMedicinalProductPackagedPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalCharacteristics', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMedicinalProductPharmaceuticalCharacteristicsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalCharacteristics', nil, 'MedicinalProductPharmaceuticalCharacteristics', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalCharacteristicsPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'firstDose', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'maxSingleDose', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'maxDosePerDay', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'maxDosePerTreatmentPeriod', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'maxTreatmentPeriod', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministration', 'targetSpecies', 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministration', nil, 'MedicinalProductPharmaceuticalRouteOfAdministration', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies', 'withdrawalPeriod', 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies', nil, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpecies', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', 'tissue', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', 'value', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', 'supportingInformation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', nil, 'MedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriod', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodPropsJs(js, def);
end;


procedure defineMedicinalProductPharmaceuticalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'administrableDoseForm', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'unitOfPresentation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'ingredient', 'Reference(MedicinalProductIngredient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'device', 'Reference(DeviceDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'characteristics', 'MedicinalProductPharmaceuticalCharacteristics', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductPharmaceutical', 'routeOfAdministration', 'MedicinalProductPharmaceuticalRouteOfAdministration', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductPharmaceuticalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductPharmaceutical', nil, 'MedicinalProductPharmaceutical', js.FHIRFactoryJs);
  defineMedicinalProductPharmaceuticalPropsJs(js, def);
end;


procedure defineMedicinalProductUndesirableEffectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MedicinalProductUndesirableEffect', 'subject', 'Reference(MedicinalProduct)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect', 'symptomConditionEffect', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect', 'classification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect', 'frequencyOfOccurrence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MedicinalProductUndesirableEffect', 'population', 'Population', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMedicinalProductUndesirableEffectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MedicinalProductUndesirableEffect', nil, 'MedicinalProductUndesirableEffect', js.FHIRFactoryJs);
  defineMedicinalProductUndesirableEffectPropsJs(js, def);
end;


procedure defineMessageDefinitionFocusPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionFocus', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'profile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'min', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionFocus', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionFocusJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionFocus', nil, 'MessageDefinitionFocus', js.FHIRFactoryJs);
  defineMessageDefinitionFocusPropsJs(js, def);
end;


procedure defineMessageDefinitionAllowedResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageDefinitionAllowedResponse', 'message', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinitionAllowedResponse', 'situation', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageDefinitionAllowedResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinitionAllowedResponse', nil, 'MessageDefinitionAllowedResponse', js.FHIRFactoryJs);
  defineMessageDefinitionAllowedResponsePropsJs(js, def);
end;


procedure defineMessageDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'MessageDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MessageDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'MessageDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'base', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'eventCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageDefinition', 'eventUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'focus', 'MessageDefinitionFocus', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageDefinition', 'responseRequired', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageDefinition', 'allowedResponse', 'MessageDefinitionAllowedResponse', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMessageDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageDefinition', nil, 'MessageDefinition', js.FHIRFactoryJs);
  defineMessageDefinitionPropsJs(js, def);
end;


procedure defineMessageHeaderDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderDestination', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'target', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderDestination', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderDestination', 'receiver', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderDestination', nil, 'MessageHeaderDestination', js.FHIRFactoryJs);
  defineMessageHeaderDestinationPropsJs(js, def);
end;


procedure defineMessageHeaderSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderSource', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'software', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderSource', 'contact', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeaderSource', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderSource', nil, 'MessageHeaderSource', js.FHIRFactoryJs);
  defineMessageHeaderSourcePropsJs(js, def);
end;


procedure defineMessageHeaderResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MessageHeaderResponse', 'identifier', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeaderResponse', 'details', 'Reference(OperationOutcome)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMessageHeaderResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeaderResponse', nil, 'MessageHeaderResponse', js.FHIRFactoryJs);
  defineMessageHeaderResponsePropsJs(js, def);
end;


procedure defineMessageHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MessageHeader', 'eventCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'eventUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MessageHeader', 'destination', 'MessageHeaderDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'sender', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'enterer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'author', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'source', 'MessageHeaderSource', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'responsible', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'reason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'response', 'MessageHeaderResponse', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MessageHeader', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MessageHeader', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMessageHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MessageHeader', nil, 'MessageHeader', js.FHIRFactoryJs);
  defineMessageHeaderPropsJs(js, def);
end;


procedure defineMolecularSequenceReferenceSeqPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'chromosome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'genomeBuild', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'orientation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'referenceSeqId', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'referenceSeqPointer', 'Reference(MolecularSequence)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'referenceSeqString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'strand', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'windowStart', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceReferenceSeq', 'windowEnd', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceReferenceSeqJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceReferenceSeq', nil, 'MolecularSequenceReferenceSeq', js.FHIRFactoryJs);
  defineMolecularSequenceReferenceSeqPropsJs(js, def);
end;


procedure defineMolecularSequenceVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceVariant', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceVariant', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceVariant', 'observedAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant', 'referenceAllele', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant', 'cigar', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceVariant', 'variantPointer', 'Reference(Observation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceVariant', nil, 'MolecularSequenceVariant', js.FHIRFactoryJs);
  defineMolecularSequenceVariantPropsJs(js, def);
end;


procedure defineMolecularSequenceQualityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceQuality', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'standardSequence', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'score', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'truthTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'queryTP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'truthFN', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'queryFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'gtFP', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'precision', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'recall', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'fScore', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'MolecularSequenceQuality', 'roc', 'MolecularSequenceQualityRoc', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceQualityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceQuality', nil, 'MolecularSequenceQuality', js.FHIRFactoryJs);
  defineMolecularSequenceQualityPropsJs(js, def);
end;


procedure defineMolecularSequenceQualityRocPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
end;

procedure defineMolecularSequenceQualityRocJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceQualityRoc', nil, 'MolecularSequenceQualityRoc', js.FHIRFactoryJs);
  defineMolecularSequenceQualityRocPropsJs(js, def);
end;


procedure defineMolecularSequenceRepositoryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceRepository', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository', 'datasetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository', 'variantsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequenceRepository', 'readsetId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineMolecularSequenceRepositoryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceRepository', nil, 'MolecularSequenceRepository', js.FHIRFactoryJs);
  defineMolecularSequenceRepositoryPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariant', 'variantType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant', 'exact', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant', 'length', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant', 'outer', 'MolecularSequenceStructureVariantOuter', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequenceStructureVariant', 'inner', 'MolecularSequenceStructureVariantInner', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineMolecularSequenceStructureVariantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariant', nil, 'MolecularSequenceStructureVariant', js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantOuterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariantOuter', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariantOuter', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceStructureVariantOuterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariantOuter', nil, 'MolecularSequenceStructureVariantOuter', js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantOuterPropsJs(js, def);
end;


procedure defineMolecularSequenceStructureVariantInnerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'MolecularSequenceStructureVariantInner', 'start', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequenceStructureVariantInner', 'end', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineMolecularSequenceStructureVariantInnerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequenceStructureVariantInner', nil, 'MolecularSequenceStructureVariantInner', js.FHIRFactoryJs);
  defineMolecularSequenceStructureVariantInnerPropsJs(js, def);
end;


procedure defineMolecularSequencePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'MolecularSequence', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequence', 'coordinateSystem', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequence', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'performer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'referenceSeq', 'MolecularSequenceReferenceSeq', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'MolecularSequence', 'variant', 'MolecularSequenceVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence', 'observedSeq', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'MolecularSequence', 'quality', 'MolecularSequenceQuality', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence', 'readCoverage', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'MolecularSequence', 'repository', 'MolecularSequenceRepository', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence', 'pointer', 'Reference(MolecularSequence)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'MolecularSequence', 'structureVariant', 'MolecularSequenceStructureVariant', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineMolecularSequenceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('MolecularSequence', nil, 'MolecularSequence', js.FHIRFactoryJs);
  defineMolecularSequencePropsJs(js, def);
end;


procedure defineNamingSystemUniqueIdPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NamingSystemUniqueId', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystemUniqueId', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNamingSystemUniqueIdJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystemUniqueId', nil, 'NamingSystemUniqueId', js.FHIRFactoryJs);
  defineNamingSystemUniqueIdPropsJs(js, def);
end;


procedure defineNamingSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'NamingSystem', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NamingSystem', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'responsible', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NamingSystem', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NamingSystem', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NamingSystem', 'uniqueId', 'NamingSystemUniqueId', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNamingSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NamingSystem', nil, 'NamingSystem', js.FHIRFactoryJs);
  defineNamingSystemPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDiet', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'nutrient', 'NutritionOrderOralDietNutrient', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'texture', 'NutritionOrderOralDietTexture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'fluidConsistencyType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderOralDiet', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderOralDietJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDiet', nil, 'NutritionOrderOralDiet', js.FHIRFactoryJs);
  defineNutritionOrderOralDietPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietNutrientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietNutrient', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietNutrient', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietNutrientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietNutrient', nil, 'NutritionOrderOralDietNutrient', js.FHIRFactoryJs);
  defineNutritionOrderOralDietNutrientPropsJs(js, def);
end;


procedure defineNutritionOrderOralDietTexturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderOralDietTexture', 'modifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderOralDietTexture', 'foodType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderOralDietTextureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderOralDietTexture', nil, 'NutritionOrderOralDietTexture', js.FHIRFactoryJs);
  defineNutritionOrderOralDietTexturePropsJs(js, def);
end;


procedure defineNutritionOrderSupplementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderSupplement', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'productName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'schedule', 'Timing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderSupplement', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderSupplementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderSupplement', nil, 'NutritionOrderSupplement', js.FHIRFactoryJs);
  defineNutritionOrderSupplementPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'baseFormulaType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'baseFormulaProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'additiveType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'additiveProductName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'caloricDensity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'routeofAdministration', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'administration', 'NutritionOrderEnteralFormulaAdministration', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'maxVolumeToDeliver', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormula', 'administrationInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineNutritionOrderEnteralFormulaJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormula', nil, 'NutritionOrderEnteralFormula', js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaPropsJs(js, def);
end;


procedure defineNutritionOrderEnteralFormulaAdministrationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'schedule', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'rateQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrderEnteralFormulaAdministration', 'rateRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineNutritionOrderEnteralFormulaAdministrationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrderEnteralFormulaAdministration', nil, 'NutritionOrderEnteralFormulaAdministration', js.FHIRFactoryJs);
  defineNutritionOrderEnteralFormulaAdministrationPropsJs(js, def);
end;


procedure defineNutritionOrderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'NutritionOrder', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'NutritionOrder', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'dateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'NutritionOrder', 'orderer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'allergyIntolerance', 'Reference(AllergyIntolerance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'foodPreferenceModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'excludeFoodModifier', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'oralDiet', 'NutritionOrderOralDiet', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'supplement', 'NutritionOrderSupplement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'NutritionOrder', 'enteralFormula', 'NutritionOrderEnteralFormula', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'NutritionOrder', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineNutritionOrderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('NutritionOrder', nil, 'NutritionOrder', js.FHIRFactoryJs);
  defineNutritionOrderPropsJs(js, def);
end;


procedure defineObservationReferenceRangePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationReferenceRange', 'low', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'high', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationReferenceRange', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationReferenceRange', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationReferenceRangeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationReferenceRange', nil, 'ObservationReferenceRange', js.FHIRFactoryJs);
  defineObservationReferenceRangePropsJs(js, def);
end;


procedure defineObservationComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationComponent', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationComponent', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ObservationComponent', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationComponent', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ObservationComponent', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationComponent', 'interpretation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationComponent', 'referenceRange', '@Observation.referenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationComponent', nil, 'ObservationComponent', js.FHIRFactoryJs);
  defineObservationComponentPropsJs(js, def);
end;


procedure defineObservationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Observation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'partOf', 'Reference(MedicationAdministration)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'focus', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'effectiveInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'issued', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Observation', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Observation', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Observation', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Observation', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'dataAbsentReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'interpretation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'specimen', 'Reference(Specimen)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'device', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Observation', 'referenceRange', 'ObservationReferenceRange', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'hasMember', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'derivedFrom', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Observation', 'component', 'ObservationComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineObservationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Observation', nil, 'Observation', js.FHIRFactoryJs);
  defineObservationPropsJs(js, def);
end;


procedure defineObservationDefinitionQuantitativeDetailsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'customaryUnit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'unit', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'conversionFactor', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ObservationDefinitionQuantitativeDetails', 'decimalPrecision', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineObservationDefinitionQuantitativeDetailsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQuantitativeDetails', nil, 'ObservationDefinitionQuantitativeDetails', js.FHIRFactoryJs);
  defineObservationDefinitionQuantitativeDetailsPropsJs(js, def);
end;


procedure defineObservationDefinitionQualifiedIntervalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'category', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'range', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'context', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'appliesTo', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'age', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'gestationalAge', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinitionQualifiedInterval', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineObservationDefinitionQualifiedIntervalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinitionQualifiedInterval', nil, 'ObservationDefinitionQualifiedInterval', js.FHIRFactoryJs);
  defineObservationDefinitionQualifiedIntervalPropsJs(js, def);
end;


procedure defineObservationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ObservationDefinition', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'multipleResultsAllowed', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ObservationDefinition', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'preferredReportName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ObservationDefinition', 'quantitativeDetails', 'ObservationDefinitionQuantitativeDetails', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'qualifiedInterval', 'ObservationDefinitionQualifiedInterval', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ObservationDefinition', 'validCodedValueSet', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'normalCodedValueSet', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'abnormalCodedValueSet', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ObservationDefinition', 'criticalCodedValueSet', 'Reference(ValueSet)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineObservationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ObservationDefinition', nil, 'ObservationDefinition', js.FHIRFactoryJs);
  defineObservationDefinitionPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameter', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'use', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'searchType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'binding', 'OperationDefinitionParameterBinding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'referencedFrom', 'OperationDefinitionParameterReferencedFrom', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinitionParameter', 'part', '@OperationDefinition.parameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameter', nil, 'OperationDefinitionParameter', js.FHIRFactoryJs);
  defineOperationDefinitionParameterPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterBindingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'strength', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterBinding', 'valueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionParameterBindingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterBinding', nil, 'OperationDefinitionParameterBinding', js.FHIRFactoryJs);
  defineOperationDefinitionParameterBindingPropsJs(js, def);
end;


procedure defineOperationDefinitionParameterReferencedFromPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionParameterReferencedFrom', 'source', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinitionParameterReferencedFrom', 'sourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionParameterReferencedFromJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionParameterReferencedFrom', nil, 'OperationDefinitionParameterReferencedFrom', js.FHIRFactoryJs);
  defineOperationDefinitionParameterReferencedFromPropsJs(js, def);
end;


procedure defineOperationDefinitionOverloadPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationDefinitionOverload', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationDefinitionOverloadJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinitionOverload', nil, 'OperationDefinitionOverload', js.FHIRFactoryJs);
  defineOperationDefinitionOverloadPropsJs(js, def);
end;


procedure defineOperationDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'OperationDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'OperationDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'affectsState', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'comment', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'base', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'system', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'type', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'instance', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OperationDefinition', 'inputProfile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'outputProfile', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationDefinition', 'parameter', 'OperationDefinitionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OperationDefinition', 'overload', 'OperationDefinitionOverload', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationDefinition', nil, 'OperationDefinition', js.FHIRFactoryJs);
  defineOperationDefinitionPropsJs(js, def);
end;


procedure defineOperationOutcomeIssuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OperationOutcomeIssue', 'severity', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'details', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OperationOutcomeIssue', 'diagnostics', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineOperationOutcomeIssueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcomeIssue', nil, 'OperationOutcomeIssue', js.FHIRFactoryJs);
  defineOperationOutcomeIssuePropsJs(js, def);
end;


procedure defineOperationOutcomePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OperationOutcome', 'issue', 'OperationOutcomeIssue', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOperationOutcomeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OperationOutcome', nil, 'OperationOutcome', js.FHIRFactoryJs);
  defineOperationOutcomePropsJs(js, def);
end;


procedure defineOrganizationContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'OrganizationContact', 'purpose', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationContact', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationContact', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineOrganizationContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationContact', nil, 'OrganizationContact', js.FHIRFactoryJs);
  defineOrganizationContactPropsJs(js, def);
end;


procedure defineOrganizationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Organization', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Organization', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Organization', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'partOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Organization', 'contact', 'OrganizationContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Organization', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Organization', nil, 'Organization', js.FHIRFactoryJs);
  defineOrganizationPropsJs(js, def);
end;


procedure defineOrganizationAffiliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'OrganizationAffiliation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'OrganizationAffiliation', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation', 'participatingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'OrganizationAffiliation', 'network', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'OrganizationAffiliation', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineOrganizationAffiliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('OrganizationAffiliation', nil, 'OrganizationAffiliation', js.FHIRFactoryJs);
  defineOrganizationAffiliationPropsJs(js, def);
end;


procedure definePatientContactPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientContact', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact', 'name', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PatientContact', 'address', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PatientContact', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientContact', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePatientContactJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientContact', nil, 'PatientContact', js.FHIRFactoryJs);
  definePatientContactPropsJs(js, def);
end;


procedure definePatientCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientCommunication', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientCommunication', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure definePatientCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientCommunication', nil, 'PatientCommunication', js.FHIRFactoryJs);
  definePatientCommunicationPropsJs(js, def);
end;


procedure definePatientLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PatientLink', 'other', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PatientLink', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePatientLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PatientLink', nil, 'PatientLink', js.FHIRFactoryJs);
  definePatientLinkPropsJs(js, def);
end;


procedure definePatientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Patient', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Patient', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient', 'deceasedBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'deceasedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Patient', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'maritalStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'multipleBirthBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Patient', 'multipleBirthInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'Patient', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'contact', 'PatientContact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'communication', 'PatientCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'generalPractitioner', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Patient', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Patient', 'link', 'PatientLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePatientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Patient', nil, 'Patient', js.FHIRFactoryJs);
  definePatientPropsJs(js, def);
end;


procedure definePaymentNoticePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentNotice', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentNotice', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentNotice', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'provider', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'payment', 'Reference(PaymentReconciliation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'paymentDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentNotice', 'payee', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'recipient', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentNotice', 'paymentStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentNoticeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentNotice', nil, 'PaymentNotice', js.FHIRFactoryJs);
  definePaymentNoticePropsJs(js, def);
end;


procedure definePaymentReconciliationDetailPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationDetail', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'predecessor', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'request', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'submitter', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'response', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'responsible', 'Reference(PractitionerRole)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'payee', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliationDetail', 'amount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePaymentReconciliationDetailJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationDetail', nil, 'PaymentReconciliationDetail', js.FHIRFactoryJs);
  definePaymentReconciliationDetailPropsJs(js, def);
end;


procedure definePaymentReconciliationProcessNotePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliationProcessNote', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliationProcessNote', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePaymentReconciliationProcessNoteJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliationProcessNote', nil, 'PaymentReconciliationProcessNote', js.FHIRFactoryJs);
  definePaymentReconciliationProcessNotePropsJs(js, def);
end;


procedure definePaymentReconciliationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PaymentReconciliation', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation', 'paymentIssuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'request', 'Reference(Task)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'requestor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'outcome', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'disposition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PaymentReconciliation', 'paymentDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PaymentReconciliation', 'paymentAmount', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'paymentIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'detail', 'PaymentReconciliationDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PaymentReconciliation', 'formCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PaymentReconciliation', 'processNote', 'PaymentReconciliationProcessNote', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePaymentReconciliationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PaymentReconciliation', nil, 'PaymentReconciliation', js.FHIRFactoryJs);
  definePaymentReconciliationPropsJs(js, def);
end;


procedure definePersonLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PersonLink', 'target', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PersonLink', 'assurance', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePersonLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PersonLink', nil, 'PersonLink', js.FHIRFactoryJs);
  definePersonLinkPropsJs(js, def);
end;


procedure definePersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Person', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Person', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Person', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Person', 'photo', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person', 'managingOrganization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Person', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Person', 'link', 'PersonLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Person', nil, 'Person', js.FHIRFactoryJs);
  definePersonPropsJs(js, def);
end;


procedure definePlanDefinitionGoalPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoal', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'description', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'priority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'start', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'addresses', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionGoal', 'target', 'PlanDefinitionGoalTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionGoalJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoal', nil, 'PlanDefinitionGoal', js.FHIRFactoryJs);
  definePlanDefinitionGoalPropsJs(js, def);
end;


procedure definePlanDefinitionGoalTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'measure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'detailCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionGoalTarget', 'due', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionGoalTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionGoalTarget', nil, 'PlanDefinitionGoalTarget', js.FHIRFactoryJs);
  definePlanDefinitionGoalTargetPropsJs(js, def);
end;


procedure definePlanDefinitionActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionAction', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'trigger', 'TriggerDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'condition', 'PlanDefinitionActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'input', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'output', 'DataRequirement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'relatedAction', 'PlanDefinitionActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'participant', 'PlanDefinitionActionParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionAction', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'definitionCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'definitionUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'transform', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionAction', 'dynamicValue', 'PlanDefinitionActionDynamicValue', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinitionAction', 'action', '@PlanDefinition.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionAction', nil, 'PlanDefinitionAction', js.FHIRFactoryJs);
  definePlanDefinitionActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionCondition', 'expression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionCondition', nil, 'PlanDefinitionActionCondition', js.FHIRFactoryJs);
  definePlanDefinitionActionConditionPropsJs(js, def);
end;


procedure definePlanDefinitionActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinitionActionRelatedAction', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionRelatedAction', nil, 'PlanDefinitionActionRelatedAction', js.FHIRFactoryJs);
  definePlanDefinitionActionRelatedActionPropsJs(js, def);
end;


procedure definePlanDefinitionActionParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionParticipant', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionParticipant', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionParticipant', nil, 'PlanDefinitionActionParticipant', js.FHIRFactoryJs);
  definePlanDefinitionActionParticipantPropsJs(js, def);
end;


procedure definePlanDefinitionActionDynamicValuePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinitionActionDynamicValue', 'expression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePlanDefinitionActionDynamicValueJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinitionActionDynamicValue', nil, 'PlanDefinitionActionDynamicValue', js.FHIRFactoryJs);
  definePlanDefinitionActionDynamicValuePropsJs(js, def);
end;


procedure definePlanDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'PlanDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PlanDefinition', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PlanDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'PlanDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PlanDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'goal', 'PlanDefinitionGoal', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PlanDefinition', 'action', 'PlanDefinitionAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePlanDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PlanDefinition', nil, 'PlanDefinition', js.FHIRFactoryJs);
  definePlanDefinitionPropsJs(js, def);
end;


procedure definePractitionerQualificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerQualification', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerQualification', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerQualification', 'issuer', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerQualificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerQualification', nil, 'PractitionerQualification', js.FHIRFactoryJs);
  definePractitionerQualificationPropsJs(js, def);
end;


procedure definePractitionerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Practitioner', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Practitioner', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Practitioner', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Practitioner', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'qualification', 'PractitionerQualification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Practitioner', 'communication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Practitioner', nil, 'Practitioner', js.FHIRFactoryJs);
  definePractitionerPropsJs(js, def);
end;


procedure definePractitionerRoleAvailableTimePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'allDay', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'availableStartTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleAvailableTime', 'availableEndTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure definePractitionerRoleAvailableTimeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleAvailableTime', nil, 'PractitionerRoleAvailableTime', js.FHIRFactoryJs);
  definePractitionerRoleAvailableTimePropsJs(js, def);
end;


procedure definePractitionerRoleNotAvailablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'PractitionerRoleNotAvailable', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRoleNotAvailable', 'during', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure definePractitionerRoleNotAvailableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRoleNotAvailable', nil, 'PractitionerRoleNotAvailable', js.FHIRFactoryJs);
  definePractitionerRoleNotAvailablePropsJs(js, def);
end;


procedure definePractitionerRolePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'PractitionerRole', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'PractitionerRole', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'practitioner', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'PractitionerRole', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'location', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'healthcareService', 'Reference(HealthcareService)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'availableTime', 'PractitionerRoleAvailableTime', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'notAvailable', 'PractitionerRoleNotAvailable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'PractitionerRole', 'availabilityExceptions', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'PractitionerRole', 'endpoint', 'Reference(Endpoint)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure definePractitionerRoleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('PractitionerRole', nil, 'PractitionerRole', js.FHIRFactoryJs);
  definePractitionerRolePropsJs(js, def);
end;


procedure defineProcedurePerformerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedurePerformer', 'function', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'actor', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedurePerformer', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedurePerformerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedurePerformer', nil, 'ProcedurePerformer', js.FHIRFactoryJs);
  defineProcedurePerformerPropsJs(js, def);
end;


procedure defineProcedureFocalDevicePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProcedureFocalDevice', 'action', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProcedureFocalDevice', 'manipulated', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProcedureFocalDeviceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProcedureFocalDevice', nil, 'ProcedureFocalDevice', js.FHIRFactoryJs);
  defineProcedureFocalDevicePropsJs(js, def);
end;


procedure defineProcedurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Procedure', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'partOf', 'Reference(Procedure)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Procedure', 'performedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Procedure', 'performedAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performedRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'recorder', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'asserter', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'performer', 'ProcedurePerformer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Procedure', 'report', 'Reference(DiagnosticReport)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complication', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'complicationDetail', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'followUp', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'focalDevice', 'ProcedureFocalDevice', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedReference', 'Reference(Device)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Procedure', 'usedCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProcedureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Procedure', nil, 'Procedure', js.FHIRFactoryJs);
  defineProcedurePropsJs(js, def);
end;


procedure defineProvenanceAgentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceAgent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'role', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ProvenanceAgent', 'who', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceAgent', 'onBehalfOf', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineProvenanceAgentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceAgent', nil, 'ProvenanceAgent', js.FHIRFactoryJs);
  defineProvenanceAgentPropsJs(js, def);
end;


procedure defineProvenanceEntityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ProvenanceEntity', 'role', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ProvenanceEntity', 'what', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ProvenanceEntity', 'agent', '@Provenance.agent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceEntityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ProvenanceEntity', nil, 'ProvenanceEntity', js.FHIRFactoryJs);
  defineProvenanceEntityPropsJs(js, def);
end;


procedure defineProvenancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Provenance', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'occurredPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'occurredDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance', 'recorded', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Provenance', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'reason', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'activity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Provenance', 'agent', 'ProvenanceAgent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'entity', 'ProvenanceEntity', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Provenance', 'signature', 'Signature', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineProvenanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Provenance', nil, 'Provenance', js.FHIRFactoryJs);
  defineProvenancePropsJs(js, def);
end;


procedure defineQuestionnaireItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItem', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'enableWhen', 'QuestionnaireItemEnableWhen', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'enableBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'repeats', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'readOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItem', 'maxLength', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItem', 'answerValueSet', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItem', 'answerOption', 'QuestionnaireItemAnswerOption', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'initial', 'QuestionnaireItemInitial', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireItem', 'item', '@Questionnaire.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItem', nil, 'QuestionnaireItem', js.FHIRFactoryJs);
  defineQuestionnaireItemPropsJs(js, def);
end;


procedure defineQuestionnaireItemEnableWhenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'question', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemEnableWhen', 'answerReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemEnableWhenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemEnableWhen', nil, 'QuestionnaireItemEnableWhen', js.FHIRFactoryJs);
  defineQuestionnaireItemEnableWhenPropsJs(js, def);
end;


procedure defineQuestionnaireItemAnswerOptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemAnswerOption', 'initialSelected', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineQuestionnaireItemAnswerOptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemAnswerOption', nil, 'QuestionnaireItemAnswerOption', js.FHIRFactoryJs);
  defineQuestionnaireItemAnswerOptionPropsJs(js, def);
end;


procedure defineQuestionnaireItemInitialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireItemInitial', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineQuestionnaireItemInitialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireItemInitial', nil, 'QuestionnaireItemInitial', js.FHIRFactoryJs);
  defineQuestionnaireItemInitialPropsJs(js, def);
end;


procedure defineQuestionnairePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'Questionnaire', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Questionnaire', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Questionnaire', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Questionnaire', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Questionnaire', 'code', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Questionnaire', 'item', 'QuestionnaireItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Questionnaire', nil, 'Questionnaire', js.FHIRFactoryJs);
  defineQuestionnairePropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItem', 'linkId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'definition', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'text', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'answer', 'QuestionnaireResponseItemAnswer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponseItem', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItem', nil, 'QuestionnaireResponseItem', js.FHIRFactoryJs);
  defineQuestionnaireResponseItemPropsJs(js, def);
end;


procedure defineQuestionnaireResponseItemAnswerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponseItemAnswer', 'item', '@QuestionnaireResponse.item', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseItemAnswerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponseItemAnswer', nil, 'QuestionnaireResponseItemAnswer', js.FHIRFactoryJs);
  defineQuestionnaireResponseItemAnswerPropsJs(js, def);
end;


procedure defineQuestionnaireResponsePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'QuestionnaireResponse', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'partOf', 'Reference(Observation)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'QuestionnaireResponse', 'questionnaire', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'QuestionnaireResponse', 'subject', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'authored', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'QuestionnaireResponse', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'source', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'QuestionnaireResponse', 'item', 'QuestionnaireResponseItem', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineQuestionnaireResponseJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('QuestionnaireResponse', nil, 'QuestionnaireResponse', js.FHIRFactoryJs);
  defineQuestionnaireResponsePropsJs(js, def);
end;


procedure defineRelatedPersonCommunicationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RelatedPersonCommunication', 'language', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPersonCommunication', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineRelatedPersonCommunicationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPersonCommunication', nil, 'RelatedPersonCommunication', js.FHIRFactoryJs);
  defineRelatedPersonCommunicationPropsJs(js, def);
end;


procedure defineRelatedPersonPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RelatedPerson', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'RelatedPerson', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson', 'relationship', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'name', 'HumanName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'telecom', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'gender', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RelatedPerson', 'birthDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RelatedPerson', 'address', 'Address', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'photo', 'Attachment', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RelatedPerson', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RelatedPerson', 'communication', 'RelatedPersonCommunication', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRelatedPersonJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RelatedPerson', nil, 'RelatedPerson', js.FHIRFactoryJs);
  defineRelatedPersonPropsJs(js, def);
end;


procedure defineRequestGroupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupAction', 'prefix', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'textEquivalent', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'code', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'documentation', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'condition', 'RequestGroupActionCondition', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'relatedAction', 'RequestGroupActionRelatedAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'timingDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroupAction', 'timingAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'timingTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'participant', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroupAction', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'groupingBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'selectionBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'requiredBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'precheckBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'cardinalityBehavior', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupAction', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupAction', 'action', '@RequestGroup.action', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupAction', nil, 'RequestGroupAction', js.FHIRFactoryJs);
  defineRequestGroupActionPropsJs(js, def);
end;


procedure defineRequestGroupActionConditionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionCondition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionCondition', 'expression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionConditionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionCondition', nil, 'RequestGroupActionCondition', js.FHIRFactoryJs);
  defineRequestGroupActionConditionPropsJs(js, def);
end;


procedure defineRequestGroupActionRelatedActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'actionId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'relationship', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'offsetDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroupActionRelatedAction', 'offsetRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineRequestGroupActionRelatedActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroupActionRelatedAction', nil, 'RequestGroupActionRelatedAction', js.FHIRFactoryJs);
  defineRequestGroupActionRelatedActionPropsJs(js, def);
end;


procedure defineRequestGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RequestGroup', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'replaces', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RequestGroup', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RequestGroup', 'author', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RequestGroup', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RequestGroup', 'action', 'RequestGroupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRequestGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RequestGroup', nil, 'RequestGroup', js.FHIRFactoryJs);
  defineRequestGroupPropsJs(js, def);
end;


procedure defineResearchDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'shortTitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchDefinition', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchDefinition', 'population', 'Reference(ResearchElementDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'exposure', 'Reference(ResearchElementDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'exposureAlternative', 'Reference(ResearchElementDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchDefinition', 'outcome', 'Reference(ResearchElementDefinition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchDefinition', nil, 'ResearchDefinition', js.FHIRFactoryJs);
  defineResearchDefinitionPropsJs(js, def);
end;


procedure defineResearchElementDefinitionCharacteristicPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'definitionCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'definitionCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'definitionExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'definitionDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'usageContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'exclude', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'unitOfMeasure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveTimeFromStart', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'studyEffectiveGroupMeasure', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveTimeFromStart', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinitionCharacteristic', 'participantEffectiveGroupMeasure', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchElementDefinitionCharacteristicJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchElementDefinitionCharacteristic', nil, 'ResearchElementDefinitionCharacteristic', js.FHIRFactoryJs);
  defineResearchElementDefinitionCharacteristicPropsJs(js, def);
end;


procedure defineResearchElementDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchElementDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'shortTitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'subtitle', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ResearchElementDefinition', 'subjectCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition', 'subjectReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'usage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ResearchElementDefinition', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchElementDefinition', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchElementDefinition', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'variableType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchElementDefinition', 'characteristic', 'ResearchElementDefinitionCharacteristic', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchElementDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchElementDefinition', nil, 'ResearchElementDefinition', js.FHIRFactoryJs);
  defineResearchElementDefinitionPropsJs(js, def);
end;


procedure defineResearchStudyArmPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyArm', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyArm', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudyArm', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineResearchStudyArmJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyArm', nil, 'ResearchStudyArm', js.FHIRFactoryJs);
  defineResearchStudyArmPropsJs(js, def);
end;


procedure defineResearchStudyObjectivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ResearchStudyObjective', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudyObjective', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchStudyObjectiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudyObjective', nil, 'ResearchStudyObjective', js.FHIRFactoryJs);
  defineResearchStudyObjectivePropsJs(js, def);
end;


procedure defineResearchStudyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchStudy', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'protocol', 'Reference(PlanDefinition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'partOf', 'Reference(ResearchStudy)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'primaryPurposeType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'phase', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'focus', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'condition', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'keyword', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'location', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchStudy', 'enrollment', 'Reference(Group)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'sponsor', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'principalInvestigator', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'site', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'reasonStopped', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchStudy', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'arm', 'ResearchStudyArm', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchStudy', 'objective', 'ResearchStudyObjective', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineResearchStudyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchStudy', nil, 'ResearchStudy', js.FHIRFactoryJs);
  defineResearchStudyPropsJs(js, def);
end;


procedure defineResearchSubjectPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ResearchSubject', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ResearchSubject', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'study', 'Reference(ResearchStudy)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'individual', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ResearchSubject', 'assignedArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'actualArm', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ResearchSubject', 'consent', 'Reference(Consent)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineResearchSubjectJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ResearchSubject', nil, 'ResearchSubject', js.FHIRFactoryJs);
  defineResearchSubjectPropsJs(js, def);
end;


procedure defineRiskAssessmentPredictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskAssessmentPrediction', 'outcome', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'probabilityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'qualitativeRisk', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'relativeRisk', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'whenPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'whenRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessmentPrediction', 'rationale', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineRiskAssessmentPredictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessmentPrediction', nil, 'RiskAssessmentPrediction', js.FHIRFactoryJs);
  defineRiskAssessmentPredictionPropsJs(js, def);
end;


procedure defineRiskAssessmentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'RiskAssessment', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'basedOn', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'parent', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskAssessment', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'condition', 'Reference(Condition)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'performer', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskAssessment', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'basis', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'prediction', 'RiskAssessmentPrediction', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskAssessment', 'mitigation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskAssessment', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskAssessmentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskAssessment', nil, 'RiskAssessment', js.FHIRFactoryJs);
  defineRiskAssessmentPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisSampleSizePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize', 'numberOfStudies', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisSampleSize', 'numberOfParticipants', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
end;

procedure defineRiskEvidenceSynthesisSampleSizeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisSampleSize', nil, 'RiskEvidenceSynthesisSampleSize', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisSampleSizePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisRiskEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'value', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'unitOfMeasure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'denominatorCount', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'numeratorCount', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimate', 'precisionEstimate', 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisRiskEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisRiskEstimate', nil, 'RiskEvidenceSynthesisRiskEstimate', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisRiskEstimatePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimatePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', 'level', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', 'from', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', 'to', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimateJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', nil, 'RiskEvidenceSynthesisRiskEstimatePrecisionEstimate', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimatePropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisCertaintyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty', 'rating', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertainty', 'certaintySubcomponent', 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisCertaintyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisCertainty', nil, 'RiskEvidenceSynthesisCertainty', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisCertaintyPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent', 'rating', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesisCertaintyCertaintySubcomponent', nil, 'RiskEvidenceSynthesisCertaintyCertaintySubcomponent', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentPropsJs(js, def);
end;


procedure defineRiskEvidenceSynthesisPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'approvalDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'lastReviewDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'effectivePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'topic', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'author', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'editor', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'reviewer', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'endorser', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'relatedArtifact', 'RelatedArtifact', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'synthesisType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'studyType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'population', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'exposure', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'outcome', 'Reference(EvidenceVariable)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'sampleSize', 'RiskEvidenceSynthesisSampleSize', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'riskEstimate', 'RiskEvidenceSynthesisRiskEstimate', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'RiskEvidenceSynthesis', 'certainty', 'RiskEvidenceSynthesisCertainty', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineRiskEvidenceSynthesisJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('RiskEvidenceSynthesis', nil, 'RiskEvidenceSynthesis', js.FHIRFactoryJs);
  defineRiskEvidenceSynthesisPropsJs(js, def);
end;


procedure defineSchedulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Schedule', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'active', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Schedule', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'actor', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Schedule', 'planningHorizon', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Schedule', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineScheduleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Schedule', nil, 'Schedule', js.FHIRFactoryJs);
  defineSchedulePropsJs(js, def);
end;


procedure defineSearchParameterComponentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SearchParameterComponent', 'definition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameterComponent', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSearchParameterComponentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameterComponent', nil, 'SearchParameterComponent', js.FHIRFactoryJs);
  defineSearchParameterComponentPropsJs(js, def);
end;


procedure defineSearchParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'SearchParameter', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'derivedFrom', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SearchParameter', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SearchParameter', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'xpathUsage', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SearchParameter', 'multipleOr', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'multipleAnd', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SearchParameter', 'component', 'SearchParameterComponent', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSearchParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SearchParameter', nil, 'SearchParameter', js.FHIRFactoryJs);
  defineSearchParameterPropsJs(js, def);
end;


procedure defineServiceRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'ServiceRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'basedOn', 'Reference(CarePlan)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'replaces', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'requisition', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'doNotPerform', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'orderDetail', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'quantityQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'quantityRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'quantityRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ServiceRequest', 'asNeededCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ServiceRequest', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performerType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ServiceRequest', 'performer', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'locationCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'locationReference', 'Reference(Location)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'supportingInfo', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'specimen', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'bodySite', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ServiceRequest', 'patientInstruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ServiceRequest', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineServiceRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ServiceRequest', nil, 'ServiceRequest', js.FHIRFactoryJs);
  defineServiceRequestPropsJs(js, def);
end;


procedure defineSlotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Slot', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'serviceCategory', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'serviceType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'specialty', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Slot', 'appointmentType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'schedule', 'Reference(Schedule)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Slot', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Slot', 'start', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Slot', 'overbooked', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'Slot', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSlotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Slot', nil, 'Slot', js.FHIRFactoryJs);
  defineSlotPropsJs(js, def);
end;


procedure defineSpecimenCollectionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenCollection', 'collector', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'collectedDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenCollection', 'collectedPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'duration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'bodySite', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'fastingStatusCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenCollection', 'fastingStatusDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenCollectionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenCollection', nil, 'SpecimenCollection', js.FHIRFactoryJs);
  defineSpecimenCollectionPropsJs(js, def);
end;


procedure defineSpecimenProcessingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenProcessing', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenProcessing', 'procedure', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenProcessing', 'additive', 'Reference(Substance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenProcessing', 'timeDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SpecimenProcessing', 'timePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenProcessingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenProcessing', nil, 'SpecimenProcessing', js.FHIRFactoryJs);
  defineSpecimenProcessingPropsJs(js, def);
end;


procedure defineSpecimenContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenContainer', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenContainer', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenContainer', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'capacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'specimenQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenContainer', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenContainer', nil, 'SpecimenContainer', js.FHIRFactoryJs);
  defineSpecimenContainerPropsJs(js, def);
end;


procedure defineSpecimenPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Specimen', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'accessionIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Specimen', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'subject', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'receivedTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Specimen', 'parent', 'Reference(Specimen)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'request', 'Reference(ServiceRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'collection', 'SpecimenCollection', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Specimen', 'processing', 'SpecimenProcessing', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'container', 'SpecimenContainer', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'condition', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Specimen', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Specimen', nil, 'Specimen', js.FHIRFactoryJs);
  defineSpecimenPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'isDerived', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'preference', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'container', 'SpecimenDefinitionTypeTestedContainer', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'requirement', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'retentionTime', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'rejectionCriterion', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTested', 'handling', 'SpecimenDefinitionTypeTestedHandling', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionTypeTestedJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTested', nil, 'SpecimenDefinitionTypeTested', js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedContainerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'material', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'cap', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'capacity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'minimumVolumeQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'minimumVolumeString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'additive', 'SpecimenDefinitionTypeTestedContainerAdditive', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainer', 'preparation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionTypeTestedContainerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedContainer', nil, 'SpecimenDefinitionTypeTestedContainer', js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedContainerPropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedContainerAdditivePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainerAdditive', 'additiveCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedContainerAdditive', 'additiveReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSpecimenDefinitionTypeTestedContainerAdditiveJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedContainerAdditive', nil, 'SpecimenDefinitionTypeTestedContainerAdditive', js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedContainerAdditivePropsJs(js, def);
end;


procedure defineSpecimenDefinitionTypeTestedHandlingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling', 'temperatureQualifier', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling', 'temperatureRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling', 'maxDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinitionTypeTestedHandling', 'instruction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSpecimenDefinitionTypeTestedHandlingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinitionTypeTestedHandling', nil, 'SpecimenDefinitionTypeTestedHandling', js.FHIRFactoryJs);
  defineSpecimenDefinitionTypeTestedHandlingPropsJs(js, def);
end;


procedure defineSpecimenDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SpecimenDefinition', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'typeCollected', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SpecimenDefinition', 'patientPreparation', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition', 'timeAspect', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SpecimenDefinition', 'collection', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SpecimenDefinition', 'typeTested', 'SpecimenDefinitionTypeTested', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSpecimenDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SpecimenDefinition', nil, 'SpecimenDefinition', js.FHIRFactoryJs);
  defineSpecimenDefinitionPropsJs(js, def);
end;


procedure defineStructureDefinitionMappingPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionMapping', 'identity', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionMapping', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionMappingJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionMapping', nil, 'StructureDefinitionMapping', js.FHIRFactoryJs);
  defineStructureDefinitionMappingPropsJs(js, def);
end;


procedure defineStructureDefinitionContextPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionContext', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinitionContext', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureDefinitionContextJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionContext', nil, 'StructureDefinitionContext', js.FHIRFactoryJs);
  defineStructureDefinitionContextPropsJs(js, def);
end;


procedure defineStructureDefinitionSnapshotPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionSnapshot', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionSnapshotJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionSnapshot', nil, 'StructureDefinitionSnapshot', js.FHIRFactoryJs);
  defineStructureDefinitionSnapshotPropsJs(js, def);
end;


procedure defineStructureDefinitionDifferentialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureDefinitionDifferential', 'element', 'ElementDefinition', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureDefinitionDifferentialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinitionDifferential', nil, 'StructureDefinitionDifferential', js.FHIRFactoryJs);
  defineStructureDefinitionDifferentialPropsJs(js, def);
end;


procedure defineStructureDefinitionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureDefinition', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureDefinition', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'keyword', 'Coding', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'fhirVersion', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'mapping', 'StructureDefinitionMapping', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureDefinition', 'context', 'StructureDefinitionContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureDefinition', 'type', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'baseDefinition', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'derivation', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureDefinition', 'snapshot', 'StructureDefinitionSnapshot', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureDefinition', 'differential', 'StructureDefinitionDifferential', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineStructureDefinitionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureDefinition', nil, 'StructureDefinition', js.FHIRFactoryJs);
  defineStructureDefinitionPropsJs(js, def);
end;


procedure defineStructureMapStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapStructure', 'url', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'alias', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapStructure', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapStructure', nil, 'StructureMapStructure', js.FHIRFactoryJs);
  defineStructureMapStructurePropsJs(js, def);
end;


procedure defineStructureMapGroupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroup', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'extends', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'typeMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroup', 'input', 'StructureMapGroupInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroup', 'rule', 'StructureMapGroupRule', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroup', nil, 'StructureMapGroup', js.FHIRFactoryJs);
  defineStructureMapGroupPropsJs(js, def);
end;


procedure defineStructureMapGroupInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupInput', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'mode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupInput', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupInput', nil, 'StructureMapGroupInput', js.FHIRFactoryJs);
  defineStructureMapGroupInputPropsJs(js, def);
end;


procedure defineStructureMapGroupRulePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRule', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRule', 'source', 'StructureMapGroupRuleSource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'target', 'StructureMapGroupRuleTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'rule', '@StructureMap.group.rule', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'dependent', 'StructureMapGroupRuleDependent', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMapGroupRule', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRule', nil, 'StructureMapGroupRule', js.FHIRFactoryJs);
  defineStructureMapGroupRulePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleSourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'min', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'max', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'type', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'defaultValueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'listMode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'condition', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'check', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleSource', 'logMessage', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleSourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleSource', nil, 'StructureMapGroupRuleSource', js.FHIRFactoryJs);
  defineStructureMapGroupRuleSourcePropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'context', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'contextType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'element', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'variable', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'listRuleId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'transform', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTarget', 'parameter', 'StructureMapGroupRuleTargetParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapGroupRuleTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTarget', nil, 'StructureMapGroupRuleTarget', js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleTargetParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'StructureMapGroupRuleTargetParameter', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
end;

procedure defineStructureMapGroupRuleTargetParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleTargetParameter', nil, 'StructureMapGroupRuleTargetParameter', js.FHIRFactoryJs);
  defineStructureMapGroupRuleTargetParameterPropsJs(js, def);
end;


procedure defineStructureMapGroupRuleDependentPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'StructureMapGroupRuleDependent', 'name', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineStructureMapGroupRuleDependentJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMapGroupRuleDependent', nil, 'StructureMapGroupRuleDependent', js.FHIRFactoryJs);
  defineStructureMapGroupRuleDependentPropsJs(js, def);
end;


procedure defineStructureMapPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'StructureMap', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'StructureMap', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'StructureMap', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'StructureMap', 'structure', 'StructureMapStructure', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'StructureMap', 'group', 'StructureMapGroup', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineStructureMapJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('StructureMap', nil, 'StructureMap', js.FHIRFactoryJs);
  defineStructureMapPropsJs(js, def);
end;


procedure defineSubscriptionChannelPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubscriptionChannel', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'endpoint', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubscriptionChannel', 'payload', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubscriptionChannelJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubscriptionChannel', nil, 'SubscriptionChannel', js.FHIRFactoryJs);
  defineSubscriptionChannelPropsJs(js, def);
end;


procedure defineSubscriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Subscription', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'contact', 'ContactPoint', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Subscription', 'end', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Subscription', 'reason', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'criteria', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'error', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Subscription', 'channel', 'SubscriptionChannel', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubscriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Subscription', nil, 'Subscription', js.FHIRFactoryJs);
  defineSubscriptionPropsJs(js, def);
end;


procedure defineSubstanceInstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceInstance', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceInstance', 'expiry', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceInstance', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceInstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceInstance', nil, 'SubstanceInstance', js.FHIRFactoryJs);
  defineSubstanceInstancePropsJs(js, def);
end;


procedure defineSubstanceIngredientPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceIngredient', 'quantity', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceIngredient', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceIngredientJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceIngredient', nil, 'SubstanceIngredient', js.FHIRFactoryJs);
  defineSubstanceIngredientPropsJs(js, def);
end;


procedure defineSubstancePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Substance', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance', 'category', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Substance', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Substance', 'instance', 'SubstanceInstance', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Substance', 'ingredient', 'SubstanceIngredient', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Substance', nil, 'Substance', js.FHIRFactoryJs);
  defineSubstancePropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'subunit', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'sequence', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'length', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'sequenceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'fivePrime', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'threePrime', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'linkage', 'SubstanceNucleicAcidSubunitLinkage', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunit', 'sugar', 'SubstanceNucleicAcidSubunitSugar', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceNucleicAcidSubunitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunit', nil, 'SubstanceNucleicAcidSubunit', js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitPropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitLinkagePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage', 'connectivity', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitLinkage', 'residueSite', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceNucleicAcidSubunitLinkageJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunitLinkage', nil, 'SubstanceNucleicAcidSubunitLinkage', js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitLinkagePropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidSubunitSugarPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcidSubunitSugar', 'residueSite', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceNucleicAcidSubunitSugarJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcidSubunitSugar', nil, 'SubstanceNucleicAcidSubunitSugar', js.FHIRFactoryJs);
  defineSubstanceNucleicAcidSubunitSugarPropsJs(js, def);
end;


procedure defineSubstanceNucleicAcidPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceNucleicAcid', 'sequenceType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcid', 'numberOfSubunits', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceNucleicAcid', 'areaOfHybridisation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceNucleicAcid', 'oligoNucleotideType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceNucleicAcid', 'subunit', 'SubstanceNucleicAcidSubunit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceNucleicAcidJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceNucleicAcid', nil, 'SubstanceNucleicAcid', js.FHIRFactoryJs);
  defineSubstanceNucleicAcidPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSet', 'ratioType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSet', 'startingMaterial', 'SubstancePolymerMonomerSetStartingMaterial', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerMonomerSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSet', nil, 'SubstancePolymerMonomerSet', js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetPropsJs(js, def);
end;


procedure defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'material', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'isDefining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstancePolymerMonomerSetStartingMaterial', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerMonomerSetStartingMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerMonomerSetStartingMaterial', nil, 'SubstancePolymerMonomerSetStartingMaterial', js.FHIRFactoryJs);
  defineSubstancePolymerMonomerSetStartingMaterialPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeat', 'numberOfUnits', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'averageMolecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'repeatUnitAmountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeat', 'repeatUnit', 'SubstancePolymerRepeatRepeatUnit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeat', nil, 'SubstancePolymerRepeat', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'orientationOfPolymerisation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'repeatUnit', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'degreeOfPolymerisation', 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnit', 'structuralRepresentation', 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnit', nil, 'SubstancePolymerRepeatRepeatUnit', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', 'degree', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', 'amount', 'SubstanceAmount', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', nil, 'SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationPropsJs(js, def);
end;


procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'representation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymerRepeatRepeatUnitStructuralRepresentation', nil, 'SubstancePolymerRepeatRepeatUnitStructuralRepresentation', js.FHIRFactoryJs);
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationPropsJs(js, def);
end;


procedure defineSubstancePolymerPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstancePolymer', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer', 'geometry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstancePolymer', 'copolymerConnectivity', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer', 'monomerSet', 'SubstancePolymerMonomerSet', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstancePolymer', 'repeat', 'SubstancePolymerRepeat', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstancePolymerJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstancePolymer', nil, 'SubstancePolymer', js.FHIRFactoryJs);
  defineSubstancePolymerPropsJs(js, def);
end;


procedure defineSubstanceProteinSubunitPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceProteinSubunit', 'subunit', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'sequence', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'length', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'sequenceAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'nTerminalModificationId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'nTerminalModification', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'cTerminalModificationId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProteinSubunit', 'cTerminalModification', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceProteinSubunitJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceProteinSubunit', nil, 'SubstanceProteinSubunit', js.FHIRFactoryJs);
  defineSubstanceProteinSubunitPropsJs(js, def);
end;


procedure defineSubstanceProteinPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceProtein', 'sequenceType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceProtein', 'numberOfSubunits', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'SubstanceProtein', 'subunit', 'SubstanceProteinSubunit', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceProteinJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceProtein', nil, 'SubstanceProtein', js.FHIRFactoryJs);
  defineSubstanceProteinPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGenePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'geneSequenceOrigin', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'gene', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGene', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGene', nil, 'SubstanceReferenceInformationGene', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGenePropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationGeneElementPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'element', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationGeneElement', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationGeneElementJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationGeneElement', nil, 'SubstanceReferenceInformationGeneElement', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationGeneElementPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationClassificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'domain', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'classification', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'subtype', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformationClassification', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationClassificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationClassification', nil, 'SubstanceReferenceInformationClassification', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationClassificationPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationTargetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'target', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'interaction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organism', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'organismType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceReferenceInformationTarget', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationTargetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformationTarget', nil, 'SubstanceReferenceInformationTarget', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationTargetPropsJs(js, def);
end;


procedure defineSubstanceReferenceInformationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceReferenceInformation', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'gene', 'SubstanceReferenceInformationGene', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'geneElement', 'SubstanceReferenceInformationGeneElement', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'classification', 'SubstanceReferenceInformationClassification', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceReferenceInformation', 'target', 'SubstanceReferenceInformationTarget', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceReferenceInformationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceReferenceInformation', nil, 'SubstanceReferenceInformation', js.FHIRFactoryJs);
  defineSubstanceReferenceInformationPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialFractionDescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialFractionDescription', 'fraction', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialFractionDescription', 'materialType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialFractionDescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialFractionDescription', nil, 'SubstanceSourceMaterialFractionDescription', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialFractionDescriptionPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'family', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'genus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'species', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'intraspecificType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'intraspecificDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'author', 'SubstanceSourceMaterialOrganismAuthor', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'hybrid', 'SubstanceSourceMaterialOrganismHybrid', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganism', 'organismGeneral', 'SubstanceSourceMaterialOrganismOrganismGeneral', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganism', nil, 'SubstanceSourceMaterialOrganism', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismAuthorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismAuthor', 'authorType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismAuthor', 'authorDescription', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSourceMaterialOrganismAuthorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismAuthor', nil, 'SubstanceSourceMaterialOrganismAuthor', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismAuthorPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismHybridPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid', 'maternalOrganismId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid', 'maternalOrganismName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid', 'paternalOrganismId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid', 'paternalOrganismName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismHybrid', 'hybridType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismHybridJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismHybrid', nil, 'SubstanceSourceMaterialOrganismHybrid', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismHybridPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialOrganismOrganismGeneralPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral', 'kingdom', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral', 'phylum', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral', 'class', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialOrganismOrganismGeneral', 'order', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialOrganismOrganismGeneralJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialOrganismOrganismGeneral', nil, 'SubstanceSourceMaterialOrganismOrganismGeneral', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialOrganismOrganismGeneralPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialPartDescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterialPartDescription', 'part', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterialPartDescription', 'partLocation', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSourceMaterialPartDescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterialPartDescription', nil, 'SubstanceSourceMaterialPartDescription', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialPartDescriptionPropsJs(js, def);
end;


procedure defineSubstanceSourceMaterialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSourceMaterial', 'sourceMaterialClass', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'sourceMaterialType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'sourceMaterialState', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'organismId', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'organismName', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'parentSubstanceId', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'countryOfOrigin', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'developmentStage', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'fractionDescription', 'SubstanceSourceMaterialFractionDescription', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'organism', 'SubstanceSourceMaterialOrganism', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSourceMaterial', 'partDescription', 'SubstanceSourceMaterialPartDescription', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSourceMaterialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSourceMaterial', nil, 'SubstanceSourceMaterial', js.FHIRFactoryJs);
  defineSubstanceSourceMaterialPropsJs(js, def);
end;


procedure defineSubstanceSpecificationMoietyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'role', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationMoiety', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationMoietyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationMoiety', nil, 'SubstanceSpecificationMoiety', js.FHIRFactoryJs);
  defineSubstanceSpecificationMoietyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropertyPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'parameters', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'definingSubstanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'definingSubstanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationProperty', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineSubstanceSpecificationPropertyJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationProperty', nil, 'SubstanceSpecificationProperty', js.FHIRFactoryJs);
  defineSubstanceSpecificationPropertyPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'stereochemistry', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'opticalActivity', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularFormula', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularFormulaByMoiety', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'isotope', 'SubstanceSpecificationStructureIsotope', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationStructure', 'representation', 'SubstanceSpecificationStructureRepresentation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationStructureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructure', nil, 'SubstanceSpecificationStructure', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructurePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'name', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'substitution', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'halfLife', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotope', 'molecularWeight', 'SubstanceSpecificationStructureIsotopeMolecularWeight', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotope', nil, 'SubstanceSpecificationStructureIsotope', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopePropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'method', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureIsotopeMolecularWeight', 'amount', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureIsotopeMolecularWeight', nil, 'SubstanceSpecificationStructureIsotopeMolecularWeight', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureIsotopeMolecularWeightPropsJs(js, def);
end;


procedure defineSubstanceSpecificationStructureRepresentationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation', 'representation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationStructureRepresentation', 'attachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationStructureRepresentationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationStructureRepresentation', nil, 'SubstanceSpecificationStructureRepresentation', js.FHIRFactoryJs);
  defineSubstanceSpecificationStructureRepresentationPropsJs(js, def);
end;


procedure defineSubstanceSpecificationCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationCode', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationCode', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationCode', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SubstanceSpecificationCode', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationCode', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationCode', nil, 'SubstanceSpecificationCode', js.FHIRFactoryJs);
  defineSubstanceSpecificationCodePropsJs(js, def);
end;


procedure defineSubstanceSpecificationNamePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationName', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'preferred', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'language', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'domain', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'synonym', '@SubstanceSpecification.name', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'translation', '@SubstanceSpecification.name', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'official', 'SubstanceSpecificationNameOfficial', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecificationName', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationNameJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationName', nil, 'SubstanceSpecificationName', js.FHIRFactoryJs);
  defineSubstanceSpecificationNamePropsJs(js, def);
end;


procedure defineSubstanceSpecificationNameOfficialPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial', 'authority', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationNameOfficial', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineSubstanceSpecificationNameOfficialJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationNameOfficial', nil, 'SubstanceSpecificationNameOfficial', js.FHIRFactoryJs);
  defineSubstanceSpecificationNameOfficialPropsJs(js, def);
end;


procedure defineSubstanceSpecificationRelationshipPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'substanceReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'substanceCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'relationship', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'isDefining', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountRatioLowLimit', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'amountType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecificationRelationship', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSubstanceSpecificationRelationshipJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecificationRelationship', nil, 'SubstanceSpecificationRelationship', js.FHIRFactoryJs);
  defineSubstanceSpecificationRelationshipPropsJs(js, def);
end;


procedure defineSubstanceSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SubstanceSpecification', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'status', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'domain', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification', 'source', 'Reference(DocumentReference)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'comment', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SubstanceSpecification', 'moiety', 'SubstanceSpecificationMoiety', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'property', 'SubstanceSpecificationProperty', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'referenceInformation', 'Reference(SubstanceReferenceInformation)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'structure', 'SubstanceSpecificationStructure', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'code', 'SubstanceSpecificationCode', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'name', 'SubstanceSpecificationName', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'molecularWeight', '@SubstanceSpecification.structure.isotope.molecularWeight', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'relationship', 'SubstanceSpecificationRelationship', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SubstanceSpecification', 'nucleicAcid', 'Reference(SubstanceNucleicAcid)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'polymer', 'Reference(SubstancePolymer)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'protein', 'Reference(SubstanceProtein)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SubstanceSpecification', 'sourceMaterial', 'Reference(SubstanceSourceMaterial)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSubstanceSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SubstanceSpecification', nil, 'SubstanceSpecification', js.FHIRFactoryJs);
  defineSubstanceSpecificationPropsJs(js, def);
end;


procedure defineSupplyDeliverySuppliedItemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDeliverySuppliedItem', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyDeliverySuppliedItemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDeliverySuppliedItem', nil, 'SupplyDeliverySuppliedItem', js.FHIRFactoryJs);
  defineSupplyDeliverySuppliedItemPropsJs(js, def);
end;


procedure defineSupplyDeliveryPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyDelivery', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'basedOn', 'Reference(SupplyRequest)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'partOf', 'Reference(SupplyDelivery)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyDelivery', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyDelivery', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'suppliedItem', 'SupplyDeliverySuppliedItem', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'supplier', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'destination', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyDelivery', 'receiver', 'Reference(Practitioner)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineSupplyDeliveryJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyDelivery', nil, 'SupplyDelivery', js.FHIRFactoryJs);
  defineSupplyDeliveryPropsJs(js, def);
end;


procedure defineSupplyRequestParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'SupplyRequestParameter', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequestParameter', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineSupplyRequestParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequestParameter', nil, 'SupplyRequestParameter', js.FHIRFactoryJs);
  defineSupplyRequestParameterPropsJs(js, def);
end;


procedure defineSupplyRequestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'SupplyRequest', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest', 'category', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'SupplyRequest', 'itemCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'itemReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'quantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'parameter', 'SupplyRequestParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'occurrenceDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest', 'occurrencePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'occurrenceTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'SupplyRequest', 'requester', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'supplier', 'Reference(Organization)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonCode', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'reasonReference', 'Reference(Condition)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'SupplyRequest', 'deliverFrom', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'SupplyRequest', 'deliverTo', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineSupplyRequestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('SupplyRequest', nil, 'SupplyRequest', js.FHIRFactoryJs);
  defineSupplyRequestPropsJs(js, def);
end;


procedure defineTaskRestrictionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskRestriction', 'repetitions', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskRestriction', 'period', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskRestriction', 'recipient', 'Reference(Patient)', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskRestrictionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskRestriction', nil, 'TaskRestriction', js.FHIRFactoryJs);
  defineTaskRestrictionPropsJs(js, def);
end;


procedure defineTaskInputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskInput', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskInput', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskInput', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskInput', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskInput', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskInput', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskInput', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskInput', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskInputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskInput', nil, 'TaskInput', js.FHIRFactoryJs);
  defineTaskInputPropsJs(js, def);
end;


procedure defineTaskOutputPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TaskOutput', 'type', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueBase64Binary', 'base64Binary', js.getFHIRBinaryProp, js.setFHIRBinaryProp);
  js.registerElement(def, 'TaskOutput', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TaskOutput', 'valueCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TaskOutput', 'valueId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueInstant', 'instant', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TaskOutput', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput', 'valueMarkdown', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueOid', 'oid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valuePositiveInt', 'positiveInt', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TaskOutput', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueTime', 'time', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUnsignedInt', 'unsignedInt', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUrl', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueUuid', 'uuid', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TaskOutput', 'valueAddress', 'Address', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAge', 'Age', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAnnotation', 'Annotation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueAttachment', 'Attachment', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCodeableConcept', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCoding', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContactPoint', 'ContactPoint', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueCount', 'Count', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDistance', 'Distance', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDuration', 'Duration', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueHumanName', 'HumanName', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueMoney', 'Money', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valuePeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueQuantity', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRange', 'Range', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRatio', 'Ratio', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueReference', 'Reference', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueSampledData', 'SampledData', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueTiming', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContactDetail', 'ContactDetail', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueContributor', 'Contributor', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDataRequirement', 'DataRequirement', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueExpression', 'Expression', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueParameterDefinition', 'ParameterDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueRelatedArtifact', 'RelatedArtifact', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueTriggerDefinition', 'TriggerDefinition', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueUsageContext', 'UsageContext', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TaskOutput', 'valueDosage', 'Dosage', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTaskOutputJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TaskOutput', nil, 'TaskOutput', js.FHIRFactoryJs);
  defineTaskOutputPropsJs(js, def);
end;


procedure defineTaskPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'Task', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'instantiatesCanonical', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'instantiatesUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'basedOn', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'groupIdentifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'partOf', 'Reference(Task)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'statusReason', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'businessStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'intent', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'priority', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'code', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'Task', 'focus', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'for', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'executionPeriod', 'Period', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'authoredOn', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'lastModified', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'Task', 'requester', 'Reference(Device)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'performerType', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'owner', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'location', 'Reference(Location)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonCode', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'reasonReference', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'insurance', 'Reference(Coverage)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'relevantHistory', 'Reference(Provenance)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'restriction', 'TaskRestriction', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'Task', 'input', 'TaskInput', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'Task', 'output', 'TaskOutput', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTaskJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Task', nil, 'Task', js.FHIRFactoryJs);
  defineTaskPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesSoftwarePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesSoftware', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesSoftware', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesSoftwareJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesSoftware', nil, 'TerminologyCapabilitiesSoftware', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesSoftwarePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesImplementationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesImplementation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesImplementation', 'url', 'url', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesImplementationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesImplementation', nil, 'TerminologyCapabilitiesImplementation', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesImplementationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'uri', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'version', 'TerminologyCapabilitiesCodeSystemVersion', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystem', 'subsumption', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystem', nil, 'TerminologyCapabilitiesCodeSystem', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'code', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'isDefault', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'compositional', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersion', 'filter', 'TerminologyCapabilitiesCodeSystemVersionFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersion', nil, 'TerminologyCapabilitiesCodeSystemVersion', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesCodeSystemVersionFilter', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesCodeSystemVersionFilter', nil, 'TerminologyCapabilitiesCodeSystemVersionFilter', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesCodeSystemVersionFilterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'hierarchical', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'paging', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'incomplete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'parameter', 'TerminologyCapabilitiesExpansionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansion', 'textFilter', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansion', nil, 'TerminologyCapabilitiesExpansion', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesExpansionParameter', 'name', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilitiesExpansionParameter', 'documentation', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTerminologyCapabilitiesExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesExpansionParameter', nil, 'TerminologyCapabilitiesExpansionParameter', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesExpansionParameterPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesValidateCodePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesValidateCode', 'translations', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesValidateCodeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesValidateCode', nil, 'TerminologyCapabilitiesValidateCode', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesValidateCodePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesTranslationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesTranslation', 'needsMap', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesTranslationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesTranslation', nil, 'TerminologyCapabilitiesTranslation', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesTranslationPropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesClosurePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilitiesClosure', 'translation', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTerminologyCapabilitiesClosureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilitiesClosure', nil, 'TerminologyCapabilitiesClosure', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesClosurePropsJs(js, def);
end;


procedure defineTerminologyCapabilitiesPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TerminologyCapabilities', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TerminologyCapabilities', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'kind', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'software', 'TerminologyCapabilitiesSoftware', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'implementation', 'TerminologyCapabilitiesImplementation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'lockedDate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TerminologyCapabilities', 'codeSystem', 'TerminologyCapabilitiesCodeSystem', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TerminologyCapabilities', 'expansion', 'TerminologyCapabilitiesExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'codeSearch', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TerminologyCapabilities', 'validateCode', 'TerminologyCapabilitiesValidateCode', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'translation', 'TerminologyCapabilitiesTranslation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TerminologyCapabilities', 'closure', 'TerminologyCapabilitiesClosure', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTerminologyCapabilitiesJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TerminologyCapabilities', nil, 'TerminologyCapabilities', js.FHIRFactoryJs);
  defineTerminologyCapabilitiesPropsJs(js, def);
end;


procedure defineTestReportParticipantPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportParticipant', 'type', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant', 'uri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportParticipant', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportParticipantJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportParticipant', nil, 'TestReportParticipant', js.FHIRFactoryJs);
  defineTestReportParticipantPropsJs(js, def);
end;


procedure defineTestReportSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetup', 'action', 'TestReportSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetup', nil, 'TestReportSetup', js.FHIRFactoryJs);
  defineTestReportSetupPropsJs(js, def);
end;


procedure defineTestReportSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupAction', 'operation', 'TestReportSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportSetupAction', 'assert', 'TestReportSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupAction', nil, 'TestReportSetupAction', js.FHIRFactoryJs);
  defineTestReportSetupActionPropsJs(js, def);
end;


procedure defineTestReportSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionOperation', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionOperation', 'detail', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionOperation', nil, 'TestReportSetupActionOperation', js.FHIRFactoryJs);
  defineTestReportSetupActionOperationPropsJs(js, def);
end;


procedure defineTestReportSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportSetupActionAssert', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert', 'message', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportSetupActionAssert', 'detail', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestReportSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportSetupActionAssert', nil, 'TestReportSetupActionAssert', js.FHIRFactoryJs);
  defineTestReportSetupActionAssertPropsJs(js, def);
end;


procedure defineTestReportTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTest', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReportTest', 'action', 'TestReportTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTest', nil, 'TestReportTest', js.FHIRFactoryJs);
  defineTestReportTestPropsJs(js, def);
end;


procedure defineTestReportTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTestAction', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReportTestAction', 'assert', '@TestReport.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTestAction', nil, 'TestReportTestAction', js.FHIRFactoryJs);
  defineTestReportTestActionPropsJs(js, def);
end;


procedure defineTestReportTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardown', 'action', 'TestReportTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestReportTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardown', nil, 'TestReportTeardown', js.FHIRFactoryJs);
  defineTestReportTeardownPropsJs(js, def);
end;


procedure defineTestReportTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestReportTeardownAction', 'operation', '@TestReport.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReportTeardownAction', nil, 'TestReportTeardownAction', js.FHIRFactoryJs);
  defineTestReportTeardownActionPropsJs(js, def);
end;


procedure defineTestReportPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'TestReport', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'testScript', 'Reference(TestScript)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'result', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'score', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'TestReport', 'tester', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestReport', 'issued', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestReport', 'participant', 'TestReportParticipant', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport', 'setup', 'TestReportSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestReport', 'test', 'TestReportTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestReport', 'teardown', 'TestReportTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestReportJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestReport', nil, 'TestReport', js.FHIRFactoryJs);
  defineTestReportPropsJs(js, def);
end;


procedure defineTestScriptOriginPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptOrigin', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptOrigin', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptOriginJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptOrigin', nil, 'TestScriptOrigin', js.FHIRFactoryJs);
  defineTestScriptOriginPropsJs(js, def);
end;


procedure defineTestScriptDestinationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptDestination', 'index', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptDestination', 'profile', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptDestinationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptDestination', nil, 'TestScriptDestination', js.FHIRFactoryJs);
  defineTestScriptDestinationPropsJs(js, def);
end;


procedure defineTestScriptMetadataPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadata', 'link', 'TestScriptMetadataLink', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptMetadata', 'capability', 'TestScriptMetadataCapability', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptMetadataJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadata', nil, 'TestScriptMetadata', js.FHIRFactoryJs);
  defineTestScriptMetadataPropsJs(js, def);
end;


procedure defineTestScriptMetadataLinkPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataLink', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataLink', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataLinkJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataLink', nil, 'TestScriptMetadataLink', js.FHIRFactoryJs);
  defineTestScriptMetadataLinkPropsJs(js, def);
end;


procedure defineTestScriptMetadataCapabilityPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptMetadataCapability', 'required', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'validated', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptMetadataCapability', 'capabilities', 'canonical', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptMetadataCapabilityJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptMetadataCapability', nil, 'TestScriptMetadataCapability', js.FHIRFactoryJs);
  defineTestScriptMetadataCapabilityPropsJs(js, def);
end;


procedure defineTestScriptFixturePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptFixture', 'autocreate', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture', 'autodelete', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptFixture', 'resource', 'Reference(Any)', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptFixtureJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptFixture', nil, 'TestScriptFixture', js.FHIRFactoryJs);
  defineTestScriptFixturePropsJs(js, def);
end;


procedure defineTestScriptVariablePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptVariable', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'defaultValue', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'hint', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptVariable', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptVariableJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptVariable', nil, 'TestScriptVariable', js.FHIRFactoryJs);
  defineTestScriptVariablePropsJs(js, def);
end;


procedure defineTestScriptSetupPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetup', 'action', 'TestScriptSetupAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptSetupJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetup', nil, 'TestScriptSetup', js.FHIRFactoryJs);
  defineTestScriptSetupPropsJs(js, def);
end;


procedure defineTestScriptSetupActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupAction', 'operation', 'TestScriptSetupActionOperation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupAction', 'assert', 'TestScriptSetupActionAssert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptSetupActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupAction', nil, 'TestScriptSetupAction', js.FHIRFactoryJs);
  defineTestScriptSetupActionPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'type', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'accept', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'destination', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'encodeRequestUrl', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'method', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'origin', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'params', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'requestHeader', 'TestScriptSetupActionOperationRequestHeader', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'requestId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'responseId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'targetId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperation', 'url', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperation', nil, 'TestScriptSetupActionOperation', js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationPropsJs(js, def);
end;


procedure defineTestScriptSetupActionOperationRequestHeaderPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader', 'field', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionOperationRequestHeader', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineTestScriptSetupActionOperationRequestHeaderJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionOperationRequestHeader', nil, 'TestScriptSetupActionOperationRequestHeader', js.FHIRFactoryJs);
  defineTestScriptSetupActionOperationRequestHeaderPropsJs(js, def);
end;


procedure defineTestScriptSetupActionAssertPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'label', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'direction', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourceId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourceExpression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'compareToSourcePath', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'contentType', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'expression', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'headerField', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'minimumId', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'navigationLinks', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'operator', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'path', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'requestMethod', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'requestURL', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'resource', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'response', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'responseCode', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'sourceId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'validateProfileId', 'id', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptSetupActionAssert', 'warningOnly', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
end;

procedure defineTestScriptSetupActionAssertJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptSetupActionAssert', nil, 'TestScriptSetupActionAssert', js.FHIRFactoryJs);
  defineTestScriptSetupActionAssertPropsJs(js, def);
end;


procedure defineTestScriptTestPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTest', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'description', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScriptTest', 'action', 'TestScriptTestAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTestJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTest', nil, 'TestScriptTest', js.FHIRFactoryJs);
  defineTestScriptTestPropsJs(js, def);
end;


procedure defineTestScriptTestActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTestAction', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScriptTestAction', 'assert', '@TestScript.setup.action.assert', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTestActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTestAction', nil, 'TestScriptTestAction', js.FHIRFactoryJs);
  defineTestScriptTestActionPropsJs(js, def);
end;


procedure defineTestScriptTeardownPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardown', 'action', 'TestScriptTeardownAction', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineTestScriptTeardownJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardown', nil, 'TestScriptTeardown', js.FHIRFactoryJs);
  defineTestScriptTeardownPropsJs(js, def);
end;


procedure defineTestScriptTeardownActionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'TestScriptTeardownAction', 'operation', '@TestScript.setup.action.operation', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptTeardownActionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScriptTeardownAction', nil, 'TestScriptTeardownAction', js.FHIRFactoryJs);
  defineTestScriptTeardownActionPropsJs(js, def);
end;


procedure defineTestScriptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'TestScript', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'identifier', 'Identifier', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'TestScript', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'TestScript', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'TestScript', 'origin', 'TestScriptOrigin', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'destination', 'TestScriptDestination', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'metadata', 'TestScriptMetadata', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'fixture', 'TestScriptFixture', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'profile', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'variable', 'TestScriptVariable', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'setup', 'TestScriptSetup', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'TestScript', 'test', 'TestScriptTest', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'TestScript', 'teardown', 'TestScriptTeardown', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineTestScriptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('TestScript', nil, 'TestScript', js.FHIRFactoryJs);
  defineTestScriptPropsJs(js, def);
end;


procedure defineValueSetComposePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetCompose', 'lockedDate', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetCompose', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetCompose', 'include', 'ValueSetComposeInclude', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetCompose', 'exclude', '@ValueSet.compose.include', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetCompose', nil, 'ValueSetCompose', js.FHIRFactoryJs);
  defineValueSetComposePropsJs(js, def);
end;


procedure defineValueSetComposeIncludePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeInclude', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'concept', 'ValueSetComposeIncludeConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetComposeInclude', 'filter', 'ValueSetComposeIncludeFilter', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeInclude', nil, 'ValueSetComposeInclude', js.FHIRFactoryJs);
  defineValueSetComposeIncludePropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConcept', 'designation', 'ValueSetComposeIncludeConceptDesignation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetComposeIncludeConceptJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConcept', nil, 'ValueSetComposeIncludeConcept', js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeConceptDesignationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'language', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'use', 'Coding', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSetComposeIncludeConceptDesignation', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeConceptDesignationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeConceptDesignation', nil, 'ValueSetComposeIncludeConceptDesignation', js.FHIRFactoryJs);
  defineValueSetComposeIncludeConceptDesignationPropsJs(js, def);
end;


procedure defineValueSetComposeIncludeFilterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'property', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'op', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetComposeIncludeFilter', 'value', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineValueSetComposeIncludeFilterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetComposeIncludeFilter', nil, 'ValueSetComposeIncludeFilter', js.FHIRFactoryJs);
  defineValueSetComposeIncludeFilterPropsJs(js, def);
end;


procedure defineValueSetExpansionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansion', 'identifier', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansion', 'timestamp', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSetExpansion', 'total', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion', 'offset', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansion', 'parameter', 'ValueSetExpansionParameter', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansion', 'contains', 'ValueSetExpansionContains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansion', nil, 'ValueSetExpansion', js.FHIRFactoryJs);
  defineValueSetExpansionPropsJs(js, def);
end;


procedure defineValueSetExpansionParameterPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionParameter', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueString', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueBoolean', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueInteger', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueDecimal', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueUri', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueCode', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionParameter', 'valueDateTime', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
end;

procedure defineValueSetExpansionParameterJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionParameter', nil, 'ValueSetExpansionParameter', js.FHIRFactoryJs);
  defineValueSetExpansionParameterPropsJs(js, def);
end;


procedure defineValueSetExpansionContainsPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'ValueSetExpansionContains', 'system', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'abstract', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'inactive', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'code', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'display', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'designation', '@ValueSet.compose.include.concept.designation', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSetExpansionContains', 'contains', '@ValueSet.expansion.contains', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineValueSetExpansionContainsJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSetExpansionContains', nil, 'ValueSetExpansionContains', js.FHIRFactoryJs);
  defineValueSetExpansionContainsPropsJs(js, def);
end;


procedure defineValueSetPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineMetadataResourcePropsJs(js, def);
  js.registerElement(def, 'ValueSet', 'url', 'uri', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'version', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'name', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'title', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'experimental', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'date', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'ValueSet', 'publisher', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'contact', 'ContactDetail', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'description', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'useContext', 'UsageContext', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'jurisdiction', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'ValueSet', 'immutable', 'boolean', js.getFHIRBooleanProp, js.setFHIRBooleanProp);
  js.registerElement(def, 'ValueSet', 'purpose', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'copyright', 'markdown', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'ValueSet', 'compose', 'ValueSetCompose', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'ValueSet', 'expansion', 'ValueSetExpansion', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineValueSetJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('ValueSet', nil, 'ValueSet', js.FHIRFactoryJs);
  defineValueSetPropsJs(js, def);
end;


procedure defineVerificationResultPrimarySourcePropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultPrimarySource', 'who', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'type', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'communicationMethod', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationStatus', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'validationDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'canPushUpdates', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultPrimarySource', 'pushTypeAvailable', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultPrimarySourceJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultPrimarySource', nil, 'VerificationResultPrimarySource', js.FHIRFactoryJs);
  defineVerificationResultPrimarySourcePropsJs(js, def);
end;


procedure defineVerificationResultAttestationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultAttestation', 'who', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'onBehalfOf', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'communicationMethod', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'date', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResultAttestation', 'sourceIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation', 'proxyIdentityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultAttestation', 'proxySignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultAttestation', 'sourceSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineVerificationResultAttestationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultAttestation', nil, 'VerificationResultAttestation', js.FHIRFactoryJs);
  defineVerificationResultAttestationPropsJs(js, def);
end;


procedure defineVerificationResultValidatorPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VerificationResultValidator', 'organization', 'Reference(Organization)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResultValidator', 'identityCertificate', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResultValidator', 'attestationSignature', 'Signature', js.getFHIRObjectProp, js.setFHIRObjectProp);
end;

procedure defineVerificationResultValidatorJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResultValidator', nil, 'VerificationResultValidator', js.FHIRFactoryJs);
  defineVerificationResultValidatorPropsJs(js, def);
end;


procedure defineVerificationResultPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VerificationResult', 'target', 'Reference(Any)', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'need', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VerificationResult', 'statusDate', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'validationType', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'validationProcess', 'CodeableConcept', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'frequency', 'Timing', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'lastPerformed', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'nextScheduled', 'date', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VerificationResult', 'failureAction', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'primarySource', 'VerificationResultPrimarySource', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VerificationResult', 'attestation', 'VerificationResultAttestation', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VerificationResult', 'validator', 'VerificationResultValidator', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVerificationResultJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VerificationResult', nil, 'VerificationResult', js.FHIRFactoryJs);
  defineVerificationResultPropsJs(js, def);
end;


procedure defineVisionPrescriptionLensSpecificationPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'product', 'CodeableConcept', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'eye', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'sphere', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'cylinder', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'axis', 'integer', js.getFHIRIntegerProp, js.setFHIRIntegerProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'prism', 'VisionPrescriptionLensSpecificationPrism', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'add', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'power', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'backCurve', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'diameter', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'duration', 'Quantity', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'color', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'brand', 'string', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecification', 'note', 'Annotation', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionLensSpecificationJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionLensSpecification', nil, 'VisionPrescriptionLensSpecification', js.FHIRFactoryJs);
  defineVisionPrescriptionLensSpecificationPropsJs(js, def);
end;


procedure defineVisionPrescriptionLensSpecificationPrismPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineBackboneElementPropsJs(js, def);
  js.registerElement(def, 'VisionPrescriptionLensSpecificationPrism', 'amount', 'decimal', js.getFHIRDecimalProp, js.setFHIRDecimalProp);
  js.registerElement(def, 'VisionPrescriptionLensSpecificationPrism', 'base', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
end;

procedure defineVisionPrescriptionLensSpecificationPrismJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescriptionLensSpecificationPrism', nil, 'VisionPrescriptionLensSpecificationPrism', js.FHIRFactoryJs);
  defineVisionPrescriptionLensSpecificationPrismPropsJs(js, def);
end;


procedure defineVisionPrescriptionPropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);
begin
  defineDomainResourcePropsJs(js, def);
  js.registerElement(def, 'VisionPrescription', 'identifier', 'Identifier', js.getFHIRArrayProp, js.setFHIRArrayProp);
  js.registerElement(def, 'VisionPrescription', 'status', 'code', js.getFHIRStringProp, js.setFHIRStringProp);
  js.registerElement(def, 'VisionPrescription', 'created', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription', 'patient', 'Reference(Patient)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'encounter', 'Reference(Encounter)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'dateWritten', 'dateTime', js.getFHIRDateTimeProp, js.setFHIRDateTimeProp);
  js.registerElement(def, 'VisionPrescription', 'prescriber', 'Reference(Practitioner)', js.getFHIRObjectProp, js.setFHIRObjectProp);
  js.registerElement(def, 'VisionPrescription', 'lensSpecification', 'VisionPrescriptionLensSpecification', js.getFHIRArrayProp, js.setFHIRArrayProp);
end;

procedure defineVisionPrescriptionJs(js : TFHIRJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('VisionPrescription', nil, 'VisionPrescription', js.FHIRFactoryJs);
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
  defineDataRequirementSortJs(js); 
  defineDataRequirementJs(js); 
  defineDosageDoseAndRateJs(js); 
  defineDosageJs(js); 
  defineMoneyJs(js); 
  defineMarketingStatusJs(js); 
  defineIdentifierJs(js); 
  defineSubstanceAmountReferenceRangeJs(js); 
  defineSubstanceAmountJs(js); 
  defineCodingJs(js); 
  defineSampledDataJs(js); 
  definePopulationJs(js); 
  defineRatioJs(js); 
  defineReferenceJs(js); 
  defineTriggerDefinitionJs(js); 
  definePeriodJs(js); 
  defineQuantityJs(js); 
  defineRangeJs(js); 
  defineRelatedArtifactJs(js); 
  defineAnnotationJs(js); 
  defineProductShelfLifeJs(js); 
  defineContactDetailJs(js); 
  defineExpressionJs(js); 
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
  defineCatalogEntryRelatedEntryJs(js); 
  defineCatalogEntryJs(js); 
  defineChargeItemPerformerJs(js); 
  defineChargeItemJs(js); 
  defineChargeItemDefinitionApplicabilityJs(js); 
  defineChargeItemDefinitionPropertyGroupJs(js); 
  defineChargeItemDefinitionPropertyGroupPriceComponentJs(js); 
  defineChargeItemDefinitionJs(js); 
  defineClaimRelatedJs(js); 
  defineClaimPayeeJs(js); 
  defineClaimCareTeamJs(js); 
  defineClaimSupportingInfoJs(js); 
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
  defineClaimResponseAddItemDetailJs(js); 
  defineClaimResponseAddItemDetailSubDetailJs(js); 
  defineClaimResponseTotalJs(js); 
  defineClaimResponsePaymentJs(js); 
  defineClaimResponseProcessNoteJs(js); 
  defineClaimResponseInsuranceJs(js); 
  defineClaimResponseErrorJs(js); 
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
  defineContractContentDefinitionJs(js); 
  defineContractTermJs(js); 
  defineContractTermSecurityLabelJs(js); 
  defineContractTermOfferJs(js); 
  defineContractTermOfferPartyJs(js); 
  defineContractTermOfferAnswerJs(js); 
  defineContractTermAssetJs(js); 
  defineContractTermAssetContextJs(js); 
  defineContractTermAssetValuedItemJs(js); 
  defineContractTermActionJs(js); 
  defineContractTermActionSubjectJs(js); 
  defineContractSignerJs(js); 
  defineContractFriendlyJs(js); 
  defineContractLegalJs(js); 
  defineContractRuleJs(js); 
  defineContractJs(js); 
  defineCoverageClassJs(js); 
  defineCoverageCostToBeneficiaryJs(js); 
  defineCoverageCostToBeneficiaryExceptionJs(js); 
  defineCoverageJs(js); 
  defineCoverageEligibilityRequestSupportingInfoJs(js); 
  defineCoverageEligibilityRequestInsuranceJs(js); 
  defineCoverageEligibilityRequestItemJs(js); 
  defineCoverageEligibilityRequestItemDiagnosisJs(js); 
  defineCoverageEligibilityRequestJs(js); 
  defineCoverageEligibilityResponseInsuranceJs(js); 
  defineCoverageEligibilityResponseInsuranceItemJs(js); 
  defineCoverageEligibilityResponseInsuranceItemBenefitJs(js); 
  defineCoverageEligibilityResponseErrorJs(js); 
  defineCoverageEligibilityResponseJs(js); 
  defineDetectedIssueEvidenceJs(js); 
  defineDetectedIssueMitigationJs(js); 
  defineDetectedIssueJs(js); 
  defineDeviceUdiCarrierJs(js); 
  defineDeviceDeviceNameJs(js); 
  defineDeviceSpecializationJs(js); 
  defineDeviceVersionJs(js); 
  defineDevicePropertyJs(js); 
  defineDeviceJs(js); 
  defineDeviceDefinitionUdiDeviceIdentifierJs(js); 
  defineDeviceDefinitionDeviceNameJs(js); 
  defineDeviceDefinitionSpecializationJs(js); 
  defineDeviceDefinitionCapabilityJs(js); 
  defineDeviceDefinitionPropertyJs(js); 
  defineDeviceDefinitionMaterialJs(js); 
  defineDeviceDefinitionJs(js); 
  defineDeviceMetricCalibrationJs(js); 
  defineDeviceMetricJs(js); 
  defineDeviceRequestParameterJs(js); 
  defineDeviceRequestJs(js); 
  defineDeviceUseStatementJs(js); 
  defineDiagnosticReportMediaJs(js); 
  defineDiagnosticReportJs(js); 
  defineDocumentManifestRelatedJs(js); 
  defineDocumentManifestJs(js); 
  defineDocumentReferenceRelatesToJs(js); 
  defineDocumentReferenceContentJs(js); 
  defineDocumentReferenceContextJs(js); 
  defineDocumentReferenceJs(js); 
  defineEffectEvidenceSynthesisSampleSizeJs(js); 
  defineEffectEvidenceSynthesisResultsByExposureJs(js); 
  defineEffectEvidenceSynthesisEffectEstimateJs(js); 
  defineEffectEvidenceSynthesisEffectEstimatePrecisionEstimateJs(js); 
  defineEffectEvidenceSynthesisCertaintyJs(js); 
  defineEffectEvidenceSynthesisCertaintyCertaintySubcomponentJs(js); 
  defineEffectEvidenceSynthesisJs(js); 
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
  defineEpisodeOfCareStatusHistoryJs(js); 
  defineEpisodeOfCareDiagnosisJs(js); 
  defineEpisodeOfCareJs(js); 
  defineEventDefinitionJs(js); 
  defineEvidenceJs(js); 
  defineEvidenceVariableCharacteristicJs(js); 
  defineEvidenceVariableJs(js); 
  defineExampleScenarioActorJs(js); 
  defineExampleScenarioInstanceJs(js); 
  defineExampleScenarioInstanceVersionJs(js); 
  defineExampleScenarioInstanceContainedInstanceJs(js); 
  defineExampleScenarioProcessJs(js); 
  defineExampleScenarioProcessStepJs(js); 
  defineExampleScenarioProcessStepOperationJs(js); 
  defineExampleScenarioProcessStepAlternativeJs(js); 
  defineExampleScenarioJs(js); 
  defineExplanationOfBenefitRelatedJs(js); 
  defineExplanationOfBenefitPayeeJs(js); 
  defineExplanationOfBenefitCareTeamJs(js); 
  defineExplanationOfBenefitSupportingInfoJs(js); 
  defineExplanationOfBenefitDiagnosisJs(js); 
  defineExplanationOfBenefitProcedureJs(js); 
  defineExplanationOfBenefitInsuranceJs(js); 
  defineExplanationOfBenefitAccidentJs(js); 
  defineExplanationOfBenefitItemJs(js); 
  defineExplanationOfBenefitItemAdjudicationJs(js); 
  defineExplanationOfBenefitItemDetailJs(js); 
  defineExplanationOfBenefitItemDetailSubDetailJs(js); 
  defineExplanationOfBenefitAddItemJs(js); 
  defineExplanationOfBenefitAddItemDetailJs(js); 
  defineExplanationOfBenefitAddItemDetailSubDetailJs(js); 
  defineExplanationOfBenefitTotalJs(js); 
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
  defineHealthcareServiceEligibilityJs(js); 
  defineHealthcareServiceAvailableTimeJs(js); 
  defineHealthcareServiceNotAvailableJs(js); 
  defineHealthcareServiceJs(js); 
  defineImagingStudySeriesJs(js); 
  defineImagingStudySeriesPerformerJs(js); 
  defineImagingStudySeriesInstanceJs(js); 
  defineImagingStudyJs(js); 
  defineImmunizationPerformerJs(js); 
  defineImmunizationEducationJs(js); 
  defineImmunizationReactionJs(js); 
  defineImmunizationProtocolAppliedJs(js); 
  defineImmunizationJs(js); 
  defineImmunizationEvaluationJs(js); 
  defineImmunizationRecommendationRecommendationJs(js); 
  defineImmunizationRecommendationRecommendationDateCriterionJs(js); 
  defineImmunizationRecommendationJs(js); 
  defineImplementationGuideDependsOnJs(js); 
  defineImplementationGuideGlobalJs(js); 
  defineImplementationGuideDefinitionJs(js); 
  defineImplementationGuideDefinitionGroupingJs(js); 
  defineImplementationGuideDefinitionResourceJs(js); 
  defineImplementationGuideDefinitionPageJs(js); 
  defineImplementationGuideDefinitionParameterJs(js); 
  defineImplementationGuideDefinitionTemplateJs(js); 
  defineImplementationGuideManifestJs(js); 
  defineImplementationGuideManifestResourceJs(js); 
  defineImplementationGuideManifestPageJs(js); 
  defineImplementationGuideJs(js); 
  defineInsurancePlanContactJs(js); 
  defineInsurancePlanCoverageJs(js); 
  defineInsurancePlanCoverageBenefitJs(js); 
  defineInsurancePlanCoverageBenefitLimitJs(js); 
  defineInsurancePlanPlanJs(js); 
  defineInsurancePlanPlanGeneralCostJs(js); 
  defineInsurancePlanPlanSpecificCostJs(js); 
  defineInsurancePlanPlanSpecificCostBenefitJs(js); 
  defineInsurancePlanPlanSpecificCostBenefitCostJs(js); 
  defineInsurancePlanJs(js); 
  defineInvoiceParticipantJs(js); 
  defineInvoiceLineItemJs(js); 
  defineInvoiceLineItemPriceComponentJs(js); 
  defineInvoiceJs(js); 
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
  defineMeasureGroupStratifierComponentJs(js); 
  defineMeasureSupplementalDataJs(js); 
  defineMeasureJs(js); 
  defineMeasureReportGroupJs(js); 
  defineMeasureReportGroupPopulationJs(js); 
  defineMeasureReportGroupStratifierJs(js); 
  defineMeasureReportGroupStratifierStratumJs(js); 
  defineMeasureReportGroupStratifierStratumComponentJs(js); 
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
  defineMedicationKnowledgeRelatedMedicationKnowledgeJs(js); 
  defineMedicationKnowledgeMonographJs(js); 
  defineMedicationKnowledgeIngredientJs(js); 
  defineMedicationKnowledgeCostJs(js); 
  defineMedicationKnowledgeMonitoringProgramJs(js); 
  defineMedicationKnowledgeAdministrationGuidelinesJs(js); 
  defineMedicationKnowledgeAdministrationGuidelinesDosageJs(js); 
  defineMedicationKnowledgeAdministrationGuidelinesPatientCharacteristicsJs(js); 
  defineMedicationKnowledgeMedicineClassificationJs(js); 
  defineMedicationKnowledgePackagingJs(js); 
  defineMedicationKnowledgeDrugCharacteristicJs(js); 
  defineMedicationKnowledgeRegulatoryJs(js); 
  defineMedicationKnowledgeRegulatorySubstitutionJs(js); 
  defineMedicationKnowledgeRegulatoryScheduleJs(js); 
  defineMedicationKnowledgeRegulatoryMaxDispenseJs(js); 
  defineMedicationKnowledgeKineticsJs(js); 
  defineMedicationKnowledgeJs(js); 
  defineMedicationRequestDispenseRequestJs(js); 
  defineMedicationRequestDispenseRequestInitialFillJs(js); 
  defineMedicationRequestSubstitutionJs(js); 
  defineMedicationRequestJs(js); 
  defineMedicationStatementJs(js); 
  defineMedicinalProductNameJs(js); 
  defineMedicinalProductNameNamePartJs(js); 
  defineMedicinalProductNameCountryLanguageJs(js); 
  defineMedicinalProductManufacturingBusinessOperationJs(js); 
  defineMedicinalProductSpecialDesignationJs(js); 
  defineMedicinalProductJs(js); 
  defineMedicinalProductAuthorizationJurisdictionalAuthorizationJs(js); 
  defineMedicinalProductAuthorizationProcedureJs(js); 
  defineMedicinalProductAuthorizationJs(js); 
  defineMedicinalProductContraindicationOtherTherapyJs(js); 
  defineMedicinalProductContraindicationJs(js); 
  defineMedicinalProductIndicationOtherTherapyJs(js); 
  defineMedicinalProductIndicationJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthJs(js); 
  defineMedicinalProductIngredientSpecifiedSubstanceStrengthReferenceStrengthJs(js); 
  defineMedicinalProductIngredientSubstanceJs(js); 
  defineMedicinalProductIngredientJs(js); 
  defineMedicinalProductInteractionInteractantJs(js); 
  defineMedicinalProductInteractionJs(js); 
  defineMedicinalProductManufacturedJs(js); 
  defineMedicinalProductPackagedBatchIdentifierJs(js); 
  defineMedicinalProductPackagedPackageItemJs(js); 
  defineMedicinalProductPackagedJs(js); 
  defineMedicinalProductPharmaceuticalCharacteristicsJs(js); 
  defineMedicinalProductPharmaceuticalRouteOfAdministrationJs(js); 
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesJs(js); 
  defineMedicinalProductPharmaceuticalRouteOfAdministrationTargetSpeciesWithdrawalPeriodJs(js); 
  defineMedicinalProductPharmaceuticalJs(js); 
  defineMedicinalProductUndesirableEffectJs(js); 
  defineMessageDefinitionFocusJs(js); 
  defineMessageDefinitionAllowedResponseJs(js); 
  defineMessageDefinitionJs(js); 
  defineMessageHeaderDestinationJs(js); 
  defineMessageHeaderSourceJs(js); 
  defineMessageHeaderResponseJs(js); 
  defineMessageHeaderJs(js); 
  defineMolecularSequenceReferenceSeqJs(js); 
  defineMolecularSequenceVariantJs(js); 
  defineMolecularSequenceQualityJs(js); 
  defineMolecularSequenceQualityRocJs(js); 
  defineMolecularSequenceRepositoryJs(js); 
  defineMolecularSequenceStructureVariantJs(js); 
  defineMolecularSequenceStructureVariantOuterJs(js); 
  defineMolecularSequenceStructureVariantInnerJs(js); 
  defineMolecularSequenceJs(js); 
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
  defineOperationDefinitionParameterJs(js); 
  defineOperationDefinitionParameterBindingJs(js); 
  defineOperationDefinitionParameterReferencedFromJs(js); 
  defineOperationDefinitionOverloadJs(js); 
  defineOperationDefinitionJs(js); 
  defineOperationOutcomeIssueJs(js); 
  defineOperationOutcomeJs(js); 
  defineOrganizationContactJs(js); 
  defineOrganizationJs(js); 
  defineOrganizationAffiliationJs(js); 
  definePatientContactJs(js); 
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
  defineProvenanceAgentJs(js); 
  defineProvenanceEntityJs(js); 
  defineProvenanceJs(js); 
  defineQuestionnaireItemJs(js); 
  defineQuestionnaireItemEnableWhenJs(js); 
  defineQuestionnaireItemAnswerOptionJs(js); 
  defineQuestionnaireItemInitialJs(js); 
  defineQuestionnaireJs(js); 
  defineQuestionnaireResponseItemJs(js); 
  defineQuestionnaireResponseItemAnswerJs(js); 
  defineQuestionnaireResponseJs(js); 
  defineRelatedPersonCommunicationJs(js); 
  defineRelatedPersonJs(js); 
  defineRequestGroupActionJs(js); 
  defineRequestGroupActionConditionJs(js); 
  defineRequestGroupActionRelatedActionJs(js); 
  defineRequestGroupJs(js); 
  defineResearchDefinitionJs(js); 
  defineResearchElementDefinitionCharacteristicJs(js); 
  defineResearchElementDefinitionJs(js); 
  defineResearchStudyArmJs(js); 
  defineResearchStudyObjectiveJs(js); 
  defineResearchStudyJs(js); 
  defineResearchSubjectJs(js); 
  defineRiskAssessmentPredictionJs(js); 
  defineRiskAssessmentJs(js); 
  defineRiskEvidenceSynthesisSampleSizeJs(js); 
  defineRiskEvidenceSynthesisRiskEstimateJs(js); 
  defineRiskEvidenceSynthesisRiskEstimatePrecisionEstimateJs(js); 
  defineRiskEvidenceSynthesisCertaintyJs(js); 
  defineRiskEvidenceSynthesisCertaintyCertaintySubcomponentJs(js); 
  defineRiskEvidenceSynthesisJs(js); 
  defineScheduleJs(js); 
  defineSearchParameterComponentJs(js); 
  defineSearchParameterJs(js); 
  defineServiceRequestJs(js); 
  defineSlotJs(js); 
  defineSpecimenCollectionJs(js); 
  defineSpecimenProcessingJs(js); 
  defineSpecimenContainerJs(js); 
  defineSpecimenJs(js); 
  defineSpecimenDefinitionTypeTestedJs(js); 
  defineSpecimenDefinitionTypeTestedContainerJs(js); 
  defineSpecimenDefinitionTypeTestedContainerAdditiveJs(js); 
  defineSpecimenDefinitionTypeTestedHandlingJs(js); 
  defineSpecimenDefinitionJs(js); 
  defineStructureDefinitionMappingJs(js); 
  defineStructureDefinitionContextJs(js); 
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
  defineSubstanceNucleicAcidSubunitJs(js); 
  defineSubstanceNucleicAcidSubunitLinkageJs(js); 
  defineSubstanceNucleicAcidSubunitSugarJs(js); 
  defineSubstanceNucleicAcidJs(js); 
  defineSubstancePolymerMonomerSetJs(js); 
  defineSubstancePolymerMonomerSetStartingMaterialJs(js); 
  defineSubstancePolymerRepeatJs(js); 
  defineSubstancePolymerRepeatRepeatUnitJs(js); 
  defineSubstancePolymerRepeatRepeatUnitDegreeOfPolymerisationJs(js); 
  defineSubstancePolymerRepeatRepeatUnitStructuralRepresentationJs(js); 
  defineSubstancePolymerJs(js); 
  defineSubstanceProteinSubunitJs(js); 
  defineSubstanceProteinJs(js); 
  defineSubstanceReferenceInformationGeneJs(js); 
  defineSubstanceReferenceInformationGeneElementJs(js); 
  defineSubstanceReferenceInformationClassificationJs(js); 
  defineSubstanceReferenceInformationTargetJs(js); 
  defineSubstanceReferenceInformationJs(js); 
  defineSubstanceSourceMaterialFractionDescriptionJs(js); 
  defineSubstanceSourceMaterialOrganismJs(js); 
  defineSubstanceSourceMaterialOrganismAuthorJs(js); 
  defineSubstanceSourceMaterialOrganismHybridJs(js); 
  defineSubstanceSourceMaterialOrganismOrganismGeneralJs(js); 
  defineSubstanceSourceMaterialPartDescriptionJs(js); 
  defineSubstanceSourceMaterialJs(js); 
  defineSubstanceSpecificationMoietyJs(js); 
  defineSubstanceSpecificationPropertyJs(js); 
  defineSubstanceSpecificationStructureJs(js); 
  defineSubstanceSpecificationStructureIsotopeJs(js); 
  defineSubstanceSpecificationStructureIsotopeMolecularWeightJs(js); 
  defineSubstanceSpecificationStructureRepresentationJs(js); 
  defineSubstanceSpecificationCodeJs(js); 
  defineSubstanceSpecificationNameJs(js); 
  defineSubstanceSpecificationNameOfficialJs(js); 
  defineSubstanceSpecificationRelationshipJs(js); 
  defineSubstanceSpecificationJs(js); 
  defineSupplyDeliverySuppliedItemJs(js); 
  defineSupplyDeliveryJs(js); 
  defineSupplyRequestParameterJs(js); 
  defineSupplyRequestJs(js); 
  defineTaskRestrictionJs(js); 
  defineTaskInputJs(js); 
  defineTaskOutputJs(js); 
  defineTaskJs(js); 
  defineTerminologyCapabilitiesSoftwareJs(js); 
  defineTerminologyCapabilitiesImplementationJs(js); 
  defineTerminologyCapabilitiesCodeSystemJs(js); 
  defineTerminologyCapabilitiesCodeSystemVersionJs(js); 
  defineTerminologyCapabilitiesCodeSystemVersionFilterJs(js); 
  defineTerminologyCapabilitiesExpansionJs(js); 
  defineTerminologyCapabilitiesExpansionParameterJs(js); 
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
  defineTestScriptSetupJs(js); 
  defineTestScriptSetupActionJs(js); 
  defineTestScriptSetupActionOperationJs(js); 
  defineTestScriptSetupActionOperationRequestHeaderJs(js); 
  defineTestScriptSetupActionAssertJs(js); 
  defineTestScriptTestJs(js); 
  defineTestScriptTestActionJs(js); 
  defineTestScriptTeardownJs(js); 
  defineTestScriptTeardownActionJs(js); 
  defineTestScriptJs(js); 
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
  defineVisionPrescriptionLensSpecificationJs(js); 
  defineVisionPrescriptionLensSpecificationPrismJs(js); 
  defineVisionPrescriptionJs(js); 

end;

end.

